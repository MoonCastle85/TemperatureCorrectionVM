library(shiny)
library(tidyverse)
library(tidymodels)
library(kknn)
library(ggthemes)
library(DT)
library(shinythemes)
library(shinyjs)
library(shinycssloaders)
library(plotly)

tidymodels_prefer()
theme_set(ggthemes::theme_clean(base_size = 20))

project_root <- "."
pred_functions <- list.files(file.path(project_root, "Functions"), pattern = "\\.R$", full.names = TRUE)
walk(pred_functions, source)

required_profile_cols <- c("month", "weekday", "day", "time", "hour", "temperature", "heat_load_kw")
required_temp_cols <- c("year", "month", "weekday", "day", "time", "hour", "temperature")
csv2_locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")
strict_mode_target_max_load <- 63
prediction_mode_choices <- setNames(
  c("standard", "strict_fixed_peak_63"),
  c(
    "Standard",
    paste0("Climate-adjusted strict (max load = ", strict_mode_target_max_load, ")")
  )
)

assert_required_columns <- function(data, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        label,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

get_csv_header <- function(path) {
  header_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  header_line <- sub("^\ufeff", "", header_line)
  header_cols <- strsplit(header_line, ";", fixed = TRUE)[[1]]
  gsub('^"|"$', "", header_cols)
}

read_required_csv2 <- function(path, required_cols, label) {
  header <- set_names(vector("list", length(get_csv_header(path))), get_csv_header(path))
  assert_required_columns(header, required_cols, label)

  suppressMessages(
    read_csv2(
      path,
      col_select = all_of(required_cols),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE,
      locale = csv2_locale
    )
  )
}

parse_mixed_decimal_number <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- str_trim(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  normalized <- map_chr(x_chr, \(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    comma_pos <- gregexpr(",", value, fixed = TRUE)[[1]]
    dot_pos <- gregexpr(".", value, fixed = TRUE)[[1]]
    has_comma <- comma_pos[1] != -1
    has_dot <- dot_pos[1] != -1

    if (has_comma && has_dot) {
      if (max(comma_pos) > max(dot_pos)) {
        value <- str_replace_all(value, stringr::fixed("."), "")
        str_replace(value, stringr::fixed(","), ".")
      } else {
        str_replace_all(value, stringr::fixed(","), "")
      }
    } else if (has_comma) {
      value <- str_replace_all(value, stringr::fixed("."), "")
      str_replace(value, stringr::fixed(","), ".")
    } else {
      str_replace_all(value, stringr::fixed(","), "")
    }
  })

  readr::parse_double(
    normalized,
    locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
    na = c("", "NA")
  )
}

coerce_required_numeric <- function(data, required_cols) {
  data %>%
    mutate(
      across(
        all_of(required_cols),
        parse_mixed_decimal_number
      )
    )
}

assert_usable_profile_rows <- function(data, stop_temp_c) {
  usable_rows <- data %>%
    filter(
      !is.na(temperature),
      !is.na(heat_load_kw),
      !is.na(is_weekend),
      temperature < stop_temp_c
    )

  if (nrow(usable_rows) == 0) {
    stop(
      paste0(
        "Heating profile file has no usable rows below the stop temperature (",
        stop_temp_c,
        " degC). Check the numeric values and decimal separators in the required columns."
      ),
      call. = FALSE
    )
  }

  data
}

read_standard_profile <- function(path) {
  read_required_csv2(
    path,
    required_cols = required_profile_cols,
    label = "Heating profile file"
  ) %>%
    coerce_required_numeric(required_profile_cols) %>%
    mutate(is_weekend = if_else(weekday >= 6, 1, 0)) %>%
    arrange(hour)
}

get_temperature_uploads <- function(file_info) {
  if (is.null(file_info) || nrow(file_info) == 0) {
    stop("Select one or more temperature CSV files.", call. = FALSE)
  }

  csv_files <- file_info %>%
    filter(grepl("\\.csv$", name, ignore.case = TRUE)) %>%
    distinct(name, size, .keep_all = TRUE)

  if (nrow(csv_files) == 0) {
    stop("No CSV files were selected.", call. = FALSE)
  }

  csv_files
}

read_temperature_file <- function(file_path, file_name) {
  read_required_csv2(
    file_path,
    required_cols = required_temp_cols,
    label = paste0("Temperature file '", file_name, "'")
  ) %>%
    coerce_required_numeric(required_temp_cols) %>%
    mutate(
      source_file = tools::file_path_sans_ext(basename(file_name)),
      is_weekend = if_else(weekday >= 6, 1, 0),
      profile_id = paste0(year, " - ", source_file)
    )
}

read_temperature_upload <- function(file_info) {
  csv_files <- get_temperature_uploads(file_info)

  map2(
    csv_files$datapath,
    csv_files$name,
    read_temperature_file
  ) %>%
    list_rbind() %>%
    arrange(year, source_file, hour) %>%
    select(profile_id, source_file, year, month, weekday, day, time, hour, temperature, is_weekend)
}

empty_prediction <- function(template) {
  template[0, , drop = FALSE] %>%
    mutate(pred_load_new = numeric())
}

predict_temperature_profile <- function(
  temp_profile,
  orig_profile,
  model_fits,
  stop_temp_c = 15,
  prediction_mode = "standard",
  target_max_load = 63
) {
  heating_input <- temp_profile %>%
    filter(temperature < stop_temp_c)

  summer_input <- temp_profile %>%
    filter(temperature >= stop_temp_c)

  heating_prediction <- if (nrow(heating_input) > 0) {
    pred_lm_and_knn(heating_input, model_fits, stop_temp_c = stop_temp_c)
  } else {
    empty_prediction(temp_profile)
  }

  summer_prediction <- if (nrow(summer_input) > 0) {
    get_pred_load_summer(
      data = orig_profile,
      new = summer_input,
      stop_temp_c = stop_temp_c
    )
  } else {
    empty_prediction(temp_profile)
  }

  combined_prediction <- bind_rows(heating_prediction, summer_prediction) %>%
    arrange(hour) %>%
    mutate(season = if_else(temperature < stop_temp_c, "heating", "summer"))

  apply_prediction_mode(
    combined_prediction,
    prediction_mode = prediction_mode,
    target_max_load = target_max_load,
    stop_temp_c = stop_temp_c
  )
}

my_ui <- fluidPage(
  theme = shinytheme("cerulean"),
  shinyjs::useShinyjs(),
  tags$style(HTML("
    .btn-disabled {
      pointer-events: none;
      opacity: 0.5;
    }
  ")),
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var input = document.getElementById('temp_files');
      if (input) {
        input.setAttribute('multiple', '');
      }
    });
  ")),
  titlePanel("Temperature-corrected heating load"),
  sidebarLayout(
    sidebarPanel(
      helpText("Required heating profile columns: month, weekday, day, time, hour, temperature, heat_load_kw"),
      fileInput(
        "profile_file",
        "Heating profile CSV",
        accept = ".csv"
      ),
      helpText("Select one temperature CSV file or multiple CSV files."),
      helpText("Required temperature file columns: year, month, weekday, day, time, hour, temperature"),
      fileInput(
        "temp_files",
        "Temperature CSV file(s)",
        multiple = TRUE,
        accept = ".csv"
      ),
      numericInput(
        "stop_temp_c",
        "Heating stop temperature (degC)",
        value = 15,
        min = 5,
        max = 40,
        step = 0.5
      ),
      selectInput(
        "prediction_mode",
        "Prediction mode",
        choices = prediction_mode_choices,
        selected = "standard"
      ),
      helpText(paste0("Strict mode rescales each predicted profile so its maximum load is exactly ", strict_mode_target_max_load, ".")),
      sliderInput(
        "pointsize",
        "Adjust the size of the points in the graphs",
        min = 1,
        max = 5,
        value = 1,
        step = 1
      ),
      actionButton("run_model", "Run Calculation", class = "btn btn-success"),
      uiOutput("draw_button_ui"),
      helpText("Run Calculation updates the result table and download. Draw Graphs updates the plots for the latest calculation."),
      uiOutput("progress_status_ui"),
      uiOutput("download_button_ui")
    ),
    mainPanel(
      withSpinner(plotlyOutput("mainPlot", height = "600px"), type = 1),
      withSpinner(plotlyOutput("secondaryPlot", height = "600px"), type = 1),
      withSpinner(DTOutput("my_DT"), type = 1)
    )
  )
)

if (FALSE) {
  knn_spec_triang <- reactive({
    nearest_neighbor(weight_func = "triangular", neighbors = input$my_neighbours) %>% 
    set_engine("kknn") %>% 
    set_mode("regression")
  })
  
  rf_spec <- reactive({
    rand_forest(trees = input$my_trees) %>%
    set_engine("ranger") %>%
    set_mode("regression")
  })

  my_workflows <- reactive({
    result <- workflow_set(preproc = list(rec = my_recipes()),
                           models = list(KNN = knn_spec_triang(), RF = rf_spec()))
  })
  
  fit_all <- reactive({
    fit_all <- map(.x = my_workflows()$wflow_id, 
                   .f = \(x) fit(extract_workflow(my_workflows(), x), data = reactive_profile_no_outliers()))
    
    return(fit_all)
  })
  
  knn_result <- reactive({
    knn_pred <- predict(fit_all()[[1]], new_data = reactive_selected_temp())
    result_knn <- bind_cols(reactive_selected_temp(), predikterat = round(knn_pred, 1))
    
    return(result_knn)
  })

  rf_result <- reactive({
    rf_pred <- predict(fit_all()[[2]], new_data = reactive_selected_temp())
    result_rf <- bind_cols(reactive_selected_temp(), predikterat = round(rf_pred, 1))
    
    return(result_rf)
  })
  
  orig_data <- reactive({
    reactive_ursprungsprofil() %>%
      rename(original_temperature = temperature, original_load = load)
  })
  
  final_knn <- reactive({
    bind_cols(orig_data(), knn_result()) %>%
    dplyr::select(original_temperature, original_load, temperature, .pred) %>%
    rename(temperature_normal = temperature, predicted_knn_load = .pred)
  })

  final_rf <- reactive({
      bind_cols(orig_data(), rf_result()) %>%
      dplyr::select(original_temperature, original_load, temperature, .pred) %>%
      rename(predicted_rf_load = .pred)
  })

  selected_algorithm_data <- reactive({
    switch(input$algorithms, "my_knn" = knn_result(), "my_rf" = rf_result())
  })
  
  selected_table_data <- reactive({
    switch(input$algorithms, "my_knn" = final_knn(), "my_rf" = final_rf())
  })
  
  output$mainPlot <- renderPlotly({
      g1 = ggplot() +
          geom_point(data = reactive_ursprungsprofil(), 
                     mapping = aes(x = temperature, y = load, colour = "Original"), size = input$pointsize) +
          geom_point(data = selected_algorithm_data(), 
                     mapping = aes(x = temperature, y = .pred, colour = "Predicted"), size = input$pointsize, shape = 21) +
          labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)") +
          scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                              name = "Data source") +
          scale_x_continuous(n.breaks = 10) +
          scale_y_continuous(n.breaks = 10)
      
      ggplotly(g1)
  })
  
  output$secondaryPlot <- renderPlotly({
    g2 = ggplot() +
      geom_point(data = reactive_ursprungsprofil(),
                 mapping = aes(x = seq(1,length(reactive_ursprungsprofil()$temperature),1), y = load, colour = "Original"),
                 size = input$pointsize) +
      geom_point(data = selected_algorithm_data(),
                 mapping = aes(x = seq(1,8760,1), y = .pred, colour = "Predicted"),
                 size = input$pointsize, shape = 21) +
      labs(title = "Load vs temperature data", x = "Hour of the year", y = "Heating load (kW or MW)") +
      scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                          name = "Data source") +
      scale_x_continuous(n.breaks = 12) +
      scale_y_continuous(n.breaks = 10)

    ggplotly(g2)
  })
  
  output$my_DT <- DT::renderDT({
      datatable(selected_table_data(),
                options = list(initComplete = JS("function(settings, json) {",
                                                 "  Shiny.setInputValue('table_rendered', true);", "}")
                )
      )
  })
  
  observeEvent(input$table_rendered, {
      shinyjs::enable("download_result")
      shinyjs::removeClass("download_result", "btn-disabled")
  })
  
  output$download_result <- downloadHandler(
      filename = function() {
          paste0("Corrected_load_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".csv")
      },
      content = function(file) {
          write_csv2(selected_table_data(), file)
      }
  )
}

my_server <- function(input, output, session) {
  calculation_bundle <- reactiveVal(NULL)
  calculation_error <- reactiveVal(NULL)
  calculation_run_id <- reactiveVal(0L)
  graph_bundle <- reactiveVal(NULL)
  drawn_run_id <- reactiveVal(0L)
  progress_total <- reactiveVal(0L)
  progress_done <- reactiveVal(0L)
  progress_detail <- reactiveVal("No calculation has been run yet.")

  bundle_status_message <- function() {
    if (!is.null(calculation_error())) {
      calculation_error()
    } else {
      "Press Run Calculation to generate results."
    }
  }

  graph_status_message <- function() {
    if (is.null(calculation_bundle())) {
      return(bundle_status_message())
    }

    if (is.null(graph_bundle()) || drawn_run_id() != calculation_run_id()) {
      return("Press Draw Graphs to render plots for the latest calculation.")
    }

    NULL
  }

  observeEvent(input$run_model, {
    req(input$profile_file)
    req(input$temp_files)

    calculation_error(NULL)
    progress_total(0L)
    progress_done(0L)
    progress_detail("Reading inputs and fitting the heating profile model.")

    tryCatch({
      prediction_mode <- input$prediction_mode
      prediction_mode_label <- get_prediction_mode_label(
        prediction_mode,
        target_max_load = strict_mode_target_max_load
      )
      profile_data <- read_standard_profile(input$profile_file$datapath) %>%
        assert_usable_profile_rows(stop_temp_c = input$stop_temp_c)
      csv_files <- get_temperature_uploads(input$temp_files)
      total_files <- nrow(csv_files)

      progress_total(total_files)
      progress_done(0L)
      progress_detail(
        sprintf(
          "Processed 0 of %d temperature file(s) in %s.",
          total_files,
          prediction_mode_label
        )
      )

      predictions <- withProgress(message = "Running calculation", value = 0, {
        incProgress(0, detail = paste0("Fitting the heating profile model for ", prediction_mode_label, "."))
        model_fits <- fit_lm_and_knn(profile_data, stop_temp_c = input$stop_temp_c)
        predictions_by_file <- vector("list", total_files)

        for (i in seq_len(total_files)) {
          temp_profiles <- read_temperature_file(csv_files$datapath[[i]], csv_files$name[[i]])

          predictions_by_file[[i]] <- temp_profiles %>%
            group_split(profile_id, .keep = TRUE) %>%
            map(\(temp_profile) {
              predict_temperature_profile(
                temp_profile = temp_profile,
                orig_profile = profile_data,
                model_fits = model_fits,
                stop_temp_c = input$stop_temp_c,
                prediction_mode = prediction_mode,
                target_max_load = strict_mode_target_max_load
              )
            }) %>%
            list_rbind()

          progress_done(i)
          progress_detail(
            sprintf(
              "Processed %d of %d temperature file(s) in %s: %s",
              i,
              total_files,
              prediction_mode_label,
              csv_files$name[[i]]
            )
          )
          incProgress(1 / total_files, detail = progress_detail())
        }

        predictions_by_file %>%
          list_rbind() %>%
          arrange(year, source_file, hour)
      })

      calculation_bundle(list(
        original_profile = profile_data,
        predictions = predictions,
        prediction_mode = prediction_mode,
        prediction_mode_label = prediction_mode_label,
        target_max_load = strict_mode_target_max_load
      ))
      calculation_run_id(calculation_run_id() + 1L)
      progress_done(total_files)
      progress_detail(
        sprintf(
          "Calculation complete in %s. Processed %d of %d temperature file(s).",
          prediction_mode_label,
          total_files,
          total_files
        )
      )

      showNotification(
        paste0(
          "Calculation complete in ",
          prediction_mode_label,
          ". Processed ",
          total_files,
          " temperature file(s)."
        ),
        type = "message"
      )
    }, error = function(e) {
      calculation_error(conditionMessage(e))
      progress_detail(paste0("Calculation failed: ", conditionMessage(e)))
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$draw_graphs, {
    req(calculation_bundle())

    graph_bundle(calculation_bundle())
    drawn_run_id(calculation_run_id())
  }, ignoreInit = TRUE)

  output$draw_button_ui <- renderUI({
    if (!is.null(calculation_bundle())) {
      actionButton("draw_graphs", "Draw Graphs", class = "btn btn-info")
    } else {
      tags$button(
        "Draw Graphs",
        class = "btn btn-info btn-disabled",
        disabled = "disabled"
      )
    }
  })

  output$progress_status_ui <- renderUI({
    total <- progress_total()
    done <- progress_done()
    detail <- progress_detail()
    percent <- if (total > 0) {
      round(100 * min(done, total) / total)
    } else {
      0
    }
    bar_class <- if (grepl("^Calculation failed:", detail)) {
      "progress-bar-danger"
    } else if (total > 0 && done >= total && grepl("^Calculation complete", detail)) {
      "progress-bar-success"
    } else {
      "progress-bar-info"
    }

    tags$div(
      style = "margin-top: 12px;",
      tags$strong("Progress"),
      tags$div(
        class = "progress",
        style = "margin-top: 8px; margin-bottom: 8px;",
        tags$div(
          class = paste("progress-bar", bar_class),
          role = "progressbar",
          style = sprintf("width: %d%%;", percent),
          paste0(percent, "%")
        )
      ),
      tags$div(detail)
    )
  })

  output$download_button_ui <- renderUI({
    if (!is.null(calculation_bundle())) {
      downloadButton("download_result", "Download result table as CSV", class = "btn btn-primary")
    } else {
      tags$button(
        "Download result table as CSV",
        class = "btn btn-primary btn-disabled",
        disabled = "disabled"
      )
    }
  })

  original_profile <- reactive({
    bundle <- calculation_bundle()
    req(bundle)
    bundle$original_profile
  })

  all_predictions <- reactive({
    bundle <- calculation_bundle()
    req(bundle)
    bundle$predictions
  })

  plotted_original_profile <- reactive({
    bundle <- graph_bundle()
    req(bundle)
    bundle$original_profile
  })

  plotted_predictions <- reactive({
    bundle <- graph_bundle()
    req(bundle)
    bundle$predictions
  })

  plotted_prediction_mode_label <- reactive({
    bundle <- graph_bundle()
    req(bundle)
    bundle$prediction_mode_label
  })

  original_plot_data <- reactive({
    predictions <- plotted_predictions()

    tidyr::crossing(
      profile_id = unique(predictions$profile_id),
      plotted_original_profile()
    )
  })

  output$mainPlot <- renderPlotly({
    graph_message <- graph_status_message()
    validate(need(is.null(graph_message), graph_message))
    predictions <- plotted_predictions()

    validate(need(nrow(predictions) > 0, "No predictions could be generated for the selected inputs."))

    g1 <- ggplot() +
      geom_point(
        data = original_plot_data(),
        mapping = aes(x = temperature, y = heat_load_kw, colour = "Original"),
        size = input$pointsize,
        alpha = 0.5
      ) +
      geom_point(
        data = predictions,
        mapping = aes(x = temperature, y = pred_load_new, colour = "Predicted"),
        size = input$pointsize,
        alpha = 0.6
      ) +
      facet_wrap(~profile_id) +
      labs(
        title = "Load vs temperature",
        subtitle = plotted_prediction_mode_label(),
        x = "Outdoor temperature (degC)",
        y = "Heating load (kW)"
      ) +
      scale_colour_manual(
        values = c("Original" = "blue", "Predicted" = "red"),
        name = "Data source"
      )

    ggplotly(g1)
  })

  output$secondaryPlot <- renderPlotly({
    graph_message <- graph_status_message()
    validate(need(is.null(graph_message), graph_message))
    predictions <- plotted_predictions()

    validate(need(nrow(predictions) > 0, "No predictions could be generated for the selected inputs."))

    g2 <- ggplot() +
      geom_point(
        data = original_plot_data(),
        mapping = aes(x = hour, y = heat_load_kw, colour = "Original"),
        size = input$pointsize,
        alpha = 0.5
      ) +
      geom_point(
        data = predictions,
        mapping = aes(x = hour, y = pred_load_new, colour = "Predicted"),
        size = input$pointsize,
        alpha = 0.6
      ) +
      facet_wrap(~profile_id) +
      labs(
        title = "Hourly load profile",
        subtitle = plotted_prediction_mode_label(),
        x = "Hour of the year",
        y = "Heating load (kW)"
      ) +
      scale_colour_manual(
        values = c("Original" = "blue", "Predicted" = "red"),
        name = "Data source"
      )

    ggplotly(g2)
  })

  output$my_DT <- DT::renderDT({
    validate(need(
      !is.null(calculation_bundle()),
      bundle_status_message()
    ))
    predictions <- all_predictions()

    validate(need(nrow(predictions) > 0, "No predictions could be generated for the selected inputs."))

    datatable(
      predictions %>%
        select(profile_id, source_file, year, month, weekday, day, time, hour, temperature, season, pred_load_new) %>%
        head(10),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  output$download_result <- downloadHandler(
    filename = function() {
      bundle <- calculation_bundle()
      req(bundle)
      mode_suffix <- if (bundle$prediction_mode == "strict_fixed_peak_63") {
        paste0("strict_", bundle$target_max_load)
      } else {
        "standard"
      }

      paste0("Corrected_load_", mode_suffix, "_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".csv")
    },
    content = function(file) {
      write_csv2(all_predictions(), file)
    }
  )
}

shinyApp(ui = my_ui, server = my_server)
