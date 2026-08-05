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
prediction_mode_choices <- setNames(
  c("standard", "strict"),
  c("Standard", "Strict")
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
  restriction_spec = NULL
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
    restriction_spec = restriction_spec
  )
}

round_half_up <- function(x, digits = 0) {
  scale <- 10 ^ digits
  sign(x) * floor(abs(x) * scale + 0.5) / scale
}

build_default_rounding_span <- function(value, rounding_unit, display_digits) {
  increment <- 10 ^ (-display_digits)
  lower_bound <- pmax(value - rounding_unit / 2, 0)
  upper_bound <- value + rounding_unit / 2 - increment

  list(
    min = as.double(lower_bound),
    max = as.double(upper_bound)
  )
}

build_monthly_restriction_table <- function(profile_data) {
  monthly_reference <- profile_data %>%
    group_by(month) %>%
    summarise(original_monthly_sum = sum(heat_load_kw, na.rm = TRUE), .groups = "drop")

  tibble(month = 1:12, month_label = month.abb) %>%
    left_join(monthly_reference, by = "month") %>%
    arrange(month) %>%
    mutate(
      original_monthly_sum = round_half_up(as.double(original_monthly_sum), digits = -3),
      monthly_span = map(original_monthly_sum, ~build_default_rounding_span(.x, rounding_unit = 1000, display_digits = 0)),
      min_heat_load_kw_sum = map_dbl(monthly_span, "min"),
      max_heat_load_kw_sum = map_dbl(monthly_span, "max")
    ) %>%
    select(month, month_label, original_monthly_sum, min_heat_load_kw_sum, max_heat_load_kw_sum)
}

build_daily_restriction_table <- function(profile_data, max_yearday = 10L) {
  profile_data %>%
    mutate(yearday = calculate_yearday(month, day)) %>%
    group_by(yearday) %>%
    summarise(original_daily_average = mean(heat_load_kw, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(original_daily_average), yearday) %>%
    slice_head(n = max_yearday) %>%
    mutate(
      rank = row_number(),
      original_daily_average = round_half_up(as.double(original_daily_average), digits = 0),
      daily_span = map(original_daily_average, ~build_default_rounding_span(.x, rounding_unit = 1, display_digits = 1)),
      min_heat_load_kw_daily_avg = map_dbl(daily_span, "min"),
      max_heat_load_kw_daily_avg = map_dbl(daily_span, "max")
    ) %>%
    select(rank, yearday, original_daily_average, min_heat_load_kw_daily_avg, max_heat_load_kw_daily_avg)
}

apply_table_edit <- function(data, edit_info, display_columns, lower_col, upper_col, label) {
  if (is.null(data) || is.null(edit_info)) {
    return(data)
  }

  column_name <- display_columns[[edit_info$col + 1]]

  if (is.na(column_name) || !nzchar(column_name)) {
    return(data)
  }

  new_value <- parse_mixed_decimal_number(edit_info$value)

  if (!is.finite(new_value)) {
    stop(paste0(label, " only accepts numeric values in editable cells."), call. = FALSE)
  }

  if (new_value < 0) {
    stop(paste0(label, " does not allow negative bounds."), call. = FALSE)
  }

  updated <- data
  updated[[column_name]][edit_info$row] <- as.double(new_value)

  invalid_rows <- which(updated[[lower_col]] > updated[[upper_col]])

  if (length(invalid_rows) > 0) {
    stop(paste0(label, " has one or more rows where the lower bound is greater than the upper bound."), call. = FALSE)
  }

  updated
}

build_strict_restriction_spec <- function(monthly_table, daily_table) {
  if (is.null(monthly_table) || is.null(daily_table)) {
    stop("Upload a heating profile to configure strict-mode restrictions.", call. = FALSE)
  }

  assert_required_columns(
    monthly_table,
    c("month", "original_monthly_sum", "min_heat_load_kw_sum", "max_heat_load_kw_sum"),
    "Monthly strict restriction table"
  )
  assert_required_columns(
    daily_table,
    c("yearday", "original_daily_average", "min_heat_load_kw_daily_avg", "max_heat_load_kw_daily_avg"),
    "Daily strict restriction table"
  )

  if (any(!is.finite(monthly_table$original_monthly_sum))) {
    stop("Monthly strict restriction table contains missing reference sums. Check the uploaded heating profile.", call. = FALSE)
  }

  if (any(!is.finite(daily_table$original_daily_average))) {
    stop("Daily strict restriction table contains missing reference daily averages. Check the uploaded heating profile.", call. = FALSE)
  }

  if (any(!is.finite(monthly_table$min_heat_load_kw_sum)) || any(!is.finite(monthly_table$max_heat_load_kw_sum))) {
    stop("Monthly strict restriction table contains missing or invalid bounds.", call. = FALSE)
  }

  if (any(!is.finite(daily_table$min_heat_load_kw_daily_avg)) || any(!is.finite(daily_table$max_heat_load_kw_daily_avg))) {
    stop("Daily strict restriction table contains missing or invalid bounds.", call. = FALSE)
  }

  if (any(monthly_table$min_heat_load_kw_sum > monthly_table$max_heat_load_kw_sum)) {
    stop("Monthly strict restriction table contains rows where the lower bound is greater than the upper bound.", call. = FALSE)
  }

  if (any(daily_table$min_heat_load_kw_daily_avg > daily_table$max_heat_load_kw_daily_avg)) {
    stop("Daily strict restriction table contains rows where the lower bound is greater than the upper bound.", call. = FALSE)
  }

  list(
    monthly = monthly_table %>%
      transmute(month, min_heat_load_kw_sum, max_heat_load_kw_sum),
    daily = daily_table %>%
      transmute(yearday, min_heat_load_kw_daily_avg, max_heat_load_kw_daily_avg)
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
      style = "max-height: calc(100vh - 140px); overflow-y: auto;",
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
      wellPanel(
        class = "prediction-settings-card",
        style = "margin-bottom: 15px;",
        tags$strong("Prediction settings"),
        tabsetPanel(
          id = "prediction_settings_tabset",
          selected = "mode_tab",
          tabPanel(
            title = "Mode",
            value = "mode_tab",
            br(),
            selectInput(
              "prediction_mode",
              "Prediction mode",
              choices = prediction_mode_choices,
              selected = "standard"
            ),
            helpText("Strict mode adds editable monthly and daily restrictions in the Strict restrictions tab.")
          ),
          tabPanel(
            title = "Strict restrictions",
            value = "strict_tab",
            br(),
            uiOutput("strict_mode_ui")
          )
        )
      ),
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

my_server <- function(input, output, session) {
  calculation_bundle <- reactiveVal(NULL)
  calculation_error <- reactiveVal(NULL)
  calculation_run_id <- reactiveVal(0L)
  graph_bundle <- reactiveVal(NULL)
  drawn_run_id <- reactiveVal(0L)
  progress_total <- reactiveVal(0L)
  progress_done <- reactiveVal(0L)
  progress_detail <- reactiveVal("No calculation has been run yet.")
  monthly_restrictions <- reactiveVal(NULL)
  daily_restrictions <- reactiveVal(NULL)

  uploaded_profile_data <- reactive({
    req(input$profile_file)

    read_standard_profile(input$profile_file$datapath)
  })

  uploaded_profile_for_model <- reactive({
    uploaded_profile_data() %>%
      assert_usable_profile_rows(stop_temp_c = input$stop_temp_c)
  })

  strict_reference_tables <- reactive({
    profile_data <- uploaded_profile_data()

    list(
      monthly = build_monthly_restriction_table(profile_data),
      daily = build_daily_restriction_table(profile_data)
    )
  })

  observeEvent(input$profile_file, {
    monthly_restrictions(NULL)
    daily_restrictions(NULL)
  }, ignoreInit = TRUE)

  observeEvent(strict_reference_tables(), {
    reference_tables <- strict_reference_tables()
    monthly_restrictions(reference_tables$monthly)
    daily_restrictions(reference_tables$daily)
  })

  strict_profile_ready <- reactive({
    !is.null(monthly_restrictions()) && !is.null(daily_restrictions())
  })

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

  observeEvent(input$prediction_mode, {
    selected_tab <- if (identical(input$prediction_mode, "strict")) {
      "strict_tab"
    } else {
      "mode_tab"
    }

    updateTabsetPanel(session, "prediction_settings_tabset", selected = selected_tab)
  }, ignoreInit = FALSE)

  observeEvent(input$run_model, {
    req(input$profile_file)
    req(input$temp_files)

    calculation_error(NULL)
    progress_total(0L)
    progress_done(0L)
    progress_detail("Reading inputs and fitting the heating profile model.")

    tryCatch({
      prediction_mode <- input$prediction_mode
      prediction_mode_label <- get_prediction_mode_label(prediction_mode)
      restriction_spec <- if (prediction_mode == "strict") {
        build_strict_restriction_spec(
          monthly_restrictions(),
          daily_restrictions()
        )
      } else {
        NULL
      }
      profile_data <- uploaded_profile_for_model()
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
                restriction_spec = restriction_spec
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
        prediction_mode_label = prediction_mode_label
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

  output$strict_mode_ui <- renderUI({
    if (input$prediction_mode != "strict") {
      return(helpText("Select strict prediction mode in the Mode tab to enable the restriction tables."))
    }

    if (!strict_profile_ready()) {
      return(helpText("Upload a valid heating profile to configure strict-mode monthly sums and year-day 1-10 daily averages."))
    }

    tagList(
      helpText("Strict mode runs the standard prediction first and then adjusts the predicted results to fit the configured monthly and daily spans."),
      tags$strong("Monthly sums"),
      tags$div(style = "overflow-x: auto; margin-top: 8px;", DTOutput("strict_monthly_table")),
      tags$div(style = "height: 12px;"),
      tags$strong("Highest daily averages (top 10 year-days)"),
      tags$div(style = "overflow-x: auto; margin-top: 8px;", DTOutput("strict_daily_table"))
    )
  })

  output$strict_monthly_table <- renderDT({
    req(input$prediction_mode == "strict")
    table_data <- monthly_restrictions()
    validate(need(strict_profile_ready() && !is.null(table_data), "Upload a valid heating profile to configure monthly restrictions."))

    datatable(
      table_data %>%
        transmute(
          Month = month_label,
          `Original sum` = original_monthly_sum,
          `Min sum` = min_heat_load_kw_sum,
          `Max sum` = max_heat_load_kw_sum
        ),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      class = "compact stripe"
    ) %>%
      formatRound(columns = c("Original sum", "Min sum", "Max sum"), digits = 0)
  }, server = FALSE)

  output$strict_daily_table <- renderDT({
    req(input$prediction_mode == "strict")
    table_data <- daily_restrictions()
    validate(need(strict_profile_ready() && !is.null(table_data), "Upload a valid heating profile to configure daily restrictions."))

    datatable(
      table_data %>%
        transmute(
          Rank = rank,
          `Original daily average` = original_daily_average,
          `Min daily average` = min_heat_load_kw_daily_avg,
          `Max daily average` = max_heat_load_kw_daily_avg
        ),
      rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = c(0, 1))),
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        scrollX = TRUE
      ),
      class = "compact stripe"
    ) %>%
      formatRound(columns = c("Original daily average", "Min daily average", "Max daily average"), digits = 1)
  }, server = FALSE)

  observeEvent(input$strict_monthly_table_cell_edit, {
    tryCatch({
      monthly_restrictions(
        apply_table_edit(
          monthly_restrictions(),
          input$strict_monthly_table_cell_edit,
          display_columns = c(NA_character_, NA_character_, "min_heat_load_kw_sum", "max_heat_load_kw_sum"),
          lower_col = "min_heat_load_kw_sum",
          upper_col = "max_heat_load_kw_sum",
          label = "Monthly strict restriction table"
        )
      )
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
  }, ignoreInit = TRUE)

  observeEvent(input$strict_daily_table_cell_edit, {
    tryCatch({
      daily_restrictions(
        apply_table_edit(
          daily_restrictions(),
          input$strict_daily_table_cell_edit,
          display_columns = c(NA_character_, NA_character_, "min_heat_load_kw_daily_avg", "max_heat_load_kw_daily_avg"),
          lower_col = "min_heat_load_kw_daily_avg",
          upper_col = "max_heat_load_kw_daily_avg",
          label = "Daily strict restriction table"
        )
      )
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = NULL)
    })
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
      mode_suffix <- if (bundle$prediction_mode == "strict") {
        "strict"
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

