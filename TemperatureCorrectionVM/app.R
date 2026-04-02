library(shiny)
library(tidyverse)
library(tidymodels)
library(kknn)
library(ggthemes)
library(DT)
library(shinythemes)
library(shinyjs)
library(kernlab)
library(shinycssloaders)
library(ranger)
library(baguette)
library(rules)
library(dlookr)
library(plotly)

tidymodels_prefer()
theme_set(ggthemes::theme_clean(base_size = 24))

# Define UI for application that draws a histogram
my_ui <- fluidPage(
    theme = shinytheme("cerulean"),
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .btn-disabled {
        pointer-events: none;
        opacity: 0.5;
      }
    ")),

    # Application title
    titlePanel("Temperature-corrected heating load"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("algorithms", "Select machine-learning algorithm",
                         choiceNames = list(
                           HTML(paste0("K-Nearest Neighbour <a href='https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm'",
                                       "target='_blank'>Wiki</a>")),
                           HTML(paste0("Random Forest <a href='https://en.wikipedia.org/wiki/Random_forest'",
                                       "target='_blank'>Wiki</a>"))),
                         choiceValues = list("my_knn", "my_rf")),
            fileInput("normal_file",
                      "Select a CSV containing independent calendar variables (monts, days, hours...) and normal temperature",
                      accept = ".csv"),
            fileInput("my_file",
                      "Select a CSV containing independent calendar variables. It must contain the variables: 'temperature' and 'load'",
                      accept = ".csv"),
            numericInput("my_neighbours", "Select number of neighbours for the KNN model", 2, min = 2, max = 20, step = 1),
            numericInput("my_trees", "Select number of trees for the RF model", 300, min = 100, max = 1000, step = 100),
            sliderInput("pointsize", "Adjust the size of the points in the graphs", min = 1, max = 5, value = 1, step = 1),
            downloadButton("download_result", "Download result table as CSV", class = "btn btn-primary")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            withSpinner(plotlyOutput("mainPlot", height = "600px"), type = 1),
            withSpinner(plotlyOutput("secondaryPlot", height = "600px"), type = 1),
            withSpinner(DTOutput("my_DT"), type = 1)
        )
    )
)

# Define server logic required to draw a histogram
my_server <- function(input, output, session) {
  Sys.setlocale(category = "LC_ALL", locale = "Swedish.UTF-8")
  
  shinyjs::disable("download_result")
  shinyjs::addClass("download_result", "btn-disabled")
  
  remove_outliers <- function(data, group, value) {
    data %>%
      group_by(!!sym(group)) %>%
      mutate(Q1 = quantile(!!sym(value), 0.25),
             Q3 = quantile(!!sym(value), 0.75),
             IQR = Q3-Q1,
             lower_bound = Q1 - 1.5 * IQR,
             upper_bound = Q3 + 1.5 * IQR,
             outlier = !!sym(value) < lower_bound | !!sym(value) > upper_bound) %>%
      filter(!outlier) %>%
      select(-Q1, -Q3, -IQR, -lower_bound, -upper_bound, -outlier)
  }
  
  reactive_ursprungsprofil <- reactive({
      my_file <- input$my_file
      req(my_file)
      result <- read_csv2(my_file$datapath, col_types = "n")
      
      return(result)
  })
  
  reactive_profile_no_outliers <- reactive({
    reactive_ursprungsprofil() %>%
      # mutate(temperature_bin = binning(temperature, nbins = 20, type = "quantile")) %>%
      remove_outliers(., "temperature_bin", "load")
  })
  
  reactive_selected_temp <- reactive({
    my_file2 <- input$normal_file
    req(my_file2)
    result2 <- read_csv2(my_file2$datapath, col_types = "n")
    
    return(result2)
  })
  
  my_recipes <- reactive({
    recipe(load ~ ., data = reactive_profile_no_outliers()) %>%
      step_normalize(temperature, id = "temp_normalize") %>%
      step_normalize(load, skip = TRUE, id = "load_normalize")
  })
  
  # unnormalize <- function(pred) {
  #   tidy_rec <- my_recipes %>%
  #     prep() %>%
  #     tidy(., id = "load_normalize")
  #   
  #   sd <- filter(tidy_rec, statistic == "sd") %>% pull(value)
  #   means <- filter(tidy_rec, statistic == "mean") %>% pull(value)
  #   
  #   unnormalized_pred <- pred * sd + means
  #   
  #   return(unnormalized_pred)
  # }
  
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

# Run the application 
shinyApp(ui = my_ui, server = my_server)
