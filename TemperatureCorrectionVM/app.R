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

normal_temp <- read_csv2("https://manborgconsulting.com/Normaltemperaturer.csv", col_types = "cnnnnnn")

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
                           HTML(paste0("Articifial Neural Network <a href='https://en.wikipedia.org/wiki/Neural_network_(machine_learning)'",
                                       "target='_blank'>Wiki</a>")),
                           HTML(paste0("Random Forest <a href='https://en.wikipedia.org/wiki/Random_forest'",
                                       "target='_blank'>Wiki</a>"))),
                         choiceValues = list("my_knn", "my_ann", "my_rf")),
            selectInput(inputId = "municipalities",
                        label = "Select municipality for normal temperatures",
                        choices = distinct(normal_temp, Ort) %>% pull(Ort),
                        selected = NULL), 
            fileInput("my_file", "Select a CSV file that must have these columns: Month (1-12), Hour (0-23), Temperature (°C) and 
                      Load (kW or MW)", accept = ".csv"),
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
      result <- read_csv2(my_file$datapath, col_types = "nnnn", 
                                   col_names = c("Month", "Hour", "Temperature", "Load"), skip = 1) %>%
        mutate(Season = case_when(Month %in% c(1,2,12) ~ "Winter",
                                  Month %in% c(3,4,5) ~ "Spring",
                                  Month %in% c(9,10,11) ~ "Autumn",
                                  Month %in% c(6,7,8) ~ "Summer"))
      
      return(result)
  })
  
  reactive_profile_no_outliers <- reactive({
    reactive_ursprungsprofil() %>%
      mutate(Temperature_bin = binning(Temperature, nbins = 20, type = "quantile")) %>%
      remove_outliers(., "Temperature_bin", "Load")
  })
  
  reactive_selected_temp <- reactive({
    dplyr::filter(normal_temp, Ort == input$municipalities) %>%
    mutate(Season = case_when(Month %in% c(1,2,12) ~ "Winter",
                              Month %in% c(3,4,5) ~ "Spring",
                              Month %in% c(9,10,11) ~ "Autumn",
                              Month %in% c(6,7,8) ~ "Summer"))
  })
  
  my_recipes <- reactive({
    recipe(Load ~ Month + Temperature + Hour, data = reactive_profile_no_outliers()) %>%
     step_normalize(Temperature, id = "temp_normalize") %>%
     step_normalize(Load, skip = TRUE, id = "load_normalize")
  })
  
  unnormalize <- function(pred) {
    tidy_rec <- my_recipes %>%
      prep() %>%
      tidy(., id = "load_normalize")
    
    sd <- filter(tidy_rec, statistic == "sd") %>% pull(value)
    means <- filter(tidy_rec, statistic == "mean") %>% pull(value)
    
    unnormalized_pred <- pred * sd + means
    
    return(unnormalized_pred)
  }
  
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
    result_knn <- bind_cols(reactive_selected_temp() %>% select(Ort, Temperature), Predikterat = round(knn_pred, 1))
    
    return(result_knn)
  })

  rf_result <- reactive({
    rf_pred <- predict(fit_all()[[2]], new_data = reactive_selected_temp())
    result_rf <- bind_cols(reactive_selected_temp() %>% select(Ort, Temperature), Predikterat = round(rf_pred, 1))
    
    return(result_rf)
  })
  
  orig_data <- reactive({
    reactive_ursprungsprofil() %>%
      rename(Original_temperature = Temperature, Original_load = Load)
  })
  
  final_knn <- reactive({
    bind_cols(orig_data(), knn_result()) %>%
    dplyr::select(Ort, Month, Hour, Original_temperature, Original_load, Temperature, .pred) %>%
    rename(Temperature_normal = Temperature, Predicted_normal_load_using_knn = .pred)
  })
  
  final_rf <- reactive({
      bind_cols(orig_data(), rf_result()) %>%
      dplyr::select(Ort, Month, Hour, Original_temperature, Original_load, Temperature, .pred) %>%
      rename(Predicted_normal_load_using_rf = .pred)
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
                     mapping = aes(x = Temperature, y = Load, colour = "Original"), size = input$pointsize) +
          geom_point(data = selected_algorithm_data(), 
                     mapping = aes(x = Temperature, y = .pred, colour = "Predicted"), size = input$pointsize, shape = 21) +
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
                 mapping = aes(x = seq(1,length(reactive_ursprungsprofil()$Month),1), y = Load, colour = "Original"), 
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
