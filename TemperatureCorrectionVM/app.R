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
library(dlookr)
library(performance)

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
                           HTML(paste0("Support Vector Machines <a href='https://en.wikipedia.org/wiki/Support_vector_machine'",
                                       "target='_blank'>Wiki</a>"))),
                         choiceValues = list("my_knn", "my_svm")),
            selectInput(inputId = "municipalities",
                        label = "Select municipality for normal temperatures",
                        choices = distinct(normal_temp, Ort) %>% pull(Ort),
                        selected = NULL), 
            fileInput("my_file", "Select a CSV file that has three columns: Hour (0-23), Temperature (°C) and Load (kW)", 
                      accept = ".csv"),
            numericInput("my_neighbours", "Select number of neighbours for the KNN model", 5, min = 2, max = 20, step = 1),
            downloadButton("download_result", "Download result table as CSV", class = "btn btn-primary")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            withSpinner(plotOutput("mainPlot", height = "600px"), type = 1),
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
        ursprungsprofil <- read_csv2(my_file$datapath, col_types = "nnnn", 
                                     col_names = c("Month", "Hour", "Temperature", "Load"), skip = 1) %>%
          mutate(Season = case_when(Month %in% c(1,2,3,12) ~ "Winter", 
                                    Month %in% c(4,5,10,11) ~ "Spring/Autumn",
                                    Month %in% c(6,7,8,9) ~ "Summer"))
        
        return(ursprungsprofil)
    })
    
    reactive_profil_no_outliers <- reactive({
      no_outliers <- reactive_ursprungsprofil() %>%
        mutate(Temperature_bin = binning(Temperature, nbins = 20, type = "quantile")) %>%
        remove_outliers(., "Temperature_bin", "Load")
      
      return(no_outliers)
    })
    
    reactive_selected_temp <- reactive({
      selected_temp <- dplyr::filter(normal_temp, Ort == input$municipalities) %>%
        mutate(Season = case_when(Month %in% c(1,2,3,12) ~ "Winter", 
                                  Month %in% c(4,5,10,11) ~ "Spring/Autumn",
                                  Month %in% c(6,7,8,9) ~ "Summer"))
      
      return(selected_temp)
    })
    
    knn_result <- reactive({
        my_knn <- nearest_neighbor(neighbors = input$my_neighbours) %>%
            set_mode("regression") %>%
            set_engine("kknn")
        
        my_knn_rec <- recipe(Load ~ Season + Temperature + Hour, data = reactive_profil_no_outliers()) %>%
          step_normalize(Temperature, id = "temp_normalize") %>%
          step_normalize(Load, skip = TRUE, id = "load_normalize")
        
        my_knn_wf <- workflow() %>%
          add_recipe(my_knn_rec) %>%
          add_model(my_knn)
        
        prep_knn <- my_knn_rec %>%
          prep()
        
        load_knn_mean <- tidy(prep_knn, id = "load_normalize") %>%
          filter(terms == "Load", statistic == "mean") %>%
          pull(value)
        
        load_knn_sd <- tidy(prep_knn, id = "load_normalize") %>%
          filter(terms == "Load", statistic == "sd") %>%
          pull(value)
        
        my_knn_fit <- fit(my_knn_wf, data = reactive_profil_no_outliers())
        my_knn_pred <- predict(my_knn_fit, new_data = reactive_selected_temp())
        my_knn_pred_unnormalized <- my_knn_pred * load_knn_sd + load_knn_mean
        
        resultat_knn <- bind_cols(reactive_selected_temp(), Predikterat = my_knn_pred_unnormalized)
        
        return(resultat_knn)
    })
    
    svm_result <- reactive({
      my_svm <- svm_rbf() %>%
        set_mode("regression") %>%
        set_engine("kernlab")
      
      my_svm_rec <- recipe(Load ~ Season + Temperature + Hour, data = reactive_profil_no_outliers()) %>%
        step_normalize(Temperature, id = "temp_normalize") %>%
        step_normalize(Load, skip = TRUE, id = "load_normalize")
      
      my_svm_wf <- workflow() %>%
        add_recipe(my_svm_rec) %>%
        add_model(my_svm)
      
      prep_svm <- my_svm_rec %>%
        prep()
      
      load_svm_mean <- tidy(prep_svm, id = "load_normalize") %>%
        filter(terms == "Load", statistic == "mean") %>%
        pull(value)
      
      load_svm_sd <- tidy(prep_svm, id = "load_normalize") %>%
        filter(terms == "Load", statistic == "sd") %>%
        pull(value)
      
      my_svm_fit <- fit(my_svm_wf, data = reactive_profil_no_outliers())
      my_svm_pred <- predict(my_svm_fit, new_data = reactive_selected_temp())
      my_svm_pred_unnormalized <- my_svm_pred * load_svm_sd + load_svm_mean
      
      resultat_svm <- bind_cols(reactive_selected_temp(), Predikterat = my_svm_pred_unnormalized)
      
      return(resultat_svm)
    })
    
    final_knn <- reactive({
        final <- reactive_ursprungsprofil() %>%
            rename(Original_hour = Hour, Original_temperature = Temperature, Original_load = Load) %>%
            bind_cols(., knn_result()) %>%
            dplyr::select(Ort, Hour, Original_temperature, Original_load, Temperature, .pred) %>%
            rename(Temperature_normal = Temperature, Predicted_load_knn = .pred)
        
        return(final)
    })
    
    final_svm <- reactive({
      final <- final_knn() %>%
        bind_cols(., svm_result() %>% select(.pred)) %>%
        rename(Predicted_load_svm = .pred)
      
      return(final)
    })
    
    selected_algorithm_data <- reactive({
      switch(input$algorithms, "my_knn" = knn_result(), "my_svm" = svm_result())
    })
    
    selected_table_data <- reactive({
      switch(input$algorithms, "my_knn" = final_knn(), "my_svm" = final_svm())
    })
    
    output$mainPlot <- renderPlot({
        ggplot() +
            geom_point(data = reactive_ursprungsprofil(), 
                       mapping = aes(x = Temperature, y = Load, colour = "Original"), size = 3) +
            geom_point(data = selected_algorithm_data(), 
                       mapping = aes(x = Temperature, y = .pred, colour = "Predicted"), size = 3) +
            labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)") +
            scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                                name = "Data source") +
            scale_x_continuous(n.breaks = 10) +
            scale_y_continuous(n.breaks = 10)
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
