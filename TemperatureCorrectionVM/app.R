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

theme_set(ggthemes::theme_clean(base_size = 24))

normal_temp <- read_csv2("https://manborgconsulting.com/Normaltemperaturer.csv", 
                         col_types = "cnnnnnn")

# Define UI for application that draws a histogram
my_ui <- fluidPage(
    theme = shinytheme("cerulean"),
    shinyjs::useShinyjs(),
    tags$style(HTML("
    .btn-disabled {
      pointer-events: none;  /* Disable click events */
      opacity: 0.5;          /* Make the button look disabled */
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
    
    reactive_ursprungsprofil <- reactive({
        my_file <- input$my_file
        req(my_file)
        ursprungsprofil <- read_csv2(my_file$datapath, col_types = "nnnn", col_names = c("Hour", "Temperature", "Load"), skip = 1)
        
        return(ursprungsprofil)
    })
    
    reactive_selected_temp <- reactive({
      selected_temp <- dplyr::filter(normal_temp, Ort == input$municipalities)
      
      return(selected_temp)
    })
    
    knn_result <- reactive({
        my_knn <- nearest_neighbor(neighbors = input$my_neighbours) %>%
            set_mode("regression") %>%
            set_engine("kknn")
        
        my_knn_fit <- my_knn %>% fit(Load ~ Temperature + Hour, data = reactive_ursprungsprofil())
        my_knn_pred <- predict(my_knn_fit, reactive_selected_temp())
        resultat_knn <- bind_cols(reactive_selected_temp(), Predikterat = my_knn_pred)
        
        return(resultat_knn)
    })
    
    svm_result <- reactive({
      my_svm <- svm_rbf() %>%
        set_mode("regression") %>%
        set_engine("kernlab")
      
      my_svm_fit <- my_svm %>% fit(Load ~ Temperature + Hour, data = reactive_ursprungsprofil())
      my_svm_pred <- predict(my_svm_fit, reactive_selected_temp())
      resultat_svm <- bind_cols(reactive_selected_temp(), my_svm_pred)
      
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
            geom_point(data = reactive_ursprungsprofil(), mapping = aes(x = Temperature, y = Load, colour = "Original"), size = 3) +
            geom_point(data = selected_algorithm_data(), mapping = aes(x = Temperature, y = .pred, colour = "Predicted"), size = 3) +
            labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)") +
            scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                                name = "Data source") +
            scale_x_continuous(n.breaks = 10) +
            scale_y_continuous(n.breaks = 10)
    })
    
    output$my_DT <- DT::renderDT({
        datatable(selected_table_data(),
                  options = list(initComplete = JS("function(settings, json) {",
                                                   "  Shiny.setInputValue('table_rendered', true);",
                                                   "}")
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
