library(shiny)
library(tidyverse)
library(tidymodels)
library(kknn)
library(ggthemes)
library(DT)
library(shinythemes)
library(shinyjs)

theme_set(ggthemes::theme_clean(base_size = 24))

normal_temp <- read_csv2(paste0("F:/Profu/Fjärrkontrollen - Profu - Documents/Admin/",
                                "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/",
                                "Normaltemperaturer.csv"), col_types = "cnnnnnn")

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
            radioButtons("algorithms", "Select machine-learning algorithm", c("K-Nearest Neighbour (KNN)", "Others...")),
            selectInput(inputId = "municipalities",
                        label = "Select municipality for normal temperatures",
                        choices = distinct(normal_temp, Ort)), 
            fileInput("my_file", "Select a CSV file that has three columns: Hour (0-23), Temperature (°C) and Load (kW)", 
                      accept = ".csv"),
            numericInput("my_neighbours", "Select number of neighbours for the KNN model", 5, min = 2, max = 20, step = 1),
            downloadButton("download_result", "Download result table as CSV", class = "btn btn-primary")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mainPlot", height = "600px"),
            DTOutput("my_DT")
        )
    )
)

# Define server logic required to draw a histogram
my_server <- function(input, output) {
    Sys.setlocale(category = "LC_ALL", locale = "Swedish.UTF-8")
    
    shinyjs::disable("download_result")
    shinyjs::addClass("download_result", "btn-disabled")
    
    reactive_ursprungsprofil <- reactive({
        my_file <- input$my_file
        req(my_file)
        ursprungsprofil <- read_csv2(my_file$datapath, col_types = "nnnn", col_names = c("Hour", "Temperature", "Load"), skip = 1)
        
        return(ursprungsprofil)
    })
    
    reactive_resultat <- reactive({
        my_knn <- nearest_neighbor(neighbors = input$my_neighbours) %>%
            set_mode("regression") %>%
            set_engine("kknn")
        
        my_knn_fit <- my_knn %>% fit(Load ~ Temperature + Hour, data = reactive_ursprungsprofil())
        selected_temp <- dplyr::filter(normal_temp, Ort == input$municipalities)
        my_knn_pred <- predict(my_knn_fit, selected_temp)
        resultat <- bind_cols(selected_temp, Predikterat = my_knn_pred)
        
        return(resultat)
    })
    
    final_result <- reactive({
        reactive_ursprungsprofil() %>%
            rename(Original_hour = Hour, Original_temperature = Temperature, Original_load = Load) %>%
            bind_cols(reactive_resultat(), .) %>%
            dplyr::select(Ort, Hour, Temperature, .pred, Original_temperature, Original_load) %>%
            rename(Predicted_load = .pred)
    })
    
    output$mainPlot <- renderPlot({
        ggplot() +
            geom_point(data = reactive_ursprungsprofil(), mapping = aes(x = Temperature, y = Load, colour = "Original"), size = 3) +
            geom_point(data = reactive_resultat(), mapping = aes(x = Temperature, y = .pred, colour = "Predicted"), size = 3) +
            labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)") +
            scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                                name = "Data source") +
            scale_x_continuous(n.breaks = 10) +
            scale_y_continuous(n.breaks = 10)
    })
    
    output$my_DT <- DT::renderDT({
        datatable(final_result(),
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
            write_csv2(final_result(), file)
        }
    )
}

# Run the application 
shinyApp(ui = my_ui, server = my_server)
