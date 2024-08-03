library(shiny)
library(tidyverse)
library(tidymodels)
library(kknn)
library(ggthemes)
library(DT)
library(shinythemes)

theme_set(ggthemes::theme_clean(base_size = 24))

normal_temp <- read_csv2(paste0("F:/Profu/Fjärrkontrollen - Profu - Documents/Admin/",
                                "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/",
                                "Normaltemperaturer.csv"), col_types = "cnnnnnn")

# Define UI for application that draws a histogram
my_ui <- fluidPage(
    theme = shinytheme("cerulean"),
    
    # Application title
    titlePanel("Temperature-corrected heating load"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "municipalities",
                        label = "Select municipality for normal temperatures",
                        choices = distinct(normal_temp, Ort)), 
            fileInput("my_file", "Select a CSV file that has three columns: Hour (0-23), Temperature (°C) and Load (kW)", 
                      accept = ".csv"),
            numericInput("my_neighbours", "Select number of neighbours for the KNN model", 5, min = 2, max = 20, step = 1)
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
    
    
    output$mainPlot <- renderPlot({
        ggplot() +
            geom_point(data = reactive_ursprungsprofil(), mapping = aes(x = Temperature, y = Load), colour = "blue") +
            geom_point(data = reactive_resultat(), mapping = aes(x = Temperature, y = .pred), colour = "red") +
            labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)")
    })
    
    output$my_DT <- DT::renderDT({
        my_metrics <- tibble(Load_original = tail(sort(reactive_ursprungsprofil(), 5)),
                             Load_corrected = tail(sort(reactive_resultat(), 5)))
    })
}

# Run the application 
shinyApp(ui = my_ui, server = my_server)
