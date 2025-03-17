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
tidymodels_prefer()

theme_set(ggthemes::theme_clean())

Sys.setlocale(category = "LC_ALL", locale = "Swedish.UTF-8")

normal_temp <- read_csv2("https://manborgconsulting.com/Normaltemperaturer.csv", col_types = "cnnnnnn")

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

my_url <- "F:/Profu/Akademiska Hus - Documents/Nytt område i Luleå 2024/2. Underlag/Värmeprofil övrig campus 2023.csv"

reactive_ursprungsprofil <- read_csv2(my_url, col_types = "nnnn", col_names = c("Month", "Hour", "Temperature", "Load"), 
                                      skip = 1) %>%
    mutate(Season = case_when(Month %in% c(1,2,3,12) ~ "Winter", 
                              Month %in% c(4,5,10,11) ~ "Spring/Autumn",
                              Month %in% c(6,7,8,9) ~ "Summer"))

reactive_profil_no_outliers <- reactive_ursprungsprofil %>%
    mutate(Temperature_bin = binning(Temperature, nbins = 20, type = "quantile")) %>%
    remove_outliers(., "Temperature_bin", "Load")

reactive_selected_temp <- dplyr::filter(normal_temp, Ort == "Lulea_Lulea") %>%
    mutate(Season = case_when(Month %in% c(1,2,3,12) ~ "Winter", 
                              Month %in% c(4,5,10,11) ~ "Spring/Autumn",
                              Month %in% c(6,7,8,9) ~ "Summer"))

my_recipes <- recipe(Load ~ Temperature + Hour, data = reactive_profil_no_outliers) %>%
  step_normalize(Temperature) %>%
  step_normalize(Load, skip = TRUE, id = "load_normalize")

unnormalize <- function(pred) {
  tidy_rec <- my_recipes %>%
    prep() %>%
    tidy(., id = "load_normalize")
  
  sd <- filter(tidy_rec, statistic == "sd") %>% pull(value)
  means <- filter(tidy_rec, statistic == "mean") %>% pull(value)

  unnormalized_pred <- pred * sd + means
  
  return(unnormalized_pred)
}

knn_spec_triang <- nearest_neighbor(weight_func = "triangular", neighbors = 2) %>% 
  set_engine("kknn") %>% 
  set_mode("regression")

my_workflows <- workflow_set(preproc = list(rec = my_recipes), models = list(KNN_triang = knn_spec_triang))

fit_all <- map(.x = my_workflows$wflow_id, 
               .f = \(x) fit(extract_workflow(my_workflows, x), data = reactive_profil_no_outliers))

all_preds <- map(.x = fit_all, .f = \(x) predict(x, new_data = reactive_selected_temp))
all_preds2 <- all_preds %>%
  map(., unnormalize) %>%
  enframe() %>%
  mutate(name = my_workflows$wflow_id,
         value = map(value, ~bind_cols(reactive_selected_temp, .x)))

ggplot() +
  geom_point(data = reactive_ursprungsprofil, 
             mapping = aes(x = Temperature, y = Load, colour = "Original"), size = 3) +
  geom_point(data = all_preds2 %>% filter(name == "rec_KNN_triang") %>% unnest(value), 
             mapping = aes(x = Temperature, y = .pred, colour = "Predicted"), size = 3) +
  labs(title = "Load vs temperature data", x = "Outdoor temperature (°C)", y = "Heating load (kW)") +
  scale_colour_manual(values = c("Original" = "blue", "Predicted" = "red"), labels = c("Original", "Predicted"),
                      name = "Data source") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(n.breaks = 10)

write_csv2(all_preds2[[2]][[1]], file = "Normalårskorrigerad värme övrig campus.csv")

knn_result <- reactive({
  knn_fit <- fit(extract_workflow(my_workflows(), id = "KNN_rec"), data = reactive_profil_no_outliers())
  knn_pred <- predict(knn_fit, new_data = reactive_selected_temp())
  # my_knn_pred_unnormalized <- my_knn_pred * load_knn_sd + load_knn_mean
  
  # prep_knn <- my_knn_rec %>%
  #   prep()
  # 
  # load_knn_mean <- tidy(prep_knn, id = "load_normalize") %>%
  #   filter(terms == "Load", statistic == "mean") %>%
  #   pull(value)
  # 
  # load_knn_sd <- tidy(prep_knn, id = "load_normalize") %>%
  #   filter(terms == "Load", statistic == "sd") %>%
  #   pull(value)
  
  # my_knn_fit <- fit(my_knn_wf, data = reactive_profil_no_outliers())
  # my_knn_pred <- predict(my_knn_fit, new_data = reactive_selected_temp())
  # my_knn_pred_unnormalized <- my_knn_pred * load_knn_sd + load_knn_mean
  
  resultat_knn <- bind_cols(reactive_selected_temp(), Predikterat = knn_pred)
  
  return(resultat_knn)
})

rf_result <- reactive({
  my_rf <- rand_forest(trees = input$my_trees) %>%
    set_mode("regression") %>%
    set_engine("ranger")
  
  my_rf_rec <- recipe(Load ~ Month + Temperature + Hour, data = reactive_profil_no_outliers()) %>%
    step_normalize(Temperature, id = "temp_normalize") %>%
    step_normalize(Load, skip = TRUE, id = "load_normalize")
  
  my_rf_wf <- workflow() %>%
    add_recipe(my_rf_rec) %>%
    add_model(my_rf)
  
  prep_rf <- my_rf_rec %>%
    prep()
  
  load_rf_mean <- tidy(prep_rf, id = "load_normalize") %>%
    filter(terms == "Load", statistic == "mean") %>%
    pull(value)
  
  load_rf_sd <- tidy(prep_rf, id = "load_normalize") %>%
    filter(terms == "Load", statistic == "sd") %>%
    pull(value)
  
  my_rf_fit <- fit(my_rf_wf, data = reactive_profil_no_outliers())
  my_rf_pred <- predict(my_rf_fit, new_data = reactive_selected_temp())
  my_rf_pred_unnormalized <- my_rf_pred * load_rf_sd + load_rf_mean
  
  resultat_rf <- bind_cols(reactive_selected_temp(), Predikterat = my_rf_pred_unnormalized)
})

svm_result <- reactive({
  my_svm <- svm_rbf() %>%
    set_mode("regression") %>%
    set_engine("kernlab")
  
  my_svm_rec <- recipe(Load ~ Month + Temperature + Hour, data = reactive_profil_no_outliers()) %>%
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
})

orig_data <- reactive({
  reactive_ursprungsprofil() %>%
    rename(Original_hour = Hour, Original_temperature = Temperature, Original_load = Load)
})

final_knn <- reactive({
  bind_cols(orig_data(), knn_result()) %>%
    dplyr::select(Ort, Hour, Original_temperature, Original_load, Temperature, .pred) %>%
    rename(Temperature_normal = Temperature, Predicted_load_knn = .pred)
})

final_svm <- reactive({
  bind_cols(orig_data(), svm_result() %>% select(.pred)) %>%
    rename(Predicted_load_svm = .pred)
})

final_rf <- reactive({
  bind_cols(orig_data(), rf_result() %>% select(.pred)) %>%
    rename(Predicted_load_rf = .pred)
})

selected_algorithm_data <- reactive({
  switch(input$algorithms, "my_knn" = knn_result(), "my_svm" = svm_result(), "my_rf" = rf_result())
})

selected_table_data <- reactive({
  switch(input$algorithms, "my_knn" = final_knn(), "my_svm" = final_svm(), "my_rf" = final_rf())
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
