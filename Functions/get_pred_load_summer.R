get_pred_load_summer <- function(data, new, stop_temp_c = 15) {
  summer_data <- data %>%
    filter(temperature >= stop_temp_c)

  if (nrow(summer_data) == 0) {
    stop(
      paste0(
        "The original profile has no rows at or above the stop temperature (",
        stop_temp_c,
        " degC), so the summer model cannot be fitted."
      ),
      call. = FALSE
    )
  }

  my_rec <- recipe(heat_load_kw ~ temperature + time + hour, data = summer_data) %>%
    step_mutate(hour_sin = sin(2 * pi * time/24),
                hour_cos = cos(2 * pi * time/24)) %>%
    step_normalize(temperature) %>%
    update_role(hour, new_role = "id")
  
  knn_spec_triang <- nearest_neighbor(weight_func = "triangular", neighbors = 2) %>%
    set_engine("kknn") %>%
    set_mode("regression")
  
  knn_wf <- workflow() %>%
    add_model(knn_spec_triang) %>%
    add_recipe(my_rec)
  
  knn_fit <- fit(knn_wf, data = summer_data)
  
  result <- new %>%
    mutate(pred_load_new = predict(knn_fit, new)$.pred)
}
