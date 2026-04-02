get_knn_model <- function(data) {
  rec_resid <- recipe(resid ~ temperature + time + hour, data = data) %>%
    step_mutate(hour_sin = sin(2 * pi * time/24),
                hour_cos = cos(2 * pi * time/24)) %>%
    step_normalize(temperature) %>%
    update_role(hour, new_role = "id")
  
  knn_spec_triang <- nearest_neighbor(weight_func = "triangular", neighbors = 2) %>%
    set_engine("kknn") %>%
    set_mode("regression")
  
  knn_wf <- workflow() %>%
    add_model(knn_spec_triang) %>%
    add_recipe(rec_resid)
  
  result <- fit(knn_wf, data = data)
}