get_linear_model <- function(data) {
  my_recipe <- recipe(heat_load_kw ~ temperature + is_weekend, data = data) %>%
    # A spline gives the heating baseline enough curvature so KNN is correcting
    # local structure instead of compensating for a globally straight line.
    step_ns(temperature, deg_free = 4)
  
  lin_model <- linear_reg() %>%
    set_engine("lm")
  
  lin_workflow <- workflow() %>%
    add_model(lin_model) %>%
    add_recipe(my_recipe)
  
  result <- fit(lin_workflow, data = data)
}
