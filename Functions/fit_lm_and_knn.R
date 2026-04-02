fit_lm_and_knn <- function(orig_profile, stop_temp_c = 15) {
  # Separate heating profile data into heating season and summer season
  heat_season <- orig_profile %>%
    filter(temperature < stop_temp_c)
  
  # Fit and predict linear model on heating season data and calculate residuals
  lin_fitted <- get_linear_model(heat_season)
  
  heat_season_pred <- heat_season %>%
    mutate(pred_load = predict(object = lin_fitted, new_data = heat_season)$.pred,
           resid = heat_load_kw - pred_load)
  
  # Fit residuals using knn
  knn_fitted <- get_knn_model(heat_season_pred)
  
  result_fitted <- list(lin_fitted, knn_fitted)
}