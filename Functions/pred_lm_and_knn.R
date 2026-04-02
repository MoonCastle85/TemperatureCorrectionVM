pred_lm_and_knn <- function(temp_profile, model_fits, stop_temp_c = 15) {
  # Separate heating profile data into heating season
  heat_season <- temp_profile %>%
    filter(temperature < stop_temp_c)
  
  # Predict new data for heating season
  heat_predict <- get_pred_load(lm_model = model_fits[[1]], knn_model = model_fits[[2]], data = heat_season)
}