get_pred_load <- function(lm_model, knn_model, data) {
  lin_pred <- predict(lm_model, data)$.pred
  resid_pred <- predict(knn_model, data)$.pred
  
  result <- data %>%
    mutate(pred_load_new = lin_pred + resid_pred)
}