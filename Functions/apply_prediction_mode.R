get_prediction_mode_label <- function(prediction_mode, target_max_load = 63) {
  switch(
    prediction_mode,
    standard = "Standard",
    strict_fixed_peak_63 = paste0("Climate-adjusted strict mode (max load = ", target_max_load, ")"),
    stop(paste0("Unknown prediction_mode: ", prediction_mode), call. = FALSE)
  )
}

apply_prediction_mode <- function(predictions, prediction_mode = "standard", target_max_load = 63) {
  if (prediction_mode == "standard") {
    return(predictions)
  }

  if (prediction_mode == "strict_fixed_peak_63") {
    return(apply_strict_fixed_peak_mode(predictions, target_max_load = target_max_load))
  }

  stop(paste0("Unknown prediction_mode: ", prediction_mode), call. = FALSE)
}

apply_strict_fixed_peak_mode <- function(predictions, target_max_load = 63) {
  predictions %>%
    group_split(profile_id, .keep = TRUE) %>%
    map(\(profile_predictions) {
      adjust_profile_to_fixed_peak(
        profile_predictions,
        target_max_load = target_max_load
      )
    }) %>%
    list_rbind() %>%
    arrange(year, source_file, hour)
}

adjust_profile_to_fixed_peak <- function(profile_predictions, target_max_load = 63) {
  profile_predictions <- profile_predictions %>%
    arrange(hour)

  heating_rows <- profile_predictions$season == "heating" & !is.na(profile_predictions$pred_load_new)

  if (!any(heating_rows)) {
    profile_predictions$pred_load_new <- pmin(profile_predictions$pred_load_new, target_max_load)
    return(profile_predictions)
  }

  heating_predictions <- profile_predictions[heating_rows, , drop = FALSE]
  heating_peak <- max(heating_predictions$pred_load_new, na.rm = TRUE)
  warmest_heating_temp <- max(heating_predictions$temperature, na.rm = TRUE)
  anchor_load <- heating_predictions %>%
    filter(temperature == warmest_heating_temp) %>%
    summarise(anchor_load = mean(pred_load_new, na.rm = TRUE)) %>%
    pull(anchor_load)

  if (!is.finite(anchor_load)) {
    anchor_load <- min(heating_predictions$pred_load_new, na.rm = TRUE)
  }

  if (!is.finite(heating_peak)) {
    profile_predictions$pred_load_new <- pmin(profile_predictions$pred_load_new, target_max_load)
    return(profile_predictions)
  }

  if (isTRUE(all.equal(heating_peak, anchor_load))) {
    adjusted_heating <- rep(target_max_load, nrow(heating_predictions))
  } else {
    scale_factor <- (target_max_load - anchor_load) / (heating_peak - anchor_load)
    adjusted_heating <- anchor_load + (heating_predictions$pred_load_new - anchor_load) * scale_factor
  }

  profile_predictions$pred_load_new[heating_rows] <- adjusted_heating
  profile_predictions$pred_load_new <- pmax(profile_predictions$pred_load_new, 0)
  profile_predictions$pred_load_new <- pmin(profile_predictions$pred_load_new, target_max_load)

  profile_predictions
}
