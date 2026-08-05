get_prediction_mode_label <- function(prediction_mode) {
  switch(
    prediction_mode,
    standard = "Standard",
    strict = "Strict mode",
    stop(paste0("Unknown prediction_mode: ", prediction_mode), call. = FALSE)
  )
}

calculate_yearday <- function(month, day, year = NULL, reference_year = 2021) {
  month <- as.integer(month)
  day <- as.integer(day)

  if (is.null(year)) {
    year <- rep.int(reference_year, length(month))
  }

  year <- as.integer(year)
  dates <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))

  if (anyNA(dates)) {
    stop("Could not calculate yearday because one or more dates are invalid.", call. = FALSE)
  }

  as.integer(format(dates, "%j"))
}

validate_strict_restriction_table <- function(data, key_col, lower_col, upper_col, label) {
  required_cols <- c(key_col, lower_col, upper_col)
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(paste0(label, " is missing required columns: ", paste(missing_cols, collapse = ", ")), call. = FALSE)
  }

  if (any(!is.finite(data[[lower_col]]) | !is.finite(data[[upper_col]]))) {
    stop(paste0(label, " contains missing or invalid lower/upper bounds."), call. = FALSE)
  }

  if (any(data[[lower_col]] > data[[upper_col]])) {
    stop(paste0(label, " contains rows where the lower bound is greater than the upper bound."), call. = FALSE)
  }

  data %>%
    arrange(.data[[key_col]])
}

validate_strict_restriction_spec <- function(restriction_spec) {
  if (is.null(restriction_spec)) {
    stop("Strict mode requires monthly and daily restriction tables.", call. = FALSE)
  }

  if (!all(c("monthly", "daily") %in% names(restriction_spec))) {
    stop("Strict mode restriction spec must contain 'monthly' and 'daily' tables.", call. = FALSE)
  }

  list(
    monthly = validate_strict_restriction_table(
      restriction_spec$monthly,
      key_col = "month",
      lower_col = "min_heat_load_kw_sum",
      upper_col = "max_heat_load_kw_sum",
      label = "Monthly strict restriction table"
    ),
    daily = validate_strict_restriction_table(
      restriction_spec$daily,
      key_col = "yearday",
      lower_col = "min_heat_load_kw_daily_avg",
      upper_col = "max_heat_load_kw_daily_avg",
      label = "Daily strict restriction table"
    )
  )
}

clamp_to_span <- function(value, lower_bound, upper_bound) {
  max(lower_bound, min(upper_bound, value))
}

enforce_daily_average_constraints <- function(profile_predictions, daily_constraints, tolerance = 1e-6) {
  for (i in seq_len(nrow(daily_constraints))) {
    constraint <- daily_constraints[i, , drop = FALSE]
    day_rows <- profile_predictions$yearday == constraint$yearday & !is.na(profile_predictions$pred_load_new)

    if (!any(day_rows)) {
      stop(
        paste0(
          "Profile '",
          profile_predictions$profile_id[[1]],
          "' has no rows for strict daily yearday ",
          constraint$yearday,
          "."
        ),
        call. = FALSE
      )
    }

    current_avg <- mean(profile_predictions$pred_load_new[day_rows], na.rm = TRUE)
    target_avg <- clamp_to_span(
      current_avg,
      constraint$min_heat_load_kw_daily_avg,
      constraint$max_heat_load_kw_daily_avg
    )

    if (abs(current_avg - target_avg) <= tolerance) {
      next
    }

    if (current_avg > tolerance) {
      profile_predictions$pred_load_new[day_rows] <-
        profile_predictions$pred_load_new[day_rows] * (target_avg / current_avg)
    } else {
      profile_predictions$pred_load_new[day_rows] <- rep(target_avg, sum(day_rows))
    }
  }

  profile_predictions$pred_load_new <- pmax(profile_predictions$pred_load_new, 0)
  profile_predictions
}

enforce_monthly_sum_constraints <- function(profile_predictions, monthly_constraints, locked_yeardays = integer(), tolerance = 1e-6) {
  for (i in seq_len(nrow(monthly_constraints))) {
    constraint <- monthly_constraints[i, , drop = FALSE]
    month_rows <- profile_predictions$month == constraint$month & !is.na(profile_predictions$pred_load_new)

    if (!any(month_rows)) {
      next
    }

    current_sum <- sum(profile_predictions$pred_load_new[month_rows], na.rm = TRUE)
    target_sum <- clamp_to_span(
      current_sum,
      constraint$min_heat_load_kw_sum,
      constraint$max_heat_load_kw_sum
    )

    if (abs(current_sum - target_sum) <= tolerance) {
      next
    }

    adjustable_rows <- month_rows & !(profile_predictions$yearday %in% locked_yeardays)

    if (any(adjustable_rows)) {
      locked_sum <- sum(profile_predictions$pred_load_new[month_rows & !adjustable_rows], na.rm = TRUE)
      required_adjustable_sum <- target_sum - locked_sum

      if (required_adjustable_sum < -tolerance) {
        stop(
          paste0(
            "Strict monthly restriction for month ",
            constraint$month,
            " is incompatible with the locked daily restrictions in profile '",
            profile_predictions$profile_id[[1]],
            "'."
          ),
          call. = FALSE
        )
      }

      adjustable_sum <- sum(profile_predictions$pred_load_new[adjustable_rows], na.rm = TRUE)

      if (adjustable_sum > tolerance) {
        scale_factor <- required_adjustable_sum / adjustable_sum

        if (scale_factor < -tolerance) {
          stop(
            paste0(
              "Strict monthly restriction for month ",
              constraint$month,
              " would require negative loads in profile '",
              profile_predictions$profile_id[[1]],
              "'."
            ),
            call. = FALSE
          )
        }

        profile_predictions$pred_load_new[adjustable_rows] <-
          profile_predictions$pred_load_new[adjustable_rows] * max(scale_factor, 0)
      } else {
        fill_value <- if (required_adjustable_sum > tolerance) {
          required_adjustable_sum / sum(adjustable_rows)
        } else {
          0
        }

        profile_predictions$pred_load_new[adjustable_rows] <- rep(fill_value, sum(adjustable_rows))
      }
    } else {
      if (current_sum > tolerance) {
        profile_predictions$pred_load_new[month_rows] <-
          profile_predictions$pred_load_new[month_rows] * (target_sum / current_sum)
      } else {
        profile_predictions$pred_load_new[month_rows] <- rep(target_sum / sum(month_rows), sum(month_rows))
      }
    }
  }

  profile_predictions$pred_load_new <- pmax(profile_predictions$pred_load_new, 0)
  profile_predictions
}

strict_constraints_satisfied <- function(profile_predictions, monthly_constraints, daily_constraints, tolerance = 1e-6) {
  monthly_ok <- map_lgl(seq_len(nrow(monthly_constraints)), \(i) {
    constraint <- monthly_constraints[i, , drop = FALSE]
    month_rows <- profile_predictions$month == constraint$month & !is.na(profile_predictions$pred_load_new)

    if (!any(month_rows)) {
      return(TRUE)
    }

    current_sum <- sum(profile_predictions$pred_load_new[month_rows], na.rm = TRUE)
    current_sum >= (constraint$min_heat_load_kw_sum - tolerance) &&
      current_sum <= (constraint$max_heat_load_kw_sum + tolerance)
  })

  daily_ok <- map_lgl(seq_len(nrow(daily_constraints)), \(i) {
    constraint <- daily_constraints[i, , drop = FALSE]
    day_rows <- profile_predictions$yearday == constraint$yearday & !is.na(profile_predictions$pred_load_new)

    if (!any(day_rows)) {
      return(FALSE)
    }

    current_avg <- mean(profile_predictions$pred_load_new[day_rows], na.rm = TRUE)
    current_avg >= (constraint$min_heat_load_kw_daily_avg - tolerance) &&
      current_avg <= (constraint$max_heat_load_kw_daily_avg + tolerance)
  })

  all(monthly_ok) && all(daily_ok)
}

adjust_profile_to_strict_restrictions <- function(profile_predictions, restriction_spec, max_iterations = 20, tolerance = 1e-6) {
  if (nrow(profile_predictions) == 0) {
    return(profile_predictions)
  }

  validated_spec <- validate_strict_restriction_spec(restriction_spec)

  profile_predictions <- profile_predictions %>%
    arrange(hour) %>%
    mutate(
      yearday = calculate_yearday(month, day, year),
      pred_load_new = as.double(pred_load_new)
    )

  locked_yeardays <- validated_spec$daily$yearday

  for (iteration in seq_len(max_iterations)) {
    previous_values <- profile_predictions$pred_load_new

    profile_predictions <- enforce_daily_average_constraints(
      profile_predictions,
      validated_spec$daily,
      tolerance = tolerance
    )

    profile_predictions <- enforce_monthly_sum_constraints(
      profile_predictions,
      validated_spec$monthly,
      locked_yeardays = locked_yeardays,
      tolerance = tolerance
    )

    if (strict_constraints_satisfied(
      profile_predictions,
      validated_spec$monthly,
      validated_spec$daily,
      tolerance = tolerance
    )) {
      return(profile_predictions %>% select(-yearday))
    }

    if (isTRUE(all.equal(previous_values, profile_predictions$pred_load_new, tolerance = tolerance))) {
      break
    }
  }

  stop(
    paste0(
      "Strict mode restrictions could not be satisfied for profile '",
      profile_predictions$profile_id[[1]],
      "'. Relax the monthly or daily spans and try again."
    ),
    call. = FALSE
  )
}

apply_strict_restriction_mode <- function(predictions, restriction_spec, max_iterations = 20, tolerance = 1e-6) {
  validated_spec <- validate_strict_restriction_spec(restriction_spec)

  predictions %>%
    group_split(profile_id, .keep = TRUE) %>%
    map(\(profile_predictions) {
      adjust_profile_to_strict_restrictions(
        profile_predictions,
        restriction_spec = validated_spec,
        max_iterations = max_iterations,
        tolerance = tolerance
      )
    }) %>%
    list_rbind() %>%
    arrange(year, source_file, hour)
}

apply_prediction_mode <- function(
  predictions,
  prediction_mode = "standard",
  restriction_spec = NULL
) {
  if (prediction_mode == "standard") {
    return(predictions)
  }

  if (prediction_mode == "strict") {
    return(apply_strict_restriction_mode(predictions, restriction_spec = restriction_spec))
  }

  stop(paste0("Unknown prediction_mode: ", prediction_mode), call. = FALSE)
}
