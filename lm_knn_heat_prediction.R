library(tidyverse)
library(tidymodels)
library(skimr)

pred_functions <- list.files("./Functions", pattern = "\\.R$", full.names = TRUE)
walk(pred_functions, source)

required_profile_cols <- c("month", "weekday", "day", "time", "hour", "temperature", "heat_load_kw")
required_temp_cols <- c("year", "month", "weekday", "day", "time", "hour", "temperature")
csv2_locale <- readr::locale(decimal_mark = ",", grouping_mark = ".")
orig_profile_path <- "C:/Users/vanja/OneDrive - Profu/Umeå Energi - Documents/Flexibla energilösningar/kv Renen/2. Underlag/originalprofil_kontor.csv"
temp_folder_path <- "./Temperatures for prediction"
output_path <- "Outputs/Varmeprofiler_GBG_2015-2025.csv"
stop_temp_c <- 15

assert_required_columns <- function(data, required_cols, label) {
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        label,
        " is missing required columns: ",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  data
}

get_csv_header <- function(path) {
  header_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  header_line <- sub("^\ufeff", "", header_line)
  header_cols <- strsplit(header_line, ";", fixed = TRUE)[[1]]
  gsub('^"|"$', "", header_cols)
}

read_required_csv2 <- function(path, required_cols, label) {
  header <- set_names(vector("list", length(get_csv_header(path))), get_csv_header(path))
  assert_required_columns(header, required_cols, label)

  suppressMessages(
    read_csv2(
      path,
      col_select = all_of(required_cols),
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE,
      locale = csv2_locale
    )
  )
}

parse_mixed_decimal_number <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- str_trim(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  normalized <- map_chr(x_chr, \(value) {
    if (is.na(value)) {
      return(NA_character_)
    }

    comma_pos <- gregexpr(",", value, fixed = TRUE)[[1]]
    dot_pos <- gregexpr(".", value, fixed = TRUE)[[1]]
    has_comma <- comma_pos[1] != -1
    has_dot <- dot_pos[1] != -1

    if (has_comma && has_dot) {
      if (max(comma_pos) > max(dot_pos)) {
        value <- str_replace_all(value, stringr::fixed("."), "")
        str_replace(value, stringr::fixed(","), ".")
      } else {
        str_replace_all(value, stringr::fixed(","), "")
      }
    } else if (has_comma) {
      value <- str_replace_all(value, stringr::fixed("."), "")
      str_replace(value, stringr::fixed(","), ".")
    } else {
      str_replace_all(value, stringr::fixed(","), "")
    }
  })

  readr::parse_double(
    normalized,
    locale = readr::locale(decimal_mark = ".", grouping_mark = ","),
    na = c("", "NA")
  )
}

coerce_required_numeric <- function(data, required_cols) {
  data %>%
    mutate(
      across(
        all_of(required_cols),
        parse_mixed_decimal_number
      )
    )
}

assert_usable_profile_rows <- function(data, stop_temp_c) {
  usable_rows <- data %>%
    filter(
      !is.na(temperature),
      !is.na(heat_load_kw),
      !is.na(is_weekend),
      temperature < stop_temp_c
    )

  if (nrow(usable_rows) == 0) {
    stop(
      paste0(
        "Heating profile file has no usable rows below the stop temperature (",
        stop_temp_c,
        " degC). Check the numeric values and decimal separators in the required columns."
      ),
      call. = FALSE
    )
  }

  data
}

read_standard_profile <- function(path) {
  read_required_csv2(
    path,
    required_cols = required_profile_cols,
    label = "Heating profile file"
  ) %>%
    coerce_required_numeric(required_profile_cols) %>%
    mutate(is_weekend = if_else(weekday >= 6, 1, 0)) %>%
    arrange(hour)
}

read_temperature_folder <- function(folder_path) {
  if (!dir.exists(folder_path)) {
    stop(paste0("Temperature folder does not exist: ", folder_path), call. = FALSE)
  }

  csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    stop("No CSV files were found in the selected temperature folder.", call. = FALSE)
  }

  map(
    csv_files,
    \(file_path) {
      read_required_csv2(
        file_path,
        required_cols = required_temp_cols,
        label = paste0("Temperature file '", basename(file_path), "'")
      ) %>%
        coerce_required_numeric(required_temp_cols) %>%
        mutate(
          source_file = tools::file_path_sans_ext(basename(file_path)),
          is_weekend = if_else(weekday >= 6, 1, 0),
          profile_id = paste0(year, " - ", source_file)
        )
    }
  ) %>%
    list_rbind() %>%
    arrange(year, source_file, hour) %>%
    select(profile_id, source_file, year, month, weekday, day, time, hour, temperature, is_weekend)
}

empty_prediction <- function(template) {
  template[0, , drop = FALSE] %>%
    mutate(pred_load_new = numeric())
}

predict_temperature_profile <- function(temp_profile, orig_profile, model_fits, stop_temp_c = 15) {
  heating_input <- temp_profile %>%
    filter(temperature < stop_temp_c)

  summer_input <- temp_profile %>%
    filter(temperature >= stop_temp_c)

  heating_prediction <- if (nrow(heating_input) > 0) {
    pred_lm_and_knn(heating_input, model_fits, stop_temp_c = stop_temp_c)
  } else {
    empty_prediction(temp_profile)
  }

  summer_prediction <- if (nrow(summer_input) > 0) {
    get_pred_load_summer(data = orig_profile, new = summer_input)
  } else {
    empty_prediction(temp_profile)
  }

  bind_rows(heating_prediction, summer_prediction) %>%
    arrange(hour) %>%
    mutate(season = if_else(temperature < stop_temp_c, "heating", "summer"))
}

orig_profile <- read_standard_profile(orig_profile_path) %>%
  assert_usable_profile_rows(stop_temp_c = stop_temp_c)
temp_profiles <- read_temperature_folder(temp_folder_path)

# =================
# Predict and check
# =================

model_fits <- fit_lm_and_knn(orig_profile, stop_temp_c = stop_temp_c)

all_predictions <- temp_profiles %>%
  group_split(profile_id, .keep = TRUE) %>%
  map(\(temp_profile) {
    predict_temperature_profile(
      temp_profile = temp_profile,
      orig_profile = orig_profile,
      model_fits = model_fits,
      stop_temp_c = stop_temp_c
    )
  }) %>%
  list_rbind() %>%
  arrange(year, hour)

skim(all_predictions)

ggplot() +
  geom_point(
    data = tidyr::crossing(profile_id = unique(all_predictions$profile_id), orig_profile),
    aes(x = temperature, y = heat_load_kw, colour = "Original")
  ) +
  geom_point(data = all_predictions, aes(x = temperature, y = pred_load_new, colour = "Adjusted")) +
  scale_colour_manual(values = c("Original" = "blue", "Adjusted" = "red"), name = "Source") +
  labs(
    x = "Outdoor temperature per hour [degC]",
    y = "Heating load per hour [kW]"
  ) +
  facet_wrap(~profile_id) +
  theme_minimal()

# =================
# Save
# =================

write_csv2(all_predictions, file = output_path)
