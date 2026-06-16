#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(tibble)
})

# -----------------
# Configuration
# -----------------

input_path <- "C:/Git/TemperatureCorrectionVM/Profiler_alla.csv"
output_dir <- "C:/Git/TemperatureCorrectionVM/Outputs"

# Each electricity source can be:
# 1. a CSV path
# 2. a data.frame / tibble
# 3. a numeric vector with one value per hour
#
# Supported CSV / data.frame formats:
# - hour + one value column
# - Timme + one value column
# - hour/Timme plus a column named "value"
# - hour/Timme plus a column with the exact output column name

elprofiler <- suppressMessages(
  read_csv2(
    paste0(
      "C:/Users/vanja/OneDrive - Profu/Fj\u00e4rrkontrollen - Profu - Documents/Admin/",
      "3. Underlag profiler, COP, inv kostnader osv/Profiler v\u00e4rme och el f\u00f6r fastigheterna/",
      "Analys profiler/elprofiler_alla.csv"
    ),
    show_col_types = FALSE,
    progress = FALSE
  )
)

el_aldre_flerbostadshus_source <- elprofiler[["elprofil_foretag aldre_flerbostadshus"]]
el_aldre_villa_source <- elprofiler[["elprofil_privat aldre_villa"]]
el_ny_flerbostadshus_source <- elprofiler[["elprofil_foretag ny_flerbostadshus"]]
el_ny_villa_source <- elprofiler[["elprofil_privat ny_villa"]]
el_ny_kontor_source <- elprofiler[["elprofil_foretag ny_kontor"]]
el_aldre_kontor_source <- elprofiler[["elprofil_foretag aldre_kontor"]]

heat_column_map <- c(
  "V\u00e4rmeprofil foretag aldre_flerbostadshus" = "aldre_flerbostadshus",
  "V\u00e4rmeprofil privat aldre_villa" = "aldre_villa",
  "V\u00e4rmeprofil foretag ny_flerbostadshus" = "ny_flerbostadshus",
  "V\u00e4rmeprofil privat ny_villa" = "ny_villa",
  "V\u00e4rmeprofil foretag ny_kontor" = "ny_kontor",
  "V\u00e4rmeprofil foretag aldre_kontor" = "aldre_kontor"
)

electricity_sources <- list(
  "Elprofil foretag aldre_flerbostadshus" = el_aldre_flerbostadshus_source,
  "Elprofil privat aldre_villa" = el_aldre_villa_source,
  "Elprofil foretag ny_flerbostadshus" = el_ny_flerbostadshus_source,
  "Elprofil privat ny_villa" = el_ny_villa_source,
  "Elprofil foretag ny_kontor" = el_ny_kontor_source,
  "Elprofil foretag aldre_kontor" = el_aldre_kontor_source
)

output_column_order <- c(
  "M\u00e5nad",
  "Veckodag",
  "Dag",
  "Klockslag",
  "Timme",
  "Utetemp",
  names(heat_column_map)[1],
  names(electricity_sources)[1],
  names(heat_column_map)[2],
  names(electricity_sources)[2],
  names(heat_column_map)[3],
  names(electricity_sources)[3],
  names(heat_column_map)[4],
  names(electricity_sources)[4],
  names(heat_column_map)[5],
  names(electricity_sources)[5],
  names(heat_column_map)[6],
  names(electricity_sources)[6]
)

required_input_cols <- c(
  "year",
  "month",
  "weekday",
  "day",
  "time",
  "hour",
  "temperature",
  "source_file",
  "profile_id",
  "pred_load_new"
)

allowed_heat_profile_keys <- unname(heat_column_map)
heat_profile_aliases <- c(
  "nytt_flerbostadshus" = "ny_flerbostadshus",
  "nytt_kontor" = "ny_kontor"
)

profiler_col_types <- cols_only(
  year = col_double(),
  month = col_double(),
  weekday = col_double(),
  day = col_double(),
  time = col_double(),
  hour = col_double(),
  temperature = col_double(),
  source_file = col_character(),
  profile_id = col_character(),
  pred_load_new = col_double()
)

# -----------------
# Helpers
# -----------------

normalize_fs_path <- function(path, must_work = TRUE) {
  normalizePath(enc2native(path), winslash = "/", mustWork = must_work)
}

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

  parse_double(
    normalized,
    locale = locale(decimal_mark = ".", grouping_mark = ","),
    na = c("", "NA")
  )
}

read_semicolon_csv <- function(path, col_select = NULL) {
  col_select_quo <- rlang::enquo(col_select)

  if (rlang::quo_is_null(col_select_quo)) {
    return(
      suppressMessages(
        read_csv2(
          normalize_fs_path(path),
          col_types = cols(.default = col_character()),
          show_col_types = FALSE,
          progress = FALSE
        )
      )
    )
  }

  suppressMessages(
    read_csv2(
      normalize_fs_path(path),
      col_select = !!col_select_quo,
      col_types = cols(.default = col_character()),
      show_col_types = FALSE,
      progress = FALSE
    )
  )
}

sanitize_filename_part <- function(x) {
  str_replace_all(as.character(x), "[\\\\/:*?\"<>|]+", "_")
}

extract_location_name <- function(source_file_value) {
  source_file_value <- as.character(source_file_value)
  source_file_value <- tools::file_path_sans_ext(source_file_value)
  location_name <- str_split_fixed(source_file_value, "_", n = 2)[, 1]
  sanitize_filename_part(location_name)
}

derive_heat_profile_key <- function(profile_id_values, allowed_keys, aliases = heat_profile_aliases) {
  profile_id_values <- str_trim(as.character(profile_id_values))
  aliased_values <- unname(aliases[profile_id_values])
  derived <- ifelse(is.na(aliased_values), profile_id_values, aliased_values)
  invalid_rows <- is.na(derived) | !derived %in% allowed_keys

  if (any(invalid_rows)) {
    missing_examples <- unique(as.character(profile_id_values[invalid_rows]))
    missing_examples <- missing_examples[seq_len(min(length(missing_examples), 5))]

    stop(
      paste0(
        "Could not extract a heat-profile key from profile_id for one or more rows. ",
        "Expected one of: ",
        paste(allowed_keys, collapse = ", "),
        ". Example profile_id values: ",
        paste(missing_examples, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  derived
}

read_profiler_results <- function(path) {
  profiler_data <- suppressMessages(
    read_csv2(
      normalize_fs_path(path),
      col_types = profiler_col_types,
      show_col_types = FALSE,
      progress = FALSE
    )
  ) %>%
    assert_required_columns(required_input_cols, "Profiler_alla.csv") %>%
    mutate(
      heat_profile_key = derive_heat_profile_key(
        profile_id,
        allowed_keys = allowed_heat_profile_keys,
        aliases = heat_profile_aliases
      )
    )

  profiler_data
}

standardize_electricity_profile <- function(source, output_col, template_hours) {
  if (is.null(source)) {
    stop(
      paste0(
        "Set the variable for '",
        output_col,
        "' at the top of the script before running the export."
      ),
      call. = FALSE
    )
  }

  if (is.numeric(source)) {
    if (length(source) != length(template_hours)) {
      stop(
        paste0(
          "Numeric electricity source for '",
          output_col,
          "' has length ",
          length(source),
          ", expected ",
          length(template_hours),
          "."
        ),
        call. = FALSE
      )
    }

    return(tibble(hour = template_hours, !!output_col := as.numeric(source)))
  }

  profile_data <- if (is.character(source) && length(source) == 1) {
    read_semicolon_csv(source)
  } else if (inherits(source, "data.frame")) {
    as_tibble(source)
  } else {
    stop(
      paste0(
        "Unsupported source type for '",
        output_col,
        "'. Use a CSV path, data.frame, or numeric vector."
      ),
      call. = FALSE
    )
  }

  names_lower <- tolower(names(profile_data))
  hour_col_index <- match(TRUE, names_lower %in% c("hour", "timme"))

  if (is.na(hour_col_index)) {
    if (nrow(profile_data) == length(template_hours)) {
      profile_data$hour <- template_hours
      hour_col <- "hour"
    } else {
      stop(
        paste0(
          "Could not find an hour/Timme column for electricity profile '",
          output_col,
          "'."
        ),
        call. = FALSE
      )
    }
  } else {
    hour_col <- names(profile_data)[[hour_col_index]]
  }

  value_candidates <- setdiff(names(profile_data), hour_col)

  value_col <- if (output_col %in% names(profile_data)) {
    output_col
  } else if ("value" %in% names_lower) {
    names(profile_data)[[match("value", names_lower)]]
  } else if (length(value_candidates) == 1) {
    value_candidates[[1]]
  } else {
    stop(
      paste0(
        "Could not identify the value column for electricity profile '",
        output_col,
        "'."
      ),
      call. = FALSE
    )
  }

  standardized <- profile_data %>%
    transmute(
      hour = parse_mixed_decimal_number(.data[[hour_col]]),
      !!output_col := parse_mixed_decimal_number(.data[[value_col]])
    ) %>%
    distinct(hour, .keep_all = TRUE) %>%
    arrange(hour)

  if (!all(template_hours %in% standardized$hour)) {
    stop(
      paste0(
        "Electricity profile '",
        output_col,
        "' is missing one or more required hours."
      ),
      call. = FALSE
    )
  }

  standardized %>%
    filter(hour %in% template_hours)
}

build_electricity_table <- function(template_hours) {
  electricity_tables <- imap(
    electricity_sources,
    \(source, output_name) {
      standardize_electricity_profile(
        source = source,
        output_col = output_name,
        template_hours = template_hours
      )
    }
  )

  reduce(
    electricity_tables,
    \(left, right) left_join(left, right, by = "hour")
  )
}

build_output_table <- function(location_data, electricity_table) {
  location_data <- location_data %>%
    arrange(hour)

  present_heat_keys <- sort(unique(location_data$heat_profile_key))
  missing_heat_keys <- setdiff(allowed_heat_profile_keys, present_heat_keys)

  if (length(missing_heat_keys) > 0) {
    stop(
      paste0(
        "Location '",
        location_data$source_file[[1]],
        "' is missing heat-profile keys: ",
        paste(missing_heat_keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(location_data[c("hour", "heat_profile_key")]) > 0) {
    stop(
      paste0(
        "Location '",
        location_data$source_file[[1]],
        "' contains duplicate rows for the same hour and heat-profile key."
      ),
      call. = FALSE
    )
  }

  base_table <- location_data %>%
    distinct(hour, month, weekday, day, time, temperature) %>%
    arrange(hour)

  heat_table <- location_data %>%
    select(hour, heat_profile_key, pred_load_new) %>%
    pivot_wider(names_from = heat_profile_key, values_from = pred_load_new)

  output_table <- base_table %>%
    left_join(heat_table, by = "hour")

  for (output_name in names(heat_column_map)) {
    input_key <- heat_column_map[[output_name]]

    if (!input_key %in% names(output_table)) {
      output_table[[input_key]] <- NA_real_
    }
  }

  output_table <- output_table %>%
    rename(!!!setNames(unname(heat_column_map), names(heat_column_map))) %>%
    left_join(electricity_table, by = "hour")

  output_table %>%
    transmute(
      month_out = month,
      weekday_out = weekday,
      day_out = day,
      time_out = time,
      hour_out = hour,
      temperature_out = temperature,
      !!!syms(output_column_order[7:length(output_column_order)])
    ) %>%
    setNames(output_column_order)
}

build_output_filename <- function(location_data) {
  source_file <- unique(location_data$source_file)

  paste0(
    "Timprofiler ",
    extract_location_name(source_file[[1]]),
    ".csv"
  )
}

write_output_csv <- function(data, path) {
  write.table(
    data,
    file = path,
    sep = ";",
    dec = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )
}

export_profiler_files <- function(profiler_data, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  template_hours <- sort(unique(profiler_data$hour))
  electricity_table <- build_electricity_table(template_hours)

  profiler_data %>%
    group_by(source_file, year) %>%
    group_walk(\(.x, .y) {
      output_table <- build_output_table(.x, electricity_table)
      output_path <- file.path(output_dir, build_output_filename(.x))
      write_output_csv(output_table[, output_column_order], output_path)
      message("Wrote: ", output_path)
    })
}

main <- function() {
  profiler_data <- read_profiler_results(input_path)

  export_profiler_files(
    profiler_data = profiler_data,
    output_dir = normalize_fs_path(output_dir, must_work = FALSE)
  )
}

if (sys.nframe() == 0) {
  main()
}
