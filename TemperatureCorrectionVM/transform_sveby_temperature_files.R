#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
})

# Edit these values and run the script in RStudio if you do not want to use command-line arguments.
# You can point to one or more CSV files and/or folders.
config_input_targets <- c(
  paste0("C:/Users/vanja/OneDrive - Profu/Fjärrkontrollen - Profu - Documents/Admin/",
         "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/",
         "Sveby Normalårstemperaturer 1991-2020"))
config_output_dir <- paste0("C:/Users/vanja/OneDrive - Profu/Fjärrkontrollen - Profu - Documents/Admin/",
                            "3. Underlag profiler, COP, inv kostnader osv/Profiler värme och el för fastigheterna/",
                            "Normalårstemperaturer 1991-2020 för modell")
config_time_mode <- "hour_of_day"

usage <- function() {
  stop(
    paste(
      "Usage:",
      "Rscript transform_sveby_temperature_files.R <input-file-or-folder> [more-files-or-folders ...] [--output-dir=PATH] [--time-mode=hour_of_day|hour_index]",
      "",
      "If you run the script without arguments, it uses config_input_targets and config_output_dir from the top of the file.",
      "Pass one or more CSV files and/or folders.",
      "--time-mode=hour_of_day keeps the source Hour column as time and writes hour = 1..8760.",
      "--time-mode=hour_index writes both time and hour as 1..8760.",
      sep = "\n"
    ),
    call. = FALSE
  )
}

to_weekday_monday_first <- function(date_value) {
  weekday_sunday_first <- as.POSIXlt(date_value)$wday
  ((weekday_sunday_first + 6L) %% 7L) + 1L
}

normalize_fs_path <- function(path, must_work = TRUE) {
  normalizePath(enc2native(path), winslash = "/", mustWork = must_work)
}

read_sveby_csv <- function(path) {
  lines <- readLines(normalize_fs_path(path), warn = FALSE, encoding = "UTF-8")
  header_index <- grep("^#\\s*Year;", lines)[1]

  if (length(header_index) == 0 || is.na(header_index)) {
    stop(
      paste0("Could not find the Sveby header row in file: ", path),
      call. = FALSE
    )
  }

  header_line <- sub("^#\\s*", "", lines[header_index])
  data_lines <- lines[(header_index + 1):length(lines)]

  utils::read.table(
    text = paste(c(header_line, data_lines), collapse = "\n"),
    sep = ";",
    dec = ".",
    header = TRUE,
    quote = "\"",
    comment.char = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

transform_sveby_file <- function(input_path, output_path = NULL, time_mode = "hour_of_day") {
  if (!time_mode %in% c("hour_of_day", "hour_index")) {
    stop("time_mode must be 'hour_of_day' or 'hour_index'.", call. = FALSE)
  }

  raw_data <- read_sveby_csv(input_path)
  temperature_col <- grep("^Dry-bulb Temperature", names(raw_data), value = TRUE)[1]

  if (is.na(temperature_col)) {
    stop(
      paste0("Could not find the dry-bulb temperature column in file: ", input_path),
      call. = FALSE
    )
  }

  required_cols <- c("Year", "Month", "Day", "Hour", temperature_col)
  missing_cols <- setdiff(required_cols, names(raw_data))

  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "File is missing required columns: ",
        paste(missing_cols, collapse = ", "),
        " in ",
        input_path
      ),
      call. = FALSE
    )
  }

  raw_data[required_cols] <- lapply(raw_data[required_cols], as.numeric)

  transformed <- data.frame(
    year = raw_data[["Year"]],
    month = raw_data[["Month"]],
    day = raw_data[["Day"]],
    time = raw_data[["Hour"]],
    temperature = raw_data[[temperature_col]],
    stringsAsFactors = FALSE
  )

  transformed <- transformed[!(transformed$month == 2 & transformed$day == 29), , drop = FALSE]

  distinct_years <- sort(unique(stats::na.omit(transformed$year)))
  output_year <- if (length(distinct_years) == 1) {
    distinct_years[[1]]
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }

  transformed$year <- rep.int(output_year, nrow(transformed))

  date_value <- as.Date(
    sprintf("%04d-%02d-%02d", transformed$year, transformed$month, transformed$day)
  )

  if (anyNA(date_value)) {
    stop(
      paste0("Failed to build valid dates from month/day values in file: ", input_path),
      call. = FALSE
    )
  }

  transformed$weekday <- to_weekday_monday_first(date_value)
  transformed$hour <- seq_len(nrow(transformed))

  if (time_mode == "hour_index") {
    transformed$time <- transformed$hour
  }

  transformed <- transformed[, c("year", "month", "weekday", "day", "time", "hour", "temperature")]

  if (nrow(transformed) != 8760) {
    stop(
      paste0(
        "Expected 8760 rows after transformation, but got ",
        nrow(transformed),
        " for file: ",
        input_path
      ),
      call. = FALSE
    )
  }

  if (!is.null(output_path)) {
    write_csv2(transformed, output_path)
  }

  invisible(transformed)
}

expand_input_paths <- function(input_targets) {
  expanded <- unlist(
    lapply(input_targets, function(target) {
      normalized_target <- normalize_fs_path(target, must_work = TRUE)

      if (dir.exists(normalized_target)) {
        list.files(normalized_target, pattern = "\\.csv$", full.names = TRUE)
      } else {
        normalized_target
      }
    }),
    use.names = FALSE
  )

  unique(expanded)
}

build_output_paths <- function(input_files, output_dir) {
  base_names <- basename(input_files)
  duplicate_index <- ave(seq_along(base_names), base_names, FUN = seq_along)
  duplicate_count <- ave(seq_along(base_names), base_names, FUN = length)
  stem_names <- tools::file_path_sans_ext(base_names)
  extensions <- tools::file_ext(base_names)

  output_names <- ifelse(
    duplicate_count > 1,
    paste0(stem_names, "_", duplicate_index, ".", extensions),
    base_names
  )

  file.path(output_dir, output_names)
}

get_configured_inputs <- function() {
  Filter(nzchar, trimws(config_input_targets))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    input_targets <- get_configured_inputs()

    if (length(input_targets) == 0) {
      stop(
        "Set config_input_targets at the top of the script, or pass input paths as command-line arguments.",
        call. = FALSE
      )
    }

    input_files <- expand_input_paths(input_targets)
    default_output_base <- if (length(input_files) == 1) dirname(input_files[[1]]) else getwd()
    output_dir <- if (nzchar(trimws(config_output_dir))) {
      normalize_fs_path(config_output_dir, must_work = FALSE)
    } else {
      file.path(default_output_base, "transformed")
    }
    time_mode <- config_time_mode
  } else {
    output_dir_arg <- grep("^--output-dir=", args, value = TRUE)
    time_mode_arg <- grep("^--time-mode=", args, value = TRUE)
    input_targets <- args[!grepl("^--output-dir=|^--time-mode=", args)]

    if (length(output_dir_arg) > 1 || length(time_mode_arg) > 1 || length(input_targets) == 0) {
      usage()
    }

    input_files <- expand_input_paths(input_targets)
    default_output_base <- if (length(input_files) == 1) dirname(input_files[[1]]) else getwd()
    output_dir <- if (length(output_dir_arg) == 1) {
      normalize_fs_path(sub("^--output-dir=", "", output_dir_arg), must_work = FALSE)
    } else {
      file.path(default_output_base, "transformed")
    }
    time_mode <- if (length(time_mode_arg) == 1) {
      sub("^--time-mode=", "", time_mode_arg)
    } else {
      "hour_of_day"
    }
  }

  if (length(input_files) == 0) {
    stop("No CSV files found to transform.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  output_paths <- build_output_paths(input_files, output_dir)

  for (i in seq_along(input_files)) {
    file_path <- input_files[[i]]
    output_path <- output_paths[[i]]
    transform_sveby_file(file_path, output_path, time_mode = time_mode)
    message("Wrote transformed file: ", output_path)
  }
}

if (sys.nframe() == 0) {
  main()
}
