#' Parse Timestamps with Multiple Formats (Internal)
#'
#' Internal function to parse timestamp columns with multiple format attempts.
#' Tries ISO 8601 (AppKit >= 1.17.0) first, then falls back to older formats.
#' Uses locale-safe parsing with English month names. If input is already POSIXct,
#' returns it unchanged.
#'
#' @param timestamp_vector Character vector of timestamps to parse, or POSIXct vector
#' @param orders Character vector of format orders. Can be simplified orders
#'   like "ymd HMS" or full format strings like "%Y-%m-%d %H:%M:%S%z"
#' @return POSIXct vector of parsed timestamps
#' @keywords internal

.parse_timestamps <- function(timestamp_vector, orders = c("ymd_T_HMS_z", "ymd HMS", "dmy HMS", "ymd", "dmy")) {
  if (length(timestamp_vector) == 0) {
    return(as.POSIXct(character(0)))
  }

  # If already POSIXct, return as-is
  if (inherits(timestamp_vector, "POSIXct")) {
    return(timestamp_vector)
  }

  # Normalize ISO 8601 timezone offsets: "+01:00" -> "+0100"
  # R's %z expects the offset without colon
  timestamp_vector <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", timestamp_vector)

  # Convert simplified orders to actual format strings if needed
  format_map <- list(
    "ymd_T_HMS_z" = "%Y-%m-%dT%H:%M:%S%z",
    "ymd HMS" = "%Y-%m-%d %H:%M:%S",
    "dmy HMS" = "%d-%m-%Y %H:%M:%S",
    "ymd" = "%Y-%m-%d",
    "dmy" = "%d-%m-%Y"
  )

  # Convert orders to formats, handling both simplified and full format strings
  formats <- character(length(orders))
  for (i in seq_along(orders)) {
    if (orders[i] %in% names(format_map)) {
      formats[i] <- format_map[[orders[i]]]
    } else {
      formats[i] <- orders[i] # Assume it's already a format string
    }
  }

  # Add additional common formats not covered by orders
  additional_formats <- c(
    "%d-%b-%Y %H:%M:%S", # 25-Dec-2023 14:30:45
    "%d/%m/%Y %H:%M:%S" # 25/12/2023 14:30:45
  )
  formats <- c(formats, additional_formats)

  result <- rep(as.POSIXct(NA), length(timestamp_vector))

  for (fmt in formats) {
    # Find which elements are still NA
    na_indices <- is.na(result)
    if (!any(na_indices)) break

    # Try to parse remaining NA values with current format using C locale
    parsed <- suppressWarnings(
      withr::with_locale(
        c("LC_TIME" = "C"),
        as.POSIXct(timestamp_vector[na_indices], format = fmt)
      )
    )

    # Update result where parsing succeeded
    success_indices <- !is.na(parsed)
    result[na_indices][success_indices] <- parsed[success_indices]
  }

  return(result)
}

#' Convert Character Columns to Logical (Internal)
#'
#' Internal function to convert character columns that contain only "true"/"false"
#' (ignoring case) and NA values into logical columns.
#' @param df Data frame to process
#' @return Data frame with eligible character columns converted to logical
#' @keywords internal

.format_char_to_logical <- function(df) {
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      # Get all unique values in lower case, excluding NA
      unique_vals <- unique(tolower(df[[col]]))
      unique_vals <- unique_vals[!is.na(unique_vals)]

      # If all non-NA values are either "true" or "false"
      if (length(unique_vals) > 0 && all(unique_vals %in% c("true", "false"))) {
        # Convert column: "true" becomes TRUE, "false" becomes FALSE, NA remains NA
        df[[col]] <- ifelse(tolower(df[[col]]) == "true", TRUE,
          ifelse(tolower(df[[col]]) == "false", FALSE, NA)
        )
      }
    }
  }
  return(df)
}

#' Format NA Values (Internal)
#'
#' Internal function to set specific values as NA with proper type checking.
#' @param df Data frame to process
#' @param na_values List or vector of values to convert to NA
#' @return Data frame with NA values set
#' @keywords internal

.format_na_values <- function(df, na_values) {
  if (is.null(na_values) || length(na_values) == 0) {
    return(df)
  }

  # Apply NA conversion to all columns for each na_value
  for (col in names(df)) {
    for (na_val in na_values) {
      if (is.numeric(df[[col]])) {
        # For numeric columns, convert if na_val can be interpreted as a number
        # (atomic na_values vectors coerce numeric codes like -1 to character)
        na_val_num <- suppressWarnings(as.numeric(na_val))
        if (!is.na(na_val_num)) {
          df[[col]][df[[col]] == na_val_num] <- NA
        }
      } else if (is.character(df[[col]]) || is.factor(df[[col]])) {
        # For character/factor columns, convert na_val to character
        df[[col]][df[[col]] == as.character(na_val)] <- NA
      }
    }
  }

  return(df)
}

#' Convert to Title Case (Internal)
#'
#' Internal function to convert strings to title case, replacing stringr::str_to_title
#' @param x Character vector to convert
#' @return Character vector with title case
#' @keywords internal

.to_title_case <- function(x) {
  # Convert to lowercase, then capitalize first letter of each word
  x <- tolower(x)
  # Split by word boundaries and capitalize first letter
  gsub("\\b([a-z])", "\\U\\1", x, perl = TRUE)
}

#' Extract Date from POSIXct (Internal)
#'
#' Internal function to extract date part from POSIXct
#' @param x POSIXct vector
#' @return Date vector
#' @keywords internal

.as_date <- function(x) {
  as.Date(x)
}

#' Format Timestamp Columns (Internal)
#'
#' Internal function to format specified timestamp columns.
#' Replaces "FALSE" values with NA before calling timestamp parsing.
#' @param df Data frame to process
#' @param timestamp_cols Character vector of column names to process as timestamps
#' @return Data frame with formatted timestamp columns
#' @keywords internal

.format_timestamp_columns <- function(df, timestamp_cols) {
  for (col in timestamp_cols) {
    if (col %in% names(df)) {
      # Replace "FALSE" values with NA before timestamp parsing
      if (is.character(df[[col]])) {
        df[[col]][df[[col]] == "FALSE"] <- NA
      }
      # Parse timestamps
      df[[col]] <- .parse_timestamps(df[[col]])
    }
  }
  return(df)
}

#' Strip HTML Formatting (Internal)
#'
#' Internal helper to remove HTML tags from character values while preserving
#' the enclosed text. Whitespace is normalized after tags are removed.
#'
#' @param x Character vector containing text that may include HTML formatting.
#' @return Character vector with HTML tags removed.
#' @keywords internal

.strip_html <- function(x) {
  vapply(x, function(text) {
    if (is.na(text)) return(NA_character_)
    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    output <- character()
    i <- 1L
    is_tag_char <- function(char) {
      char %in% c(letters, LETTERS, 0:9, "-", "_", ":")
    }
    while (i <= length(chars)) {
      if (chars[i] != "<") {
        output <- c(output, chars[i])
        i <- i + 1L
        next
      }
      j <- i + 1L
      if (j <= length(chars) && chars[j] == "/") j <- j + 1L
      # Preserve literal "<" unless followed by a plausible tag name
      if (j > length(chars) || !chars[j] %in% c(letters, LETTERS)) {
        output <- c(output, "<")
        i <- i + 1L
        next
      }
      name_start <- j
      while (j <= length(chars) && is_tag_char(chars[j])) j <- j + 1L
      tag_name <- tolower(paste0(chars[name_start:(j - 1L)], collapse = ""))
      quote <- NULL
      while (j <= length(chars)) {
        if (!is.null(quote)) {
          if (chars[j] == quote) quote <- NULL
        } else if (chars[j] %in% c("\"", "'")) {
          quote <- chars[j]
        } else if (chars[j] == ">") {
          break
        }
        j <- j + 1L
      }
      # Preserve malformed, unclosed markup
      if (j > length(chars)) {
        output <- c(output, "<")
        i <- i + 1L
        next
      }
      if (tag_name %in% c("br", "p", "div", "li", "hr")) {
        output <- c(output, " ")
      }
      i <- j + 1L
    }

    trimws(gsub("[[:space:]]+", " ", paste0(output, collapse = "")))
  }, character(1), USE.NAMES = FALSE)
}


#' Check a Data Frame for Required Columns (Internal)
#'
#' Internal function to check that a data frame contains the given columns,
#' erroring informatively when any are missing. Works with arbitrary column
#' names. For missing columns registered in [.appkit_cols] with a
#' `min_version`, the error states the required AppKit export version.
#'
#' @param df Data frame to check
#' @param cols Character vector of required column names
#' @param context Optional survey name used in error messages
#' @return `invisible(TRUE)` if all columns are present
#' @keywords internal

.check_df_cols <- function(df, cols, context = NULL) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) == 0L) {
    return(invisible(TRUE))
  }

  where <- if (is.null(context)) "data frame" else paste0("survey `", context, "`")

  min_versions <- .appkit_cols$min_version[match(missing_cols, .appkit_cols$column)]
  versioned <- !is.na(min_versions)
  version_hints <- paste0(
    "Column `", missing_cols[versioned], "` requires an AppKit >= ", min_versions[versioned], " export. ",
    collapse = ""
  )

  stop(
    if (length(missing_cols) == 1L) "Column " else "Columns ",
    paste0("`", missing_cols, "`", collapse = ", "),
    " not found in ", where, ". ",
    version_hints,
    "Available columns: ",
    paste(utils::head(names(df), 5), collapse = ", "),
    if (ncol(df) > 5) ", ..." else "",
    call. = FALSE
  )
  }

#' Ask for a Validated Non-Negative Number (Internal)
#'
#' Internal function to repeat a console prompt until a valid, non-negative
#' number is entered.
#' @param message Text shown to the user
#' @param max_decimals Optional integer. If set, rejects input with more decimals
#' (useful for currencies or whole numbers).
#' @return A single numeric value
#' @keywords internal

.ask_number <- function(message, max_decimals = NULL) {
  repeat {
    raw <- trimws(readline(message))
    value <- suppressWarnings(as.numeric(raw))
    if (is.na(value)) {
      message("Invalid number, please try again (use '.' as decimal separator, e.g. 2.10).")
      next
    }
    if (value < 0) {
      message("Value must be zero or positive, please try again.")
      next
    }
    if (!is.null(max_decimals) && round(value, max_decimals) != value) {
      message(sprintf("Please enter max. %d decimal place(s).", max_decimals))
      next
    }
    return(value)
  }
}

#' Ask for a Validated Yes/No Answer (Internal)
#'
#' Internal function to repeat a console prompt until "y"/"yes" or "n"/"no"
#' @param message Text shown to the user
#' @return TRUE for yes, FALSE for no
#' @keywords internal

.ask_yes_no <- function(message) {
  repeat {
    raw <- tolower(trimws(readline(message)))
    if (raw %in% c("y", "yes")) {
      return(TRUE)
    }
    if (raw %in% c("n", "no")) {
      return(FALSE)
    }
    message("Please answer 'y' or 'n'.")
  }
}

#' Ask for a Validated Choice from a Set of Options (Internal)
#'
#' Internal function to repeat a console prompt until one of the given
#' option codes is entered (case-insensitive).
#' @param message Text shown to the user
#' @param choices Character vector of valid option codes (lowercase)
#' @param choices_hint Text describing the choices, shown on invalid input
#' @return The matched choice (lowercase)
#' @keywords internal

.ask_choice <- function(message, choices, choices_hint) {
  repeat {
    raw <- tolower(trimws(readline(message)))
    if (raw %in% choices) {
      return(raw)
    }
    message(sprintf("Please answer %s.", choices_hint))
  }
}

#' Normalize survey input into a list of data frames
#'
#' Internal helper for `participation_table()` and `calculate_payout()`.
#' Both functions accept either a single survey data frame or a list of
#' survey data frames.
#' The name for a single data frame is passed in via `fallback_name`.
#' The functions `participation_table()` and `calculate_payout()` must compute
#' their own `deparse(substitute(data))` and pass the result in as `fallback_name`.
#'
#' @param data A single data frame, or a list of data frames.
#' @param fallback_name Character. Name to use for `data` if it is a single
#'   data frame.
#' @return A list of data frames.
#' @keywords internal

.normalize_survey_data <- function(data, fallback_name) {
  # wrap a single data frame into a one-element list
  if (is.data.frame(data)) {
    data <- setNames(list(data), fallback_name)
  }
  # here data must be a named list, no matter whether it came in as one or was wrapped above
  if (!is.list(data) || is.null(names(data)) || any(names(data) == "")) {
    stop(
      "`data` must be a single data frame, or a list of data frames ",
      "(e.g. output of read_appkit_surveys()).",
      call. = FALSE
    )
  }

  data
}

#' Interactive Binding for Mocking (Internal)
#'
#' Empty binding so with_mocked_bindings() can mock interactive() in tests.
#' See https://testthat.r-lib.org/reference/local_mocked_bindings.html
#' @noRd

interactive <- NULL
