#' Inspect Variables in a Data Frame or Single Variable
#'
#' Provides a formatted overview of variables in a data frame, showing variable names,
#' labels, unique values, and value labels. Can also inspect a single variable.
#'
#' @param data A data frame or a single variable to inspect
#' @param max_unique Integer indicating the maximum number of unique values to display (default: 10)
#' @param max_width Integer indicating the maximum width for displaying unique values (default: 50)
#' @param verbose Logical, if set to FALSE, suppresses all console output (default: TRUE)
#' @return A tibble-like display of variable information (returned invisibly)
#'
#' @examples
#' # Inspect all variables in the bundled example survey
#' inspect_variables(appkit_survey_entry)
#'
#' @export
#' @importFrom cli rule col_cyan style_italic col_grey
#' @importFrom utils head

inspect_variables <- function(data, max_unique = 10, max_width = 50, verbose = TRUE) {
  # Check if input is a dataframe or a single variable
  if (is.data.frame(data)) {
    # Process each variable in the dataframe
    results <- lapply(names(data), function(var_name) {
      inspect_variable(data[[var_name]], var_name, max_unique, max_width)
    })
    result_df <- do.call(rbind, results)
  } else {
    # Process a single variable
    var_name <- deparse(substitute(data))
    result_df <- inspect_variable(data, var_name, max_unique, max_width)
  }

  if (verbose) {
    # Print with formatting
    cat("\n")
    cat(cli::rule(left = "Variable Inspector", col = "cyan", width = 80), "\n\n")

    # Print each row with formatting
    for (i in seq_len(nrow(result_df))) {
      cat(cli::col_cyan(paste0(result_df$var_name[i], " (", result_df$var_type[i], "): ")))

      # Print variable label if it exists
      if (!is.na(result_df$var_label[i]) && result_df$var_label[i] != "") {
        cat(cli::style_italic(result_df$var_label[i]), "\n")
      } else {
        cat("<no label>\n")
      }

      # Print unique values
      cat("  Unique values: ", cli::col_grey(result_df$unique_values[i]), "\n")

      # Print value labels if they exist
      if (!is.na(result_df$value_labels[i]) && result_df$value_labels[i] != "") {
        cat("  Value labels: ", cli::col_grey(result_df$value_labels[i]), "\n")
      } else {
        cat("  Value labels: ", cli::col_grey("<none>"), "\n")
      }

      # Add separator between variables
      if (i < nrow(result_df)) {
        cat(cli::rule(col = "grey", width = 80), "\n")
      }
    }

    cat("\n", cli::rule(width = 80), "\n")
  }

  # Return invisibly for potential further processing
  invisible(result_df)
}

#' Inspect a Single Variable
#'
#' Extracts and formats information about a single variable including its label,
#' unique values, and value labels.
#'
#' @param var The variable to inspect
#' @param var_name The name of the variable (optional, auto-detected if not provided)
#' @param max_unique Maximum number of unique values to display (default: 10)
#' @param max_width Maximum width for displaying unique values (default: 50)
#' @return A one-row data frame with variable information
#'
#' @examples
#' # Inspect a single variable of the bundled example survey
#' var_info <- inspect_variable(appkit_survey_entry$aim01)
#'
#' @export

inspect_variable <- function(var, var_name = NULL, max_unique = 10, max_width = 50) {
  # Auto-detect variable name if not provided
  if (is.null(var_name)) {
    var_name <- deparse(substitute(var))
  }

  # Extract variable label (checking multiple common attribute names)
  var_label <- ""
  label_attrs <- c("label", "Label", "description", "Description", "var.label")
  for (attr_name in label_attrs) {
    if (!is.null(attr(var, attr_name))) {
      var_label <- attr(var, attr_name)
      break
    }
  }

  # Get variable type
  var_type <- class(var)[1] # Get the first/primary class

  # Get unique values
  unique_vals <- unique(var)
  unique_vals <- unique_vals[!is.na(unique_vals)]

  # Format unique values for display
  if (length(unique_vals) > max_unique) {
    unique_text <- paste0(
      toString(head(unique_vals, max_unique)),
      " ... (", length(unique_vals), " total)"
    )
  } else {
    unique_text <- toString(unique_vals)
  }

  # Truncate if too long
  if (nchar(unique_text) > max_width) {
    unique_text <- paste0(substr(unique_text, 1, max_width), "...")
  }

  # Extract value labels (checking multiple common attribute names)
  value_labels <- ""
  label_attrs <- c("labels", "Levels", "value.labels", "values", "valueLabels")

  # First check for specific label attributes
  for (attr_name in label_attrs) {
    if (!is.null(attr(var, attr_name))) {
      labels <- attr(var, attr_name)
      if (is.null(names(labels))) {
        # If no names, create generic labels
        if (is.character(labels) || is.factor(labels)) {
          label_text <- paste(labels, collapse = ", ")
        } else {
          label_text <- paste(seq_along(labels), "=", labels, collapse = ", ")
        }
      } else {
        label_text <- paste(names(labels), "=", labels, collapse = ", ")
      }

      # Truncate if too long
      if (nchar(label_text) > max_width) {
        label_text <- paste0(substr(label_text, 1, max_width), "...")
      }

      value_labels <- label_text
      break
    }
  }

  # Handle factors separately, as they have levels but not necessarily a "labels" attribute
  if (value_labels == "" && is.factor(var)) {
    factor_levels <- levels(var)
    if (length(factor_levels) > 0) {
      level_text <- paste(seq_along(factor_levels), "=", factor_levels, collapse = ", ")

      # Truncate if too long
      if (nchar(level_text) > max_width) {
        level_text <- paste0(substr(level_text, 1, max_width), "...")
      }

      value_labels <- level_text
    }
  }

  # Create a one-row dataframe with the information
  data.frame(
    var_name = var_name,
    var_type = var_type,
    var_label = var_label,
    unique_values = unique_text,
    value_labels = value_labels,
    stringsAsFactors = FALSE
  )
}
