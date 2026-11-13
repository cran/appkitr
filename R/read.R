#' Apply Labels to Data Frame (Internal)
#'
#' Internal function to apply extracted variable and value labels to a data frame.
#' Sets the "label" attribute for variables and converts variables with value labels
#' into factors.
#' @param df Data frame to apply labels to
#' @param labels List containing var_labels and value_labels
#' @return Data frame with labels applied
#' @keywords internal

.apply_labels <- function(df, labels) {
  # Apply value labels: for each variable with a value label mapping,
  # convert that column into a factor so that the value labels are visible.
  for (var in names(labels$value_labels)) {
    df_col <- names(df)[tolower(names(df)) == var]
    if (length(df_col) == 1) {
      mapping <- labels$value_labels[[var]]
      df[[df_col]] <- factor(as.character(df[[df_col]]),
        levels = names(mapping),
        labels = mapping
      )
    }
  }

  # Apply variable labels as an attribute
  for (var in names(labels$var_labels)) {
    df_col <- names(df)[tolower(names(df)) == var]
    if (length(df_col) == 1) {
      attr(df[[df_col]], "label") <- labels$var_labels[[var]]
    }
  }

  return(df)
}

#' Format Survey Data (Internal)
#'
#' Internal function to handle data formatting including NA values,
#' logical conversion, and timestamp parsing.
#' @param df Data frame to format
#' @param na_values Optional list or vector of values to treat as NA
#' @param timestamp_cols Character vector of column names to parse as timestamps
#' @return Formatted data frame
#' @keywords internal

.format_survey_data <- function(df, na_values = NULL, timestamp_cols = .appkit_timestamp_cols()) {
  # Set NAs for the na_values if they are set
  df <- .format_na_values(df, na_values)

  # Convert 'true'/'TRUE' and 'false'/'FALSE' strings to logical values
  df <- .format_char_to_logical(df)

  # Keep only the timestamp columns present in the data frame and format them
  timestamp_cols <- timestamp_cols[timestamp_cols %in% names(df)]

  df <- .format_timestamp_columns(df, timestamp_cols)

  return(df)
}

#' Read AppKit Survey Data from Directory
#'
#' Reads an AppKit survey dataset from a directory containing both a CSV file
#' and an SPS syntax file. Automatically applies variable and value labels,
#' converts eligible character columns to logical, and handles NA values.
#'
#' @param survey_directory A character string specifying the path to a directory
#'   containing both a .csv data file and a .sps syntax file.
#' @param na_values Optional list or vector of values to treat as NA
#'   (default: list(-1, "", "Not applicable", "N/A")). All specified values will be converted to
#'   NA across all columns with appropriate type handling.
#' @eval paste0(
#'   "@param timestamp_cols Character vector of column names to parse as timestamps ",
#'   "(default: the AppKit timestamp columns ",
#'   paste(.appkit_timestamp_cols(), collapse = ", "),
#'   "). Columns not present in the data are ignored."
#' )
#'
#' @return A data frame containing the processed survey data with variable and value
#'   labels applied as attributes and appropriate data types set.
#'
#' @details
#' The function performs the following operations
#' - Reads the CSV file using semicolon separator
#' - Converts specified values to NA with type-appropriate handling
#' - Converts character columns containing only "true"/"false" to logical
#' - Parses the `timestamp_cols` columns with multiple format attempts
#' - Extracts variable and value labels from the SPS file
#' - Applies labels to the data frame (variable labels as attributes, value labels as factors)
#'
#' @examples
#' survey_dir <- system.file("extdata", "example_survey",
#'   package = "appkitr", mustWork = TRUE
#' )
#'
#' # Read survey from directory with default settings
#' survey <- read_appkit_survey(survey_dir)
#'
#' # Read survey and treat custom values as missing
#' survey <- read_appkit_survey(survey_dir, na_values = c(-1, -99))
#'
#' @export
#' @importFrom utils read.csv

read_appkit_survey <- function(survey_directory, na_values = list(-1, "", "Not applicable", "N/A"),
                               timestamp_cols = .appkit_timestamp_cols()) {
  # Find the CSV file
  csv_files <- list.files(survey_directory, pattern = "\\.csv$", full.names = TRUE)

  if (length(csv_files) == 0) {
    stop("No CSV file found in the survey directory.")
  } else if (length(csv_files) > 1) {
    warning("Multiple CSV files found in the survey directory. Using the first one.")
  }

  survey_csv_path <- csv_files[1]

  # Read the CSV file
  survey_data <- read.csv(survey_csv_path, stringsAsFactors = FALSE, sep = ";")

  # Format the data (NA values, logical conversion, timestamps)
  survey_data <- .format_survey_data(survey_data, na_values, timestamp_cols)

  # Find the SPS file(s) and apply the labels if available
  sps_files <- list.files(survey_directory, pattern = "\\.sps$", full.names = TRUE)

  if (length(sps_files) == 0) {
    warning("No SPS file found in the survey directory. Variable and value labels will not be applied.")
    return(survey_data)
  } else if (length(sps_files) > 1) {
    warning("Multiple SPS files found in the survey directory. Using the first one.")
  }

  sps_file <- sps_files[1]
  labels <- .extract_sps_labels(sps_file)

  # strip html
  labels$var_labels <- .strip_html(labels$var_labels)

  return(.apply_labels(survey_data, labels))
}

#' Read Multiple AppKit Surveys from Directory
#'
#' Reads multiple AppKit surveys from a parent directory where each subdirectory
#' contains one survey (CSV + SPS files). Returns a named list of data frames.
#'
#' @param surveys_directory A character string specifying the path to a directory
#'   containing subdirectories, each with survey data (CSV + SPS files).
#' @param na_values Optional list or vector of values to treat as NA across all surveys
#'   (default: list(-1, "", "Not applicable", "N/A")). All specified values will be converted to
#'   NA across all columns with appropriate type handling.
#' @param duplicate_names Character string specifying how to handle surveys with identical names.
#'   Options are:
#'   \describe{
#'     \item{"error"}{(default) Throw an error when duplicate survey names are found}
#'     \item{"merge"}{Combine surveys with the same name using rbind()}
#'     \item{"distinct"}{Keep surveys distinct by adding numeric suffixes (e.g., "survey", "survey_2", "survey_3")}
#'   }
#' @param timestamp_cols Character vector of column names to parse as timestamps
#'   across all surveys (default: the AppKit timestamp columns). Columns not
#'   present in a survey are ignored.
#'
#' @return A named list where each element is a processed survey data frame.
#'   Names are derived from the CSV filenames (with .csv extension removed and
#'   spaces replaced with underscores).
#'
#' @examples
#' surveys_dir <- system.file("extdata", "example_surveys",
#'   package = "appkitr", mustWork = TRUE
#' )
#'
#' # Read all surveys from a parent directory (default: error on duplicates)
#' surveys <- read_appkit_surveys(surveys_dir)
#'
#' # Read surveys and merge those with identical names
#' surveys <- read_appkit_surveys(surveys_dir, duplicate_names = "merge")
#'
#' # Read surveys and keep duplicates distinct with suffixes
#' surveys <- read_appkit_surveys(surveys_dir, duplicate_names = "distinct")
#'
#' # Read surveys with custom NA values
#' surveys <- read_appkit_surveys(surveys_dir, na_values = c(-1, -99, ""))
#'
#' # Access individual surveys
#' survey1 <- surveys[["survey_name_1"]]
#'
#' @export

read_appkit_surveys <- function(surveys_directory,
                                na_values = list(-1, "", "Not applicable", "N/A"),
                                duplicate_names = "error",
                                timestamp_cols = .appkit_timestamp_cols()) {
  # Validate duplicate_names parameter
  if (!duplicate_names %in% c("error", "merge", "distinct")) {
    stop("duplicate_names must be one of: 'error', 'merge', 'distinct'")
  }

  survey_directories <- list.dirs(surveys_directory, full.names = TRUE, recursive = FALSE)
  surveys <- list()

  # Keep track of survey names for distinct mode
  survey_name_counts <- list()

  for (survey_directory in survey_directories) {
    # Create a survey name based on the CSV file name (with spaces replaced by underscores)
    csv_files <- list.files(survey_directory, pattern = "\\.csv$", full.names = FALSE)
    if (length(csv_files) > 0) {
      survey_name <- csv_files[1]
      survey_name <- sub("\\.csv$", "", survey_name)
      survey_name <- gsub(" ", "_", survey_name)

      survey_data <- read_appkit_survey(survey_directory,
        na_values = na_values,
        timestamp_cols = timestamp_cols
      )

      # Handle duplicate names based on the chosen strategy
      if (survey_name %in% names(surveys)) {
        if (duplicate_names == "error") {
          stop(paste(
            "Duplicate survey name found:", survey_name,
            "\nUse duplicate_names = 'merge' to combine surveys or ",
            "duplicate_names = 'distinct' to keep them separate."
          ))
        } else if (duplicate_names == "merge") {
          # Merge with existing survey using rbind
          existing_data <- surveys[[survey_name]]
          surveys[[survey_name]] <- tryCatch(
            rbind(existing_data, survey_data),
            error = function(e) {
              stop(paste0(
                "Error combining surveys with name '", survey_name, "': ", e$message,
                "\nEnsure both surveys have compatible column structures."
              ))
            }
          )
        } else if (duplicate_names == "distinct") {
          # Create a unique name by adding a suffix
          if (!survey_name %in% names(survey_name_counts)) {
            survey_name_counts[[survey_name]] <- 1
          }
          survey_name_counts[[survey_name]] <- survey_name_counts[[survey_name]] + 1
          unique_name <- paste0(survey_name, "_", survey_name_counts[[survey_name]])
          surveys[[unique_name]] <- survey_data
        }
      } else {
        # First occurrence of this survey name
        surveys[[survey_name]] <- survey_data
        survey_name_counts[[survey_name]] <- 1
      }
    }
  }

  # stop if no surveys are found, return surveys otherwise
  if (length(surveys) == 0) {
    stop(paste0(
      "No directories with AppKit survey files found in given directory '",
      surveys_directory, "'. Did you specify the right directory?"
    ))
  }

  # return data
  return(surveys)
}
