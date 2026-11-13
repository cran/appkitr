# Helper function to test basic survey structure
test_basic_survey_structure <- function(survey, survey_name) {
  # Basic structure tests
  expect_s3_class(survey, "data.frame")
  expect_true(nrow(survey) > 0)
  expect_true(ncol(survey) > 0)

  # Check core AppKit columns (should be present in all surveys)
  expect_true("personalParticipantCode" %in% colnames(survey))
  expect_true("generalLoginCode" %in% colnames(survey))
}

# Helper function to test data types and formats
test_data_types <- function(survey, survey_name) {

  # Test AppKit metadata columns have correct types
  if ("committed" %in% colnames(survey)) {
    expect_true(inherits(survey$committed, c("POSIXct", "POSIXt")))
  }
  if ("scheduled" %in% colnames(survey)) {
    expect_true(inherits(survey$scheduled, c("POSIXct", "POSIXt")))
  }
  if ("published" %in% colnames(survey)) {
    expect_true(inherits(survey$published, c("POSIXct", "POSIXt")))
  }
  if ("firstOpened" %in% colnames(survey)) {
    expect_true(inherits(survey$firstOpened, c("POSIXct", "POSIXt")))
  }
  if ("exported" %in% colnames(survey)) {
    expect_true(inherits(survey$exported, c("POSIXct", "POSIXt")))
  }
  if ("expired" %in% colnames(survey)) {
    expect_true(inherits(survey$expired, c("POSIXct", "POSIXt")))
  }
  if ("lastStatusChange" %in% colnames(survey)) {
    expect_true(inherits(survey$lastStatusChange, c("POSIXct", "POSIXt")))
  }

  # Test character columns
  if ("personalParticipantCode" %in% colnames(survey)) {
    expect_true(is.character(survey$personalParticipantCode))
  }
  if ("generalLoginCode" %in% colnames(survey)) {
    expect_true(is.character(survey$generalLoginCode))
  }
  if ("osVersion" %in% colnames(survey)) {
    expect_true(is.character(survey$osVersion))
  }
  if ("smartphoneType" %in% colnames(survey)) {
    expect_true(is.character(survey$smartphoneType))
  }

  # Test logical columns (exit column should be logical if present)
  if ("exit" %in% colnames(survey)) {
    expect_true(is.logical(survey$exit))
  }
}

# Helper function to test data quality
test_data_quality <- function(survey, survey_name) {
  # Test that core columns have no completely empty values
  if ("personalParticipantCode" %in% colnames(survey)) {
    expect_true(any(!is.na(survey$personalParticipantCode)))
  }
  if ("generalLoginCode" %in% colnames(survey)) {
    expect_true(any(!is.na(survey$generalLoginCode)))
  }

  # Test timestamp columns are properly formatted (should be POSIXct/POSIXt, not character)
  timestamp_cols <- c("committed", "scheduled", "published", "firstOpened", "exported", "lastStatusChange",
                      "expired")
  for (col in timestamp_cols) {
    if (col %in% colnames(survey)) {
      # Should be POSIXct/POSIXt type (not character with "FALSE" strings)
      expect_true(inherits(survey[[col]], c("POSIXct", "POSIXt")))
      # All non-NA values should be finite timestamps
      non_na_values <- survey[[col]][!is.na(survey[[col]])]
      if (length(non_na_values) > 0) {
        expect_true(all(is.finite(as.numeric(non_na_values))))
      }
    }
  }

  # Test that numeric columns don't have unexpected string values
  numeric_cols <- colnames(survey)[sapply(survey, is.numeric)]
  for (col in numeric_cols) {
    expect_true(all(is.na(survey[[col]]) | is.finite(survey[[col]])))
  }
}

# Automatically discover all test datasets in data/ folder (folders not starting with _)
if (dir.exists("data")) {
  all_dirs <- list.dirs("data", full.names = FALSE, recursive = FALSE)
  test_datasets <- file.path("data", all_dirs[!grepl("^_", all_dirs)])
  test_datasets <- test_datasets[test_datasets != "data"] # Remove the data folder itself
} else {
  test_datasets <- c() # Fallback
  warning("No 'data' directory found. Skipping tests that require datasets.")
}

# Function to get all survey subdirectories from a dataset
get_survey_subdirs <- function(dataset_path) {
  if (!dir.exists(dataset_path)) {
    return(character(0))
  }

  subdirs <- list.dirs(dataset_path, full.names = FALSE, recursive = FALSE)
  subdirs <- subdirs[!grepl("^_", subdirs)] # Exclude directories starting with _
  return(subdirs)
}

# Test read_appkit_surveys on each dataset (tests multiple surveys at once)
for (dataset in test_datasets) {
  test_that(paste("read_appkit_surveys works with", basename(dataset)), {
    # Skip if test data doesn't exist
    skip_if_not(dir.exists(dataset), paste("Test data directory not found:", dataset))

    surveys <- read_appkit_surveys(dataset)

    # Should return a list of surveys
    expect_type(surveys, "list")
    expect_true(length(surveys) > 0)

    # Each survey should be a data frame with proper structure
    for (survey_name in names(surveys)) {
      survey <- surveys[[survey_name]]

      # Run comprehensive tests on each survey
      test_basic_survey_structure(survey, survey_name)
      test_data_types(survey, survey_name)
      test_data_quality(survey, survey_name)
    }
  })
}

# Test read_appkit_survey on individual survey directories (dynamically discovered)
for (dataset in test_datasets) {
  survey_subdirs <- get_survey_subdirs(dataset)

  for (subdir in survey_subdirs) {
    test_that(paste("read_appkit_survey works with", subdir, "from", basename(dataset)), {
      survey_path <- file.path(dataset, subdir)

      # Skip if survey directory doesn't exist
      skip_if_not(dir.exists(survey_path), paste("Survey directory not found:", survey_path))

      survey <- read_appkit_survey(survey_path)

      # Run comprehensive tests
      test_basic_survey_structure(survey, basename(subdir))
      test_data_types(survey, basename(subdir))
      test_data_quality(survey, basename(subdir))
    })
  }
}

test_that(".parse_timestamps handles ISO 8601 format with timezone offset", {
  iso_timestamps <- c(
    "2025-03-10T09:49:56+01:00",
    "2025-03-10T12:06:17+01:00",
    NA
  )

  result <- appkitr:::.parse_timestamps(iso_timestamps)

  expect_s3_class(result, "POSIXct")
  expect_equal(sum(!is.na(result)), 2)
  expect_true(is.na(result[3]))
})

test_that(".parse_timestamps handles mixed old and ISO 8601 formats", {
  # A vector mixing old-style and new ISO 8601 timestamps
  # Note: old format uses English month abbreviations (parsed via C locale)
  # German locale abbreviations like "Mär" were never parseable, which is
  # one reason AppKit switched to ISO 8601
  mixed <- c(
    "10-Mar-2025 09:49:56",
    "2025-03-10T12:06:17+01:00"
  )

  result <- appkitr:::.parse_timestamps(mixed)

  expect_s3_class(result, "POSIXct")
  expect_equal(sum(!is.na(result)), 2)
})

test_that("N/A values in AppKit >= 2.0.0 exports are converted to NA by default", {
  dataset <- "data/emotionality_test_data_2.0.0"
  skip_if_not(dir.exists(dataset), paste("Test data directory not found:", dataset))

  surveys <- read_appkit_surveys(dataset)

  # The raw CSV files contain "N/A" as a missing value marker (#35),
  # none of it should survive reading with the default na_values
  for (survey_name in names(surveys)) {
    survey <- surveys[[survey_name]]
    char_cols <- colnames(survey)[sapply(survey, is.character)]
    for (col in char_cols) {
      expect_false(any(survey[[col]] == "N/A", na.rm = TRUE),
        info = paste0("'N/A' not converted in ", survey_name, "$", col)
      )
    }
  }
})

test_that("-1 missing codes in numeric columns are converted to NA by default", {
  dataset <- "data/emotionality_test_data_2.0.0/Entry_Survey"
  skip_if_not(dir.exists(dataset), paste("Test data directory not found:", dataset))

  # The raw age column contains -1 as a missing value marker (#47)
  survey <- read_appkit_survey(dataset)
  expect_false(any(survey$age == -1, na.rm = TRUE))
})

test_that("timestamp_cols parameter controls which columns are parsed as timestamps", {
  dataset <- "data/emotionality_test_data_2.0.0/Entry_Survey"
  skip_if_not(dir.exists(dataset), paste("Test data directory not found:", dataset))

  # Default: registered timestamp columns become POSIXct
  survey_default <- read_appkit_survey(dataset)
  expect_s3_class(survey_default$committed, "POSIXct")
  expect_s3_class(survey_default$scheduled, "POSIXct")

  # Restricting timestamp_cols leaves the other columns unparsed
  survey_subset <- read_appkit_survey(dataset, timestamp_cols = "committed")
  expect_s3_class(survey_subset$committed, "POSIXct")
  expect_false(inherits(survey_subset$scheduled, "POSIXct"))

  # Empty timestamp_cols disables timestamp parsing entirely
  survey_none <- read_appkit_survey(dataset, timestamp_cols = character(0))
  expect_false(inherits(survey_none$committed, "POSIXct"))
})

test_that("error handling works correctly", {
  # Test with non-existent directory
  expect_error(read_appkit_survey("nonexistent"),
    class = "error"
  )

  # Test with directory without required files
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_test")
  dir.create(empty_dir, showWarnings = FALSE)

  expect_error(read_appkit_survey(empty_dir),
    class = "error"
  )

  unlink(empty_dir, recursive = TRUE)
})

test_that("read_appkit_surveys throws error when no surveys found", {
  # Create a unique empty directory for testing
  empty_surveys_dir <- tempfile(pattern = "empty_surveys_test")
  dir.create(empty_surveys_dir)

  # Expect error when no survey directories are found
  expect_error(
    read_appkit_surveys(empty_surveys_dir),
    "No directories with AppKit survey files found"
  )

  # Clean up
  unlink(empty_surveys_dir, recursive = TRUE)
})

test_that("duplicate_names parameter works correctly", {
  # Test invalid duplicate_names value
  expect_error(
    read_appkit_surveys(".", duplicate_names = "invalid"),
    "duplicate_names must be one of: 'error', 'merge', 'distinct'"
  )

  # Skip the following tests if no test data is available
  skip_if(length(test_datasets) == 0, "No test datasets available")

  # Use the first available dataset for testing
  test_dataset <- test_datasets[1]
  skip_if_not(dir.exists(test_dataset), paste("Test data directory not found:", test_dataset))

  # Test default behavior (error on duplicates) with original function
  # We'll test with a modified directory structure for this
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "duplicate_test")
  dir.create(test_dir, showWarnings = FALSE)

  # Copy first survey directory twice to create duplicates
  survey_subdirs <- get_survey_subdirs(test_dataset)
  if (length(survey_subdirs) > 0) {
    source_dir <- file.path(test_dataset, survey_subdirs[1])

    # Create two directories with surveys that will have the same CSV name
    if (dir.exists(source_dir)) {
      csv_files <- list.files(source_dir, pattern = "\\.csv$", full.names = FALSE)
      if (length(csv_files) > 0) {
        # Create first copy
        dest_dir1 <- file.path(test_dir, "survey1")
        dir.create(dest_dir1, showWarnings = FALSE)
        file.copy(list.files(source_dir, full.names = TRUE), dest_dir1)

        # Create second copy (will have same CSV name)
        dest_dir2 <- file.path(test_dir, "survey2")
        dir.create(dest_dir2, showWarnings = FALSE)
        file.copy(list.files(source_dir, full.names = TRUE), dest_dir2)

        # Test error mode (default)
        expect_error(
          read_appkit_surveys(test_dir),
          "Duplicate survey name found"
        )

        # Test distinct mode
        surveys_distinct <- read_appkit_surveys(test_dir, duplicate_names = "distinct")
        expect_type(surveys_distinct, "list")
        expect_equal(length(surveys_distinct), 2)
        # Should have names like "surveyname" and "surveyname_2"
        survey_names <- names(surveys_distinct)
        expect_true(any(grepl("_2$", survey_names)))

        # Test merge mode
        surveys_merged <- read_appkit_surveys(test_dir, duplicate_names = "merge")
        expect_type(surveys_merged, "list")
        expect_equal(length(surveys_merged), 1)
        # Merged survey should have twice as many rows as the original
        original_survey <- read_appkit_survey(source_dir)
        merged_survey <- surveys_merged[[1]]
        expect_equal(nrow(merged_survey), 2 * nrow(original_survey))
      }
    }
  }

  unlink(test_dir, recursive = TRUE)
})

test_that(".strip_html removes HTML while preserving text", {

  html_label <- paste0(
    '<span style="color: rgb(0, 0, 0);">',
    "Dies ist eine klassische Frage vom Typ Likert. ",
    "In diesem Fall hat der Forschende eingestellt, ",
    'dass die Skala 5-<em style="color: rgb(0, 0, 0);">',
    'stufig<span style="color: rgb(0, 0, 0);">',
    " und links, recht sowie in der Mitte ",
    "beschriftet sein soll. "
  )

  expected_label <- paste(
    "Dies ist eine klassische Frage vom Typ Likert.",
    "In diesem Fall hat der Forschende eingestellt,",
    "dass die Skala 5-stufig und links, recht sowie",
    "in der Mitte beschriftet sein soll."
  )



  input <- c(
    "<span>Formatted <em>text</em></span>",
    "Text<br>More",
    "value < 5",
    html_label,
    NA_character_
  )

  result <- appkitr:::.strip_html(input)

  expect_equal(
    result,
    c(
      "Formatted text",
      "Text More",
      "value < 5",
      expected_label,
      NA_character_
    )
  )
})
