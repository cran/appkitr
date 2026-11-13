#### Registry ####

test_that("registry is well-formed", {
  registry <- appkitr:::.appkit_cols
  expect_equal(anyDuplicated(registry$key), 0L)
  expect_equal(anyDuplicated(registry$column), 0L)
  expect_type(registry$timestamp, "logical")
  expect_false(anyNA(registry$timestamp))
  versions <- registry$min_version[!is.na(registry$min_version)]
  expect_s3_class(numeric_version(versions), "numeric_version")
})

test_that(".appkit_col returns registered column names", {
  expect_identical(appkitr:::.appkit_col("participant"), "personalParticipantCode")
  expect_identical(appkitr:::.appkit_col("expired"), "expired")
})

test_that(".appkit_timestamp_cols returns the registered timestamp columns", {
  registry <- appkitr:::.appkit_cols
  expect_identical(appkitr:::.appkit_timestamp_cols(), registry$column[registry$timestamp])
  expect_true(all(c("scheduled", "committed", "expired") %in% appkitr:::.appkit_timestamp_cols()))
})

test_that(".appkit_col errors on unknown or invalid keys", {
  expect_error(appkitr:::.appkit_col("no_such_key"), "unknown AppKit column key")
  expect_error(appkitr:::.appkit_col(1), "single character string")
  expect_error(appkitr:::.appkit_col(c("participant", "login")), "single character string")
})

#### Validator ####

test_that(".check_df_cols passes silently when columns are present", {
  df <- data.frame(personalParticipantCode = "A", committed = "x", stringsAsFactors = FALSE)
  expect_invisible(appkitr:::.check_df_cols(df, c("personalParticipantCode", "committed")))
  expect_true(appkitr:::.check_df_cols(df, "committed"))
})

test_that(".check_df_cols errors informatively on a missing column", {
  df <- data.frame(a = 1, b = 2)
  expect_error(
    appkitr:::.check_df_cols(df, "foo"),
    "Column `foo` not found in data frame"
  )
  expect_error(appkitr:::.check_df_cols(df, "foo"), "Available columns: a, b")
})

test_that(".check_df_cols states the required AppKit version", {
  df <- data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE)
  expect_error(
    appkitr:::.check_df_cols(df, "expired"),
    "Column `expired` requires an AppKit >= 2.0.0 export"
  )
})

test_that(".check_df_cols reports multiple missing columns in one error", {
  df <- data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE)
  expect_error(
    appkitr:::.check_df_cols(df, c("expired", "foo")),
    "Columns `expired`, `foo` not found"
  )
  expect_error(
    appkitr:::.check_df_cols(df, c("expired", "foo")),
    "Column `expired` requires an AppKit >= 2.0.0 export"
  )
})

test_that(".check_df_cols uses the survey context in error messages", {
  df <- data.frame(a = 1)
  expect_error(
    appkitr:::.check_df_cols(df, "foo", context = "s2"),
    "not found in survey `s2`"
  )
})

test_that(".check_df_cols truncates the available columns list", {
  df <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  expect_error(
    appkitr:::.check_df_cols(df, "foo"),
    "Available columns: a, b, c, d, e, \\.\\.\\."
  )
})

#### Fixture cross-check ####

test_that("registry matches fixture export headers across AppKit versions", {
  skip_if_not(dir.exists("data"), "No test data directory available")

  fixture_dirs <- list.dirs("data", full.names = TRUE, recursive = FALSE)
  fixture_dirs <- fixture_dirs[grepl("emotionality_test_data_", fixture_dirs)]
  skip_if_not(length(fixture_dirs) > 0, "No versioned fixture exports available")

  registry <- appkitr:::.appkit_cols

  for (fixture_dir in fixture_dirs) {
    version <- sub(".*emotionality_test_data_", "", fixture_dir)
    min_versions <- ifelse(is.na(registry$min_version), "0.0.0", registry$min_version)
    expected <- registry$column[numeric_version(min_versions) <= numeric_version(version)]
    csv_files <- list.files(fixture_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
    for (csv_file in csv_files) {
      header <- strsplit(readLines(csv_file, n = 1), ";", fixed = TRUE)[[1]]
      present <- intersect(registry$column, header)
      mismatch <- union(setdiff(present, expected), setdiff(expected, present))
      expect_true(
        setequal(present, expected),
        info = paste0(csv_file, ": registry/header mismatch: ", paste(mismatch, collapse = ", "))
      )
    }
  }
})

test_that("registry timestamp flags match a parsed 2.0.0 fixture export", {
  fixture_dir <- "data/emotionality_test_data_2.0.0/Entry_Survey"
  skip_if_not(dir.exists(fixture_dir), "No 2.0.0 fixture export available")

  survey <- read_appkit_survey(fixture_dir)
  registry <- appkitr:::.appkit_cols

  for (i in seq_len(nrow(registry))) {
    col <- registry$column[i]
    if (col %in% names(survey)) {
      expect_equal(
        inherits(survey[[col]], "POSIXct"), registry$timestamp[i],
        info = paste0("timestamp flag mismatch for column ", col)
      )
    }
  }
})
