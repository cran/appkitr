#### Structure tests ####

test_that("appkit_survey_entry exists and has expected structure", {
  expect_true(exists("appkit_survey_entry"), info = "File does not exist")
  expect_s3_class(appkit_survey_entry, "data.frame")
  expect_true("personalParticipantCode" %in% colnames(appkit_survey_entry))
  expect_false(anyNA(appkit_survey_entry$personalParticipantCode))
})

test_that("appkit_survey_esm exists and has expected structure", {
  expect_true(exists("appkit_survey_esm"), info = "File does not exist")
  expect_s3_class(appkit_survey_esm, "data.frame")
  expect_true("personalParticipantCode" %in% colnames(appkit_survey_esm))
  expect_false(anyNA(appkit_survey_esm$personalParticipantCode))
})

test_that("appkit_survey_exit exists and has expected structure", {
  expect_true(exists("appkit_survey_exit"), info = "File does not exist")
  expect_s3_class(appkit_survey_exit, "data.frame")
  expect_true("personalParticipantCode" %in% colnames(appkit_survey_exit))
  expect_false(anyNA(appkit_survey_exit$personalParticipantCode))
})

.has_label <- function(v) {
  lab <- attr(v, "label", exact = TRUE)
  is_labelled_class <- inherits(v, "labelled") || inherits(v, "haven_labelled")
  (!is.null(lab) && nzchar(as.character(lab))) || is_labelled_class
}

test_that("entry data have variable labels", {
  labs_ok <- vapply(appkit_survey_entry, .has_label, logical(1))
  expect_true(all(labs_ok),
    info = paste(
      "Missing labels for columns:",
      paste(names(labs_ok)[!labs_ok], collapse = ", ")
    )
  )
})

test_that("esm data have variable labels", {
  labs_ok <- vapply(appkit_survey_esm, .has_label, logical(1))
  expect_true(all(labs_ok),
    info = paste(
      "Missing labels for columns:",
      paste(names(labs_ok)[!labs_ok], collapse = ", ")
    )
  )
})

test_that("exit data have variable labels", {
  labs_ok <- vapply(appkit_survey_exit, .has_label, logical(1))
  expect_true(all(labs_ok),
    info = paste(
      "Missing labels for columns:",
      paste(names(labs_ok)[!labs_ok], collapse = ", ")
    )
  )
})

test_that("example datasets contain every registered AppKit metadata column", {
  all_registry_cols <- appkitr:::.appkit_cols$column

  expect_true(
    all(all_registry_cols %in% names(appkit_survey_entry)),
    info = paste(
      "missing in appkit_survey_entry:",
      paste(setdiff(all_registry_cols, names(appkit_survey_entry)), collapse = ", ")
    )
  )
  expect_true(
    all(all_registry_cols %in% names(appkit_survey_esm)),
    info = paste(
      "missing in appkit_survey_esm:",
      paste(setdiff(all_registry_cols, names(appkit_survey_esm)), collapse = ", ")
    )
  )
  expect_true(
    all(all_registry_cols %in% names(appkit_survey_exit)),
    info = paste(
      "missing in appkit_survey_exit:",
      paste(setdiff(all_registry_cols, names(appkit_survey_exit)), collapse = ", ")
    )
  )
})

#### Participant tests ####

test_that("participants are unique in entry and exit surveys", {
  entry_ids <- appkit_survey_entry$personalParticipantCode
  exit_ids <- appkit_survey_exit$personalParticipantCode

  expect_equal(length(entry_ids), length(unique(entry_ids)),
    info = "Duplicate participants in entry survey"
  )
  expect_equal(length(exit_ids), length(unique(exit_ids)),
    info = "Duplicate participants in exit survey"
  )
})

test_that("participant counts can't increase: entry >= esm >= exit", {
  n_entry <- length(unique(appkit_survey_entry$personalParticipantCode))
  n_esm <- length(unique(appkit_survey_esm$personalParticipantCode))
  n_exit <- length(unique(appkit_survey_exit$personalParticipantCode))

  expect_true(
    n_entry >= n_esm,
    sprintf("n_entry (%d) < n_esm (%d)", n_entry, n_esm)
  )
  expect_true(
    n_esm >= n_exit,
    sprintf("n_esm (%d) < n_exit (%d)", n_esm, n_exit)
  )
})

test_that("one device per participant per survey", {
  fun_check_device_consistency <- function(df, survey_name) {
    device_cols <- vapply(
      c("participant", "smartphone_type", "os_version"),
      appkitr:::.appkit_col, character(1),
      USE.NAMES = FALSE
    )
    combos <- unique(df[device_cols])
    n_combos <- nrow(combos)
    n_participants <- length(unique(df[[appkitr:::.appkit_col("participant")]]))
    expect_equal(n_combos, n_participants, info = paste(survey_name, ": device should be fixed per participant"))
  }
  fun_check_device_consistency(appkit_survey_entry, "appkit_survey_entry")
  fun_check_device_consistency(appkit_survey_esm, "appkit_survey_esm")
  fun_check_device_consistency(appkit_survey_exit, "appkit_survey_exit")
})

#### Timestamp tests ####

test_that("timestamp order within each row is correct for all surveys", {
  check_timestamp_order <- function(df, name) {
    timestamps_row_ok <- (df$published <= df$scheduled &
      df$scheduled <= df$firstOpened &
      df$firstOpened <= df$committed)

    # Does that hold for all rows?
    expect_true(all(timestamps_row_ok, na.rm = TRUE), sprintf("Timestamp order violated in %s", name))
  }

  check_timestamp_order(appkit_survey_entry, "entry survey")
  check_timestamp_order(appkit_survey_esm, "esm survey")
  check_timestamp_order(appkit_survey_exit, "exit survey")
})

test_that("'scheduled' is increasing from top to bottom within each participant in ESM", {
  # Split the row indices by participants
  idx_by_id <- split(
    seq_len(nrow(appkit_survey_esm)),
    appkit_survey_esm$personalParticipantCode
  )

  # For each participant's indices, verify that scheduled increases
  is_non_decreasing <- function(idxs) {
    v <- appkit_survey_esm$scheduled[idxs] # POSIXct vector in current row order
    if (length(v) <= 1) {
      return(TRUE)
    } # nothing to compare
    all(diff(v) > 0) # increasing?
  }

  timestamps_participant_ok <- vapply(idx_by_id, is_non_decreasing, logical(1))
  expect_true(
    all(timestamps_participant_ok),
    "'scheduled' is not (always) increasing within participant"
  )
})

test_that("example datasets have consistent scheduled/first_opened/committed ordering", {
  fun_check_order <- function(df, survey_name) {
    scheduled <- df[[appkitr:::.appkit_col("scheduled")]]
    opened <- df[[appkitr:::.appkit_col("first_opened")]]
    committed <- df[[appkitr:::.appkit_col("committed")]]

    expect_true(
      all(is.na(opened) | scheduled <= opened),
      info = paste(survey_name, ": scheduled must be <= first_opened where first_opened is set")
    )
    expect_true(
      all(is.na(committed) | is.na(opened) | opened <= committed),
      info = paste(survey_name, ": first_opened must be <= committed where both are set")
    )
  }

  fun_check_order(appkit_survey_entry, "appkit_survey_entry")
  fun_check_order(appkit_survey_esm, "appkit_survey_esm")
  fun_check_order(appkit_survey_exit, "appkit_survey_exit")
})
