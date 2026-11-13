# Create small test survey se: entry_survey and momentary_assessment (repeated)
make_test_surveys <- function() {
  entry_survey <- data.frame(
    personalParticipantCode = c("p1", "p2", "p3"),
    scheduled = as.POSIXct(c("2025-01-01 09:00:00", "2025-01-01 09:00:00", "2025-01-03 09:00:00")),
    committed = as.POSIXct(c("2025-01-01 10:00:00", NA, "2025-01-01 11:00:00")),
    exported = as.POSIXct("2025-01-02 00:00:00")
  )

  momentary_assessment <- data.frame(
    personalParticipantCode = c("p1", "p1", "p1", "p2", "p2"),
    scheduled = as.POSIXct(c(
      "2025-01-01 09:00:00", "2025-01-01 15:00:00", "2025-01-04 09:00:00",
      "2025-01-01 09:00:00", "2025-01-01 15:00:00"
    )),
    committed = as.POSIXct(c(
      "2025-01-01 10:00:00", NA, NA,
      NA, NA
    )),
    exported = as.POSIXct("2025-01-03 00:00:00")
    # p1: 2 expected, 1 committed -> 50%
    # p2: 2 expected, 0 committed -> 0%
    # p3: no rows at all in this survey -> NA on percentage, 0 on count
  )

  list(entry_survey = entry_survey, momentary_assessment = momentary_assessment)
}

test_that("counts committed entries correctly", {
  result <- participation_table(make_test_surveys())

  expect_equal(nrow(result), 3) # participants across both surveys
  expect_equal(result$entry_survey[result$personalParticipantCode == "p1"], 1)
  expect_equal(result$entry_survey[result$personalParticipantCode == "p2"], 0)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p1"], 1)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p3"], 0)
})

test_that("computes percentage as committed / expected per participant", {
  result <- participation_table(make_test_surveys(), value = "percentage")

  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p1"], 50)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p2"], 0)
  # p3 has zero expected rows in this survey -> NA, distinct from "0% completed"
  expect_true(is.na(result$momentary_assessment[result$personalParticipantCode == "p3"]))
})

test_that("accepts a single bare data frame and names the column after it", {
  ma <- make_test_surveys()$momentary_assessment
  result <- participation_table(ma)

  expect_true("ma" %in% colnames(result))
  expect_equal(nrow(result), 2) # only participants present in ma
})

test_that("accepts a manually assembled list", {
  surveys <- make_test_surveys()
  result <- participation_table(list(entry = surveys$entry_survey, ma = surveys$momentary_assessment))

  expect_true(all(c("entry", "ma") %in% colnames(result)))
})

test_that("errors on unlist input", {
  surveys <- make_test_surveys()
  expect_error(participation_table(unname(surveys)), "list")
})

# Structural tests across all AppKit test data versions
version_dirs <- list.dirs(test_path("data"), recursive = FALSE)

if (length(version_dirs) == 0) {
  testthat::skip("No AppKit test data versions found under tests/testthat/data")
}

for (version_dir in version_dirs) {
  version_name <- basename(version_dir)

  test_that(paste("works structurally for AppKit test data", version_name), {
    surveys <- read_appkit_surveys(version_dir)
    skip_if(length(surveys) == 0, paste("no surveys found in", version_name))

    result <- participation_table(surveys, include_future_scheduled = TRUE)

    expect_s3_class(result, "data.frame")
    expect_true(all(names(surveys) %in% colnames(result)))
    expect_equal(anyDuplicated(result$personalParticipantCode), 0)

    # committed counts per survey must match the raw data
    for (survey_name in names(surveys)) {
      raw_committed <- sum(!is.na(surveys[[survey_name]]$committed))
      expect_equal(sum(result[[survey_name]]), raw_committed)
    }

    # value = percentage: every value is within 0 to 100 or NA
    result_pct <- participation_table(surveys, value = "percentage", include_future_scheduled = TRUE)
    for (survey_name in names(surveys)) {
      non_na <- result_pct[[survey_name]][!is.na(result_pct[[survey_name]])]
      expect_true(all(non_na >= 0 & non_na <= 100))
    }
  })
}

test_that("errors on missing required columns", {
  bad_survey <- data.frame(wrongColumnName = c("p1", "p2"))
  expect_error(
    participation_table(list(bad_survey = bad_survey)),
    "not found in survey"
  )
})

test_that("excludes rows scheduled after the export by default", {
  future_survey <- data.frame(
    personalParticipantCode = c("p1", "p2"),
    scheduled = as.POSIXct(c(Sys.time() - 3600, Sys.time() + 3600)),  # p1: past, p2: future
    committed = as.POSIXct(c(Sys.time() - 1800, NA)),
    exported = as.POSIXct(Sys.time())
  )

  result <- participation_table(future_survey)
  # p2's row is excluded entirely (future), so only p1 should have a nonzero count
  expect_equal(result$future_survey[result$personalParticipantCode == "p1"], 1)
  expect_equal(result$future_survey[result$personalParticipantCode == "p2"], 0)
})

test_that("includes rows scheduled in the future when include_future_scheduled = TRUE", {
  future_survey <- data.frame(
    personalParticipantCode = c("p1"),
    scheduled = as.POSIXct(Sys.time() + 3600),
    committed = as.POSIXct(NA)
  )

  result <- suppressMessages(
    participation_table(future_survey, value = "percentage", include_future_scheduled = TRUE)
  )
  # row is included as "expected" even though not yet committed -> 0%, not NA
  expect_equal(result$future_survey[result$personalParticipantCode == "p1"], 0)
})

test_that("prints a message when future-scheduled rows are present", {
  future_survey <- data.frame(
    personalParticipantCode = c("p1"),
    scheduled = as.POSIXct(Sys.time() + 3600),
    committed = as.POSIXct(NA),
    exported = as.POSIXct(Sys.time())
  )

  expect_message(participation_table(future_survey), "excluding")
  expect_no_message(participation_table(future_survey, include_future_scheduled = TRUE))
})

test_that("supports custom column names via the *_col parameters", {
  custom_survey <- data.frame(
    pid = c("p1", "p1", "p2"),
    sched = as.POSIXct(c("2025-01-01 09:00:00", "2025-01-03 09:00:00", "2025-01-01 09:00:00")),
    comm = as.POSIXct(c("2025-01-01 10:00:00", NA, NA)),
    exp_ts = as.POSIXct("2025-01-02 00:00:00")
  )

  # default include_future_scheduled = FALSE exercises the exported_col filter
  result <- suppressMessages(participation_table(
    list(custom_survey = custom_survey),
    participant_col = "pid",
    scheduled_col = "sched",
    committed_col = "comm",
    exported_col = "exp_ts"
  ))

  expect_equal(colnames(result), c("pid", "custom_survey"))
  # p1's second row is scheduled after the export and excluded
  expect_equal(result$custom_survey[result$pid == "p1"], 1)
  expect_equal(result$custom_survey[result$pid == "p2"], 0)
})

test_that("handles mixed character and POSIXct timestamp columns without erroring", {
  mixed_survey <- data.frame(
    personalParticipantCode = c("p1", "p2"),
    scheduled = as.POSIXct(c("2025-01-01 09:00:00", "2025-01-01 09:00:00")),
    committed = as.character(c("2025-01-01 10:00:00", NA)),
    exported = as.POSIXct(c("2025-01-02 00:00:00", "2025-01-02 00:00:00"))
  )

  result <- suppressMessages(participation_table(list(mixed_survey = mixed_survey)))

  expect_equal(result$mixed_survey[result$personalParticipantCode == "p1"], 1)
  expect_equal(result$mixed_survey[result$personalParticipantCode == "p2"], 0)
})
