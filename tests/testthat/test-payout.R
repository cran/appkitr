# Create small test survey set: entry_survey and momentary_assessment (repeated)
make_test_surveys <- function() {
  entry_survey <- data.frame(
    personalParticipantCode = c("p1", "p2", "p3"),
    scheduled = as.POSIXct(c("2025-01-01 09:00:00", "2025-01-01 09:00:00", "2025-01-01 09:00:00")),
    committed = as.POSIXct(c("2025-01-01 10:00:00", NA, "2025-01-01 11:00:00")),
    exported = as.POSIXct("2025-01-02 00:00:00")
  )

  momentary_assessment <- data.frame(
    personalParticipantCode = rep(c("p1", "p2"), c(25, 15)),
    scheduled = as.POSIXct(rep("2025-01-01 09:00:00", 40)),
    committed = as.POSIXct(c(
      rep("2025-01-01 10:00:00", 22), rep(NA, 3), # p1: 25 possible, 22 committed
      rep("2025-01-01 10:00:00", 10), rep(NA, 5)  # p2: 15 possible, 10 committed
    )),
    exported = as.POSIXct("2025-01-02 00:00:00")
  )

  list(entry_survey = entry_survey, momentary_assessment = momentary_assessment)
}

# --- create_payout_scheme() tests -------------------------------------------
# readline() is mocked to return pre-scripted answers

test_that("create_payout_scheme builds a scheme from simulated input", {
  answers <- c(
    # entry_survey: yes, per_questionnaire, amount
    "y", "q", "2.00",
    # momentary_assessment: yes, both, amount per questionnaire, threshold, bonus
    "y", "b", "0.50", "20", "15.00"
  )
  answer_index <- 0

  fake_readline <- function(prompt = "") {
    answer_index <<- answer_index + 1
    answers[answer_index]
  }

  scheme <- with_mocked_bindings(
    create_payout_scheme(make_test_surveys()),
    readline = fake_readline,
    interactive = function() TRUE,
    .package = "base"
  )

  expect_true(attr(scheme, "built_by_create_payout_scheme"))
  expect_equal(nrow(scheme), 3)
  expect_true(all(c("entry_survey", "momentary_assessment") %in% scheme$survey))
})

# should throw an error when session is not interactive
test_that("create_payout_scheme errors outside an interactive session", {
  expect_error(
    with_mocked_bindings(
      create_payout_scheme(make_test_surveys()),
      interactive = function() FALSE,
      .package = "base"
    ),
    "requires an interactive R session"
  )
})

# answering no to every survey leaves nothing to build
test_that("create_payout_scheme errors when no survey is included", {
  answers <- c("n", "n")
  answer_index <- 0

  fake_readline <- function(prompt = "") {
    answer_index <<- answer_index + 1
    answers[answer_index]
  }

  expect_error(
    with_mocked_bindings(
      create_payout_scheme(make_test_surveys()),
      readline = fake_readline,
      interactive = function() TRUE,
      .package = "base"
    ),
    "No payout rules were added"
  )
})

# --- calculate_payout() tests -----------------------------------------------

test_that("calculates per_questionnaire payout correctly", {
  scheme <- data.frame(
    survey = "entry_survey",
    type = "per_questionnaire",
    threshold_n = NA,
    amount = 2.00
  )

  result <- suppressWarnings(calculate_payout(make_test_surveys(), scheme))

  expect_equal(result$entry_survey[result$personalParticipantCode == "p1"], 2.00)
  expect_equal(result$entry_survey[result$personalParticipantCode == "p2"], 0.00)
  expect_equal(result$total, result$entry_survey)
})

test_that("calculates threshold bonus correctly", {
  scheme <- data.frame(
    survey = "momentary_assessment",
    type = "threshold",
    threshold_n = 20,
    amount = 15.00
  )

  result <- suppressWarnings(calculate_payout(make_test_surveys(), scheme))

  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p1"], 15.00)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p2"], 0.00)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p3"], 0.00)
})

test_that("combines per_questionnaire and threshold for the same survey", {
  scheme <- data.frame(
    survey = c("momentary_assessment", "momentary_assessment"),
    type = c("per_questionnaire", "threshold"),
    threshold_n = c(NA, 20),
    amount = c(0.50, 15.00)
  )

  result <- suppressWarnings(calculate_payout(make_test_surveys(), scheme))

  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p1"], 22 * 0.50 + 15.00)
  expect_equal(result$momentary_assessment[result$personalParticipantCode == "p2"], 10 * 0.50)
})

test_that("total sums payouts across surveys", {
  scheme <- data.frame(
    survey = c("entry_survey", "momentary_assessment"),
    type = c("per_questionnaire", "per_questionnaire"),
    threshold_n = c(NA, NA),
    amount = c(2.00, 0.50)
  )

  result <- suppressWarnings(calculate_payout(make_test_surveys(), scheme))

  expect_equal(result$total, result$entry_survey + result$momentary_assessment)
})

test_that("defaults to per_questionnaire when type/threshold_n are omitted", {
  scheme <- data.frame(
    survey = "entry_survey",
    amount = 2.00
  )

  result <- suppressWarnings(calculate_payout(make_test_surveys(), scheme))
  expect_equal(result$entry_survey[result$personalParticipantCode == "p1"], 2.00)
})

test_that("errors on threshold rule without threshold_n", {
  scheme <- data.frame(
    survey = "momentary_assessment",
    type = "threshold",
    amount = 15.00
  )

  expect_error(suppressWarnings(calculate_payout(make_test_surveys(), scheme)), "must have a non-NA")
})

test_that("errors on multiple per_questionnaire rules for the same survey", {
  scheme <- data.frame(
    survey = c("entry_survey", "entry_survey"),
    type = c("per_questionnaire", "per_questionnaire"),
    threshold_n = c(NA, NA),
    amount = c(2.00, 3.00)
  )

  expect_error(suppressWarnings(calculate_payout(make_test_surveys(), scheme)),
               "Only one 'per_questionnaire' rule")
})

test_that("errors on multiple threshold rules for the same survey", {
  scheme <- data.frame(
    survey = c("momentary_assessment", "momentary_assessment"),
    type = c("threshold", "threshold"),
    threshold_n = c(10, 20),
    amount = c(5.00, 15.00)
  )

  expect_error(suppressWarnings(calculate_payout(make_test_surveys(), scheme)), "Only one 'threshold' rule")
})

test_that("errors on payout_scheme referencing unknown survey", {
  scheme <- data.frame(
    survey = "nonexistent_survey",
    type = "per_questionnaire",
    threshold_n = NA,
    amount = 1.00
  )

  expect_error(suppressWarnings(calculate_payout(make_test_surveys(), scheme)), "not found in `data`")
})

test_that("errors on missing required payout_scheme columns", {
  bad_scheme <- data.frame(survey = "entry_survey")
  expect_error(suppressWarnings(calculate_payout(make_test_surveys(), bad_scheme)), "missing required column")
})

test_that("always warns with a disclaimer", {
  scheme <- data.frame(survey = "entry_survey", amount = 2.00)
  # suppress the warning to test the disclaimer message isolated
  expect_message(suppressWarnings(calculate_payout(make_test_surveys(), scheme)), "calculation aid only")
})

test_that("warns when payout_scheme was not built with create_payout_scheme()", {
  scheme <- data.frame(survey = "entry_survey", amount = 2.00)
  # suppress the disclaimer message to test the warning isolated
  expect_warning(
    suppressMessages(calculate_payout(make_test_surveys(), scheme)),
    "not built with create_payout_scheme"
  )
})

test_that("calculate_payout works with a single data frame and a manual scheme", {
  entry <- make_test_surveys()$entry_survey
  scheme <- data.frame(
    survey = "entry", type = "per_questionnaire",
    threshold_n = NA, amount = 2.00
  )
  result <- suppressWarnings(calculate_payout(entry, scheme))
  expect_true("entry" %in% colnames(result))
  expect_equal(result$entry[result$personalParticipantCode == "p1"], 2.00)
})

test_that("calculate_payout works with a single data frame and a create_payout_scheme() scheme", {
  entry <- make_test_surveys()$entry_survey
  answers <- c("y", "q", "2.00")
  answer_index <- 0
  fake_readline <- function(prompt = "") {
    answer_index <<- answer_index + 1
    answers[answer_index]
  }
  scheme <- with_mocked_bindings(
    create_payout_scheme(entry),
    readline = fake_readline,
    interactive = function() TRUE,
    .package = "base"
  )
  result <- suppressWarnings(calculate_payout(entry, scheme))
  expect_true("entry" %in% colnames(result))
})

test_that("calculate_payout errors on missing required columns via participation_table", {
  bad_survey <- data.frame(wrongColumnName = c("p1", "p2"))
  scheme <- data.frame(
    survey = "bad_survey", type = "per_questionnaire",
    threshold_n = NA, amount = 2.00
  )
  expect_error(
    suppressWarnings(calculate_payout(list(bad_survey = bad_survey), scheme)),
    "not found in survey"
  )
})
