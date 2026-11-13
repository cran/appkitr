#### Input validation ####

test_that("errors on non-data.frame, non-list input", {
  expect_error(drop_participants(1:10, "A"), "must be a data frame or a list")
  expect_error(drop_participants("hello", "A"), "must be a data frame or a list")
})

test_that("errors when id_col is not found in data frame", {
  df <- data.frame(x = 1:3, stringsAsFactors = FALSE)
  expect_error(
    drop_participants(df, "A", id_col = "missing_col"),
    "Column `missing_col` not found in data frame"
  )
})

test_that("missing version-dependent id_col states the required AppKit version", {
  df <- data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE)
  expect_error(
    drop_participants(df, "A", id_col = "expired"),
    "Column `expired` requires an AppKit >= 2.0.0 export"
  )
})

test_that("errors when list contains non-data.frame elements", {
  bad_list <- list(a = data.frame(id = "A"), b = "not a df")
  expect_error(
    drop_participants(bad_list, "A", id_col = "id"),
    "Non-data-frame elements"
  )
})

test_that("errors on invalid id_col type", {
  df <- data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE)
  expect_error(drop_participants(df, "A", id_col = 123), "single character string")
  expect_error(drop_participants(df, "A", id_col = c("a", "b")), "single character string")
})

test_that("errors on non-atomic participant_ids", {
  df <- data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE)
  expect_error(drop_participants(df, list("A")), "character vector")
})

#### Data frame path ####

test_that("drops matching rows from a data frame", {
  df <- data.frame(
    personalParticipantCode = c("A", "B", "C", "D"),
    value = 1:4,
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(drop_participants(df, c("A", "C")))
  expect_equal(nrow(result), 2)
  expect_equal(result$personalParticipantCode, c("B", "D"))
  expect_equal(result$value, c(2L, 4L))
})

test_that("returns unchanged data frame when no IDs match", {
  df <- data.frame(
    personalParticipantCode = c("A", "B"),
    value = 1:2,
    stringsAsFactors = FALSE
  )
  expect_warning(
    result <- suppressMessages(drop_participants(df, c("X", "Y"))),
    "not found"
  )
  expect_equal(nrow(result), 2)
})

test_that("returns unchanged data frame for empty participant_ids", {
  df <- data.frame(
    personalParticipantCode = c("A", "B"),
    value = 1:2,
    stringsAsFactors = FALSE
  )
  expect_message(
    result <- drop_participants(df, character(0)),
    "No participant IDs"
  )
  expect_equal(nrow(result), 2)
})

test_that("works with custom id_col", {
  df <- data.frame(
    my_id = c("A", "B", "C"),
    value = 1:3,
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(drop_participants(df, "B", id_col = "my_id"))
  expect_equal(nrow(result), 2)
  expect_equal(result$my_id, c("A", "C"))
})

#### List path ####

test_that("drops participants from each data frame in a list", {
  surveys <- list(
    entry = data.frame(
      personalParticipantCode = c("A", "B", "C"),
      val = 1:3, stringsAsFactors = FALSE
    ),
    exit = data.frame(
      personalParticipantCode = c("A", "B"),
      val = 4:5, stringsAsFactors = FALSE
    )
  )
  result <- suppressMessages(drop_participants(surveys, "A"))
  expect_equal(nrow(result$entry), 2)
  expect_equal(nrow(result$exit), 1)
  expect_false("A" %in% result$entry$personalParticipantCode)
  expect_false("A" %in% result$exit$personalParticipantCode)
  expect_equal(names(result), c("entry", "exit"))
})

test_that("warns about IDs not found in any survey", {
  surveys <- list(
    s1 = data.frame(personalParticipantCode = c("A", "B"), stringsAsFactors = FALSE)
  )
  expect_warning(
    suppressMessages(drop_participants(surveys, c("A", "Z"))),
    "not found in any survey"
  )
})

test_that("errors when id_col missing in a list element", {
  surveys <- list(
    s1 = data.frame(personalParticipantCode = "A", stringsAsFactors = FALSE),
    s2 = data.frame(other_col = "B", stringsAsFactors = FALSE)
  )
  expect_error(drop_participants(surveys, "A"), "not found in survey `s2`")
})

#### Edge cases ####

test_that("preserves data frame attributes and column types", {
  df <- data.frame(
    personalParticipantCode = c("A", "B", "C"),
    score = c(1.5, 2.5, 3.5),
    stringsAsFactors = FALSE
  )
  attr(df$score, "label") <- "Test Score"
  result <- suppressMessages(drop_participants(df, "B"))
  expect_equal(attr(result$score, "label"), "Test Score")
  expect_type(result$score, "double")
})

test_that("handles duplicate participant_ids gracefully", {
  df <- data.frame(
    personalParticipantCode = c("A", "B", "C"),
    value = 1:3,
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(drop_participants(df, c("A", "A", "A")))
  expect_equal(nrow(result), 2)
})

test_that("handles ESM data with repeated participant IDs", {
  df <- data.frame(
    personalParticipantCode = c("A", "A", "A", "B", "B"),
    measurement = 1:5,
    stringsAsFactors = FALSE
  )
  result <- suppressMessages(drop_participants(df, "A"))
  expect_equal(nrow(result), 2)
  expect_equal(result$personalParticipantCode, c("B", "B"))
})
