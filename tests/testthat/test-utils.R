#### .format_na_values ####

test_that("the default na_values list converts numeric and character codes", {
  df <- data.frame(
    age = c(25L, -1L, 40L),
    note = c("fine", "", "N/A"),
    stringsAsFactors = FALSE
  )

  result <- appkitr:::.format_na_values(df, list(-1, "", "Not applicable", "N/A"))

  expect_equal(result$age, c(25L, NA, 40L))
  expect_equal(result$note, c("fine", NA, NA))
})

test_that("numeric codes in a mixed atomic vector convert in numeric columns", {
  # c() coerces the numeric code to the character "-1" (#47)
  df <- data.frame(score = c(-1, 3, 7), stringsAsFactors = FALSE)

  result <- appkitr:::.format_na_values(df, c(-1, ""))

  expect_equal(result$score, c(NA, 3, 7))
})

test_that("numeric na_values vectors convert in numeric and character columns", {
  df <- data.frame(
    score = c(-1, -99, 5),
    label = c("-1", "-99", "ok"),
    stringsAsFactors = FALSE
  )

  result <- appkitr:::.format_na_values(df, c(-1, -99))

  expect_equal(result$score, c(NA, NA, 5))
  expect_equal(result$label, c(NA, NA, "ok"))
})

test_that("non-numeric na_values leave numeric columns untouched", {
  df <- data.frame(score = c(0, 1, 2), stringsAsFactors = FALSE)

  result <- appkitr:::.format_na_values(df, list("", "N/A", "Not applicable"))

  expect_equal(result$score, c(0, 1, 2))
})

test_that("NULL or empty na_values return the data frame unchanged", {
  df <- data.frame(a = c(-1, 1), b = c("", "x"), stringsAsFactors = FALSE)

  expect_identical(appkitr:::.format_na_values(df, NULL), df)
  expect_identical(appkitr:::.format_na_values(df, list()), df)
})
