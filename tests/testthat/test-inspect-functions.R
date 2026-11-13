test_that("inspect_variable returns a data frame with correct structure", {
  # Test with a simple vector
  test_var <- c(1, 2, 3, 4, 5)
  result <- inspect_variable(test_var, "test_var")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true(all(c("var_name", "var_type", "var_label", "unique_values", "value_labels") %in% names(result)))
  expect_equal(result$var_name, "test_var")
  expect_equal(result$var_type, "numeric")
})

test_that("inspect_variable handles different data types correctly", {
  # Test character vector
  char_var <- c("a", "b", "c")
  result_char <- inspect_variable(char_var, "char_test")
  expect_equal(result_char$var_type, "character")

  # Test factor
  factor_var <- factor(c("low", "medium", "high"))
  result_factor <- inspect_variable(factor_var, "factor_test")
  expect_equal(result_factor$var_type, "factor")

  # Test logical
  logical_var <- c(TRUE, FALSE, TRUE)
  result_logical <- inspect_variable(logical_var, "logical_test")
  expect_equal(result_logical$var_type, "logical")
})

test_that("inspect_variable handles variables with labels", {
  # Create a variable with a label attribute
  labeled_var <- c(1, 2, 3)
  attr(labeled_var, "label") <- "Test variable label"

  result <- inspect_variable(labeled_var, "labeled_test")
  expect_equal(result$var_label, "Test variable label")
})

test_that("inspect_variable handles factors with levels", {
  # Create a factor with levels
  factor_var <- factor(c("low", "medium", "high"), levels = c("low", "medium", "high"))

  result <- inspect_variable(factor_var, "factor_test")
  # Check that value labels are generated for factors
  expect_true(nchar(result$value_labels) > 0)
  expect_true(grepl("low", result$value_labels))
  expect_true(grepl("medium", result$value_labels))
  expect_true(grepl("high", result$value_labels))
})

test_that("inspect_variables returns a data frame when given a data frame", {
  # Create a test data frame
  test_df <- data.frame(
    x = c(1, 2, 3),
    y = c("a", "b", "c"),
    z = c(TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  # Test with verbose = FALSE to avoid console output during testing
  result <- inspect_variables(test_df, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3) # Should have 3 rows for 3 variables
  expect_true(all(c("var_name", "var_type", "var_label", "unique_values", "value_labels") %in% names(result)))
})

test_that("inspect_variables works with single variable", {
  # Test with a single variable (not a data frame)
  test_var <- c(1, 2, 3, 4, 5)

  # This should work without errors
  result <- inspect_variables(test_var, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
})

test_that("inspect_variables handles empty data frame", {
  # Create an empty data frame
  empty_df <- data.frame()

  result <- inspect_variables(empty_df, verbose = FALSE)

  # Should return NULL or empty structure for empty data frame
  expect_true(is.null(result) || (is.data.frame(result) && nrow(result) == 0))
})

test_that("inspect_variable handles NA values correctly", {
  # Test with NA values
  test_var <- c(1, 2, NA, 3, NA)

  result <- inspect_variable(test_var, "na_test")

  expect_s3_class(result, "data.frame")
  expect_equal(result$var_name, "na_test")
  # Should not include NA in unique values display
  expect_false(grepl("NA", result$unique_values))
})

test_that("inspect functions handle max_unique parameter", {
  # Create a variable with many unique values
  many_values <- 1:20

  result <- inspect_variable(many_values, "many_test", max_unique = 5)

  expect_s3_class(result, "data.frame")
  expect_true(grepl("total", result$unique_values)) # Should show truncation message
})

test_that("inspect functions handle max_width parameter", {
  # Create a variable with long string values
  long_strings <- paste(rep("x", 100), collapse = "")
  test_var <- rep(long_strings, 3)

  result <- inspect_variable(test_var, "long_test", max_width = 20)

  expect_s3_class(result, "data.frame")
  expect_true(nchar(result$unique_values) <= 23)
})

test_that("inspect functions handle POSIXct timestamp columns", {
  # Create a POSIXct test variable
  timestamp_var <- as.POSIXct(c("2023-01-01 10:00:00", "2023-01-02 15:30:00", "2023-01-03 09:15:00"))

  result <- inspect_variable(timestamp_var, "timestamp_test")

  expect_s3_class(result, "data.frame")
  expect_equal(result$var_name, "timestamp_test")
  expect_equal(result$var_type, "POSIXct")
})

test_that("inspect functions work with mixed data frame", {
  # Create a comprehensive test data frame
  test_df <- data.frame(
    id = as.numeric(1:5), # Explicitly make numeric
    name = c("Alice", "Bob", "Charlie", "Diana", "Eve"),
    active = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    category = factor(c("A", "B", "A", "C", "B")),
    stringsAsFactors = FALSE
  )

  # Add a label to one variable
  attr(test_df$name, "label") <- "Participant name"

  result <- inspect_variables(test_df, verbose = FALSE)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4) # 4 variables

  # Check specific variable types (be flexible about integer vs numeric)
  expect_true(any(c("numeric", "integer") %in% result$var_type)) # id
  expect_true("character" %in% result$var_type) # name
  expect_true("logical" %in% result$var_type) # active
  expect_true("factor" %in% result$var_type) # category

  # Check that the label was captured
  name_row <- result[result$var_name == "name", ]
  expect_equal(name_row$var_label, "Participant name")
})

test_that("inspect functions work with package example datasets", {
  # Just verify they work without errors and return proper structure
  # Don't test specific content to avoid dependencies

  expect_no_error({
    result_entry <- inspect_variables(appkit_survey_entry, verbose = FALSE)
    result_esm <- inspect_variables(appkit_survey_esm, verbose = FALSE)
    result_exit <- inspect_variables(appkit_survey_exit, verbose = FALSE)
  })

  # Basic structure checks
  result <- inspect_variables(appkit_survey_entry, verbose = FALSE)
  expect_s3_class(result, "data.frame")
  expect_true(all(c("var_name", "var_type", "var_label", "unique_values", "value_labels") %in% names(result)))
  expect_true(nrow(result) > 0)
})
