#' Interactively create a payout scheme
#'
#' This command walks the user through each survey in the input `data`,
#' asking whether to include a payout rule for each one and, if so, which type(s).
#' This avoids manually constructing a `payout_scheme` data frame,
#' which might be more error-prone.
#'
#' @param data Either a single survey data frame (as returned by
#'   `read_appkit_survey()`), or a list of survey data frames (as
#'   returned by `read_appkit_surveys()`, or assembled manually, e.g.
#'   `list(entry = entry_survey, exit = exit_survey)`).
#'
#' @return A `payout_scheme` data frame, suitable for `calculate_payout()`.
#' @seealso [calculate_payout()]
#'
#' @examples
#' if (interactive()) {
#'   payout_scheme <- create_payout_scheme(appkit_survey_esm)
#'   payout_scheme
#' }
#'
#' @export
create_payout_scheme <- function(data) {

  if (!interactive()) {
    stop(
      "create_payout_scheme() requires an interactive R session. ",
      "Construct the payout_scheme data frame directly instead ",
      "(see ?calculate_payout).",
      call. = FALSE
    )
  }

  # Normalize input to a list with .normalize_survey_data

  # allow a single data frame (e.g. straight from read_appkit_survey()) and
  # wrap it into a one-element list.
  if (is.data.frame(data)) {
    data <- .normalize_survey_data(data, deparse(substitute(data)))
  } else {
    data <- .normalize_survey_data(data, NULL)
  }

  survey_names <- names(data)
  rows <- list()

  # ask about each survey in turn
  for (survey_name in survey_names) {
    include <- .ask_yes_no(sprintf("Include payout for survey '%s'? (y/n): ", survey_name))
    # if the answer is no: skip this survey (don't add a row)
    if (!include) next

    rule_type <- .ask_choice(
      "  Type: per_questionnaire, threshold, or both? (q/t/b): ",
      choices = c("q", "t", "b"),
      choices_hint = "'q' (per_questionnaire), 't' (threshold), or 'b' (both)"
    )

    # add a per_questionnaire row if the user chose "q" or "b"
    if (rule_type %in% c("q", "b")) {
      amount_q <- .ask_number("  Amount per committed questionnaire: ", max_decimals = 2)
      rows[[length(rows) + 1]] <- data.frame(
        survey = survey_name, type = "per_questionnaire",
        threshold_n = NA_integer_, amount = amount_q,
        stringsAsFactors = FALSE
      )
    }

    # add a threshold row if the user chose "t" or "b"
    if (rule_type %in% c("t", "b")) {
      # .ask_number() allows decimals
      # So combine with as.integer() for number of min. committed questionnaires
      threshold_n <- as.integer(.ask_number("  Threshold (min. committed questionnaires) for bonus payment: ",
                                            max_decimals = 0))
      amount_t <- .ask_number("  Bonus amount at that threshold: ", max_decimals = 2)
      rows[[length(rows) + 1]] <- data.frame(
        survey = survey_name, type = "threshold",
        threshold_n = threshold_n, amount = amount_t,
        stringsAsFactors = FALSE
      )
    }
  }

  # if every survey was answered "n" or data was empty
  if (length(rows) == 0) {
    stop("No payout rules were added.", call. = FALSE)
  }

  # combine all per-rule data frames into one payout_scheme data frame
  scheme <- do.call(rbind, rows)

  # marks schemes built through this function,
  # so calculate_payout() can warn when a scheme was instead constructed manually
  attr(scheme, "built_by_create_payout_scheme") <- TRUE

  scheme
}

#' Calculate payout per participant based on survey participation
#'
#' Calculates the payout per participant according to a payout scheme.
#' Supports two payment types per survey: a fixed amount per committed
#' questionnaire, and/or a one-time bonus once a minimum number of committed
#' questionnaires per survey is reached.
#'
#' @section Disclaimer:
#' This function is provided as a calculation aid only. It comes with no
#' guarantee of correctness. Always manually verify payout amounts before
#' making any actual payments.
#'
#' @param data Either a single survey data frame (as returned by
#'   `read_appkit_survey()`), or a list of survey data frames (as
#'   returned by `read_appkit_surveys()`, or assembled manually, e.g.
#'   `list(entry = entry_survey, exit = exit_survey)`).
#' @param payout_scheme A data frame with one row per payout rule, and
#'   columns:
#'   \describe{
#'     \item{survey}{Character. Survey name, must match a name in `data`.}
#'     \item{amount}{Numeric. Amount per questionnaire (default rule type,
#'       `"per_questionnaire"`), or one-time bonus amount (`"threshold"`
#'       rule type).}
#'     \item{type}{Optional. Character, either `"per_questionnaire"` (pay
#'       `amount` for every committed questionnaire in this survey) or
#'       `"threshold"` (pay `amount` once if the participant's committed
#'       count for this survey is >= `threshold_n`). Defaults to
#'       `"per_questionnaire"` for every row if the column is omitted
#'       entirely.}
#'     \item{threshold_n}{Optional. Integer, minimum number of committed
#'       questionnaires required for a `"threshold"` rule. Required
#'       (non-`NA`) only for rows with `type = "threshold"`. Omit the
#'       column if you only use `"per_questionnaire"` rules.}
#'   }
#' @eval paste0(
#'   "@param participant_col Character string naming the participant ID column ",
#'   "(default: `\"", .appkit_col("participant"), "\"`)."
#' )
#' @eval paste0(
#'   "@param scheduled_col Character string naming the scheduled-timestamp column ",
#'   "(default: `\"", .appkit_col("scheduled"), "\"`)."
#' )
#' @eval paste0(
#'   "@param committed_col Character string naming the committed-timestamp column ",
#'   "(default: `\"", .appkit_col("committed"), "\"`)."
#' )
#' @eval paste0(
#'   "@param exported_col Character string naming the export-timestamp column, ",
#'   "used internally by `participation_table()` (default: `\"",
#'   .appkit_col("exported"), "\"`)."
#' )
#'
#' @return A data frame with one row per participant and one column per
#'   survey (payout amount for that survey), plus a `total` column (sum
#'   across all surveys).
#'
#' @examples
#' payout_scheme <- data.frame(
#'   survey = c("appkit_survey_esm", "appkit_survey_esm"),
#'   type = c("per_questionnaire", "threshold"),
#'   threshold_n = c(NA, 10),
#'   amount = c(2.00, 15.00)
#' )
#'
#' payout <- calculate_payout(appkit_survey_esm, payout_scheme)
#' head(payout)
#'
#' @export
calculate_payout <- function(data, payout_scheme,
                             participant_col = .appkit_col("participant"),
                             scheduled_col = .appkit_col("scheduled"),
                             committed_col = .appkit_col("committed"),
                             exported_col = .appkit_col("exported")) {

  # first call the shared normalizer
  # and call deparse(substitute()) here to feed the string of the survey name

  if (is.data.frame(data)) {
    data <- .normalize_survey_data(data, deparse(substitute(data)))
  } else {
    data <- .normalize_survey_data(data, NULL)
  }

  # flags schemes built by hand, since those are more prone to recycling
  # mistakes than ones built via create_payout_scheme()
  if (!isTRUE(attr(payout_scheme, "built_by_create_payout_scheme"))) {
    warning(
      "`payout_scheme` was not built with create_payout_scheme(). ",
      call. = FALSE
    )
  }

  # only survey and amount are required; type/threshold_n are optional
  required_cols <- c("survey", "amount")
  missing_cols <- setdiff(required_cols, colnames(payout_scheme))
  if (length(missing_cols) > 0) {
    stop(
      sprintf("`payout_scheme` is missing required column(s): %s", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }

  # default every row to per_questionnaire if type is omitted entirely
  if (!"type" %in% colnames(payout_scheme)) {
    payout_scheme$type <- "per_questionnaire"
  }
  # default threshold_n to NA if omitted (only meaningful for type = "threshold")
  if (!"threshold_n" %in% colnames(payout_scheme)) {
    payout_scheme$threshold_n <- NA_integer_
  }

  if (!all(payout_scheme$type %in% c("per_questionnaire", "threshold"))) {
    stop("`payout_scheme$type` must be either 'per_questionnaire' or 'threshold'.", call. = FALSE)
  }

  # a threshold rule without threshold_n has no defined trigger point
  if (any(payout_scheme$type == "threshold" & is.na(payout_scheme$threshold_n))) {
    stop("Rows with type 'threshold' must have a non-NA `threshold_n`.", call. = FALSE)
  }

  # per_questionnaire and threshold duplicates both throw hard errors,
  # since this function's output determines payments
  per_q_rules <- payout_scheme[payout_scheme$type == "per_questionnaire", ]
  if (anyDuplicated(per_q_rules$survey) > 0) {
    dupes <- unique(per_q_rules$survey[duplicated(per_q_rules$survey)])
    stop(
      sprintf(
        "Only one 'per_questionnaire' rule per survey is supported. Found multiple for: %s",
        paste(dupes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  threshold_rules <- payout_scheme[payout_scheme$type == "threshold", ]
  if (anyDuplicated(threshold_rules$survey) > 0) {
    dupes <- unique(threshold_rules$survey[duplicated(threshold_rules$survey)])
    stop(
      sprintf(
        "Only one 'threshold' rule per survey is supported. Found multiple for: %s",
        paste(dupes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # reuse participation_table()
  counts <- participation_table(
    data,
    value = "count",
    participant_col = participant_col,
    scheduled_col = scheduled_col,
    committed_col = committed_col,
    exported_col = exported_col
  )

  survey_names <- unique(payout_scheme$survey)

  # catches typos in payout_scheme$survey (for manually created schemes)
  missing_surveys <- setdiff(survey_names, colnames(counts))
  if (length(missing_surveys) > 0) {
    stop(
      sprintf(
        "`payout_scheme` references survey(s) not found in `data`: %s",
        paste(missing_surveys, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  result <- setNames(
    data.frame(counts[[participant_col]], stringsAsFactors = FALSE),
    participant_col
  )

  for (survey_name in survey_names) {
    committed_n <- counts[[survey_name]]
    survey_rules <- payout_scheme[payout_scheme$survey == survey_name, ]

    payout <- rep(0, length(committed_n))

    # per_questionnaire: amount * number of committed questionnaires
    per_q_rule <- survey_rules[survey_rules$type == "per_questionnaire", ]
    if (nrow(per_q_rule) > 0) {
      payout <- payout + committed_n * per_q_rule$amount[1]
    }

    # threshold: one-time bonus if committed_n >= threshold_n, added on top
    threshold_rule <- survey_rules[survey_rules$type == "threshold", ]
    if (nrow(threshold_rule) > 0) {
      payout <- payout + ifelse(committed_n >= threshold_rule$threshold_n[1], threshold_rule$amount[1], 0)
    }

    result[[survey_name]] <- payout
  }

  # total across all surveys covered by payout_scheme
  result$total <- rowSums(result[, survey_names, drop = FALSE])

  message(
    "calculate_payout() is a calculation aid only, with no guarantee of ",
    "correctness. Always manually verify payout amounts before making any ",
    "actual payments."
  )

  result
}
