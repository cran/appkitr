#' Report committed questionnaires by participant
#'
#' Creates a table summarizing, for each participant, how many
#' questionnaires they committed per survey, either as absolute counts or as a
#' percentage.
#'
#' @param data Either a single survey data frame (as returned by
#'   `read_appkit_survey()`), or a list of survey data frames (as
#'   returned by `read_appkit_surveys()`, or assembled manually, e.g.
#'   `list(entry = entry_survey, exit = exit_survey)`).
#' @param value Character. Either `"count"` (absolute number of committed
#'   questionnaires per participant per survey, default) or `"percentage"`
#'   (committed questionnaires as a percentage of that participant's total questionnaires
#'   recorded for that survey, i.e. a per-participant completion rate).
#' @eval paste0(
#'   "@param include_future_scheduled Logical. The export contains rows for questionnaires ",
#'   "scheduled in the future. Calculating percentages mid-study can therefore be misleading. ",
#'   "If `FALSE` (default), rows with a `scheduled` timestamp after the export's `exported` ",
#'   "timestamp are excluded; this requires AppKit >= ",
#'   .appkit_cols$min_version[.appkit_cols$key == "exported"],
#'   ". Set to `TRUE` to include them in the calculation."
#' )
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
#'   "@param exported_col Character string naming the export-timestamp column, used as ",
#'   "the reference time for `include_future_scheduled`. ",
#'   "(default: `\"", .appkit_col("exported"), "\"`)."
#' )
#' @eval paste0(
#'   "@return A data frame with one row per participant and one column per ",
#'   "survey (plus `", .appkit_col("participant"), "`), containing counts or ",
#'   "completion percentages. Participants are included if they appear in at ",
#'   "least one of the supplied surveys."
#' )
#'
#' @examples
#' # count of committed questionnaires per participant
#' survey_count <- participation_table(appkit_survey_esm, include_future_scheduled = TRUE)
#' head(survey_count)
#'
#' # percentage of committed questionnaires per participant
#' survey_pct <- participation_table(
#'   appkit_survey_esm,
#'   value = "percentage",
#'   include_future_scheduled = TRUE
#' )
#' head(survey_pct)
#'
#' # participation for multiple surveys, combined in a list
#' surveys_count <- participation_table(
#'   list(survey_esm = appkit_survey_esm, survey_exit = appkit_survey_exit),
#'   include_future_scheduled = TRUE
#' )
#' head(surveys_count)
#'
#' @export
#' @importFrom stats setNames

participation_table <- function(data, value = c("count", "percentage"),
                                include_future_scheduled = FALSE,
                                participant_col = .appkit_col("participant"),
                                scheduled_col = .appkit_col("scheduled"),
                                committed_col = .appkit_col("committed"),
                                exported_col = .appkit_col("exported")) {
  # match.arg() takes the first element if no value is supplied and validates it
  value <- match.arg(value)

  # substitute() must be called here, in participation_table()s own function
  if (is.data.frame(data)) {
    data <- .normalize_survey_data(data, deparse(substitute(data)))
  } else {
    # already a list (or invalid): fallback_name unused
    # then .normalize_survey_data just validates the structure
    data <- .normalize_survey_data(data, NULL)
  }

  survey_names <- names(data)

  required_cols <- c(participant_col, scheduled_col, committed_col)
  if (!include_future_scheduled) {
    required_cols <- c(required_cols, exported_col)
  }

  # validate the structure of each survey
  for (survey_name in survey_names) {
    survey_df <- data[[survey_name]]
    if (!is.data.frame(survey_df)) {
      stop(sprintf("Element '%s' of `data` is not a data frame.", survey_name), call. = FALSE)
    }
    .check_df_cols(survey_df, required_cols, context = survey_name)
  }

  # determine the complete set of participants across surveys
  all_participants <- unique(unlist(
    lapply(data, function(df) df[[participant_col]]),
    use.names = FALSE
  ))

  if (length(all_participants) == 0) {
    warning("No participants found across the supplied surveys.", call. = FALSE)
  }

  committed_per_survey <- vector("list", length(survey_names))
  expected_per_survey <- vector("list", length(survey_names))
  names(committed_per_survey) <- survey_names
  names(expected_per_survey) <- survey_names

  # required timestamp columns
  ts_cols <- intersect(required_cols, .appkit_timestamp_cols())

  for (survey_name in survey_names) {
    survey_df <- data[[survey_name]]

    # parse required timestamp columns
    survey_df <- .format_timestamp_columns(survey_df, ts_cols)

    # the export already contains rows for future questionnaires
    # since they are not yet answerable, exclude them by default
    # and print a message
    if (!include_future_scheduled) {
      n_future <- sum(survey_df[[scheduled_col]] > survey_df[[exported_col]])
      if (n_future > 0) {
        message(sprintf(
          "Survey '%s': excluding %d row(s) scheduled after this export was generated.",
          survey_name, n_future
        ))
      }
      survey_df <- survey_df[survey_df[[scheduled_col]] <= survey_df[[exported_col]], , drop = FALSE]
    }

    # expected: count all rows per participant, committed or not
    expected_counts <- table(survey_df[[participant_col]])
    expected_out <- setNames(rep(0L, length(all_participants)), all_participants)
    expected_out[names(expected_counts)] <- as.integer(expected_counts)
    expected_per_survey[[survey_name]] <- expected_out

    # committed: same, but filtered to rows actually committed
    committed_rows <- survey_df[!is.na(survey_df[[committed_col]]), , drop = FALSE]
    committed_counts <- table(committed_rows[[participant_col]])
    committed_out <- setNames(rep(0L, length(all_participants)), all_participants)
    committed_out[names(committed_counts)] <- as.integer(committed_counts)
    committed_per_survey[[survey_name]] <- committed_out
  }

  # information for output table
  result <- setNames(
    data.frame(all_participants, stringsAsFactors = FALSE),
    participant_col
  )

  for (survey_name in survey_names) {
    # index both vectors by result[[participant_col]], so order matches result's rows
    committed_n <- committed_per_survey[[survey_name]][result[[participant_col]]]
    expected_n <- expected_per_survey[[survey_name]][result[[participant_col]]]

    if (value == "count") {
      # absolute number of committed questionnaires for this survey
      result[[survey_name]] <- as.integer(committed_n)
    } else {
      # per-participant completion rate
      pct <- ifelse(expected_n == 0, NA_real_, round(100 * committed_n / expected_n, 1))
      result[[survey_name]] <- as.numeric(pct)
    }
  }

  rownames(result) <- NULL
  result
}
