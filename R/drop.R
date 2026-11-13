#' Drop Participants from Survey Data
#'
#' Removes rows matching specified participant IDs from a data frame or
#' a list of data frames (as returned by [read_appkit_surveys()]).
#'
#' @param data A data frame or a named list of data frames.
#' @param participant_ids Character vector of participant IDs to remove.
#' @eval paste0(
#'   "@param id_col Character string naming the column that contains ",
#'   "participant IDs (default: the AppKit participant code column `\"",
#'   .appkit_col("participant"),
#'   "\"`)."
#' )
#'
#' @return Object of the same type as `data`, with matching rows removed.
#'
#' @details
#' When `data` is a list of data frames, the function applies the drop
#' operation to each element and reports results per survey.
#'
#' If any values in `participant_ids` are not found in the data, a warning
#' is issued listing the unmatched IDs.
#'
#' @examples
#' # Drop two participants from the bundled example survey
#' ids <- appkit_survey_entry$personalParticipantCode[1:2]
#' entry_small <- drop_participants(appkit_survey_entry, ids)
#'
#' # Drop participants from a named list of surveys
#' surveys <- list(entry = appkit_survey_entry)
#' surveys <- drop_participants(surveys, ids)
#'
#' @export
#' @importFrom cli cli_inform cli_warn
drop_participants <- function(data, participant_ids,
                              id_col = .appkit_col("participant")) {
  if (!is.character(id_col) || length(id_col) != 1L) {
    stop("`id_col` must be a single character string.", call. = FALSE)
  }

  if (!is.atomic(participant_ids)) {
    stop("`participant_ids` must be a character vector.", call. = FALSE)
  }
  participant_ids <- as.character(participant_ids)
  participant_ids <- unique(participant_ids)

  if (length(participant_ids) == 0L) {
    cli::cli_inform("No participant IDs provided. Returning data unchanged.")
    return(data)
  }

  if (is.data.frame(data)) {
    result <- .drop_from_df(data, participant_ids, id_col)
    .report_drop(result, participant_ids)
    return(result$data)
  }

  if (is.list(data)) {
    non_df <- !vapply(data, is.data.frame, logical(1))
    if (any(non_df)) {
      stop(
        "All elements of `data` must be data frames. ",
        "Non-data-frame elements at position(s): ",
        paste(which(non_df), collapse = ", "),
        call. = FALSE
      )
    }

    survey_names <- if (!is.null(names(data))) names(data) else seq_along(data)
    results <- Map(
      .drop_from_df,
      df = data,
      context = survey_names,
      MoreArgs = list(participant_ids = participant_ids, id_col = id_col)
    )

    all_ids_found <- character(0)
    for (i in seq_along(results)) {
      r <- results[[i]]
      all_ids_found <- union(all_ids_found, r$ids_found)
      if (r$n_removed > 0L) {
        cli::cli_inform(
          "{.val {survey_names[i]}}: dropped {r$n_removed} row{?s}."
        )
      }
    }

    missing_ids <- setdiff(participant_ids, all_ids_found)
    if (length(missing_ids) > 0L) {
      cli::cli_warn(
        "Participant ID{?s} not found in any survey: {.val {missing_ids}}"
      )
    }

    return(lapply(results, `[[`, "data"))
  }

  stop("`data` must be a data frame or a list of data frames.", call. = FALSE)
}

#' Drop participants from a single data frame
#'
#' @param df A data frame.
#' @param participant_ids Character vector of IDs to remove.
#' @param id_col Column name containing participant IDs.
#' @param context Optional survey name used in error messages.
#' @return A list with `data` (filtered data frame), `n_removed` (integer),
#'   and `ids_found` (character vector of IDs that were present).
#' @keywords internal
.drop_from_df <- function(df, participant_ids, id_col, context = NULL) {
  .check_df_cols(df, id_col, context = context)

  matches <- df[[id_col]] %in% participant_ids
  ids_found <- unique(df[[id_col]][matches])

  # Preserve column-level attributes (e.g., "label") that [.data.frame drops
  col_attrs <- lapply(df, attributes)
  result <- df[!matches, , drop = FALSE]
  for (col in names(result)) {
    attrs <- col_attrs[[col]]
    if (!is.null(attrs)) {
      attributes(result[[col]]) <- attrs
    }
  }

  list(
    data = result,
    n_removed = sum(matches),
    ids_found = ids_found
  )
}

#' Report drop results for a single data frame
#'
#' @param result List as returned by `.drop_from_df()`.
#' @param participant_ids The originally requested IDs.
#' @keywords internal
.report_drop <- function(result, participant_ids) {
  if (result$n_removed > 0L) {
    cli::cli_inform(
      "Dropped {result$n_removed} row{?s} from {length(result$ids_found)} participant{?s}."
    )
  }

  missing_ids <- setdiff(participant_ids, result$ids_found)
  if (length(missing_ids) > 0L) {
    cli::cli_warn(
      "Participant ID{?s} not found in data: {.val {missing_ids}}"
    )
  }
}
