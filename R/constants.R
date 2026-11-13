#' AppKit Export Metadata Columns (Internal)
#'
#' Registry of the metadata columns of AppKit survey exports. Survey item
#' columns are not listed. appkitr never subsets data; unknown columns always
#' pass through.
#'
#' @format A data frame with one row per metadata column and four fields:
#' \describe{
#'   \item{key}{Stable identifier used to reference the column in code.}
#'   \item{column}{Column name as it appears in AppKit exports.}
#'   \item{timestamp}{Whether the column is parsed as a timestamp.}
#'   \item{min_version}{Earliest AppKit version whose exports contain the
#'     column, `NA` if present in all supported versions.}
#' }
#' @keywords internal

.appkit_cols <- data.frame(
  key = c(
    "participant", "login", "status", "last_status_change", "note",
    "first_opened", "committed", "scheduled", "expired", "published",
    "exported", "os_version", "smartphone_type", "push_notification_allowed",
    "notification_type", "notification_title", "notification_text"
  ),
  column = c(
    "personalParticipantCode", "generalLoginCode", "status", "lastStatusChange", "note",
    "firstOpened", "committed", "scheduled", "expired", "published",
    "exported", "osVersion", "smartphoneType", "pushNotificationAllowed",
    "notificationType", "notificationTitle", "notificationText"
  ),
  timestamp = c(
    FALSE, FALSE, FALSE, TRUE, FALSE,
    TRUE, TRUE, TRUE, TRUE, TRUE,
    TRUE, FALSE, FALSE, FALSE,
    FALSE, FALSE, FALSE
  ),
  min_version = c(
    NA, NA, "1.13.0", "1.13.0", "1.13.0",
    "1.9.0", NA, NA, "2.0.0", NA,
    "1.17.0", NA, NA, "1.13.0",
    "1.13.0", "1.13.0", "1.13.0"
  ),
  stringsAsFactors = FALSE
)

#' Look up an AppKit Column Name by Key (Internal)
#'
#' Returns the export column name registered under a key in [.appkit_cols].
#'
#' @param key Single character string, a `key` from [.appkit_cols].
#' @return Character string, the corresponding export column name.
#' @keywords internal

.appkit_col <- function(key) {
  if (!is.character(key) || length(key) != 1L) {
    stop("Internal error: `key` must be a single character string.", call. = FALSE)
  }
  idx <- match(key, .appkit_cols$key)
  if (is.na(idx)) {
    stop("Internal error: unknown AppKit column key `", key, "`.", call. = FALSE)
  }
  .appkit_cols$column[idx]
}

#' AppKit Timestamp Columns (Internal)
#'
#' Returns the export column names registered as timestamps in [.appkit_cols].
#'
#' @return Character vector of timestamp column names.
#' @keywords internal

.appkit_timestamp_cols <- function() {
  .appkit_cols$column[.appkit_cols$timestamp]
}
