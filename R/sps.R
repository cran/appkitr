#' Extract Variable and Value Labels from SPSS Syntax File (Internal)
#'
#' Internal function to read an SPSS syntax file and extract variable and value labels
#' with robust quote handling and apostrophe processing.
#'
#' @param file A character string specifying the path to an SPSS syntax file (.sps).
#' @return A list containing var_labels and value_labels.
#' @keywords internal
#' @importFrom stats setNames

.extract_sps_labels <- function(file) {
  lines <- readLines(file, warn = FALSE)

  var_labels <- list()
  value_labels <- list()

  # ---- Extract VARIABLE LABELS ----
  for (i in seq_along(lines)) {
    line <- trimws(lines[i])
    # Match individual VARIABLE LABELS statements
    if (grepl("^VARIABLE\\s+LABELS\\s+", line, ignore.case = TRUE)) {
      # Remove the VARIABLE LABELS prefix
      content <- sub("^VARIABLE\\s+LABELS\\s+", "", line, ignore.case = TRUE)

      # Extract variable name (everything before the first quote)
      var_name <- trimws(sub("^([^'\"]+).*$", "\\1", content))

      # Extract the label text (everything between quotes)
      if (grepl("'", content)) {
        # Handle single quotes with proper apostrophe handling
        # Find position of first and last quote
        first_quote <- regexpr("'", content)[1]
        if (first_quote > 0) {
          quote_part <- substr(content, first_quote, nchar(content))
          # Parse quotes properly accounting for doubled apostrophes
          in_quote <- FALSE
          end_pos <- 0

          for (pos in 1:nchar(quote_part)) {
            char <- substr(quote_part, pos, pos)
            if (char == "'") {
              # Check if it's an escaped apostrophe (two consecutive apostrophes)
              if (pos < nchar(quote_part) && substr(quote_part, pos + 1, pos + 1) == "'") {
                # Skip the next one as it's part of the escape
                pos <- pos + 1
              } else {
                if (in_quote) {
                  end_pos <- pos
                  break
                } else {
                  in_quote <- TRUE
                }
              }
            }
          }

          if (end_pos > 0) {
            # Extract quoted part without outer quotes
            quoted_text <- substr(quote_part, 2, end_pos - 1)
            # Replace doubled apostrophes with single apostrophes
            var_label <- gsub("''", "'", quoted_text)
          } else {
            next # Malformed quote
          }
        } else {
          next # No quote found
        }
      } else if (grepl('"', content)) {
        # Double quotes handling
        var_label <- sub('^[^"]*"([^"]*)".*$', "\\1", content)
      } else {
        # No proper quotes found, skip
        next
      }

      var_labels[[var_name]] <- var_label
    }
  }

  # ---- Extract ADD VALUE LABELS ----
  in_val_block <- FALSE
  val_block_lines <- c()
  current_var <- NULL

  for (line in lines) {
    if (grepl("^\\s*ADD VALUE LABELS", line)) {
      in_val_block <- TRUE
      # Remove the command part and trim whitespace
      temp_line <- sub("^\\s*ADD VALUE LABELS", "", line)
      temp_line <- trimws(temp_line)
      # The first token should be the variable name.
      tokens <- strsplit(temp_line, "\\s+")[[1]]
      if (length(tokens) >= 1) {
        current_var <- tokens[1]
      }
      # Add any remaining content from the same line (which might start the parenthesis block)
      remaining <- paste(tokens[-1], collapse = " ")
      val_block_lines <- remaining
      if (grepl("\\.$", line)) {
        # Block complete on the same line
        full_block <- paste(val_block_lines, collapse = " ")
        full_block <- sub("\\.$", "", full_block)

        # Parse value labels properly with apostrophe handling
        mapping <- .extract_sps_value_label_pairs(full_block)

        value_labels[[current_var]] <- mapping
        in_val_block <- FALSE
        val_block_lines <- c()
        current_var <- NULL
      }
    } else if (in_val_block) {
      # Continue collecting the block until we see a line ending with a period
      val_block_lines <- c(val_block_lines, line)
      if (grepl("\\.$", line)) {
        full_block <- paste(val_block_lines, collapse = " ")
        full_block <- sub("\\.$", "", full_block)

        # Parse value labels properly with apostrophe handling
        mapping <- .extract_sps_value_label_pairs(full_block)

        value_labels[[current_var]] <- mapping
        in_val_block <- FALSE
        val_block_lines <- c()
        current_var <- NULL
      }
    }
  }

  return(list(
    var_labels = setNames(var_labels, tolower(names(var_labels))),
    value_labels = setNames(value_labels, tolower(names(value_labels)))
  ))
}

#' Extract Value-Label Pairs (Internal)
#'
#' Helper function to extract value-label pairs from SPSS syntax,
#' properly handling apostrophes.
#' @param text Character string containing value-label pairs
#' @return Named character vector of value-label mappings
#' @keywords internal

.extract_sps_value_label_pairs <- function(text) {
  mapping <- c()
  remain <- trimws(text)

  while (nchar(remain) > 0) {
    # Find code (first quoted string)
    code_match <- .extract_sps_next_quoted_string(remain)
    if (is.null(code_match)) break

    code <- code_match$content
    remain <- trimws(substr(remain, code_match$end_pos + 1, nchar(remain)))

    # Find label (second quoted string)
    label_match <- .extract_sps_next_quoted_string(remain)
    if (is.null(label_match)) break

    label <- label_match$content
    remain <- trimws(substr(remain, label_match$end_pos + 1, nchar(remain)))

    # Add to mapping
    mapping[code] <- label
  }

  return(mapping)
}

#' Extract Next Quoted String (Internal)
#'
#' Helper function to extract the next quoted string from text with proper
#' apostrophe handling.
#' @param text Character string to parse
#' @return List with content and end position, or NULL if no quote found
#' @keywords internal

.extract_sps_next_quoted_string <- function(text) {
  if (!grepl("'", text)) {
    return(NULL)
  }

  # Find position of first quote
  first_quote <- regexpr("'", text)[1]
  if (first_quote <= 0) {
    return(NULL)
  }

  # Process the string character by character
  quote_part <- substr(text, first_quote, nchar(text))
  in_quote <- FALSE
  end_pos <- 0
  content <- ""

  pos <- 1
  while (pos <= nchar(quote_part)) {
    char <- substr(quote_part, pos, pos)

    if (char == "'") {
      # Check if it's an escaped apostrophe (two consecutive apostrophes)
      if (pos < nchar(quote_part) && substr(quote_part, pos + 1, pos + 1) == "'") {
        if (in_quote) {
          # Inside the quote, append a single apostrophe
          content <- paste0(content, "'")
        }
        # Skip the next apostrophe
        pos <- pos + 2
        next
      } else {
        if (in_quote) {
          # End of quote
          end_pos <- pos + first_quote - 1
          break
        } else {
          # Start of quote
          in_quote <- TRUE
          pos <- pos + 1
          next
        }
      }
    } else if (in_quote) {
      # Regular character inside quote
      content <- paste0(content, char)
    }

    pos <- pos + 1
  }

  if (end_pos > 0) {
    return(list(content = content, end_pos = end_pos))
  } else {
    return(NULL) # Malformed quote
  }
}
