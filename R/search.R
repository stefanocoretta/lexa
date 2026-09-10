# Search entries ----

#' Search lexicon entries
#'
#' Search entries in the lexicon, by entry form or sense definitions.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param headword A regular expression to search among headwords.
#' @param whole Whether to search for whole words (only applies to `entry`,
#'    `FALSE` by default).
#' @param definition A regular expression to search among sense definitions.
#' @param word_class A regular expression to match the word class/part of speech.
#' @param show_entry Whether to print all the entry info (uses `print.lexalx`, default is `FALSE``).
#'
#' @return A list of `lexalx` objects.
#' @export
#'
#' @examples
#' db_path <- system.file("extdata/eleryon_lexadb", package = "lexaR")
#' eleryon <- load_lexadb(db_path)
#'
#' # Search for "chǭs"
#' search_lexicon(eleryon, "chǭs")
#'
#' # Search for all verbs
#' search_lexicon(eleryon, ".*", word_class = "verb")
#'
#' # Search for entry with meaning "love"
#' search_lexicon(eleryon, definition = "love")
search_lexicon <- function(lexadb,
                           headword = NULL,
                           whole = FALSE,
                           definition = NULL,
                           word_class = NULL,
                           show_entry = FALSE) {
  if (is.null(headword) & is.null(definition)) {
    cli::cli_abort("Please, provide either an entry or a definition to search
      in the lexicon.")
  }

  lexicon <- lexadb$lexicon

  if (!is.null(headword)) {
    hits <- lapply(lexicon, function(x) {
      if (whole) {
        any(stringr::str_detect(x$headword, paste0("\\b", headword, "\\b")))
      } else {
        any(stringr::str_detect(x$headword, headword))
      }

    })
  } else if (!is.null(definition)) {
    hits <- lapply(lexicon, function(x) {
      check_definitions(x, definition)
    })
  }

  searched <- lexicon[unlist(hits)]

  if (!is.null(word_class)) {
    hits <- lapply(searched, function(x) {
      stringr::str_detect(x$word_class, word_class)
    })
    searched <- searched[unlist(hits)]
  }

  if (length(searched) > 0) {
    cli::cli_alert_success("Found {length(searched)} entr{?y/ies}.")
    if (show_entry) {
      class(searched) <- c("lexalxs", "list")
      return(searched)
    } else {
      class(searched) <- c("lexalxscompact", "list")
      return(searched)
    }
  } else {
    cli::cli_alert_warning("No entry found!")
  }
}

# Show entry ----

#' Show lexicon entry with given id
#'
#' It shows the entry with the given id.
#'
#' @param lexadb   A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry_id A string with the entry id (the `lx_` prefix and leading
#'        zeros can be omitted.)
#'
#' @return A `lexalx` object.
#' @export
#'
#' @examples
#' db_path <- system.file("extdata/eleryon_lexadb", package = "lexaR")
#' eleryon <- load_lexadb(db_path)
#'
#' show_entry(eleryon, 6)
#' # Same as:
#' show_entry(eleryon, "lx_000006")
show_entry <- function(lexadb, entry_id) {
  lexicon <- lexadb$lexicon

  if (!stringr::str_detect(entry_id, "lx")) {
    entry_id <- stringr::str_pad(entry_id, 6, "left", "0")
    entry_id <- paste0("lx_", entry_id)
  }

  entries <- names(lexicon)

  if (entry_id %in% entries) {
    return(lexicon[[entry_id]])
  } else {
    cli::cli_abort(c("x" = "Sorry, there is no entry with the given id!"))
  }
}

# Entry helpers ----

# Helper function to search through definitions

check_definitions <- function(entry, pattern) {
  defs <- lapply(entry$senses, function(y) y$definition)
  hits <- any(stringr::str_detect(unlist(defs), pattern))
  return(hits)
}
