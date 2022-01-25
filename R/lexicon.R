# Add entry ----

#' Add entry to lexicon
#'
#' This function creates a new entry in the lexicon, i.e. a new empty entry
#' skeleton is written to disk, in the `lexicon/` directory, for the user
#' to edit at will.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry The entry as a string.
#' @param gloss The gloss as a string.
#' @param part_of_speech The part of speech as a string.
#' @param phon The phonetic transcription as a string.
#' @param morph_category The morphosyntactic category as a string (`"lexical"` or `"grammatical"`).
#' @param morph_type The type of morpheme as a string.
#' @param definition The definition of the entry as a string.
#' @param etymology The etymology of the entry as a string.
#' @param notes Further notes as a string.
#'
#' @return Nothing. Used for its side effects
#' @export
add_entry <- function(lexadb,
                      entry = NULL,
                      gloss = NULL,
                      part_of_speech = NULL,
                      phon = NULL,
                      morph_category = NULL,
                      morph_type = NULL,
                      definition = gloss,
                      etymology = NULL,
                      notes = NULL) {

  lx_entry <- create_entry(
    lexadb,
    entry = entry,
    gloss = gloss,
    part_of_speech = part_of_speech,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type,
    definition = definition,
    etymology = etymology,
    notes = notes
  )

  write_entry(lexadb, lx_entry)
}




# Search entries ----

#' Search lexicon entries
#'
#' Search entries in the lexicon, by entry form or sense definitions.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry A regular expression to search among entries.
#' @param whole Whether to search for whole words (only applies to `entry`,
#'    `TRUE` by default).
#' @param definition A regular expression to search among sense definitions.
#' @param pos A regular expression to match the part of speech.
#'
#' @return A list of `lexalx` objects.
#' @export
search_lexicon <- function(lexadb,
                            entry = NULL,
                            whole = TRUE,
                            definition = NULL,
                            pos = NULL) {
  if (is.null(entry) & is.null(definition)) {
    cli::cli_abort("Please, provide either an entry or a definition to search
      in the lexicon.")
  }

  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)

  if (!is.null(entry)) {
    hits <- lapply(lexicon, function(x) {
      if (whole) {
        stringr::str_detect(x$entry, paste0("\\b", entry, "\\b"))
      } else {
        stringr::str_detect(x$entry, entry)
      }

    })
  } else if (!is.null(definition)) {
    hits <- lapply(lexicon, function(x) {
      check_definitions(x, definition)
    })
  }

  searched <- lexicon[unlist(hits)]

  if (!is.null(pos)) {
    hits <- lapply(searched, function(x) {
      stringr::str_detect(x$part_of_speech, pos)
    })
    searched <- searched[unlist(hits)]
  }

  return(searched)
}

# Helper function to search through definitions

check_definitions <- function(entry, pattern) {
  defs <- lapply(entry$senses, function(y) y$definition)
  sums <- sum(stringr::str_detect(defs, pattern))
  if (sums != 0) {
    TRUE
  } else {
    FALSE
  }
}
