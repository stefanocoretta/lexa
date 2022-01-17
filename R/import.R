# Import lexicon from csv ----

#' Import lexicon from a csv file
#'
#' It imports entries from a `.csv` file with lexical data. The file must have
#' specific columns, see Details for file specifics. The lexicon is imported
#' into an existing Lexa database.
#'
#' The file should have the following columns (they can be omitted, but
#' if present they must be so named):
#'
#' * `entry`: the lexical entry, as it should appear in the head entry.
#' * `phon`: phonetic transcription of the entry.
#' * `morph_category`: category of entry (e.g. lexical vs grammatical).
#' * `morph_type`: type of morpheme (e.g. root vs affix).
#' * `part_of_speech`: part of speech of entry.
#' * `class`: lexical class of entry (e.g. verbal conjugations, noun classes).
#' * `gloss`: .
#' * `etymology`:
#' * `notes`:
#'
#' Note that this list is temporary and *it will change* in the future.
#'
#' @param lexadb A `lexadb` object as returned by `load_lexadb()`.
#' @param path The path to the lexicon .csv file as a string.
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
import_lexicon_csv <- function(lexadb, path) {
  lexicon_tab <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  lexicon_list <- purrr::transpose(lexicon_tab)

  purrr::walk(
    lexicon_list,
    function(x) {

      lx_id <- create_lx_id(lexadb)

      lx_entry <- list(
        id = lx_id,
        entry = x$entry,
        phon = x$phon,
        morph_category = x$morph_category,
        morph_type = x$morph_type,
        part_of_speech = x$part_of_speech,
        inflectional_features = list(class = x$class),
        etymology = x$etymology,
        notes = x$notes,
        allomorphs = list(
          al_01 = list(
            id = "al_01",
            morph = x$entry,
            phon = x$phon
          )
        ),
        senses = list(
          se_01 = list(
            id = "se_01",
            gloss = x$gloss,
            definition = x$gloss,
            inflectional_features = list(class = x$class)
          )
        ),
        date_created = as.character(Sys.time()),
        date_modified = as.character(Sys.time())
      )
      write_entry(lexadb, lx_entry)
    }
  )
}
