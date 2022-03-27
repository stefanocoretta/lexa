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

  lx_entry <- list()
  today <- as.character(Sys.time())

  purrr::walk(
    lexicon_list,
    function(x) {

      lx_entry$id <- create_lx_id(lexadb)

      out <- glue::glue(
        'id: {lx_entry$id}
        entry: {x$entry}
        phon: {x$phon}
        morph_category: {x$morph_category}
        morph_type: {x$morph_type}
        part_of_speech: {x$part_of_speech}
        inflectional_features:
          class:
        etymology: {x$etymology}
        notes: {x$notes}
        allomorphs:
          al_01:
            id: al_01
            morph: {x$entry}
            phon: {x$phon}
        senses:
          se_01:
            id: se_01
            gloss: {x$gloss}
            definition: "{x$definition}"
        date_created: {today}
        date_modified: {today}',
        .null = ""
      )

      lx_entry$out <- out
      write_entry(lexadb, lx_entry)
    }
  )
}
