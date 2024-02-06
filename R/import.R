# Import lexicon from csv ----

#' Import lexicon from a csv file
#'
#' It imports entries from a `.csv` file with lexical data. The file must have
#' specific columns, see Details for file specifics. The lexicon is imported
#' into an existing Lexa database.
#'
#' The file must have at least the following columns:
#'
#' * `entry`: the lexical entry, as it should appear in the head entry.
#' * `gloss`: the gloss of the entry.
#'
#' Optionally, the file can have the following columns:
#'
#' * `definition`: the full definition of the entry. This normally provides
#'    more details about the meaning than the gloss. If this column is not
#'    present, the definition field is filled with the gloss.
#' * `phon`: phonetic transcription of the entry.
#' * `morph_category`: category of entry (e.g. lexical vs grammatical).
#' * `morph_type`: type of morpheme (e.g. root vs affix).
#' * `part_of_speech`: part of speech of entry.
#' * `class`: lexical class of entry (e.g. verbal conjugations, noun classes).
#' * `etymology`: the etymology of the entry.
#' * `notes`: free text notes.
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

  today <- as.character(Sys.Date())

  purrr::walk(
    lexicon_list,
    function(x) {
      lx_entry <- list()
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
            definition: "{ifelse(!is.null(x$definition), x$definition, x$gloss)}"
        date_created: {today}
        date_modified: {today}

        ',
        .null = ""
      )

      lx_entry$out <- out
      write_entry(lexadb, lx_entry)
    }
  )
}


# Lift to Lexa lexicon ----
lift_to_lexa <- function(path) {
  lift <- xml2::read_xml(path)
  lift_list <- xml2::as_list(lift)

  lift_header <- lift_list$lift$header
  lift_entries <- lift_list$lift[-1]

  today <- as.character(Sys.Date())

  out <- lapply(
    lift_entries,
    function(entry) {
      entry_names <- names(entry)
      lx_senses <- entry[entry_names == "sense"]
      lx_senses_n <- length(lx_senses)

      # If there is more than one sense
      if (lx_senses_n > 1) {
        lx_gram_info <- lapply(
          lx_senses,
          function(sense) {
            attr(sense[["grammatical-info"]], "value")
          }
        )

        lx_gram_uniq <- unique(unlist(lx_gram_info))

        if (length(lx_gram_uniq) > 1) {
          # If there is more than one POS
          for (lx_gram in lx_gram_uniq) {
            lx_senses_pos <- lx_senses[unlist(lapply(lx_senses, function (x) attr(x[["grammatical-info"]], "value") == lx_gram))]

            lx_senses_list <- lapply(
              lx_senses_pos,
              function(this_sense) {
                list(
                  id = "",
                  gloss = this_sense[["gloss"]][["text"]][[1]],
                  definition = this_sense[["definition"]][["form"]][["text"]][[1]]
                )
              }
            )

            these_senses_n <- length(lx_senses_list)

            if (these_senses_n == 0) {
              lx_senses_list <- list(
                sense = list(
                  id = NULL,
                  gloss = NULL,
                  definition = NULL
                )
              )
              these_senses_n = 1
            }

            these_senses_ids <- paste0("se_", format(as.hexmode(paste0(1:these_senses_n)), width = 2))

            names(lx_senses_list) <- these_senses_ids
            for (i in 1:length(lx_senses_list)) {
              this_name <- names(lx_senses_list[i])
              lx_senses_list[[i]][["id"]] <- this_name
            }

            lexeme <- entry[["lexical-unit"]][["form"]][["text"]][[1]]

            out <- list(
              id = NULL,
              lexeme = lexeme,
              phon = lexeme,
              morph_category = NULL,
              morph_type = NULL,
              part_of_speech = lx_gram,
              inflectional_features = list(class = NULL),
              etymology = NULL,
              notes = list(entry[["note"]][["form"]][["text"]][[1]]),
              homophone = NULL,
              allomorphs = list(
                al_01 = list(
                  id = "al_01",
                  morph = lexeme,
                  phon = lexeme
                )
              ),
              senses = lx_senses_list,
              date_created = today,
              date_modified = today
            )

            return(list(out = out))
          }
        } else {
          # If there is only one POS
          lx_senses_list <- lapply(
            lx_senses,
            function(this_sense) {
              list(
                id = "",
                gloss = this_sense[["gloss"]][["text"]][[1]],
                definition = this_sense[["definition"]][["form"]][["text"]][[1]]
              )
            }
          )

          these_senses_n <- length(lx_senses_list)

          if (these_senses_n == 0) {
            lx_senses_list <- list(
              sense = list(
                id = NULL,
                gloss = NULL,
                definition = NULL
              )
            )
            these_senses_n = 1
          }

          these_senses_ids <- paste0("se_", format(as.hexmode(paste0(1:these_senses_n)), width = 2))

          names(lx_senses_list) <- these_senses_ids
          for (i in 1:length(lx_senses_list)) {
            this_name <- names(lx_senses_list[i])
            lx_senses_list[[i]][["id"]] <- this_name
          }

          lexeme <- entry[["lexical-unit"]][["form"]][["text"]][[1]]

          out <- list(
            id = NULL,
            lexeme = lexeme,
            phon = lexeme,
            morph_category = NULL,
            morph_type = NULL,
            part_of_speech = lx_gram_uniq,
            inflectional_features = list(class = NULL),
            etymology = NULL,
            notes = list(entry[["note"]][["form"]][["text"]][[1]]),
            homophone = NULL,
            allomorphs = list(
              al_01 = list(
                id = "al_01",
                morph = lexeme,
                phon = lexeme
              )
            ),
            senses = lx_senses_list,
            date_created = today,
            date_modified = today
          )

          return(list(out = out))
        }

      } else {
        # If there is only one sense
        lx_gram_info <- lapply(
          lx_senses,
          function(sense) {
            attr(sense[["grammatical-info"]], "value")
          }
        )

        lx_gram_uniq <- unique(unlist(lx_gram_info))

        lx_senses_list <- lapply(
          lx_senses,
          function(this_sense) {
            list(
              id = "",
              gloss = this_sense[["gloss"]][["text"]][[1]],
              definition = this_sense[["definition"]][["form"]][["text"]][[1]]
            )
          }
        )

        these_senses_n <- length(lx_senses_list)

        if (these_senses_n == 0) {
          lx_senses_list <- list(
            sense = list(
              id = NULL,
              gloss = NULL,
              definition = NULL
            )
          )
          these_senses_n = 1
        }

        these_senses_ids <- paste0("se_", format(as.hexmode(paste0(1:these_senses_n)), width = 2))

        names(lx_senses_list) <- these_senses_ids
        for (i in 1:length(lx_senses_list)) {
          this_name <- names(lx_senses_list[i])
          lx_senses_list[[i]][["id"]] <- this_name
        }

        lexeme <- entry[["lexical-unit"]][["form"]][["text"]][[1]]

        out <- list(
          id = NULL,
          lexeme = lexeme,
          phon = lexeme,
          morph_category = NULL,
          morph_type = NULL,
          part_of_speech = lx_gram_uniq,
          inflectional_features = list(class = NULL),
          etymology = NULL,
          notes = list(entry[["note"]][["form"]][["text"]][[1]]),
          homophone = NULL,
          allomorphs = list(
            al_01 = list(
              id = "al_01",
              morph = lexeme,
              phon = lexeme
            )
          ),
          senses = lx_senses_list,
          date_created = today,
          date_modified = today
        )

        return(list(out = out))
      }
    }
  )

  out_length <- length(out)
  ids <- c()
  for (i in 1:out_length) {
    new_id_hex <- format(as.hexmode(i), width = 6)
    new_id <- paste0("lx_", new_id_hex)
    out[[i]][["id"]] <- new_id
    out[[i]][["out"]][["id"]] <- new_id
    ids <- c(ids, new_id)
  }

  return(out)
}

# Import lexicon from LIFT file ----

#' Import lexicon from LIFT file
#'
#' It imports the lexicon from a LIFT file (exported from LexiquePro).
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param lexadb A `lexadb` object as returned by `load_lexadb()`.
#' @param path The path to the lexicon LIFT file as a string.
#'
#' @return Nothing. Used for its side effects.
#'
#' @export
import_lexicon_lift <- function(lexadb, path) {
  lexalx <- lift_to_lexa(normalizePath(path))
  write_lexicon(lexadb, lexalx)
}
