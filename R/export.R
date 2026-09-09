#' Convert lexicon to a tibble
#'
#' It converts the lexicon from a Lexa database to a tibble (data frame). Each
#' lexical entry is one row in the resulting tibble.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#'
#' @return A tibble.
#' @export
lexadb_to_tbl <- function(lexadb) {
  if (!("lexadb" %in% class(lexadb))) {
    cli::cli_abort(c("x" = "'{lexadb}' is not a lexadb!"))
  }

  lexicon <- lexadb$lexicon

  df <- purrr::map_dfr(lexicon, function(entry) {

    # Extract lexeme
    lexeme <- list()

    if (is.list(entry$lexeme)) {
      for (key in names(entry$lexeme)) {
        value <- entry$lexeme[[key]]

        if (is.list(value) && !is.null(value$text)) {
          value <- value$text
        }

        lexeme[[paste0("lexeme_", key)]] <- value
      }
    } else {
      lexeme[["lexeme"]] <- entry$lexeme
    }

    # Extract phonemic/phonetic
    phonemic <- if (is.null(entry$phonemic)) {
      NA_character_
    } else if (is.list(entry$phonemic)) {
      paste(unlist(entry$phonemic), collapse = "; ")
    } else {
      entry$phonemic
    }
    if (all(is.na(phonemic))) {
      phonemic <- NULL
    }

    phonetic <- if (is.null(entry$phonetic)) {
      NA_character_
    } else if (is.list(entry$phonetic)) {
      paste(unlist(entry$phonetic), collapse = "; ")
    } else {
      entry$phonetic
    }
    if (all(is.na(phonetic))) {
      phonetic <- NULL
    }

    # Extract glosses
    gloss <- list()

    for (sense in entry$senses) {
      if (is.null(sense$gloss)) {
        next
      }

      if (is.list(sense$gloss)) {
        for (key in names(sense$gloss)) {
          column <- paste0("gloss_", key)
          gloss[[column]] <- c(
            gloss[[column]],
            sense$gloss[[key]]
          )
        }
      } else {
        gloss[["gloss"]] <- c(
          gloss[["gloss"]],
          sense$gloss
        )
      }
    }

    # Collapse multiple senses
    gloss <- purrr::map(
      gloss,
      ~ paste(.x, collapse = "; ")
    )

    # Collect notes
    # TODO: check if this can be the same as phonetic/phonemic above
    notes <- if (!is.null(entry$notes)) {
      paste(entry$notes, collapse = "; ")
    } else {
      NULL
    }

    # Fixed fields
    result <- list(
      id = entry$id,
      phonemic = phonemic,
      phonetic = phonetic,
      word_type = entry$word_type,
      word_class = entry$word_class,
      etymology = entry$etymology,
      notes = notes
    )

    # Collate all columns
    c(result, lexeme, gloss)
  })

  df <- tibble::as_tibble(df)

  df <- dplyr::relocate(
    df,
    id,
    dplyr::starts_with("lexeme"),
    dplyr::starts_with("gloss"),
    dplyr::matches("phonemic"),
    dplyr::matches("phonetic"),
    dplyr::starts_with("word_"),
    dplyr::matches("etymology"),
    dplyr::matches("notes")
  )

  return(df)
}
