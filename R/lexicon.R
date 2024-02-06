# Add entry ----

#' Add entry to lexicon
#'
#' This function creates a new entry in the lexicon, i.e. a new empty entry
#' skeleton is written to disk, in the `lexicon/` directory, for the user
#' to edit at will.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param lexeme The entry as a string.
#' @param gloss The gloss as a string.
#' @param part_of_speech The part of speech as a string.
#' @param phon The phonetic transcription as a string.
#' @param morph_category The morphosyntactic category as a string
#'    (`"lexical"` or `"grammatical"`).
#' @param morph_type The type of morpheme as a string.
#' @param definition The definition of the entry as a string.
#' @param etymology The etymology of the entry as a string.
#' @param notes Further notes as a string.
#' @param homophone The homophone numeric index.
#' @param edit Open file for editing after creation (default is `TRUE`)
#'
#' @return Nothing. Used for its side effects
#' @export
add_entry <- function(lexadb,
                      lexeme = NULL,
                      gloss = NULL,
                      part_of_speech = NULL,
                      phon = NULL,
                      morph_category = NULL,
                      morph_type = NULL,
                      definition = gloss,
                      etymology = NULL,
                      notes = NULL,
                      homophone = NULL,
                      edit = TRUE) {

  db_path <- attr(lexadb, "meta")$path
  entries <- lapply(
    read_lexicon(db_path),
    function(entry) entry$lexeme
  )

  if (!is.null(lexeme)) {
    if (lexeme %in% entries) {
      homophones_n <- sum(entries == lexeme)
      cli::cli_alert_warning(
        cli::pluralize("{homophones_n} homophone{?s} found!")
      )
      cont <- usethis::ui_yeah(
        "Continue?",
        yes = "Yes",
        no = "No",
        shuffle = FALSE
      )

      if (!cont) {
        return(cli::cli_alert_warning("Entry not created!"))
      } else (
        homophone <- homophones_n + 1L
      )
    }
  }

  lx_entry <- create_entry(
    lexadb,
    lexeme = lexeme,
    gloss = gloss,
    part_of_speech = part_of_speech,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type,
    definition = definition,
    etymology = etymology,
    notes = notes,
    homophone = homophone
  )

  write_entry(lexadb, lx_entry)
  cli::cli_alert_success("Entry {cli::col_blue(lx_entry$id)} added to the lexicon!")

  if (edit) {
    edit_entry(lexadb, lx_entry$id)
  }
}




# Search entries ----

#' Search lexicon entries
#'
#' Search entries in the lexicon, by entry form or sense definitions.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param lexeme A regular expression to search among entries.
#' @param whole Whether to search for whole words (only applies to `entry`,
#'    `FALSE` by default).
#' @param definition A regular expression to search among sense definitions.
#' @param pos A regular expression to match the part of speech.
#' @param show_entry Whether to print all the entry info (uses `print.lexalx`, default is `FALSE``).
#'
#' @return A list of `lexalx` objects.
#' @export
#'
#' @examples
#' db_path <- system.file("extdata/eleryon_lexadb", package = "lexa")
#' eleryon <- load_lexadb(db_path)
#'
#' # Search for "chǭs"
#' search_lexicon(eleryon, "chǭs")
#'
#' # Search for all verbs
#' search_lexicon(eleryon, ".*", pos = "verb")
#'
#' # Search for entry with meaning "love"
#' search_lexicon(eleryon, definition = "love")
search_lexicon <- function(lexadb,
                            lexeme = NULL,
                            whole = FALSE,
                            definition = NULL,
                            pos = NULL,
                            show_entry = FALSE) {
  if (is.null(lexeme) & is.null(definition)) {
    cli::cli_abort("Please, provide either an entry or a definition to search
      in the lexicon.")
  }

  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)

  if (!is.null(lexeme)) {
    hits <- lapply(lexicon, function(x) {
      if (whole) {
        stringr::str_detect(x$lexeme, paste0("\\b", lexeme, "\\b"))
      } else {
        stringr::str_detect(x$lexeme, lexeme)
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
#' db_path <- system.file("extdata/eleryon_lexadb", package = "lexa")
#' eleryon <- load_lexadb(db_path)
#'
#' show_entry(eleryon, 6)
#' # Same as:
#' show_entry(eleryon, "lx_000006")
show_entry <- function(lexadb, entry_id) {
  db_path <- attr(lexadb, "meta")$path

  if (!stringr::str_detect(entry_id, "lx")) {
    entry_id <- stringr::str_pad(entry_id, 6, "left", "0")
    entry_id <- paste0("lx_", entry_id)
  }

  lx_path <- file.path(
    normalizePath(db_path), "lexicon",
    paste0(entry_id, ".yaml")
  )

  if (file.exists(lx_path)) {
    lx <- yaml::read_yaml(lx_path)
  } else {
    cli::cli_abort("Sorry, there is no entry with the given id!")
  }


  attr(lx, "dbpath") <- db_path
  class(lx) <- c("lexalx", "list")

  return(lx)
}




# Actually writes entry on disk in lexicon/.

write_entry <- function(lexadb, lx_entry) {
  db_path <- attr(lexadb, "meta")$path
  lx_path <- file.path("lexicon", paste0(lx_entry$id, ".yaml"))
  lx_full_path <- file.path(db_path, lx_path)
  yaml::write_yaml(lx_entry$out, lx_full_path)
}

write_lexicon <- function(lexadb, lexalx) {
  purrr::walk(
    lexalx,
    function(entry) {
      write_entry(lexadb, entry)
    }
  )
}

# Show and edit entries ----

#' Open a lexical entry
#'
#' It opens the file of the specified lexical entry.
#'
#' @param lexadb   A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry_id A string with the entry id (the `lx_` prefix and leading
#'        zeros can be omitted.)
#'
#' @return Nothing. Used for its side effects
#' @export
open_entry <- function(lexadb, entry_id) {
  db_path <- attr(lexadb, "meta")$path

  if (!stringr::str_detect(entry_id, "lx")) {
    entry_id <- stringr::str_pad(entry_id, 6, "left", "0")
    entry_id <- paste0("lx_", entry_id)
  }

  lx_path <- file.path(
    normalizePath(db_path), "lexicon",
    paste0(entry_id, ".yaml")
  )

  if (file.exists(lx_path)) {
    usethis::edit_file(lx_path)
  } else {
    cli::cli_abort("Sorry, there is no entry with the given id!")
  }
}

#' Edit a lexical entry
#'
#' It opens the file of the specified lexical entry for editing and updates the `date_modified` field.
#'
#' @param lexadb   A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry_id A string with the entry id (the `lx_` prefix and leading
#'        zeros can be omitted.)
#'
#' @return Nothing. Used for its side effects
#' @export
edit_entry <- function(lexadb, entry_id) {
  db_path <- attr(lexadb, "meta")$path

  if (!stringr::str_detect(entry_id, "lx")) {
    entry_id <- stringr::str_pad(entry_id, 6, "left", "0")
    entry_id <- paste0("lx_", entry_id)
  }

  lx_path <- file.path(
    normalizePath(db_path), "lexicon",
    paste0(entry_id, ".yaml")
  )

  if (file.exists(lx_path)) {
    lx_yaml <- yaml::read_yaml(normalizePath(lx_path))
    lx_yaml$date_modified <- as.character(Sys.Date())
    yaml::write_yaml(lx_yaml, lx_path)
    usethis::edit_file(lx_path)
  } else {
    cli::cli_abort("Sorry, there is no entry with the given id!")
  }
}

# Merge and split lexicon ----

# Merge individual yaml entries to single list
merge_lexicon <- function(lexadb, sort) {
  db_path <- attr(lexadb, "meta")$path
  lx_path <- glue::glue("{db_path}/lexicon")
  lx_files <- list.files(lx_path, "*.yaml", full.names = TRUE)

  lexicon <- list()

  for (file in lx_files) {
    file_yaml <- yaml::read_yaml(file)
    lexicon[[file_yaml$id]] <- file_yaml
  }

  # Sorting entries alphabetically
  if (sort) {
    entry_ids <- sapply(lexicon, function(entry) entry$lexeme)
    ordered_ids <- names(lexicon)[order(entry_ids)]

    # Reorder the list based on the ordered names
    lexicon_reordered <- lexicon[ordered_ids]

    return(lexicon_reordered)
  } else {
    return(lexicon)
  }
}

#' Write merged lexicon to disk
#'
#' It writes a yaml file which contains all the entries in the lexicon. The file
#' is output in the `src` directory.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param sort Whether to sort the entries alphabetically (default is `TRUE`).
#' @param overwrite Whether to overwrite an existing `lexicon.yaml` file.
#'
#' @export
write_merged_lexicon <- function(lexadb, sort = TRUE, overwrite = FALSE) {
  merged <- merge_lexicon(lexadb, sort = sort)

  db_path <- attr(lexadb, "meta")$path
  merged_file <- glue::glue("{db_path}/src/lexicon.yaml")

  if (file.exists(merged_file)) {
    if (overwrite) {
      yaml::write_yaml(merged, merged_file)
    } else {
      cli::cli_abort(c(
        "x" = "A lexicon.yaml file already exists! Set {.arg overwrite = TRUE} to overwrite."
      ))
    }
  } else {
    dir.create(glue::glue("{db_path}/src"), showWarnings = FALSE)
    yaml::write_yaml(merged, merged_file)
  }

}
