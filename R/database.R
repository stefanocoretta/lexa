#' Create a new Lexa database
#'
#' @param parent Parent directory (default is current working directory).
#' @param name Name of the Lexa database (`_lexadb` will be appended to the
#'   name).
#'
#' @return Nothing. Used for its side effects.
#' @export
create_lexadb <- function(parent = ".", name) {
  name_db <- paste0(name, "_lexadb")
  path <- file.path(parent, name_db)
  dir.create(path, FALSE, TRUE)

  create_config(path, name)
  create_lexicon(path)
  create_grammar(path)
  create_texts(path)
}

create_config <- function(path, name) {
  config <- list(
    schema = "lexadb",
    name = name
  )
  yaml::write_yaml(config, file.path(path, "config.yaml"))
}

create_lexicon <- function(path) {
  dir.create(file.path(path, "lexicon"), FALSE, TRUE)

  lx_entry <- create_entry(NULL)
  yaml::write_yaml(lx_entry, file.path(path, "lexicon/lx_000001.yaml"))
}

create_grammar <- function(path) {
  grammar <- list()
  yaml::write_yaml(grammar, file.path(path, "grammar.yaml"))
}

create_texts <- function(path) {
  dir.create(file.path(path, "texts"), FALSE, TRUE)
  text_example <- list()
  yaml::write_yaml(text_example, file.path(path, "texts", "text_example.yaml"))
}




# Load lexadb ----

#' Load Lexa database
#'
#' It loadds a Lexa database from the specified path. The path is the directory
#' containing the database.
#'
#' @param path The path to the database as a string.
#'
#' @return A `lexadb` object.
#' @export
#'
load_lexadb <- function(path) {
  if (
    !stringr::str_ends(path, "_lexadb/?") |
    !file.exists(file.path(path, "config.yaml"))
  ) {
    cli::cli_abort("The provided path is not a Lexa database.")
  }

  cli::cli_alert_info("Loading lexa database...")

  config <- read_config(path)

  lexadb <- list(
    config = config
  )

  class(lexadb) <- c("lexadb", "list")
  attr(lexadb, "meta") <- list(path = normalizePath(path))

  return(lexadb)
}

read_config <- function(path) {
  yaml::read_yaml(file.path(path, "config.yaml"))
}

read_lexicon <- function(path) {
  lexicon_path <- file.path(path, "lexicon")
  lexicon_files <- list.files(lexicon_path, full.names = TRUE)
  lexicon <- lapply(lexicon_files, function(lexeme) {
      lx <- yaml::read_yaml(lexeme)
      structure(lx, class = c("lexalx", "list"))
    }
  )
  lx_ids <- lapply(lexicon, function(x) x[["id"]])
  names(lexicon) <- lx_ids
  lexicon
}

read_grammar <- function(path) {
  yaml::read_yaml(file.path(path, "grammar.yaml"))
}

read_texts <- function(path) {
  texts_path <- file.path(path, "texts")
  texts_files <- list.files(texts_path, full.names = TRUE)
  lapply(texts_files, function(file) yaml::read_yaml(file))
}




# Import lexicon from csv ----

#' Import lexicon to existing database
#'
#' It imports entries from a lexicon .csv file with specific column names.
#' See Details for file specifics.
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
#' Note that this list is temporary and it will change in the future.
#'
#'
#' @param lexadb A `lexadb` object as returned by `load_lexadb()`.
#' @param path The path to the lexicon .csv file as a string.
#'
#' @return A `lexadb` object.
#' @export
#'
import_lexicon <- function(lexadb, path) {
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




# Import lexicon from MDF ----

#' Import lexicon to existing database
#'
#' It imports entries from a lexicon in the MDF format.
#'
#' @param lexadb A `lexadb` object as returned by `load_lexadb()`.
#' @param path The path to the lexicon MDF file as a string.
#' @param skip Lines in the file to skip.
#'
#' @return A `lexadb` object.
#' @importFrom magrittr "%>%"
#' @export
import_lexicon_mdf <- function(lexadb, path, skip = 0) {
  mdf <- readr::read_fwf(
    path,
    readr::fwf_widths(c(4, 200), c("marker", "value")),
    skip = skip,
    show_col_types = FALSE
  ) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(
    marker = stringr::str_remove(marker, "\\\\"),
    is_lx = marker == "lx"
  )

  count <- 0
  idx <- numeric()
  is_lx <- mdf$is_lx
  for (i in 1:length(is_lx)) {
    if (is_lx[i]) {
      count <- count + 1
      idx <- c(idx, count)
    } else {
      idx <- c(idx, count)
    }
  }
  mdf$idx <- idx

  mdf <- tidyr::nest(dplyr::select(mdf, -is_lx), data = c(marker, value))
  return(mdf)
}
