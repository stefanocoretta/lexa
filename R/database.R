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
  readr::write_file(lx_entry$out, file.path(path, "lexicon", "lx_000001.yaml"))
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
  texts <- lapply(texts_files, function(text) {
      tx <- yaml::read_yaml(text)
      tx <- structure(tx, class = c("lexatx", "list"))
      return(tx)
    }
  )
  texts <- lapply(texts, function(text) {
    text_i <- lapply(text$text, function(st) {
      structure(st, class = c("lexast", "list"))
    })
    text$text <- text_i
    return(text)
  })
  texts_ids <- lapply(texts, function(x) x[["id"]])
  names(texts) <- texts_ids
  return(texts)
}
