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
  lexicon <- list()
  yaml::write_yaml(lexicon, file.path(path, "lexicon.yaml"))
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
  lexicon <- read_lexicon(path)
  grammar <- read_grammar(path)
  texts <- read_texts(path)

  lexadb <- list(
    config = config,
    lexicon = lexicon,
    grammar = grammar,
    texts = texts
  )

  class(lexadb) <- c("lexadb", "list")
  attr(lexadb, "meta") <- list(path = file.path(path))

  return(lexadb)
}

read_config <- function(path) {
  yaml::read_yaml(file.path(path, "config.yaml"))
}

read_lexicon <- function(path) {
  lexicon <- yaml::read_yaml(file.path(path, "lexicon.yaml"))
  lapply(lexicon, function(lexeme) {
      structure(lexeme, class = c("lexalx", "list"))
    }
  )
}

read_grammar <- function(path) {
  yaml::read_yaml(file.path(path, "grammar.yaml"))
}

read_texts <- function(path) {
  texts_path <- file.path(path, "texts")
  texts_files <- list.files(texts_path, full.names = TRUE)
  lapply(texts_files, function(file) yaml::read_yaml(file))
}
