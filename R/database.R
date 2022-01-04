#' Create a new Lexa database
#'
#' @param parent Parent directory (default is current working directory).
#' @param name Name of the Lexa database (`_lexadb` will be appended to the name).
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
  texts <- list()
  yaml::write_yaml(texts, file.path(path, "texts.yaml"))
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
  if (!stringr::str_ends(path, "_lexadb/?") | !file.exists(file.path(path, "config.yaml"))) {
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

  return(lexadb)
}

read_config <- function(path) {
  yaml::read_yaml(file.path(path, "config.yaml"))
}

read_lexicon <- function(path) {
  yaml::read_yaml(file.path(path, "lexicon.yaml"))
}

read_grammar <- function(path) {
  yaml::read_yaml(file.path(path, "grammar.yaml"))
}

read_texts <- function(path) {
  yaml::read_yaml(file.path(path, "texts.yaml"))
}




# Print methods ----

#' Print method for Lexa databases
#'
#' Print method for objects of class `lexadb`, which prints database info and
#' statistics.
#'
#' @param x An object of class `lexadb`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
print.lexadb <- function(x, ...) {
  cli::cli_rule("Database info")
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Name:')} {lexadb$config$name}"
  )
  cli::cli_text(
    "{crayon::silver(cli::symbol$info)} {crayon::blue('Entries:')} {length(lexadb$lexicon)} {crayon::silver(cli::symbol$em_dash)} {crayon::blue('Texts:')} {length(lexadb$texts)}"
  )
}
