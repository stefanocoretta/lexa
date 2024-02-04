#' Create a new Lexa database
#'
#' @param parent Parent directory (default is current working directory).
#' @param name Name of the Lexa database (`_lexadb` will be appended to the
#'   name).
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' create_lexadb(parent = "./", name = "my_new")
#' }
create_lexadb <- function(parent = ".", name) {
  name_db <- paste0(name, "_lexadb")
  path <- file.path(parent, name_db)
  dir.create(path, FALSE, TRUE)

  create_config(path, name)
  create_lexicon(path)
  create_grammar(path)
  create_texts(path)
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
#' @examples
#' db_path <- system.file("extdata/albanian_lexadb", package = "lexa")
#' albanian <- load_lexadb(db_path)
#' albanian
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
  attr(lexadb, "meta") <- list(
    path = normalizePath(path)
  )

  return(lexadb)
}
