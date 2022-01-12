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
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Name:')}
    {x$config$name}"
  )
  db_path <- attr(x, "meta")$path
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {length(list.files(file.path(db_path, 'lexicon'), '*.yaml'))}
    {crayon::green('|')}
    {crayon::blue('Texts:')}
    {length(list.files(file.path(db_path, 'texts'), '*.yaml'))}"
  )
}

#' Print method for lexemes
#'
#' Print method for objects of class `lexalx`, which prints lexeme info.
#'
#' @param x An object of class `lexalx`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexalx <- function(x, ...) {
  lemma <- ifelse(is.null(x$lemma), x$lexeme, x$lemma)
  n_senses <- length(x$senses)
  if (!is.null(x$phon)) {
    lemma_line <- "{crayon::blue(lemma)}
    [{x$phon}] {.emph {crayon::green(x$part_of_speech)}}"
  } else {
    lemma_line <- "{crayon::blue(lemma)}
    {.emph {crayon::green(x$part_of_speech)}}"
  }

  cli::cli_rule("Lemma", right = "{.emph {x$id}}")
  cli::cli_text(lemma_line)
  cli::cli_h3("Senses")
  for (sense in x$senses) {
    if (!is.null(sense$inflectional_features)) {
      cli::cli_text("{cli::symbol$bullet}
        {crayon::blue('[', sense$inflectional_features, ']', sep = '')}
        {sense$definition}")
    } else {
      cli::cli_text("{cli::symbol$bullet} {sense$definition}")
    }
  }

}
