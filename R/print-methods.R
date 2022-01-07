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
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {length(x$lexicon)} {crayon::green('|')}
    {crayon::blue('Texts:')} {length(x$texts)}"
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

  cli::cli_rule("Lemma")
  cli::cli_text(
    "{crayon::blue(lemma)} [{x$phon}] {.emph {crayon::green(x$part_of_speech)}}"
  )
  for (sense in 1:n_senses) {
    se_i <- x$senses[[sense]]
    cli::cli_rule("Sense {sense}")
    cli::cli_text(
      "{se_i$definition}"
    )
  }

}
