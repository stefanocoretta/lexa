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
    {lexadb$config$name}"
  )
  cli::cli_text(
    "{crayon::silver(cli::symbol$info)} {crayon::blue('Entries:')}
    {length(lexadb$lexicon)} {crayon::silver(cli::symbol$em_dash)}
    {crayon::blue('Texts:')} {length(lexadb$texts)}"
  )
}
