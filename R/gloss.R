#' Typeset interlinear gloss
#'
#' The function returns the selected sentence as properly formatted html/latex code.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param text Text id.
#' @param sentence Sentence id to print.
#' @param format Format to print out with (either \code{html} or \code{latex}).
#'
#' @export
typeset_gloss <- function(lexadb, text, sentence, format = "latex") {

  db_path <- attr(lexadb, "meta")$path
  text <- yaml::read_yaml(file.path(db_path, "texts", paste0(text, ".yaml")))

  sentence_i <- text$sentences[[sentence]]$sentence
  morpho <- text$sentences[[sentence]]$morpho
  gloss <- text$sentences[[sentence]]$gloss
  translation <- text$sentences[[sentence]]$translation

  if (format == "latex") {
    glue::glue(
      "```{{=latex}}
      \\ex \\begingl
      \\glpreamble {sentence_i}//
      \\gla {morpho}//
      \\glb {gloss}//
      \\glft {translation}//
      \\endgl \\xe
      ```"
    )

  } else if (format == "html") {
    htmltools::div(
      "data-gloss" = NA,
      htmltools::p(glue::glue("{sentence_i}")),
      htmltools::p(glue::glue("{morpho}")),
      htmltools::p(glue::glue("{gloss}")),
      htmltools::p(glue::glue("\u2018{translation}\u2019")),
    )
  }

}

#' Include gloss in Rmd
#'
#' @param ... Arguments passed to `print_gloss()`.
#'
#' @export
include_gloss <- function(...) {
  if (knitr::is_latex_output()) {
    typeset_gloss(..., "latex")
  } else if (knitr::is_html_output()) {
    typeset_gloss(..., "html")
  }
}
