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
  if (!stringr::str_detect(text, "tx")) {
    text <- stringr::str_pad(text, 6, "left", "0")
    text <- paste0("tx_", text)
  }

  if (!stringr::str_detect(sentence, "st")) {
    sentence <- stringr::str_pad(sentence, 6, "left", "0")
    sentence <- paste0("st_", sentence)
  }

  db_path <- attr(lexadb, "meta")$path
  text <- yaml::read_yaml(file.path(db_path, "texts", paste0(text, ".yaml")))

  sentence_i <- text$sentences[[sentence]]$sentence
  transcr <- text$sentences[[sentence]]$transcription
  translit <- text$sentences[[sentence]]$transliteration
  phon <- text$sentences[[sentence]]$phon
  morpho <- text$sentences[[sentence]]$morpho
  gloss <- text$sentences[[sentence]]$gloss
  translation <- text$sentences[[sentence]]$translation

  if (format == "latex") {
    gloss <- texify_gloss(gloss)

    gloss_ex <- glue::glue(
      "```{{=latex}}
      \\ex \\begingl
      \\glpreamble {sentence_i}//
      ",
      if (!is.null(transcr)) {"\\glpreamble {transcr}//
      "} else {""},
      if (!is.null(translit)) {"\\glpreamble {translit}//
      "} else {""},
      if (!is.null(phon)) {"\\glpreamble [{phon}]//
      "} else {""},
      "\\gla {morpho}//
      \\glb {gloss}//
      \\glft `{translation}'//
      \\endgl \\xe
      ```"
    )

    return(gloss_ex)

  } else if (format == "html") {
    gloss_div <- htmltools::div(
      "data-gloss" = NA,
      htmltools::p(class = "gloss__line--original", glue::glue("{sentence_i}")),
      if (!is.null(transcr)) htmltools::p(class = "gloss__line--original", glue::glue("{transcr}")),
      if (!is.null(translit)) htmltools::p(class = "gloss__line--original", glue::glue("{translit}")),
      if (!is.null(phon)) htmltools::p(class = "gloss__line--original", style = "font-weight: 400;", glue::glue("[{phon}]")),
      htmltools::p(glue::glue("{morpho}")),
      htmltools::p(glue::glue("{gloss}")),
      htmltools::p(glue::glue("\u2018{translation}\u2019")),
    )

    return(gloss_div)
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



# Glossing helper functions ----

texify_gloss <- function(gloss) {
  gloss <- stringr::str_replace_all(gloss, "([A-Z]+)", "\\\\\\1\\{\\}")
  gloss <- stringr::str_replace_all(gloss, "([A-Z]+)", stringr::str_to_sentence)
  gloss <- stringr::str_replace_all(gloss, "_", "\\\\textunderscore\\{\\}")
  return(gloss)
}
