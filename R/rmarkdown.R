#' Lexa PDF document
#'
#' A custom Rmarkdown template to typeset PDF documents with extra support for interlinear glosses.
#'
#' @param ... Arguments passed to \link[rmarkdown]{pdf_document}.
#'
#' @export
lexa_pdf <- function(...) {
  rmarkdown::pdf_document(
    ...,
    includes = rmarkdown::includes(
      in_header = system.file("rmarkdown/templates/lexa_pdf/resources/preamble.tex", package = "lexa")
    )
  )
}

#' Lexa HTML document
#'
#' A custom Rmarkdown template to typeset HTML documents with extra support for interlinear glosses. # nolint
#'
#' @param ... Arguments passed to \link[rmarkdown]{html_document}.
#'
#' @export
lexa_html <- function(...) {
  rmarkdown::html_document(
    ...,
    includes = rmarkdown::includes(
      in_header = system.file("rmarkdown/templates/lexa_html/resources/head.html", package = "lexa"),
      after_body = system.file("rmarkdown/templates/lexa_html/resources/foot.html", package = "lexa")
    )
  )
}
