#' Lexa PDF document
#'
#' @param ... Arguments passed to \link[rmarkdown]{pdf_document}.
#'
#' @export
lexa_pdf <- function(...) {
  rmarkdown::pdf_document(
    ...,
    latex_engine = "xelatex",
    keep_tex = TRUE,
    number_sections = TRUE,
    includes = rmarkdown::includes(
      in_header = system.file("rmarkdown/templates/lexa_pdf/resources/preamble.tex", package = "lexa")
    )
  )
}

#' Lexa HTML document
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
