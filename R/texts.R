# Search words ----

#' Search words in texts
#'
#' Search words in the texts collection.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param word A regular expression to search (entire words only will be matched).
#'
#' @return A list of `lexalx` objects.
#' @export
search_texts <- function(lexadb, word) {
  db_path <- attr(lexadb, "meta")$path
  texts <- read_texts(db_path)

  matched <- list()
  for (text in texts) {
    hits <- lapply(text$text, function(x) {
      stringr::str_detect(x$sentence, paste0("\\b", word, "\\b"))
    })
    matched <- c(matched, text$text[unlist(hits)])
  }
  cli::cli_alert_info("Found {length(matched)} matches.")

  return(matched)
}
