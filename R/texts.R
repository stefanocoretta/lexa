# Search words ----

#' Search words in texts
#'
#' Search words in the texts collection.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param word A regular expression to search in sentences (entire words only will be matched).
#' @param gloss A regular expression to search in glosses.
#'
#' @return A list of `lexalx` objects.
#' @export
search_texts <- function(lexadb, word = NULL, gloss = NULL) {
  db_path <- attr(lexadb, "meta")$path
  texts <- read_texts(db_path)

  matched <- list()
  n_matches <- 0
  text_ids <- c()
  for (text in texts) {
    hits <- lapply(text$text, function(x) {
      if (!is.null(word)) {
        stringr::str_detect(x$sentence, paste0("\\b", word, "\\b"))
      } else if (!is.null(gloss)) {
        # "\\b" conveniently matches morpheme separators '., -, ='
        stringr::str_detect(x$gloss, paste0("\\b", gloss, "\\b"))
      } else {
        cli::cli_abort("Please provide either a word or a gloss to
          search in the texts.")
      }
    })
    text$text <- text$text[unlist(hits)]
    n_matches <- n_matches + length(text$text)
    text_ids <- c(text_ids, text$id)

    matched <- c(matched, list(text))
  }
  cli::cli_alert_info("Found {n_matches} matches.")

  names(matched) <- text_ids
  return(matched)
}
