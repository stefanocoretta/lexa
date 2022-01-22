# Search words ----

#' Search words in texts
#'
#' Search words in the texts collection.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param word A regular expression to search among the sentences.
#' @param whole Whether to search for whole words (`TRUE` by default).
#' @param gloss A regular expression to search among the glosses.
#'
#' @return A list of `lexalx` objects.
#' @export
search_texts <- function(lexadb, word = NULL, whole = TRUE, gloss = NULL) {
  db_path <- attr(lexadb, "meta")$path
  texts <- read_texts(db_path)

  matched <- list()
  n_matches <- 0
  text_ids <- c()
  for (text in texts) {
    hits <- lapply(text$sentences, function(x) {
      if (!is.null(word)) {
        if (whole) {
          stringr::str_detect(x$sentence, paste0("\\b", word, "\\b"))
        } else {
          stringr::str_detect(x$sentence, word)
        }

      } else if (!is.null(gloss)) {
        # "\\b" conveniently matches morpheme separators '., -, ='
        stringr::str_detect(x$gloss, paste0("\\b", gloss, "\\b"))
      } else {
        cli::cli_abort("Please provide either a word or a gloss to
          search in the texts.")
      }
    })
    text$sentences <- text$sentences[unlist(hits)]
    n_matches <- n_matches + length(text$sentences)
    text_ids <- c(text_ids, text$id)

    matched <- c(matched, list(text))
  }
  cli::cli_alert_info("Found {n_matches} matches.")

  names(matched) <- text_ids
  return(matched)
}
