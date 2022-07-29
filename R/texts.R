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
#'
#' @examples
#' db_path <- system.file("extdata/albanian_lexadb", package = "lexa")
#' albanian <- load_lexadb(db_path)
#'
#' search_texts(albanian, "rrezet")
#' search_texts(albanian, gloss = "sun")
#' search_texts(albanian, gloss = "traveller")
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



# Show text ----

#' Show text or sentence with given id
#'
#' It shows the text or sentence with the given id.
#'
#' @param lexadb   A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param text_id  A string with the text id (the `tx_` prefix and leading
#'        zeros can be omitted.)
#' @param sent_id A string with the sentence id (the `st_` prefix and leading
#'        zeros can be omitted.)
#'
#' @return A `lexast` object.
#' @export
#'
#' @examples
#' db_path <- system.file("extdata/albanian_lexadb", package = "lexa")
#' albanian <- load_lexadb(db_path)
#'
#' show_text(albanian, 1)
#' show_text(albanian, 1, 3)
show_text <- function(lexadb, text_id, sent_id = NULL) {
  db_path <- attr(lexadb, "meta")$path

  if (!stringr::str_detect(text_id, "tx")) {
    text_id <- stringr::str_pad(text_id, 6, "left", "0")
    text_id <- paste0("tx_", text_id)
  }

  tx_path <- file.path(
    normalizePath(db_path), "texts",
    paste0(text_id, ".yaml")
  )

  if (file.exists(tx_path)) {
    tx <- yaml::read_yaml(tx_path)
  } else {
    cli::cli_abort("Sorry, there is no text with the given id!")
  }

  if (is.null(sent_id)) {
    attr(tx, "dbpath") <- db_path
    class(tx) <- c("lexatx", "list")

    return(tx)
  } else {
    if (!stringr::str_detect(sent_id, "st")) {
      sent_id <- stringr::str_pad(sent_id, 6, "left", "0")
      sent_id <- paste0("st_", sent_id)
    }

    st <- tx$sentences[[sent_id]]
    attr(st, "dbpath") <- db_path
    class(st) <- c("lexast", "list")

    return(st)
  }

}
