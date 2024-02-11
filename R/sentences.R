# Add collection ----

#' Add text to database
#'
#' This function creates a new text in the database, i.e. a new empty text
#' skeleton is written to disk, in the `texts/` directory, for the user
#' to edit at will.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}})
#' @param title The text title as a string.
#'
#' @return Nothing. Used for its side effects
#' @export
add_collection <- function(lexadb,
                      title = NULL) {

  cl_entry <- construct_collection(
    lexadb,
    title = title
  )

  write_collection(lexadb, cl_entry)
  cli::cli_alert_success("Text {cli::col_blue(cl_entry$id)} added to the texts!")
}




# Search collections ----

#' Search words in sentence collections
#'
#' Search words in the sentence collections.
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
#' search_collections(albanian, "rrezet")
#' search_collections(albanian, gloss = "sun")
#' search_collections(albanian, gloss = "traveller")
search_collections <- function(lexadb, word = NULL, whole = TRUE, gloss = NULL) {
  db_path <- attr(lexadb, "meta")$path
  search_collections <- read_collections(db_path)

  matched <- list()
  n_matches <- 0
  collection_ids <- c()
  for (collection in search_collections) {
    hits <- lapply(collection$sentences, function(x) {
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
          search in the collections.")
      }
    })
    collection$sentences <- collection$sentences[unlist(hits)]
    n_matches <- n_matches + length(collection$sentences)
    collection_ids <- c(collection_ids, collection$id)

    matched <- c(matched, list(collection))
  }
  cli::cli_alert_info("Found {n_matches} matches.")

  names(matched) <- collection_ids
  return(matched)
}



# Show collection ----

#' Show collection or sentence with given id
#'
#' It shows the collection or sentence with the given id.
#'
#' @param lexadb   A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param coll_id  A string with the collection id (the `cl_` prefix and leading
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
#' show_collection(albanian, 1)
#' show_collection(albanian, 1, 3)
show_collection <- function(lexadb, coll_id, sent_id = NULL) {
  db_path <- attr(lexadb, "meta")$path

  if (!stringr::str_detect(coll_id, "cl")) {
    coll_id <- stringr::str_pad(coll_id, 6, "left", "0")
    coll_id <- paste0("cl_", coll_id)
  }

  cl_path <- file.path(
    normalizePath(db_path), "sentences",
    paste0(coll_id, ".yaml")
  )

  if (file.exists(cl_path)) {
    cl <- yaml::read_yaml(cl_path)
  } else {
    cli::cli_abort("Sorry, there is no text with the given id!")
  }

  if (is.null(sent_id)) {
    attr(cl, "dbpath") <- db_path
    class(cl) <- c("lexacl", "list")

    return(cl)
  } else {
    if (!stringr::str_detect(sent_id, "st")) {
      sent_id <- stringr::str_pad(sent_id, 6, "left", "0")
      sent_id <- paste0("st_", sent_id)
    }

    st <- cl$sentences[[sent_id]]
    attr(st, "dbpath") <- db_path
    class(st) <- c("lexast", "list")

    return(st)
  }

}
