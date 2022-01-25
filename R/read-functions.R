# Lexa read functions ----

read_config <- function(path) {
  yaml::read_yaml(file.path(path, "config.yaml"))
}

read_lexicon <- function(path) {
  lexicon_path <- file.path(path, "lexicon")
  lexicon_files <- list.files(lexicon_path, full.names = TRUE)
  lexicon <- lapply(lexicon_files, function(lexeme) {
      lx <- yaml::read_yaml(lexeme)
      attr(lx, "dbpath") <- path
      structure(lx, class = c("lexalx", "list"))
    }
  )
  lx_ids <- lapply(lexicon, function(x) x[["id"]])
  names(lexicon) <- lx_ids
  lexicon
}

read_grammar <- function(path) {
  yaml::read_yaml(file.path(path, "grammar.yaml"))
}

read_texts <- function(path) {
  texts_path <- file.path(path, "texts")
  texts_files <- list.files(texts_path, full.names = TRUE)
  texts <- lapply(texts_files, function(text) {
      tx <- yaml::read_yaml(text)
      tx <- structure(tx, class = c("lexatx", "list"))
      return(tx)
    }
  )
  texts <- lapply(texts, function(text) {
    sent_i <- lapply(text$sentences, function(sent) {
      st <- structure(sent, class = c("lexast", "list"))
      return(st)
    })
    text$sentences <- sent_i
    return(text)
  })
  texts_ids <- lapply(texts, function(x) x[["id"]])
  names(texts) <- texts_ids
  return(texts)
}
