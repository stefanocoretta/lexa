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

read_collections <- function(path) {
  collections_path <- file.path(path, "sentences")
  collections_files <- list.files(collections_path, full.names = TRUE)
  collections <- lapply(collections_files, function(collection) {
      cl <- yaml::read_yaml(collection)
      cl <- structure(cl, class = c("lexacl", "list"))
      return(cl)
    }
  )
  collections <- lapply(collections, function(collection) {
    sent_i <- lapply(collection$sentences, function(sent) {
      st <- structure(sent, class = c("lexast", "list"))
      return(st)
    })
    collection$sentences <- sent_i
    return(collection)
  })
  collections_ids <- lapply(collections, function(x) x[["id"]])
  names(collections) <- collections_ids
  return(collections)
}
