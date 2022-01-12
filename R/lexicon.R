# Add entries ----

#' Add entry to lexicon
#'
#' This function creates a new entry in the lexicon, i.e. a new empty entry
#' skeleton is written to disk, in the `lexicon/` directory, for the user
#' to edit at will.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}})
#'
#' @return Nothing. Used for its side effects
#' @export
add_entry <- function(lexadb) {
  lx_entry <- create_entry(lexadb)
  write_entry(lexadb, lx_entry)
}

# Write entry helpers ----
#
# The following are helper functions used when creating a new lexical entry.

# Check last entry ID and increase hex by 1.
create_lx_id <- function(lexadb) {
  db_path <- attr(lexadb, "meta")$path
  lx_files <- list.files(file.path(db_path, "lexicon"), pattern = "*.yaml")
  last_id <- as.integer(
    as.hexmode(stringr::str_sub(lx_files[[length(lx_files)]], 4, 9))
  )
  new_id_n <- last_id + 1
  new_id_hex <- format(as.hexmode(new_id_n), width = 6)
  new_id <- paste0("lx_", new_id_hex)
  return(new_id)
}

# Prepare empty entry skeleton.
create_entry <- function(lexadb = NULL) {
  list(
    id = ifelse(is.null(lexadb), "lx_000001", create_lx_id(lexadb)),
    lexeme = "",
    lemma = "",
    phon = "",
    morph_category = "",
    morph_type = "",
    part_of_speech = "",
    etymology = "",
    allomorphs = list(
      al_01 = list(
        id = "al_01",
        morph = "",
        phon = ""
      )
    ),
    senses = list(
      se_01 = list(
        id = "se_01",
        gloss = "",
        definition = "",
        inflectional_features = list(
          class = ""
        )
      )
    ),
    date_created = as.character(Sys.time()),
    date_modified = as.character(Sys.time())
  )
}

# Actually writes entry on disk in lexicon/.
write_entry <- function(lexadb, lx_entry) {
  db_path <- attr(lexadb, "meta")$path
  lx_path <- file.path("lexicon", paste0(lx_entry$id, ".yaml"))
  lx_full_path <- file.path(db_path, lx_path)
  yaml::write_yaml(lx_entry, lx_full_path)
}



# Search entries ----

search_lexicon <- function(lexadb, pattern) {
  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)

  hits <- lapply(lexicon, function(x) {
      stringr::str_detect(x$lexeme, pattern)
    }
  )

  lexicon[unlist(hits)]
}
