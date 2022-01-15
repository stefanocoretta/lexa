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
  if (length(lx_files) > 0) {
    last_id <- as.integer(
      as.hexmode(stringr::str_sub(lx_files[[length(lx_files)]], 4, 9))
    )
    new_id_n <- last_id + 1
    new_id_hex <- format(as.hexmode(new_id_n), width = 6)
    new_id <- paste0("lx_", new_id_hex)
  } else {
    new_id <- "lx_000001"
  }
  return(new_id)
}

# Prepare empty entry skeleton.
create_entry <- function(lexadb = NULL) {
  list(
    id = ifelse(is.null(lexadb), "lx_000001", create_lx_id(lexadb)),
    entry = "",
    phon = "",
    morph_category = "",
    morph_type = "",
    part_of_speech = "",
    inflectional_features = list(class = ""),
    etymology = "",
    notes = "",
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
        inflectional_features = list(class = "")
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

#' Search lexicon entries
#'
#' Search entries in the lexicon, by entry form or sense definitions.
#'
#' @param lexadb A `lexadb` object (created with \code{\link{load_lexadb}}).
#' @param entry A regular expression to search across entries.
#' @param definition A regular expression to search across sense definitions.
#'
#' @return A list of `lexalx` objects.
#' @export
search_lexicon <- function(lexadb, entry = NULL, definition = NULL) {
  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)

  if (!is.null(entry)) {
    hits <- lapply(lexicon, function(x) {
      stringr::str_detect(x$entry, entry)
    })
  } else if (!is.null(definition)) {
    hits <- lapply(lexicon, function(x) {
      check_definitions(x, definition)
    })
  }

  lexicon[unlist(hits)]
}

# Helper function to search through definitions
check_definitions <- function(entry, pattern) {
  defs <- lapply(entry$senses, function(y) y$definition)
  sums <- sum(stringr::str_detect(defs, pattern))
  if (sums != 0) {
    TRUE
  } else {
    FALSE
  }
}
