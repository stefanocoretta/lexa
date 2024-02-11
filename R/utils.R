# Check last entry ID and increase hex by 1.

generate_lx_id <- function(lexadb) {
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

# Check last collection ID and increase hex by 1.

generate_cl_id <- function(lexadb) {
  db_path <- attr(lexadb, "meta")$path
  cl_files <- list.files(file.path(db_path, "sentences"), pattern = "*.yaml")
  if (length(cl_files) > 0) {
    last_id <- as.integer(
      as.hexmode(stringr::str_sub(cl_files[[length(cl_files)]], 4, 9))
    )
    new_id_n <- last_id + 1
    new_id_hex <- format(as.hexmode(new_id_n), width = 6)
    new_id <- paste0("cl_", new_id_hex)
  } else {
    new_id <- "cl_000001"
  }
  return(new_id)
}


validate_lexicon <- function(lexadb) {
  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)
  lx_validator <- jsonvalidate::json_validator(
    system.file("extdata/json_schemas/lx_schema.json", package = "lexa"),
    engine = "ajv"
  )
  lapply(
    lexicon,
    function(entry) {
      entry_json <- jsonlite::toJSON(entry, auto_unbox = TRUE)
      lx_validator(entry_json)
    }
  )
}
