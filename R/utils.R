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
