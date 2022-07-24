# Lexa create functions ----

create_config <- function(path, name) {
  config <- list(
    schema = "lexadb",
    name = name
  )
  yaml::write_yaml(config, file.path(path, "config.yaml"))
}

create_lexicon <- function(path) {
  dir.create(file.path(path, "lexicon"), FALSE, TRUE)

  lx_entry <- create_entry(NULL)
  readr::write_file(lx_entry$out, file.path(path, "lexicon", "lx_000001.yaml"))
}

create_grammar <- function(path) {
  grammar <- list()
  yaml::write_yaml(grammar, file.path(path, "grammar.yaml"))
}

create_texts <- function(path) {
  dir.create(file.path(path, "texts"), FALSE, TRUE)
  text_example <- create_text()
  readr::write_file(text_example$out, file.path(path, "texts", "tx_000001.yaml"))
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
# Outputs a list with entry id (`id`) and output string (`out`).

create_entry <-  function(lexadb = NULL,
                          entry = NULL,
                          gloss = NULL,
                          part_of_speech = NULL,
                          phon = NULL,
                          morph_category = NULL,
                          morph_type = NULL,
                          definition = gloss,
                          etymology = NULL,
                          notes = NULL) {

  lx_id <- ifelse(is.null(lexadb), "lx_000001", create_lx_id(lexadb))
  today <- as.character(Sys.time())

  out <- glue::glue(
    'id: {lx_id}
    entry: {entry}
    phon: {phon}
    morph_category: {morph_category}
    morph_type: {morph_type}
    part_of_speech: {part_of_speech}
    inflectional_features:
      class:
    etymology: {etymology}
    notes: {notes}
    allomorphs:
      al_01:
        id: al_01
        morph: {entry}
        phon: {phon}
    senses:
      se_01:
        id: se_01
        gloss: {gloss}
        definition: "{definition}"
    date_created: {today}
    date_modified: {today}

    ',
    .null = ""
  )

  entry <- list(id = lx_id, out = out)
  return(entry)

}


# Prepare empty text skeleton.
# Outputs a list with text id (`id`) and output string (`out`).

create_text <- function() {
  out <- glue::glue(
    'id: tx_000001
    title: ""
    sentences:
      st_000001:
        id: st_000001
        sentence:
        transcription:
        morpho:
        gloss:
        phon:
        translation:

    ',
    .null = ""
  )

  text <- list(id = "tx_000001", out = out)
  return(text)
}
