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
  yaml::write_yaml(lx_entry$out, file.path(path, "lexicon", "lx_000001.yaml"))
}

create_grammar <- function(path) {
  grammar <- list()
  yaml::write_yaml(grammar, file.path(path, "grammar.yaml"))
}

create_texts <- function(path) {
  dir.create(file.path(path, "texts"), FALSE, TRUE)
  text_example <- create_text()
  yaml::write_yaml(text_example$out, file.path(path, "texts", "tx_000001.yaml"))
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
# Outputs a list with entry id (`id`) and output list (`out`).

create_entry <-  function(lexadb = NULL,
                          lexeme = NULL,
                          gloss = NULL,
                          part_of_speech = NULL,
                          phon = NULL,
                          morph_category = NULL,
                          morph_type = NULL,
                          definition = gloss,
                          etymology = NULL,
                          notes = NULL,
                          homophone = NULL) {

  lx_id <- ifelse(is.null(lexadb), "lx_000001", create_lx_id(lexadb))
  today <- as.character(Sys.Date())

  # entry schema
  out <- list(
    id = lx_id,
    lexeme = lexeme,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type,
    part_of_speech = part_of_speech,
    inflectional_features = list(class = NULL),
    etymology = etymology,
    notes = notes,
    homophone = homophone,
    allomorphs = list(
      al_01 = list(
        id = "al_01",
        morph = lexeme,
        phon = phon
      )
    ),
    senses = list(
      se_01 = list(
        id = "se_01",
        gloss = gloss,
        definition = definition
      )
    ),
    date_created = today,
    date_modified = today
  )

  entry <- list(id = lx_id, out = out)
  return(entry)

}

# Check last text ID and increase hex by 1.

create_tx_id <- function(lexadb) {
  db_path <- attr(lexadb, "meta")$path
  tx_files <- list.files(file.path(db_path, "texts"), pattern = "*.yaml")
  if (length(tx_files) > 0) {
    last_id <- as.integer(
      as.hexmode(stringr::str_sub(tx_files[[length(tx_files)]], 4, 9))
    )
    new_id_n <- last_id + 1
    new_id_hex <- format(as.hexmode(new_id_n), width = 6)
    new_id <- paste0("tx_", new_id_hex)
  } else {
    new_id <- "tx_000001"
  }
  return(new_id)
}

# Prepare empty text skeleton.
# Outputs a list with text id (`id`) and output string (`out`).

create_text <- function(lexadb = NULL, title = NULL) {
  tx_id <- ifelse(is.null(lexadb), "tx_000001", create_tx_id(lexadb))

  out <- list(
    id = tx_id,
    title = title,
    sentences = list(
      st_000001 = list(
        id = "st_000001",
        sentence = NULL,
        transcription = NULL,
        morpho = NULL,
        gloss = NULL,
        phon = NULL,
        translation = NULL
      )
    )
  )

  text <- list(id = tx_id, out = out)
  return(text)
}
