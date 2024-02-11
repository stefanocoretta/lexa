# Lexa create functions ----

init_config <- function(path, name) {
  config <- list(
    schema = "lexadb",
    name = name
  )
  yaml::write_yaml(config, file.path(path, "config.yaml"))
}

init_lexicon <- function(path) {
  dir.create(file.path(path, "lexicon"), FALSE, TRUE)

  lx_entry <- create_entry(NULL)
  yaml::write_yaml(lx_entry$out, file.path(path, "lexicon", "lx_000001.yaml"))
}

init_grammar <- function(path) {
  grammar <- list()
  yaml::write_yaml(grammar, file.path(path, "grammar.yaml"))
}

init_collections <- function(path) {
  dir.create(file.path(path, "sentences"), FALSE, TRUE)
  cl_example <- create_collection()
  yaml::write_yaml(cl_example$out, file.path(path, "sentences", "cl_000001.yaml"))
}

# Write entry helpers ----
#
# The following are helper functions used when creating a new lexical entry.

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
  today <- as.character(Sys.time())

  # entry schema
  out <- list(
    id = lx_id,
    lexeme = lexeme,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type,
    part_of_speech = part_of_speech,
    inflectional_features = list(class = ""),
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

# Prepare empty collection skeleton.
# Outputs a list with collection id (`id`) and output string (`out`).

create_collection <- function(lexadb = NULL, title = NULL) {
  cl_id <- ifelse(is.null(lexadb), "cl_000001", create_cl_id(lexadb))

  out <- list(
    id = cl_id,
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

  collection <- list(id = cl_id, out = out)
  return(collection)
}
