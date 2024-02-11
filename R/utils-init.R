# Lexa init functions ----
# These functions initialise different parts of a Lexa database. They are used
# when creating a new Lexa database with `create_lexadb()`.

init_config <- function(path, name) {
  config <- list(
    schema = "lexadb",
    name = name
  )
  yaml::write_yaml(config, file.path(path, "config.yaml"))
}

init_lexicon <- function(path) {
  dir.create(file.path(path, "lexicon"), FALSE, TRUE)

  lx_entry <- construct_entry(NULL)
  yaml::write_yaml(lx_entry$out, file.path(path, "lexicon", "lx_000001.yaml"))
}

init_grammar <- function(path) {
  grammar <- list()
  yaml::write_yaml(grammar, file.path(path, "grammar.yaml"))
}

init_collections <- function(path) {
  dir.create(file.path(path, "sentences"), FALSE, TRUE)
  cl_example <- construct_collection()
  yaml::write_yaml(cl_example$out, file.path(path, "sentences", "cl_000001.yaml"))
}

# Write entry helpers ----
#
# The following are helper functions used when creating a new lexical entry.

# Prepare empty entry skeleton.
# Outputs a list with entry id (`id`) and output list (`out`).

construct_entry <-  function(lexadb = NULL,
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

  if (!is.null(lexadb)) {
    db_path <- attr(lexadb, "meta")$path
    entries <- lapply(
      read_lexicon(db_path),
      function(entry) entry$lexeme
    )

    if (!is.null(lexeme)) {
      if (lexeme %in% entries) {
        homophones_n <- sum(entries == lexeme)
        cli::cli_alert_warning(
          cli::pluralize("{homophones_n} homophone{?s} found!")
        )
        cont <- usethis::ui_yeah(
          "Continue?",
          yes = "Yes",
          no = "No",
          shuffle = FALSE
        )

        if (!cont) {
          return(cli::cli_alert_warning("Entry not created!"))
        } else (
          homophone <- homophones_n + 1L
        )
      }
    }

    lx_id <- generate_lx_id(lexadb)
  } else {
    lx_id <- generate_lx_id()
  }

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

construct_collection <- function(lexadb = NULL, title = NULL) {
  cl_id <- ifelse(is.null(lexadb), "cl_000001", generate_cl_id(lexadb))

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
