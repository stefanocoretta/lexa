add_entry <- function(lexadb, lexeme, lemma = lexeme, phon = NULL,
  morph_category = NULL, morph_type = NULL, part_of_speech = NULL,
  etymology = NULL, allomorphs = NULL, senses = NULL) {
  new_id <- create_lx_id(lexadb$lexicon)

  new_lx <- list(
    id = new_id,
    lexeme = lexeme,
    lemma = lemma,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type,
    part_of_speech = part_of_speech,
    etymology = etymology,
    allomorphs = allomorphs,
    senses = senses
  )

  class(new_lx) <- c("lexalx", "list")

  lexadb$lexicon[[new_id]] <- new_lx
  return(lexadb)
}

write_lexicon <- function(lexicon, path) {
  yaml::write_yaml(lexicon, path)
}

create_lx_id <- function(lexicon) {
  last_id <- as.integer(as.hexmode(stringr::str_sub(lexicon[[length(lexicon)]]$id, 4, 9)))
  new_id_n <- last_id + 1
  new_id_hex <- format(as.hexmode(new_id_n), width = 6)
  new_id <- paste0("lx_", new_id_hex)
  return(new_id)
}
