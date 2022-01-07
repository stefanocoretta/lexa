add_lexeme <- function(lexadb, lexeme, lemma = lexeme, phon = NULL, morph_category = NULL, morph_type = NULL, part_of_speech = NULL) {
  list(
    id = create_lx_id(lexadb$lexicon),
    lexeme = lexeme,
    lemma = lemma,
    phon = phon,
    morph_category = morph_category,
    morph_type = morph_type
  )
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
