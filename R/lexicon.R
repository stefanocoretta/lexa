save_lexicon <- function(lexicon, path) {
  yaml::write_yaml(lexicon, path)
}
