# Actually writes entry on disk in lexicon/.

write_entry <- function(lexadb, lx_entry) {
  db_path <- attr(lexadb, "meta")$path
  lx_path <- file.path("lexicon", paste0(lx_entry$id, ".yaml"))
  lx_full_path <- file.path(db_path, lx_path)
  yaml::write_yaml(lx_entry$out, lx_full_path)
}

write_lexicon <- function(lexadb, lexalx) {
  purrr::walk(
    lexalx,
    function(entry) {
      write_entry(lexadb, entry)
    }
  )
}

# Actually writes text on disk in texts/.

write_collection <- function(lexadb, cl_entry) {
  db_path <- attr(lexadb, "meta")$path
  cl_path <- file.path("sentences", paste0(cl_entry$id, ".yaml"))
  cl_full_path <- file.path(db_path, cl_path)
  readr::write_file(cl_entry$out, cl_full_path)
}
