# Main function ----

#' Open connection to Lexa database
#'
#' @param path Path to Lexa database file.
#'
#' @return A lexadb connection (`lexacon` object).
#' @export
load_lexadb <- function(path) {
  lexadb <- yaml::read_yaml(path)

  validation <- validate_lexadb(lexadb)

  if (!validation) {
    cli::cli_alert_danger("The lexadb does not match the expected schema.")
    validation_tbl <- tibble::as_tibble(attr(validation, "errors")[,c("instancePath", "message")])
    validation_tbl <- dplyr::rename(validation_tbl, path = instancePath, problem = message)
    print(validation_tbl)
    return(validation_tbl)
  }

  lexadb_con <- list(
    metadata = lexadb$metadata,
    dbpath = normalizePath(path)
  )
  class(lexadb_con) <- c("lexacon", "list")

  return(lexadb_con)
}

#' Create a new Lexa database
#'
#' @param name Name of the Lexa database (the `.yaml` extension will be appended to the name automatically).
#' @param parent Parent directory (default is current working directory).
#'
#' @return A lexadb connection.
#' @export
#'
#' @examples
#' \dontrun{
#' create_lexadb(name = "my_db")
#' }
create_lexadb <- function(name, parent = ".", author = NULL) {
  file <- paste0(name, ".yaml")
  path <- file.path(parent, file)

  if (file.exists(normalizePath(path, mustWork = FALSE))) {
    cli::cli_abort(c("x" = "LexaDB '{file}' already exists!"))
  }

  lexadb <- new_lexadb(name, author, schema_version = "0.0.0.9001")

  dir.create(parent, FALSE, TRUE)
  write_lexadb(lexadb, path)

  lexadb_con <- list(
    metadata = lexadb$metadata,
    dbpath = normalizePath(path)
  )
  class(lexadb_con) <- c("lexacon", "list")

  return(lexadb_con)
}

#' Add entry to lexicon
#'
#' This function creates a new entry in the lexicon, i.e. a new empty entry
#' skeleton is written to disk, in the `lexicon/` directory, for the user
#' to edit at will.
#'
#' @param lexacon A `lexacon` object (created with \code{\link{load_lexadb}}).
#' @param lexeme The entry as a string.
#' @param gloss The gloss as a string.
#' @param word_type The type of lexical entry (root, stem, affix, clitic, particle, compound, phrase).
#' @param word_class The word class of the lexical entry.
#' @param definition The definition of the entry as a string.
#' @param homophone The homophone numeric index.
#'
#' @return Nothing. Used for its side effects
#' @export
add_entry <- function(lexacon,
                      lexeme,
                      gloss,
                      word_type = NULL,
                      word_class = NULL,
                      definition = gloss,
                      homophone = NULL) {

  if (!("lexacon" %in% class(lexacon))) {
    cli::cli_abort(c("x" = "'{lexacon}' is not a lexadb connection!"))
  }

  if (is.null(word_type)) {
    word_type = "stem"
  }
  if (is.null(word_class)) {
    word_class = ""
  }

  db_path <- lexacon$dbpath
  lexadb <- read_lexadb(lexacon)
  entries <- lapply(
    lexadb$lexicon,
    function(entry) entry$lexeme
  )

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

  lx_id <- generate_lx_id(lexadb)

  today <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  new_lx <- list()
  # entry schema
  new_lx[[lx_id]] <- list(
    id = lx_id,
    lexeme = lexeme,
    word_type = word_type,
    word_class = word_class,
    homophone = homophone,
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

  # Drop null fields
  new_lx[[lx_id]] <- remove_null_fields(new_lx[[lx_id]])

  out <- yaml::as.yaml(new_lx)

  cat(out, file = db_path, append = TRUE, sep = "")
  cli::cli_alert_success("Entry '{lx_id}' added!")

}

# Internals ----

new_lexadb <- function(name, author, schema_version) {
  metadata <- list(
    schema = "lexadb",
    schema_version = schema_version,
    name = name,
    author = ifelse(is.null(author), Sys.info()[["user"]], author)
  )

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  lx_000001 <- list(
    id = "lx_000001",
    lexeme = "rat",
    word_type = "stem",
    word_class = "noun",
    senses = list(
      se_01 = list(
        id = "se_01",
        gloss = "rat",
        definition = "a sweet and very sociable rodent"
      )
    ),
    date_created = now,
    date_modified = now
  )

  lexadb <- list(
    metadata = metadata,
    lx_000001 = lx_000001
  )
  class(lexadb) <- c("lexadb", "list")

  return(lexadb)
}

read_lexadb <- function(lexadb_con) {
  dbpath <- lexadb_con$dbpath

  lexadb_yaml <- yaml::read_yaml(dbpath)
  lx_names <- grep("^lx_[0-9]+$", names(lexadb_yaml), value = TRUE)
  lexadb <- list(
    metadata = lexadb_yaml$metadata,
    lexicon = lexadb_yaml[lx_names]
  )
  lexadb$lexicon <- lapply(
    lexadb$lexicon,
    function(x) {
      class(x) <- "lexalx"
      x
    }
  )

  return(lexadb)
}

validate_lexadb <- function(lexacon) {
  if (lexacon$metadata$schema_version == "0.0.0.9001") {
    lexadb_json <- jsonlite::toJSON(lexacon, auto_unbox = TRUE)
    validated <- jsonvalidate::json_validate(
      lexadb_json,
      system.file("extdata/json-schemas/0.0.0.9001/lexadb-schema.json", package = "lexaR"),
      verbose = TRUE,
      engine = "ajv"
    )
    return(validated)
  } else {
    cli::cli_abort(c("x" = "Validation scheme not supported: {lexacon$metadata$schema_version}."))
  }
}

generate_lx_id <- function(lexadb) {
  lexicon <- lexadb$lexicon
  idn <- as.numeric(stringr::str_sub(names(lexicon), 4, 9))

  new_id_n <- max(idn) + 1
  new_id_str <- sprintf("%06d", new_id_n)
  new_id <- paste0("lx_", new_id_str)

  return(new_id)
}

remove_null_fields <- function(x) {
  if (is.list(x)) {
    x <- x[!sapply(x, function(y) is.null(y) || (is.list(y) && length(y) == 0))]
  }
  x
}
