# Print methods ----

#' Print method for Lexa databases
#'
#' Print method for objects of class `lexacon`, which prints database info and
#' statistics.
#'
#' @param x An object of class `lexacon`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
print.lexacon <- function(x, ...) {
  lexadb <- read_lexadb(x)

  lexicon <- lexadb$lexicon
  lexicon_length <- length(lexicon)

  wtypes <- table(unlist(lapply(lexicon, function(x) x$word_type)))
  wtypes_length <- length(wtypes)

  if (wtypes_length > 0) {
    names(wtypes) <- paste0("{crayon::red('", stringr::str_to_sentence(names(wtypes)), ":')}")
    types <- "{crayon::red(cli::symbol$circle_filled)} Word types {crayon::green(cli::symbol$arrow_right)} "
    for (type_i in 1:wtypes_length) {
      types <- paste(types, names(wtypes)[type_i], wtypes[[type_i]])
      if (type_i < wtypes_length) {
        types <- paste(types, crayon::green('|'))
      }
    }
  } else {
    types <- "{crayon::red(cli::symbol$circle_filled)} Types {crayon::green(cli::symbol$arrow_right)} "
  }

  wclass <- table(unlist(lapply(lexicon, function(x) x$word_class)))
  wclass_length <- length(wclass)

  if (wclass_length > 0) {
    names(wclass) <- paste0("{crayon::red('", stringr::str_to_sentence(names(wclass)), ":')}")
    classes <- "{crayon::red(cli::symbol$circle_filled)} Word classes {crayon::green(cli::symbol$arrow_right)} "
    for (class_i in 1:wclass_length) {
      classes <- paste(classes, names(wclass)[class_i], wclass[[class_i]])
      if (class_i < wclass_length) {
        classes <- paste(classes, crayon::green('|'))
      }
    }
  } else {
    classes <- "{crayon::red(cli::symbol$circle_filled)} Classes {crayon::green(cli::symbol$arrow_right)} "
  }

  cli::cli_h1("Database info")
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Name:')}
    {x$metadata$name}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Author:')}
    {x$metadata$author}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {lexicon_length}"
  )
  cli::cli_h2("Lexicon breakdown")
  cli::cli_text(types)
  cli::cli_text(classes)
}

#' Print method for lexemes
#'
#' Print method for objects of class `lexalx`, which prints lexeme info.
#'
#' @param x An object of class `lexalx`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexalx <- function(x, ...) {
  n_senses <- length(x$senses)
  lexeme <- x$lexeme

  lexeme_part <- ""
  if (is.character(lexeme)) {
    lexeme_part <- "{crayon::blue(lexeme)}"
  } else {
    for (script in lexeme) {
      text <- if (is.list(script) && !is.null(script$text)) script$text else script
      lexeme_part <- paste0(lexeme_part, "{crayon::blue('", text, "')} ")
    }
  }

  phonemic <- if (!is.null(x$phonemic)) x$phonemic[[1]] else NULL
  phonetic <- if (!is.null(x$phonetic)) x$phonetic[[1]] else NULL

  pronunciation <- paste0(
    if (!is.null(phonemic)) paste0("/", phonemic, "/") else "",
    if (!is.null(phonetic)) paste0(" [", phonetic, "]") else ""
  )

  lexeme_line <- paste(
    lexeme_part,
    pronunciation,
    "{.emph {crayon::green(x$word_class)}}"
  )

  if (!is.null(x$grammatical_features)) {
    lexeme_line <- paste(lexeme_line, "({glue::glue_collapse(x$grammatical_features, sep = ', ')})")
  }

  cli::cli_h1("Entry {x$id}")
  cli::cli_text(lexeme_line)

  cli::cli_h2("Senses")
  for (sense in 1:length(x$senses)) {
    definitions <- x$senses[[sense]]$definition

    for (i in seq_along(definitions)) {
      lang <- names(definitions)[i]
      definition <- definitions[[i]]

      text <- if (is.list(definition) && !is.null(definition$text)) {
        definition$text
      } else {
        definition
      }

      if (i == 1) {
        cli::cli_text(
          "{cli::col_red(sense, '.')} {orange(lang)} {text}"
        )
      } else {
        d <- cli::cli_div(
          class = "definition",
          theme = list(.definition = list(`margin-left` = 3))
        )
        cli::cli_text(
          "{orange(lang)} {text}"
        )
        cli::cli_end(d)
      }
    }

    if (!is.null(x$senses[[sense]]$examples)) {
      d <- cli::cli_div(
        class = "example",
        theme = list(.example = list(`margin-left` = 10))
      )
      cli::cli_h3("Examples")
      for (example in x$senses[[sense]]$examples) {
        sentence_part <- ""
        if (is.character(example)) {
          sentence_part <- "{crayon::blue(example)}"
        } else {
          for (i in seq_along(example$sentence)) {
            script <- example$sentence[[i]]
            text <- if (is.list(script) && !is.null(script$text)) script$text else script
            if (i < length(example$sentence)) {
              sentence_part <- paste0(sentence_part, " {crayon::blue('", text, "')} {cli::symbol$en_dash} ")
            } else {
              sentence_part <- paste0(sentence_part, " {crayon::blue('", text, "')} ")
            }
          }
        }
        cli::cli_text(
          paste0("{cli::symbol$bullet}", sentence_part, "{example$translation}")
        )
      }
      cli::cli_end(d)
    }
  }

  if (!is.null(x$etymology)) {
    cli::cli_h2("Etymology")
    cli::cli_text(x$etymology)
  }

  if (!is.null(x$notes)) {
    cli::cli_h2("Notes")
    cli::cli_ul(x$notes)
  }

}

#' Print method for list of entries
#'
#' Print method for the output of `search_lexicon()`, which returns an object
#'    of class `lexalxs` when `show_entry` is `TRUE`.
#'
#' @param x An object of class `lexalxs`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexalxs <- function(x, ...) {
  purrr::walk(x, function(i) print.lexalx(i))
}

#' Compact print method for list of entries
#'
#' Compact print method for the output of `search_lexicon()`, which returns an object
#'    of class `lexalxscompact` when `show_entry` is `FALSE`..
#'
#' @param x An object of class `lexalxscompact`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexalxscompact <- function(x, ...) {
  purrr::walk(
    x,
    function(i) {
      lexeme_line <- "{crayon::blue(i$lexeme)} {.emph {crayon::green(i$word_class)}} {i$senses$se_01$definition} [{crayon::silver(i$id)}]"
      cli::cli_bullets(c("*" = lexeme_line))
    }
  )
}

orange <- crayon::make_style("orange")
