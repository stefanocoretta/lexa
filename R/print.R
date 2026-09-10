# Print methods ----

#' Print method for Lexa databases
#'
#' Print method for objects of class `lexdb`, which prints database info and
#' statistics.
#'
#' @param x An object of class `lexadb`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
print.lexadb <- function(x, ...) {
  metadata <- x$config$metadata
  lexicon <- x$lexicon
  lexicon_length <- length(lexicon)

  mtypes <- table(unlist(lapply(lexicon, function(x) x$morph_type)))
  mtypes_length <- length(mtypes)

  if (mtypes_length > 0) {
    names(mtypes) <- paste0("{crayon::red('", stringr::str_to_sentence(names(mtypes)), ":')}")
    types <- "{crayon::red(cli::symbol$circle_filled)} Morphological types {crayon::green(cli::symbol$arrow_right)} "
    for (type_i in 1:mtypes_length) {
      types <- paste(types, names(mtypes)[type_i], mtypes[[type_i]])
      if (type_i < mtypes_length) {
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
    {metadata$name}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Author:')}
    {metadata$author}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {lexicon_length}"
  )
  cli::cli_h2("Lexicon breakdown")
  cli::cli_text(types)
  cli::cli_text(classes)
}

#' Print method for lexical entries
#'
#' Print method for objects of class `lexalx`, which prints the entry's info.
#'
#' @param x An object of class `lexalx`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexalx <- function(x, ...) {
  n_senses <- length(x$senses)
  headword <- x$headword

  headword_part <- ""
  if (is.character(headword)) {
    headword_part <- "{crayon::blue(headword)}"
  } else {
    for (script in headword) {
      text <- if (is.list(script) && !is.null(script$text)) script$text else script
      headword_part <- paste0(headword_part, "{crayon::blue('", text, "')} ")
    }
  }

  phonemic <- if (!is.null(x$phonemic)) x$phonemic[[1]] else NULL
  phonetic <- if (!is.null(x$phonetic)) x$phonetic[[1]] else NULL

  pronunciation <- paste0(
    if (!is.null(phonemic)) paste0("/", phonemic, "/") else "",
    if (!is.null(phonetic)) paste0(" [", phonetic, "]") else ""
  )

  headword_line <- paste(
    headword_part,
    pronunciation,
    "{.emph {crayon::green(x$word_class)}}"
  )

  if (!is.null(x$grammatical_features)) {
    headword_line <- paste(headword_line, "({glue::glue_collapse(x$grammatical_features, sep = ', ')})")
  }

  cli::cli_h1("Entry {x$id}")
  cli::cli_text(headword_line)

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
      headword_line <- "{crayon::blue(i$headword)} {.emph {crayon::green(i$word_class)}} {i$senses$se_01$definition} [{crayon::silver(i$id)}]"
      cli::cli_bullets(c("*" = headword_line))
    }
  )
}

#' Print method for sentences
#'
#' Print method for objects of class `lexast`, which prints collection sentences.
#'
#' @param x An object of class `lexast`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexast <- function(x, ...) {
  cli::cli_h1(cli::col_blue(x$sentence))
  if (!is.null(x$transcription)) {
    cli::cli_text(cli::col_blue(x$transcription))
  }
  if (!is.null(x$transliteration)) {
    cli::cli_text(cli::col_blue(x$transliteration))
  }
  cli::cli_text("[", x$phonetic, "]")
  cli::cli_text("")

  morph_split <- unlist(stringr::str_split(stringr::str_squish(x$morph), " "))
  gloss_split <- unlist(stringr::str_split(stringr::str_squish(x$gloss), " "))
  morph_n <- cli::utf8_nchar(morph_split)
  gloss_n <- cli::utf8_nchar(gloss_split)
  max_n <- pmax(morph_n, gloss_n) + 2
  morph_pad <- stringr::str_pad(morph_split, max_n, "right")
  gloss_pad <- stringr::str_pad(gloss_split, max_n, "right")

  if (sum(max_n) > 80) {
    n <- 0
    i <- 1
    y <- 1
    morph <- vector()
    gloss <- vector()

    while (i <= length(max_n)) {
      if (sum(max_n[y:i]) < 81) {
        morph <- c(morph, morph_pad[i])
        gloss <- c(gloss, gloss_pad[i])
        if (i == length(max_n)) {
          cat(cli::col_green(morph), "\n")
          cat(gloss, "\n")
          cat("\n")
        }
        i <- i + 1
      } else {
        cat(cli::col_green(morph), "\n")
        cat(gloss, "\n")
        cat("\n")
        morph <- vector()
        gloss <- vector()
        y <- i
      }
    }
  } else {
    cat(cli::col_green(morph_pad), "\n")
    cat(gloss_pad, "\n")
  }

  cli::cli_text("")
  cli::cli_text("\u2018", x$translation, "\u2019")
}

orange <- crayon::make_style("orange")

