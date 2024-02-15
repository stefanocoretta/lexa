# Print methods ----

#' Print method for Lexa databases
#'
#' Print method for objects of class `lexadb`, which prints database info and
#' statistics.
#'
#' @param x An object of class `lexadb`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
#'
print.lexadb <- function(x, ...) {
  db_path <- attr(x, "meta")$path

  lexicon <- read_lexicon(db_path)
  lexicon_length <- length(lexicon)

  collections <- read_collections(db_path)
  collections_length <- length(collections)

  mcats <- table(unlist(lapply(lexicon, function(x) x$morph_category)))
  mcats_length <- length(mcats)

  if (mcats_length > 0) {
    names(mcats) <- paste0("{crayon::red('", stringr::str_to_sentence(names(mcats)), ":')}")
    cats <- "{crayon::red(cli::symbol$circle_filled)} Categories {crayon::green(cli::symbol$arrow_right)} "
    for (cat_i in 1:mcats_length) {
      cats <- paste(cats, names(mcats)[cat_i], mcats[[cat_i]])
      if (cat_i < mcats_length) {
        cats <- paste(cats, crayon::green('|'))
      }
    }
  } else {
    cats <- "{crayon::red(cli::symbol$circle_filled)} Categories {crayon::green(cli::symbol$arrow_right)} "
  }


  mtypes <- table(unlist(lapply(lexicon, function(x) x$morph_type)))
  mtypes_length <- length(mtypes)

  if (mtypes_length > 0) {
    names(mtypes) <- paste0("{crayon::red('", stringr::str_to_sentence(names(mtypes)), "s:')}")
    types <- "{crayon::red(cli::symbol$circle_filled)} Morpheme types {crayon::green(cli::symbol$arrow_right)} "
    for (type_i in 1:mtypes_length) {
      types <- paste(types, names(mtypes)[type_i], mtypes[[type_i]])
      if (type_i < mtypes_length) {
        types <- paste(types, crayon::green('|'))
      }
    }
  } else {
    types <- "{crayon::red(cli::symbol$circle_filled)} Morpheme types {crayon::green(cli::symbol$arrow_right)} "
  }


  poses <- table(unlist(lapply(lexicon, function(x) x$part_of_speech)))
  poses_length <- length(poses)

  if (poses_length > 0) {
    names(poses) <- paste0("{crayon::red('", stringr::str_to_sentence(names(poses)), "s:')}")
    pos <- "{crayon::red(cli::symbol$circle_filled)} POS {crayon::green(cli::symbol$arrow_right)} "
    for (pos_i in 1:poses_length) {
      pos <- paste(pos, names(poses)[pos_i], poses[[pos_i]])
      if (pos_i < poses_length) {
        pos <- paste(pos, crayon::green('|'))
      }
    }
  } else {
    pos <- "{crayon::red(cli::symbol$circle_filled)} POS {crayon::green(cli::symbol$arrow_right)} "
  }


  cli::cli_h1("Database info")
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Name:')}
    {x$config$name}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {lexicon_length}
    {crayon::green('|')}
    {crayon::blue('Collections:')} {collections_length}"
  )
  cli::cli_h2("Lexicon breakdown")
  cli::cli_text(cats)
  cli::cli_text(types)
  cli::cli_text(pos)
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
  dbpath <- attr(x, "dbpath")
  config <- read_config(dbpath)
  config[["languages"]][["meta"]] <- NULL
  the_language <- names(config[["languages"]])

  n_senses <- length(x$senses)
  lexeme <- x$lexeme
  if (length(lexeme) > 1) {
    lexeme_tr <- lexeme[["translit"]]
    lexeme_part <- "{crayon::blue(lexeme[[1]])}, {.emph {lexeme_tr}}"
  } else {
    lexeme_part <- lexeme
  }
  if (!is.null(x$phon)) {
    if (length(x$phon) == 2 ){
      phon <- x$phon$pht
    } else {
      phon <- x$phon
    }

    lexeme_line <- paste(lexeme_part,
    "[{phon}] {.emph {crayon::green(x$part_of_speech)}}")

  } else {
    lexeme_line <- paste(lexeme_part,
    "{.emph {crayon::green(x$part_of_speech)}}")
  }

  if (!is.null(x$inflectional_features)) {
    lexeme_line <- paste(lexeme_line, "({x$inflectional_features})")
  }

  cli::cli_h1("Entry {x$id}")
  cli::cli_text(lexeme_line)

  cli::cli_h2("Senses")
  for (sense in 1:length(x$senses)) {
    definitions <- x$senses[[sense]]$definition
    if (length(definitions) > 1) {
      definitions_langs <- names(definitions)
      definitions[[the_language]] <- NULL
      definition_part <- paste(definitions, collapse = ". ")
    } else {
      definition_part <- definitions[[1]]
    }
    if (!is.null(x$senses[[sense]]$inflectional_features)) {
      cli::cli_text("{cli::col_red(sense, '.')}
        {crayon::blue('(', x$senses[[sense]]$inflectional_features, ')', sep = '')}
        {definition_part}")
    } else {
      cli::cli_text("{cli::col_red(sense, '.')} {definition_part}")
    }
    examples_id <- x$senses[[sense]]$examples
    if (!is.null(examples_id)) {
      if (length(examples_id) == 1) {
        cl_st <- stringr::str_split(examples_id, ":")
        collection <- yaml::read_yaml(
          file.path(attr(x, "dbpath"), "sentences", paste0(cl_st[[1]][1], ".yaml"))
        )
        sentence <- collection$sentences[[cl_st[[1]][2]]]

        d <- cli::cli_div(
          class = "example",
          theme = list(.example = list(`margin-left` = 10))
        )
        cli::cli_h3("Examples")
        cli::cli_text(
          "{cli::col_blue(sentence$sentence)}
          [{examples_id}]
          "
        )
        cli::cli_text(sentence$translation)
        cli::cli_end(d)
      } else {
        d <- cli::cli_div(
          class = "example",
          theme = list(.example = list(`margin-left` = 10))
        )
        cli::cli_h3("Examples")
        for (ex in seq_len(length(examples_id))) {
          cl_st <- stringr::str_split(examples_id[[ex]], ":")
          collection <- yaml::read_yaml(
            file.path(attr(x, "dbpath"), "sentences", paste0(cl_st[[1]][1], ".yaml"))
          )
          sentence <- collection$sentences[[cl_st[[1]][2]]]
          cli::cli_text(
            "{cli::col_blue(sentence$sentence)}
            [{examples_id[[ex]]}]
            "
          )
          cli::cli_text(sentence$translation)
          cli::cli_text("")
        }
        cli::cli_end(d)
      }
    }

  }

  if (!is.null(x$etymology)) {
    cli::cli_h2("Etymology")
    cli::cli_div(theme = list(span.etym = list(
      `font-style` = "italic",
      color = "blue"
    )))
    cli::cli_text(x$etymology)
    cli::cli_end()
  }

  cli::cli_h2("Grammatical info")
  cli::cli_text("{crayon::red('Category:')} {x$morph_category}")
  cli::cli_text("{crayon::red('Type:')} {x$morph_type}")
  cli::cli_h2("Allomorphs")
  for (allo in seq_len(length(x$allomorphs))) {
    conditioning <- x$allomorphs[[allo]]$condition
    morph <- x$allomorphs[[allo]]$morph
    if (length(morph) > 1) {
      morph_tr <- morph[["translit"]]
      morph_part <- "{crayon::blue(morph[[1]])}, {.emph {morph_tr}}"
    } else {
      morph_part <- morph
    }
    morph_line <- paste(
      "{cli::col_red(cli::symbol$bullet)}",
      morph_part,
      "[{x$allomorphs[[allo]]$phon}]
      {ifelse(!is.null(conditioning), paste0('(',
      cli::col_green(conditioning$type), ': ',
      conditioning$context, ')'), '')}"
    )
    cli::cli_text(morph_line)

  }

  if (!is.null(x$notes)) {
    cli::cli_h2("Notes")
    cli::cli_ul(x$notes)
  }

}

#' Print method for list of entries
#'
#' Print method for the output of `search_lexicon()`, which returns an object
#'    of class `lexalxs`.
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
      lexeme_line <- "{crayon::blue(i$lexeme)} {.emph {crayon::green(i$part_of_speech)}} {i$senses$se_01$definition} [{crayon::silver(i$id)}]"
      cli::cli_bullets(c("*" = lexeme_line))
    }
  )
}

#' Print method for collections
#'
#' Print method for objects of class `lexacl`, which prints collection info.
#'
#' @param x An object of class `lexacl`.
#' @param ... Arguments passed to print.
#'
#' @return Nothing. Used for its side effects.
#' @export
print.lexacl <- function(x, ...) {
  cli::cli_h1(cli::col_blue(x$title))
  for (sentence in seq_len(length(x$sentences))) {
    cli::cli_h2(x$sentences[[sentence]]$id)
    if (!is.null(x$sentences[[sentence]]$transcription)) {
      cli::cli_text(cli::col_green(x$sentences[[sentence]]$transcription))
    }
    if (!is.null(x$sentences[[sentence]]$transliteration)) {
      cli::cli_text(cli::col_green(x$sentences[[sentence]]$transliteration))
    }
    cli::cli_text(cli::col_green(x$sentences[[sentence]]$sentence))
    cli::cli_text(x$sentences[[sentence]]$translation)
  }
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
  cli::cli_text("[", x$phon, "]")
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
