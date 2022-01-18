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

  texts <- read_texts(db_path)
  texts_length <- length(texts)

  mcats <- table(unlist(lapply(lexicon, function(x) x$morph_category)))
  names(mcats) <- paste0("{crayon::red('", stringr::str_to_sentence(names(mcats)), ":')}")
  mcats_length <- length(mcats)
  cats <- "{crayon::red(cli::symbol$circle_filled)} Categories {crayon::green(cli::symbol$arrow_right)} "
  for (cat_i in 1:mcats_length) {
    cats <- paste(cats, names(mcats)[cat_i], mcats[[cat_i]])
    if (cat_i < mcats_length) {
      cats <- paste(cats, crayon::green('|'))
    }
  }

  mtypes <- table(unlist(lapply(lexicon, function(x) x$morph_type)))
  names(mtypes) <- paste0("{crayon::red('", stringr::str_to_sentence(names(mtypes)), "s:')}")
  mtypes_length <- length(mtypes)
  types <- "{crayon::red(cli::symbol$circle_filled)} Morpheme types {crayon::green(cli::symbol$arrow_right)} "
  for (type_i in 1:mtypes_length) {
    types <- paste(types, names(mtypes)[type_i], mtypes[[type_i]])
    if (type_i < mtypes_length) {
      types <- paste(types, crayon::green('|'))
    }
  }

  poses <- table(unlist(lapply(lexicon, function(x) x$part_of_speech)))
  names(poses) <- paste0("{crayon::red('", stringr::str_to_sentence(names(poses)), "s:')}")
  poses_length <- length(poses)
  pos <- "{crayon::red(cli::symbol$circle_filled)} POS {crayon::green(cli::symbol$arrow_right)} "
  for (pos_i in 1:poses_length) {
    pos <- paste(pos, names(poses)[pos_i], poses[[pos_i]])
    if (pos_i < poses_length) {
      pos <- paste(pos, crayon::green('|'))
    }
  }

  cli::cli_rule("Database info")
  cli::cli_text(
    "{crayon::green(cli::symbol$circle_filled)} {crayon::blue('Name:')}
    {x$config$name}"
  )
  cli::cli_text(
    "{crayon::green(cli::symbol$info)} {crayon::blue('Entries:')}
    {lexicon_length}
    {crayon::green('|')}
    {crayon::blue('Texts:')} {texts_length}"
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
  n_senses <- length(x$senses)
  entry <- x$entry
  if (!is.null(x$phon)) {
    entry_line <- "{crayon::blue(entry)}
    [{x$phon}] {.emph {crayon::green(x$part_of_speech)}}"
  } else {
    entry_line <- "{crayon::blue(entry)}
    {.emph {crayon::green(x$part_of_speech)}}"
  }

  if (!is.null(x$inflectional_features)) {
    entry_line <- paste(entry_line, "({x$inflectional_features})")
  }

  cli::cli_rule("Entry", right = "{.emph {x$id}}")
  cli::cli_text(entry_line)

  cli::cli_h3("Senses")
  for (sense in 1:length(x$senses)) {
    if (!is.null(x$senses[[sense]]$inflectional_features)) {
      cli::cli_text("{cli::col_red(sense, '.')}
        {crayon::blue('(', x$senses[[sense]]$inflectional_features, ')', sep = '')}
        {x$senses[[sense]]$definition}")
    } else {
      cli::cli_text("{cli::col_red(sense, '.')} {x$senses[[sense]]$definition}")
    }
  }

  if (!is.null(x$etymology)) {
    cli::cli_h3("Etymology")
    cli::cli_div(theme = list(span.etym = list(
      `font-style` = "italic",
      color = "blue"
    )))
    cli::cli_text(x$etymology)
    cli::cli_end()
  }

  cli::cli_h3("Grammatical info")
  cli::cli_text("{crayon::red('Category:')} {x$morph_category}")
  cli::cli_text("{crayon::red('Type:')} {x$morph_type}")

  if (!is.null(x$notes)) {
    cli::cli_h3("Notes")
    cli::cli_ul(x$notes)
  }

}


print.lexastn <- function(x, ...) {
  morph_split <- unlist(stringr::str_split(x$morph, " "))
  gloss_split <- unlist(stringr::str_split(x$gloss, " "))
  morph_n <- cli::utf8_nchar(morph_split)
  gloss_n <- cli::utf8_nchar(gloss_split)
  max_n <- pmax(morph_n, gloss_n) + 2
  morph_pad <- stringr::str_pad(morph_split, max_n, "right")
  gloss_pad <- stringr::str_pad(gloss_split, max_n, "right")
  # morph <- stringr::str_flatten(morph_pad)
  # gloss <- stringr::str_flatten(gloss_pad)

  cli::cli_h1(cli::col_blue(x$sentence))
  cli::cli_text("[", x$phon, "]")
  cli::cli_text("")
  cat(cli::col_green(morph_pad), "\n")
  cat(gloss_pad, "\n")
  cli::cli_text("")
  cli::cli_text("‘", x$translation, "’")
}
