
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lexa: Manage documentary and descriptive linguistic data <a href="https://stefanocoretta.github.io/lexa/"><img src="man/figures/logo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.2.9000-orange.svg)](https://github.com/stefanocoretta/lexa)
<!-- badges: end -->

The goal of lexa is to provide a framework and tools to manage
linguistic fieldwork data.

The package is still in its infancy and only a very limited set of
features are being developed for the time being. The package contains
highly unstable code, so expect breaking changes at any point (although
I will try to keep these at a minimum).

The current available features are:

-   Create a Lexa database.
-   Add lexical entries to the database.
-   Search lexical entries by word or definition.
-   Import lexical entries from a `.csv` file.

## Installation

You can install the latest version of lexa like so:

``` r
remotes::install_github(
  "stefanocoretta/lexa@v0.0.2",
  build_vignettes = TRUE
)
```

## Quick start

To create a new database:

``` r
library(lexa)

create_lexadb(
  parent = "./",
  name = "new"
)
```

This will create a directory `new_lexadb/` in the parent directory
(`./`). See `vignette("database-schema", package = "lexa")` for details.

The `lexicon/` folder is populated with a file with an entry skeleton
you can manually edit.

To create new entries you first need to load the database:

``` r
new_db <- load_lexadb("./new_lexadb")

new_db
```

Now you can add a new entry with:

``` r
add_entry(new_db)
```

This will create a new file with the entry skeleton, which you can edit.
The new `id` is automatically created for you based on the existing
files.

To search your lexicon:

``` r
db_path <- system.file("extdata/eleryon_lexadb", package = "lexa")
eleryon <- load_lexadb(db_path)
#> ℹ Loading lexa database...
eleryon
#> 
#> ── Database info ───────────────────────────────────────────────────────────────
#> ◉ Name: eleryon
#> ℹ Entries: 6 | Texts: 1
#> 
#> ── Lexicon breakdown ──
#> 
#> ◉ Categories → Lexical: 6
#> ◉ Morpheme types → Roots: 6
#> ◉ POS → Adverbs: 1 | Nouns: 1 | Verbs: 4

search_lexicon(eleryon, entry = "unullose")
#> ✔ Found 1 entry.
#> 
#> ── Entry lx_000002 ─────────────────────────────────────────────────────────────
#> unullose [unullose] verb (IV)
#> 
#> ── Senses ──
#> 
#> 1. to love
#> 
#> ── Grammatical info ──
#> 
#> Category: lexical
#> Type: root
#> 
#> ── Allomorphs ──
#> 
#> • nul [nul]
#> • unul [unul]
search_lexicon(eleryon, definition = "tomorrow")
#> ✔ Found 1 entry.
#> 
#> ── Entry lx_000005 ─────────────────────────────────────────────────────────────
#> chǭs [tʃɵːs] adverb
#> 
#> ── Senses ──
#> 
#> 1. tomorrow
#> 
#> ── Grammatical info ──
#> 
#> Category: lexical
#> Type: root
#> 
#> ── Allomorphs ──
#> 
#> • chǭs [tʃɵːs]
#> 
#> ── Notes ──
#> 
#> • Note that in Eleryon this word means 'tomorrow' if used by noon, otherwise it
#> means 'the day after tomorrow'.
```

You can also display texts, sentences and lexical entries!

``` r
show_text(eleryon, 1)
#> 
#> ── Example sentences ───────────────────────────────────────────────────────────
#> 
#> ── st_000001 ──
#> 
#> Ęs ętsu urųrtō enēim kę̄syoh bhųl enēim āireᵃph likhpyūaq.
#> And then I sat on a rock, while it was raining over me.
#> 
#> ── st_000002 ──
#> 
#> Ksǫnteziṇ gartosesī ōroi Vāisi su Meukha su vēsyēl selo ellāimōma enēim
#> āireᵃph.
#> With pleasure I tell you about the Sun, the Moon and the stars that are above
#> us.

show_entry(eleryon, 6)
#> 
#> ── Entry lx_000006 ─────────────────────────────────────────────────────────────
#> urųrtose [uryrtose] verb (I)
#> 
#> ── Senses ──
#> 
#> 1. to sit
#> 
#>           ── Examples
#>           Ęs ętsu urųrtō enēim kę̄syoh bhųl enēim āireᵃph likhpyūaq.
#>           [tx_000001:st_000001]
#>           And then I sat on a rock, while it was raining over me.
#> 
#>           Ksǫnteziṇ gartosesī ōroi Vāisi su Meukha su vēsyēl selo ellāimōma
#>           enēim āireᵃph. [tx_000001:st_000002]
#>           With pleasure I tell you about the Sun, the Moon and the stars that
#>           are above us.
#> 
#> 
#> ── Grammatical info ──
#> 
#> Category: lexical
#> Type: root
#> 
#> ── Allomorphs ──
#> 
#> • ųrt [yrt]
#> • urųrt [uryrt]
```

To include interlinear glosses in HTML and LaTeX documents, check out
the `vignette("interlinear-gloss")`.
