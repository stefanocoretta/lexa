
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lexa: Manage documentary and descriptive linguistic data

<!-- badges: start -->

[![](https://img.shields.io/badge/devel%20version-0.0.1-orange.svg)](https://github.com/stefanocoretta/lexa)
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
  "stefanocoretta/lexa@v0.0.1",
  build_vignettes = TRUE
)
```

## Quick start

To create a new database:

``` r
library(lexa)

create_lexadb(
  parent = "./",
  name = "new-db"
)
```

This will create a directory `new-db_lexadb/` in the parent directory
(`./`). See `vignette("database-schema", package = "lexa")` for details.

The `lexicon/` folder is populated with a file with an entry scheleton
you can manually edit.

To create new entries you first need to load the database:

``` r
new_db <- load_lexadb("./new-db_lexadb")

new_db
```

Now you can add a new entry with:

``` r
add_entry(new_db)
```

This will create a new file with the entry scheleton which you can edit.
The new `id` is automatically created for you based on the existing
files.

To search your lexicon:

``` r
db_path <- system.file("extdata/eleryon_lexadb", package = "lexa")
eleryon <- load_lexadb(db_path)
eleryon

search_lexicon(eleryon, entry = "unullose")
search_lexicon(eleryon, definition = "tomorrow")
```
