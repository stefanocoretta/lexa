# lexa 0.0.0.9005

## Fixed

- Error with `lexa_pdf` if using bookdown with `base_format`.

## Changed

- `typeset_gloss()` now outputs gloss abbreviations as leipzig commands if the output format is PDF.




# lexa 0.0.0.9004

## Fixed

- Error in `typeset_gloss()` when phon is not present in latex format.



# lexa 0.0.0.9003

## Fixed

- `typeset_gloss()` was not returning the latex gloss when the phonetic transcription was present.




# lexa 0.0.0.9002

## Breaking changes

* Renamed `import_lexicon()` to `import_lexicon_csv()`.

* `create_entry()` is no longer exported.

* Renamed `print_gloss()` to `typeset_gloss()`.

## Added

* `search_lexicon()` adds `pos` argument to filter by part of speech.

* The print method for `lexadb` also includes a lexicon breakdown with numerical summaries.

* The print method for `lexalx` also prints allomorph information and, if present, examples, etymology and notes.

* The lexicon entry schema now has the following fields: `loan_word`, `crossref`, `variants`, `semantics` (`semantic_domain`, `synonyms`, `antonyms`), and (added to `senses`) `literal`, `scientific` and `usage`.

* Print methods for `lexatx` and `lexast`.

* Print method for `lexalxs` (returned by `search_lexicon()`).

* Added `albanian` database.

* `search_texts()` to search for words or glosses in the texts.

* `show_entry()` which prints the entry with the given `entry_id`.

* Custom RMarkdown templates `lexa_pdf` and `lexa_html`.

## Changed

* `create_entry()` is now exported and outputs a list with the entry id and a string containing the entry skeleton.

* `create_entry()` now allows the user to specify the values of the entry fields.

* `add_entry()` now passes arguments to `create_entry()` so that the user can specify fields.

* The entry senses are now numbered when printed.

* Changed texts schema.

* Both `search_lexicon()` and `search_texts()` now search whole words by default. This can be changed by specifying `whole = FALSE`.

* `lexalx` printing method improved.

## Fixed

* Printing interlinear glosses now works even when there are repeated spaces between words.


# lexa 0.0.0.9001

First beta release.

