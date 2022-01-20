# lexa 0.0.0.9002

## Breaking changes

* Renamed `import_lexicon()` to `import_lexicon_csv()`.

## Added

* `search_lexicon()` adds `pos` argument to filter by part of speech.

* The print method for `lexalx` also prints a lexicon breakdown with numerical summaries, and etymology and notes if present.

* The lexicon entry schema now has the following fields: `loan_word`, `crossref`, `variants`, `semantics` (`semantic_domain`, `synonyms`, `antonyms`), and (added to `senses`) `literal`, `scientific` and `usage`.

* Print methods for `lexatx` and `lexast`.

* Added `albanian` database.

* `search_texts()` to search for words or glosses in the texts.

## Changed

* `create_entry()` is now exported and outputs a list with the entry id and a string containing the entry skeleton.

* `create_entry()` now allows the user to specify the values of the entry fields.

* `add_entry()` now passes arguments to `create_entry()` so that the user can specify fields.

* The entry senses are now numbered when printed.

* Changed texts schema.


# lexa 0.0.0.9001

First beta release.

