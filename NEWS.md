# lexa 0.0.0.9002

## Breaking changes

* Renamed `import_lexicon()` to `import_lexicon_csv()`.

## Added

* `search_lexicon()` adds `pos` argument to filter by part of speech.

* The print method for `lexalx` also prints a lexicon breakdown with numerical summaries, and etymology and notes if present.

## Changed

* `create_entry()` is now exported and outputs a list with the entry id and a string containing the entry skeleton.

* `create_entry()` now allows the user to specify the values of the entry fields.

* `add_entry()` now passes arguments to `create_entry()` so that the user can specify fields.

* The entry senses are now numbered when printed.


# lexa 0.0.0.9001

First beta release.

