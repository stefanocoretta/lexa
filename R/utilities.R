validate_lexicon <- function(lexadb) {
  db_path <- attr(lexadb, "meta")$path
  lexicon <- read_lexicon(db_path)
  lapply(
    lexicon,
    function(entry) {
      entry_json <- jsonlite::toJSON(entry, auto_unbox = TRUE)
      lx_validator(entry_json)
    }
  )
}

lx_json_schema <- '{
"$schema": "http://json-schema.org/draft-07/schema#",
"type": "object",
"properties": {
  "id": { "type": "string" },
  "lexeme": { "type": "string" },
  "phon": { "type": "string" },
  "morph_category": { "type": "string" },
  "morph_type": { "type": "string" },
  "part_of_speech": { "type": "string" },
  "inflectional_features": {
    "type": "object",
    "properties": {
      "class": { "type": "string" }
    },
    "required": ["class"]
  },
  "allomorphs": {
    "type": "object",
    "patternProperties": {
      "^al_\\d+$": {
        "type": "object",
        "properties": {
          "id": { "type": "string" },
          "morph": { "type": "string" },
          "phon": { "type": "string" }
        },
        "required": ["id", "morph", "phon"]
      }
    }
  },
  "senses": {
    "type": "object",
    "patternProperties": {
      "^se_\\d+$": {
        "type": "object",
        "properties": {
          "id": { "type": "string" },
          "gloss": { "type": "string" },
          "definition": { "type": "string" },
          "examples": {
            "type": "array",
            "items": { "type": "string" }
          }
        },
        "required": ["id", "gloss", "definition", "examples"]
      }
    }
  },
  "date_created": { "type": "string", "format": "date-time" },
  "date_modified": { "type": "string", "format": "date-time" }
},
"required": [
  "id",
  "lexeme",
  "phon",
  "morph_category",
  "morph_type",
  "part_of_speech",
  "inflectional_features",
  "allomorphs",
  "senses",
  "date_created",
  "date_modified"
]
}'

lx_validator <- jsonvalidate::json_validator(lx_json_schema, engine = "ajv")
