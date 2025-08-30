
# Used in SNOTE record, SOUR record, NOTE structure, and SOURCE_CITATION
# For NOTE/SNOTE, a language or media type is required
# For SOUR, neither is needed

#' Create a text translation object
#' 
#' @inheritParams prop_definitions
#' @returns An S7 object representing a GEDCOM text translation substructure.
#' @export
#' @tests
#' expect_error(TranslationText(), regexp = "@text has too few elements")
#' expect_error(TranslationText(letters[1:2]), regexp = "@text has too many elements")
#' expect_snapshot_value(TranslationText("test", language = "en")@GEDCOM, "json2")
TranslationText <- S7::new_class(
  "TranslationText",
  parent = GedcomS7class,
  properties = list(
    text = prop_char(1, 1, 1),
    language = prop_char(0, 1, 1),
    media_type = prop_char(0, 1, choices = c("text/plain","text/html")),
    
    GEDCOM = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language)
        )
      })
  )
)


parse_translations <- function(lines, location = NULL){
  # TEXT to handle source citations
  tran_lst <- find_ged_values(lines, c(location, "TRAN|TEXT"), return_list = TRUE)

  lapply(tran_lst, \(x){
    TranslationText(
      text = find_ged_values(x, "TRAN|TEXT"),
      language = find_ged_values(x, c("TRAN|TEXT","LANG")),
      media_type = find_ged_values(x, c("TRAN|TEXT","MIME"))
    )
  })
}

S7::method(summary, TranslationText) <- function(object, ...){
  exdent <- 15
  to_console("Translation:", object@text, exdent)
  cat("\n")
  to_console("Language:", object@language, exdent)
  to_console("Format:", object@media_type, exdent)
}
