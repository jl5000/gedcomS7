#' @include cls_validators.R
NULL

# Used in SNOTE record, SOUR record, NOTE structure, and SOURCE_CITATION
#' Create a text translation object
#' 
#' @inheritParams prop_definitions
#' @return An S7 object representing a GEDCOM text translation substructure.
#' @export
#' @tests
#' expect_error(class_translation_txt(), regexp = "@text has too few elements")
#' expect_error(class_translation_txt(letters[1:2]), regexp = "@text has too many elements")
#' expect_error(class_translation_txt("test"), regexp = "A @language or @media_type must be defined")
#' expect_snapshot_value(class_translation_txt("test", language = "en")@as_ged, "json2")
class_translation_txt <- S7::new_class(
  "class_translation_txt",
  package = "gedcomS7",
  properties = list(
    text = S7::new_property(S7::class_character,
                            validator = function(value){
                              chk_input_size(value, 1, 1)
                            }),
    language = S7::new_property(S7::class_character,
                                validator = function(value){
                                  c(
                                    chk_input_size(value, 0, 1, 1)
                                    #TODO: language option
                                  )
                                }),
    media_type = S7::new_property(S7::class_character,
                                  validator = function(value){
                                    c(
                                      chk_input_size(value, 0, 1),
                                      chk_input_choice(value, c("text/plain","text/html"))
                                    )
                                  }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 TRAN %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language)
        )
      })
  ),
  validator = function(self){
    if(length(self@language) + length(self@media_type) == 0)
      return("A @language or @media_type must be defined.")
  }
)


extract_translations <- function(lines, location = NULL){
  # TEXT to handle source citations
  tran_lst <- find_ged_values(lines, c(location, "TRAN|TEXT"), return_list = TRUE)
  if(length(tran_lst) == 0) return(list())
  
  lapply(tran_lst, \(x){
    class_translation_txt(
      text = find_ged_values(x, "TRAN|TEXT"),
      language = find_ged_values(x, c("TRAN|TEXT","LANG")),
      media_type = find_ged_values(x, c("TRAN|TEXT","MIME"))
    )
  })
}