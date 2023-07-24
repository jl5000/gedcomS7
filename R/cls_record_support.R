#' @include cls_validators.R
NULL

#' @export
class_media_file <- S7::new_class(
  "class_media_file",
  package = "gedcomS7",
  properties = list(
    file_ref = S7::class_character,
    format = S7::class_character,
    medium = S7::class_character,
    title = S7::class_character,
    titles_alt = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 FILE %s", self@file_ref),
          sprintf("1 FORM %s", self@format),
          named_vec_to_ged(self@medium, "MEDI", "PHRASE") |> increase_level(by = 2),
          sprintf("1 TITL %s", self@title),
          named_vec_to_ged(self@titles_alt, "TRAN", "FORM") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@file_ref, "@file_ref", 1, 1, 1),
      chk_input_size(self@format, "@format", 1, 1, 1),
      #chk_input_choice(self@format, "@format", val_multimedia_formats()), TODO
      chk_input_size(self@medium, "@medium", 0, 1),
      chk_input_choice(self@medium, "@medium", val_medium_types()),
      chk_input_size(self@title, "@title", 0, 1, 1),
      chk_input_size(self@titles_alt, "@titles_alt", 0, 1, 1)
      #chk_input_choice(names(self@titles_alt), "@titles_alt types", val_source_media_types()),
    )
  }
)

#' @export
class_translation_txt <- S7::new_class(
  "class_translation_txt",
  package = "gedcomS7",
  properties = list(
    text = S7::class_character,
    language = S7::class_character,
    media_type = S7::class_character,
    
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
    input_err <- NULL
    if(length(self@language) + length(self@media_type) == 0)
      input_err <- "A note language or media_type must be defined."
    c(
      chk_input_size(self@text, "@text", 1, 1),
      chk_input_size(self@language, "@language", 0, 1, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1, 1),
      #TODO: media type pattern
      input_err
    )
  })

#' @export
#' @include cls_common.R
class_repository_citation <- S7::new_class(
  "class_repository_citation",
  package = "gedcomS7",
  properties = list(
    repo_uid = S7::class_character,
    notes = S7::class_list,
    note_uids = S7::class_character,
    source_call_number = S7::class_character,
    # media = S7::class_character, TODO: too much nesting
    # media_phrase = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 REPO %s", self@repo_uid),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1),
          sprintf("1 CALN %s", self@source_call_number)
          # sprintf("2 MEDI %s", self@media),
          # sprintf("3 PHRASE %s", self@media_phrase)
        )
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@repo_uid, "@repo_uid", 1, 1),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note),
      chk_input_pattern(self@repo_uid, "@repo_uid", reg_uuid(TRUE)),
      chk_input_size(self@source_call_number, "@source_call_number", min_char = 1)
    )
  }
)

#' @export
#' @include cls_dates.R cls_locations.R
class_events_recorded <- S7::new_class(
  "class_events_recorded",
  package = "gedcomS7",
  properties = list(
    events = S7::class_character,
    date_period = NULL | class_date_period | S7::class_character,
    date_phrase = S7::class_character,
    jurisdiction_place = NULL | class_place | S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 EVEN %s", self@events),
          sprintf("1 DATE %s", date_to_val(self@date_period)),
          sprintf("2 PHRASE %s", self@date_phrase),
          obj_to_ged(self@jurisdiction_place, "PLAC") |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@events, "@events", 1, 1),
      chk_input_size(self@date_period, "@date_period", 0, 1),
      chk_input_pattern(self@date_period, "@date_period", reg_date_period()),
      chk_input_size(self@date_phrase, "@date_phrase", 0, 1, 1),
      chk_input_size(self@jurisdiction_place, "@jurisdiction_place", 0, 1, 1)
    )
  })
