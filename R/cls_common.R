#' @include cls_validators.R 
NULL

#' @export
#' @include cls_record_support.R
class_note <- S7::new_class(
  "class_note",
  properties = list(
    text = S7::class_character,
    language = S7::class_character,
    media_type = S7::class_character,
    alt_text = S7::class_list,
    #citations = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 NOTE %s", self@text),
          sprintf("1 MIME %s", self@media_type),
          sprintf("1 LANG %s", self@language),
          lst_to_ged(self@alt_text) |> increase_level(by = 1)
          #   lst_to_ged(self@citations) |> increase_level(by = 1),
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@text, "@text", 1, 1, 1),
      chk_input_size(self@language, "@language", 0, 1),
      #TODO: language option
      chk_input_size(self@media_type, "@media_type", 0, 1),
      #TODO: media type pattern
      chk_input_S7classes(self@alt_text, "@alt_text", class_translation_txt)
      #  chk_input_S7classes(self@citations, "@citations", class_citation)
    )
  }
)

#' @export
class_media_link <- S7::new_class(
  "class_media_link",
  properties = list(
    media_uid = S7::class_character,
    title = S7::class_character,
    crop = S7::new_property(S7::class_logical, default = FALSE),
    top = S7::new_property(S7::class_numeric, default = 0),
    left = S7::new_property(S7::class_numeric, default = 0),
    height = S7::class_numeric,
    width = S7::class_numeric,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 OBJE %s", self@media_uid),
          rep("1 CROP", self@crop),
          rep(sprintf("2 TOP %s", self@top), self@crop),
          rep(sprintf("2 LEFT %s", self@left), self@crop),
          rep(sprintf("2 HEIGHT %s", self@height), self@crop),
          rep(sprintf("2 WIDTH %s", self@width), self@crop),
          sprintf("1 TITL %s", self@title)
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@media_uid, "@media_uid", 1, 1),
      chk_input_size(self@title, "@title", 0, 1, 1),
      chk_input_size(self@crop, "@crop", 1, 1),
      chk_input_size(self@top, "@top", 0, 1, 0),
      chk_input_size(self@left, "@left", 0, 1, 0),
      chk_input_size(self@height, "@height", 0, 1, 1),
      chk_input_size(self@width, "@width", 0, 1, 1),
      chk_whole_number(self@top, "@top"),
      chk_whole_number(self@left, "@left"),
      chk_whole_number(self@height, "@height"),
      chk_whole_number(self@width, "@width"),
      chk_input_pattern(self@media_uid, "@media_uid", reg_uuid(TRUE))
    )
  }
)


#' @export
#' @include cls_dates.R cls_record_support.R
class_citation <- S7::new_class(
  "class_citation",
  properties = list(
    sour_uid = S7::class_character,
    where = S7::class_character,
    date = S7::new_property(S7::new_union(NULL, class_date_value)),
    source_text = S7::class_list,
    event_type = S7::class_character,
    event_phrase = S7::class_character,
    role = S7::class_character,
    role_phrase = S7::class_character,
    certainty = S7::class_numeric,
    media_links = S7::class_list,
    note_uids = S7::class_character,
    notes = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@sour_uid),
          sprintf("1 PAGE %s", self@where),
          rep("1 DATA", length(self@date) + 
                length(self@source_text) > 0),
          obj_to_ged(self@date) |> increase_level(by = 2),
          lst_to_ged(self@source_text) |> increase_level(by = 2),
          sprintf("1 EVEN %s", self@event_type),
          sprintf("2 PHRASE %s", self@event_phrase),
          sprintf("2 ROLE %s", self@role),
          sprintf("3 PHRASE %s", self@role_phrase),
          sprintf("1 QUAY %s", self@certainty),
          lst_to_ged(self@media_links) |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1)
        ) 
      })
  ),
  
  validator = function(self){
    c(
      chk_input_size(self@sour_uid, "@sour_uid", 1, 1),
      chk_input_size(self@where, "@where", 0, 1, 1),
      chk_input_size(self@date, "@date", 0, 1),
      chk_input_size(self@event_type, "@event_type", 0, 1),
      chk_input_size(self@date_phrase, "@event_phrase", 0, 1, 1),
      chk_input_size(self@role, "@role", 0, 1),
      chk_input_size(self@role_phrase, "@role_phrase", 0, 1, 1),
      chk_input_size(self@certainty, "@certainty", 0, 1, 0, 3),
      chk_input_parents(self@event_phrase, "@event_phrase", 
                        self@event_type, "@event_type"),
      chk_input_parents(self@role, "@role", 
                        self@event_type, "@event_type"),
      chk_input_parents(self@role_phrase, "@role_phrase", 
                        self@role, "@role"),
      chk_input_pattern(self@sour_uid, "@sour_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_choice(self@event_type, "@event_type", val_fact_types()),
      chk_input_choice(self@role, "@role", val_roles()),
      chk_whole_number(self@certainty, "@certainty"),
      chk_input_S7classes(self@source_text, "@source_text", class_translation_txt),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link),
      chk_input_S7classes(self@notes, "@notes", class_note)
    )
  }
  
)



#' @export
#' @include cls_dates.R
class_creation_date <- S7::new_class(
  "class_creation_date",
  properties = list(
    date = S7::new_property(S7::new_union(NULL, 
                                          class_date_exact, 
                                          S7::class_character), 
                            default = date_exact_current()),
    time = S7::new_property(S7::new_union(NULL, 
                                          class_time, 
                                          S7::class_character)),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CREA",
          sprintf("1 DATE %s", date_to_val(self@date)),
          sprintf("2 TIME %s", self@time)
        )
      })
  ),
  validator = function(self) {
    c(
      chk_input_size(self@date, "@date", 1, 1),
      chk_input_size(self@time, "@time", 0, 1),
      chk_input_pattern(self@date, "@date", reg_date_exact()),
      chk_input_pattern(self@time, "@time", reg_time())
    )
  }
)

#' @export
class_change_date <- S7::new_class(
  "class_change_date", 
  parent = class_creation_date,
  properties = list(
    note_uids = S7::class_character,
    notes = S7::class_list,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          "0 CHAN",
          sprintf("1 DATE %s", date_to_val(self@date)),
          sprintf("2 TIME %s", self@time),
          sprintf("1 SNOTE %s", self@note_uids),
          lst_to_ged(self@notes) |> increase_level(by = 1)
        )
        
      })
  ),
  validator = function(self) {
    c(
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note)
    )
  }
)

