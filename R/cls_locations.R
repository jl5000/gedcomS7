#' @include cls_validators.R
NULL


#' @export
#' @include cls_common.R
class_place <- S7::new_class(
  "class_place",
  properties = list(
    name = S7::class_character,
    form = S7::class_character,
    language = S7::class_character,
    names_alt = S7::class_character,
    lat_long = S7::class_character,
    external_ids = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list,
    
    lat = S7::new_property(S7::class_character,
                           getter = function(self){
                             if(length(self@lat_long) == 1){
                               unlist(strsplit(self@lat_long, split = " "))[1]
                             } else { character() }
                           }),
    long = S7::new_property(S7::class_character,
                            getter = function(self){
                              if(length(self@lat_long) == 1){
                                unlist(strsplit(self@lat_long, split = " "))[2]
                              } else { character() }
                            }),
    
    as_val = S7::new_property(S7::class_character, 
                              getter = function(self) self@name),
    
    as_ged = S7::new_property(S7::class_character,
                              getter = function(self){
                                c(
                                  sprintf("0 PLAC %s", self@name),
                                  sprintf("1 FORM %s", self@form),
                                  sprintf("1 LANG %s", self@language),
                                  named_vec_to_ged(self@names_alt, "TRAN", "LANG") |> increase_level(by = 1),
                                  rep("1 MAP", length(self@lat_long)),
                                  sprintf("2 LATI %s", self@lat),
                                  sprintf("2 LONG %s", self@long),
                                  named_vec_to_ged(self@external_ids, "EXID", "TYPE") |> increase_level(by = 1),
                                  sprintf("1 SNOTE %s", self@note_uids),
                                  lst_to_ged(self@notes) |> increase_level(by = 1)
                                )
                              })
  ),
  
  validator = function(self) {
    c(
      chk_input_size(self@name, "@name", 1, 1, 1),
      chk_input_size(self@form, "@form", 0, 1, 1),
      chk_input_size(self@language, "@language", 0, 1),
      #TODO: language lookup
      chk_input_size(self@names_alt, "@names_alt", min_char = 1),
      chk_input_size(names(self@names_alt), "@names_alt names", length(self@names_alt), length(self@names_alt)),
      #TODO: language lookup
      chk_input_size(self@lat_long, "@lat_long", 0, 1),
      chk_input_pattern(self@lat_long, "@lat_long", "^[NS]\\d{1,2}(\\.\\d{1,6})? [EW]\\d{1,3}(\\.\\d{2,6})?$"),
      chk_input_size(self@external_ids, "@external_ids", min_char = 1),
      chk_input_size(names(self@external_ids), "@external_ids names", length(self@external_ids), length(self@external_ids)),
      #TODO: EXID and TYPE pattern
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note)
    )
  }
)



#' @export
class_address <- S7::new_class(
  "class_address",
  properties = list(
    full = S7::class_character,
    local_address_lines = S7::class_character,
    city = S7::class_character,
    state = S7::class_character,
    postal_code = S7::class_character,
    country = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 ADDR %s", self@full),
          sprintf("1 %s %s", paste0("ADR", seq_along(self@local_address_lines)), self@local_address_lines),
          sprintf("1 CITY %s", self@city),
          sprintf("1 STAE %s", self@state),
          sprintf("1 POST %s", self@postal_code),
          sprintf("1 CTRY %s", self@country)
        )
      })
  ),
  
  validator = function(self) {
    c(
      chk_input_size(self@full, "@full", 1, 1, 1),
      chk_input_size(self@local_address_lines, "@local_address_lines", 0, 1, 1),
      chk_input_size(self@city, "@city", 0, 1, 1),
      chk_input_size(self@state, "@state", 0, 1, 1),
      chk_input_size(self@postal_code, "@postal_code", 0, 1, 1),
      chk_input_size(self@country, "@country", 0, 1, 1)
    )
  }
)
