#' @include cls_validators.R
NULL

#' @export
#' @include cls_note.R
class_place <- S7::new_class(
  "class_place",
  package = "gedcomS7",
  properties = list(
    name = S7::class_character,
    form = S7::class_character,
    language = S7::class_character,
    names_alt = S7::class_character,
    lat_long = S7::class_character,
    external_ids = S7::class_character,
    note_uids = S7::class_character,
    notes = S7::class_list | S7::class_character,
    
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
      chk_input_size(self@names_alt, "@names_alt", min_val = 1),
      chk_input_size(names(self@names_alt), "@names_alt names", length(self@names_alt), length(self@names_alt)),
      #TODO: language lookup
      chk_input_size(self@lat_long, "@lat_long", 0, 1),
      chk_input_pattern(self@lat_long, "@lat_long", "^[NS]\\d{1,2}(\\.\\d{1,6})? [EW]\\d{1,3}(\\.\\d{2,6})?$"),
      chk_input_size(self@external_ids, "@external_ids", min_val = 1),
      chk_input_size(names(self@external_ids), "@external_ids names", length(self@external_ids), length(self@external_ids)),
      #TODO: EXID and TYPE pattern
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+")
    )
  }
)

