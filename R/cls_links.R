#' @include cls_validators.R
NULL

#' @export
#' @include cls_common.R
class_association <- S7::new_class("class_association",
                                   properties = list(
                                     xref = S7::class_character,
                                     relation_is = S7::class_character,
                                     citations = S7::class_list,
                                     note_links = S7::class_character,
                                     notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
                                     
                                     as_ged = S7::new_property(
                                       S7::class_character,
                                       getter = function(self){
                                         c(
                                           sprintf("0 ASSO %s", self@xref),
                                           sprintf("1 RELA %s", self@relation_is),
                                           lst_to_ged(self@citations) |> increase_level(by = 1),
                                           sprintf("1 NOTE %s", self@note_links),
                                           sprintf("1 NOTE %s", self@notes)
                                         )
                                       })
                                   ),
                                   
                                   validator = function(self){
                                     c(
                                       chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                       chk_input_size(self@relation_is, "@relation_is", 1, 1, 1, 25),
                                       chk_input_S7classes(self@citations, "@citations", class_citation),
                                       chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                       chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                       chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767)
                                     )
                                   }
)

#' @export
#' @include cls_common.R
class_spouse_family_link <- S7::new_class("class_spouse_family_link",
                                             properties = list(
                                               xref = S7::class_character,
                                               note_links = S7::class_character,
                                               notes = S7::new_property(S7::new_union(S7::class_character, S7::class_list)),
                                               
                                               as_ged = S7::new_property(
                                                 S7::class_character,
                                                 getter = function(self){
                                                   c(
                                                     sprintf("0 FAMS %s", self@xref),
                                                     sprintf("1 NOTE %s", self@note_links),
                                                     sprintf("1 NOTE %s", self@notes)
                                                   )
                                                 })
                                             ),
                                             validator = function(self){
                                               c(
                                                 chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                                 chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                                 chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                                 chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                                 chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767)
                                               )
                                             }
)

#' @export
class_child_family_link_biol <- S7::new_class("class_child_family_link_biol", parent = class_spouse_family_link,
                                            properties = list(
                                              pedigree = S7::new_property(S7::class_character,
                                                                          getter = function(self) "birth"),
                                              
                                              as_ged = S7::new_property(
                                                S7::class_character,
                                                getter = function(self){
                                                  c(
                                                    sprintf("0 FAMC %s", self@xref),
                                                    sprintf("1 PEDI %s", self@pedigree),
                                                    sprintf("1 NOTE %s", self@note_links),
                                                    sprintf("1 NOTE %s", self@notes)
                                                  )
                                                })
                                            )
)

#' @export
class_child_family_link_adop <- S7::new_class("class_child_family_link_adop", parent = class_child_family_link_biol,
                                             properties = list(
                                               pedigree = S7::new_property(S7::class_character,
                                                                           getter = function(self) "adopted")
                                             )
)

#' @export
class_child_family_link_fost <- S7::new_class("class_child_family_link_fost", parent = class_child_family_link_biol,
                                             properties = list(
                                               pedigree = S7::new_property(S7::class_character,
                                                                           getter = function(self) "foster")
                                             )
)

