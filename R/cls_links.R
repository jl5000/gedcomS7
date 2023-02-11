#' @include utils_at.R cls_validators.R
NULL

#' @export
class_repository_citation <- R7::new_class("class_repository_citation",
                                           properties = list(
                                             xref = R7::class_character,
                                             source_call_number = R7::class_character,
                                             
                                             as_ged = R7::new_property(
                                               R7::class_character,
                                               getter = function(self){
                                                 c(
                                                   sprintf("0 REPO %s", self@xref),
                                                   sprintf("1 CALN %s", self@source_call_number)
                                                 )
                                               })
                                           ),
                                           
                                           validator = function(self){
                                             c(
                                               chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                                               chk_input_pattern(self@xref, "@xref", reg_xref(TRUE)),
                                               chk_input_size(self@source_call_number, "@source_call_number", 0, 1, 1, 120)
                                             )
                                           }
)

#' @export
class_association <- R7::new_class("class_association",
                                   properties = list(
                                     xref = R7::class_character,
                                     relation_is = R7::class_character,
                                     citations = R7::class_list,
                                     note_links = R7::class_character,
                                     notes = R7::class_character,
                                     
                                     as_ged = R7::new_property(
                                       R7::class_character,
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
                                       chk_input_R7classes(self@citations, "@citations", class_citation),
                                       chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                       chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                       chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767)
                                     )
                                   }
)

#' @export
class_spouse_family_link <- R7::new_class("class_spouse_family_link",
                                             properties = list(
                                               xref = R7::class_character,
                                               note_links = R7::class_character,
                                               notes = R7::class_character,
                                               
                                               as_ged = R7::new_property(
                                                 R7::class_character,
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
class_child_family_link_biol <- R7::new_class("class_child_family_link_biol", parent = class_spouse_family_link,
                                            properties = list(
                                              pedigree = R7::new_property(R7::class_character,
                                                                          getter = function(self) "birth"),
                                              
                                              as_ged = R7::new_property(
                                                R7::class_character,
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
class_child_family_link_adop <- R7::new_class("class_child_family_link_adop", parent = class_child_family_link_biol,
                                             properties = list(
                                               pedigree = R7::new_property(R7::class_character,
                                                                           getter = function(self) "adopted")
                                             )
)

#' @export
class_child_family_link_fost <- R7::new_class("class_child_family_link_fost", parent = class_child_family_link_biol,
                                             properties = list(
                                               pedigree = R7::new_property(R7::class_character,
                                                                           getter = function(self) "foster")
                                             )
)

