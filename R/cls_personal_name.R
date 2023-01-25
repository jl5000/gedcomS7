#' @include helpers.R validators.R
NULL

class_name_pieces <- R7::new_class("class_name_pieces", #abstract = TRUE,
                                   properties = list(
                                     prefix = R7::class_character,
                                     given = R7::class_character,
                                     nickname = R7::class_character,
                                     surname_prefix = R7::class_character,
                                     surname = R7::class_character,
                                     suffix = R7::class_character,
                                     note_links = R7::class_character,
                                     notes = R7::class_character,
                                     citations = R7::class_list
                                   ),
                                   validator = function(self) {
                                     c(
                                       chk_input_size(self@prefix, "@prefix", 0, 1, 1, 30),
                                       chk_input_size(self@given, "@given", 0, 1, 1, 120),
                                       chk_input_size(self@nickname,"@nickname", 0, 1, 1, 30),
                                       chk_input_size(self@surname_prefix, "@surname_prefix", 0, 1, 1, 30),
                                       chk_input_size(self@surname, "@surname", 0, 1, 1, 120),
                                       chk_input_size(self@suffix, "@suffix", 0, 1, 1, 30),
                                       chk_input_size(self@note_links, "@note_links", 0, 10000, 3, 22),
                                       chk_input_pattern(self@note_links, "@note_links", reg_xref(TRUE)),
                                       chk_input_size(self@notes, "@notes", 0, 10000, 1, 32767),
                                       chk_input_R7classes(self@citations, "@citations", class_citation)
                                     )
                                   }
                                   
)


class_name_info <- R7::new_class("class_name_info", parent = class_name_pieces,
                                 properties = list(
                                   full = R7::class_character,
                                   type = R7::class_character,
                                   
                                   as_ged = R7::new_property(
                                     R7::class_character,
                                     getter = function(self){
                                       c(
                                         sprintf("0 NAME %s", self@full),
                                         sprintf("1 TYPE %s", self@type),
                                         sprintf("1 NPFX %s", self@prefix),
                                         sprintf("1 GIVN %s", self@given),
                                         sprintf("1 NICK %s", self@nickname),
                                         sprintf("1 SPFX %s", self@surname_prefix),
                                         sprintf("1 SURN %s", self@surname),
                                         sprintf("1 NSFX %s", self@suffix),
                                         sprintf("1 NOTE %s", c(self@note_links, self@notes)),
                                         lst_to_ged(self@citations) |> increase_level(by = 1)
                                       )
                                     })
                                 ),
                                 
                                 validator = function(self){
                                   pieces_error <- NULL
                                   if(length(self@prefix) + length(self@given) + length(self@nickname) +
                                      length(self@surname_prefix) + length(self@surname) + length(self@suffix) == 0)
                                     pieces_error <- "Names require at least one name piece (prefix/given/nickname/surname_prefix/surname/suffix)"
                                   c(
                                     chk_input_size(self@full, "@full", 1, 1, 1, 120),
                                     # Relaxed type for main name, even though phon/rom need it
                                     chk_input_size(self@type, "@type", 0, 1, 5, 30),
                                     pieces_error
                                   )
                                 }
)


class_personal_name <- R7::new_class("class_personal_name",
                                     properties = list(
                                       name = class_name_info,
                                       phon_names = R7::class_list,
                                       rom_names = R7::class_list,
                                       
                                       as_ged = R7::new_property(
                                         R7::class_character,
                                         getter = function(self){
                                           
                                           phon <- lst_to_ged(self@phon_names) |> increase_level(by = 1)
                                           if(length(phon) > 0)
                                             phon <- sub("^(.) NAME", "\\1 FONE", phon)
                                           
                                           rom <- lst_to_ged(self@rom_names) |> increase_level(by = 1)
                                           if(length(rom) > 0)
                                             rom <- sub("^(.) NAME", "\\1 ROMN", rom)
                                           
                                           c(
                                             obj_to_ged(self@name),
                                             phon,
                                             rom
                                           )
                                         })
                                     ),
                                     
                                     validator = function(self){
                                       type_error <- NULL
                                       for(phon in self@phon_names){
                                         if(length(phon@type) == 0)
                                           type_error <- "Every phonetic and romanised name variation requires a type"
                                       }
                                       for(rom in self@rom_names){
                                         if(length(rom@type) == 0)
                                           type_error <- "Every phonetic and romanised name variation requires a type"
                                       }
                                       
                                       c(
                                         chk_input_size(self@name, "@name", 1, 1),
                                         chk_input_R7classes(self@phon_names, "@phon_names", class_name_info),
                                         chk_input_R7classes(self@rom_names, "@rom_names", class_name_info),
                                         type_error
                                       )
                                     }
)

