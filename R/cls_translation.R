
class_translation <- S7::new_class("class_translation", #abstract = TRUE,
                                   properties = list(
                                     text = S7::class_character
                                   ),
                                   validator = function(self){
                                     chk_input_size(self@text, "@text", 1, 1)
                                   })

class_translation_note <- S7::new_class("class_translation_note", parent = class_translation,
                                        properties = list(
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
                                            chk_input_size(self@language, "@language", 0, 1),
                                            #TODO: language option
                                            chk_input_size(self@media_type, "@media_type", 0, 1),
                                            #TODO: media type pattern
                                            input_err
                                          )
                                        })




class_translation_media <- S7::new_class("class_translation_media", parent = class_translation,
                                        properties = list(
                                          media_type = S7::class_character,
                                          
                                          as_ged = S7::new_property(
                                            S7::class_character,
                                            getter = function(self){
                                              c(
                                                sprintf("0 TRAN %s", self@text),
                                                sprintf("1 FORM %s", self@media_type)
                                              )
                                            })
                                        ),
                                        validator = function(self){
                                          c(
                                            chk_input_size(self@media_type, "@media_type", 1, 1)
                                            #TODO: media type pattern
                                          )
                                        })
