


class_change_date <- new_class("class_change_date",
                               properties = list(
                                 change_date = class_character,
                                 change_time = class_character,
                                 notes = class_list
                               ),
                               validator = function(self) {
                                 c(
                                   chk_input_size(self@change_date, "@change_date", 1, 1, 10, 11),
                                   chk_input_size(self@change_time, "@change_time", 0, 1, 7, 12),

                                 )
                               }
)

class_note <- new_class("class_note",
                   properties = list(
                     xref = class_character,
                     text = class_character
                   ),
                   validator = function(self) {
                     c(
                       chk_input_size(self@xref, "@xref", 1, 1, 3, 22),
                       chk_input_size(self@text, "@text", 1, 1, 1, 32767),
                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE))
                     )
                   }
)


class_citation <- new_class("class_citation")


class_media_link <- new_class("class_media_link")
