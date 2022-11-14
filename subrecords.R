




notes <- new_class("notes",
                   properties = list(
                     xref = class_character,
                     text = class_character
                   ),
                   validator = function(self) {
                     c(
                       chk_input_size(self@xref, "@xref", 1, 3, 22),
                       chk_input_size(self@text, "@text", 1, 1, 32767),
                       chk_input_pattern(self@xref, "@xref", reg_xref(TRUE))
                     )
                   }
)

name_pieces <- new_class("name_pieces",
                         properties = list(
                           prefix = class_character,
                           given = class_character,
                           nickname = class_character,
                           surname_prefix = class_character,
                           surname = class_character,
                           suffix = class_character,
                           notes = class_list
                         ),
                         validator = function(self) {
                           c(
                             chk_input_size(self@prefix, "@prefix", 1, 1, 30),
                             chk_input_size(self@given, "@given", 1, 1, 120),
                             chk_input_size(self@nickname,"@nickname", 1, 1, 30),
                             chk_input_size(self@surname_prefix, "@surname_prefix", 1, 1, 30),
                             chk_input_size(self@surname, "@surname", 1, 1, 120),
                             chk_input_size(self@suffix, "@suffix", 1, 1, 30)
                           )
                         }
                         
)
