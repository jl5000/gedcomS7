
class_name_pieces <- new_class("class_name_pieces",
                         properties = list(
                           prefix = class_character,
                           given = class_character,
                           nickname = class_character,
                           surname_prefix = class_character,
                           surname = class_character,
                           suffix = class_character,
                           notes = class_list,
                           citations = class_list
                         ),
                         validator = function(self) {
                           c(
                             chk_input_size(self@prefix, "@prefix", 0, 1, 1, 30),
                             chk_input_size(self@given, "@given", 0, 1, 1, 120),
                             chk_input_size(self@nickname,"@nickname", 0, 1, 1, 30),
                             chk_input_size(self@surname_prefix, "@surname_prefix", 0, 1, 1, 30),
                             chk_input_size(self@surname, "@surname", 0, 1, 1, 120),
                             chk_input_size(self@suffix, "@suffix", 0, 1, 1, 30)
                           )
                         }
                         
)


class_name_info <- new_class("class_name_info",
                  properties = list(
                    full_name = class_character,
                    name_type = class_character,
                    name_pieces = class_name_pieces
                  ))

class_personal_name <- new_class("class_personal_name",
                           properties = list(
                             name = class_name_info,
                             name_phonetic = class_list,
                             name_romanised = class_list
                           ))


