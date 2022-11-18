
class_name_pieces <- R7::new_class("class_name_pieces",
                         properties = list(
                           prefix = R7::class_character,
                           given = R7::class_character,
                           nickname = R7::class_character,
                           surname_prefix = R7::class_character,
                           surname = R7::class_character,
                           suffix = R7::class_character,
                           notes = R7::class_list,
                           citations = R7::class_list
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


class_name_info <- R7::new_class("class_name_info",
                  properties = list(
                    full_name = R7::class_character,
                    name_type = R7::class_character,
                    name_pieces = class_name_pieces
                  ))

class_personal_name <- R7::new_class("class_personal_name",
                           properties = list(
                             name = class_name_info,
                             name_phonetic = R7::class_list,
                             name_romanised = R7::class_list
                           ))


