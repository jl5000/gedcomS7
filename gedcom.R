
gedcomR7 <- new_class("gedcomR7",
                      properties = list(
                        gedcom_version = new_property(getter = function(self) "5.5.5"),
                        gedcom_form = new_property(getter = function(self) "LINEAGE-LINKED"),
                        character_encoding = new_property(getter = function(self) "UTF-8"),
                        system_id = class_character,
                        product_version = class_character,
                        product_name = class_character,
                        business_name = class_character,
                        source_data_name = class_character,
                        source_data_pubdate = class_character,
                        source_data_copyright = class_character,
                        receiving_system = class_character,
                        creation_date = class_character,
                        creation_time = class_character,
                        language = class_character,
                        xref_subm = class_character,
                        file_name = class_character,
                        gedcom_copyright = class_character,
                        content_description = class_character
                      ))

