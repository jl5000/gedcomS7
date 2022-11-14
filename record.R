
record <- new_class("record",
                    properties = list(
                      xref = class_character,
                      user_reference_number = class_character
                    )
                    )


famg <- new_class("famg", parent = record,
                  properties = list(
                    husb_xref = class_character,
                    wife_xref = class_character,
                    chil_xref = class_character,
                    num_children = class_integer
                  ))

indi <- new_class("indi", parent = record,
                  properties = list(
                    sex = class_character
                  ))

media <- new_class("media", parent = record,
                   properties = list(
                     file_ref = class_character,
                     format = class_character,
                     media_type = class_character,
                     title = class_character
                   ))

sour <- new_class("sour", parent = record,
                  properties = list(
                    events_recorded = class_character,
                    date_period = class_character,
                    jurisdiction_place = class_character,
                    responsible_agency = class_character,
                    originator = class_character,
                    full_title = class_character,
                    short_title = class_character,
                    publication_facts = class_character,
                    source_text = class_character
                  ))

repo <- new_class("repo", parent = record,
                  properties = list(
                    name = class_character
                  ))

note <- new_class("note", parent = record,
                  properties = list(
                    text = class_character
                  ))

subm <- new_class("subm", parent = record,
                  properties = list(
                    name = class_character
                  ))

