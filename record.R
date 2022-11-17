
class_record <- new_class("class_record", abstract = TRUE,
                          properties = list(
                            xref = new_property(
                              class_character),
                            change_date = new_property(new_union(NULL, class_change_date))
                          )
)

class_record_subm <- new_class("class_record_subm", parent = class_record,
                               properties = list(
                                 name = class_character,
                                 address = new_property(new_union(NULL, class_address)),
                                 media_links = class_list,
                                 notes = class_list
                               )
)

class_record_lin <- new_class("class_record_lin", parent = class_record, abstract = TRUE,
                              properties = list(
                                user_reference_numbers = class_character
                              )
)

class_record_famg <- new_class("class_record_famg", parent = class_record_lin,
                               properties = list(
                                 events = class_list,
                                 husb_xref = class_character,
                                 wife_xref = class_character,
                                 chil_xref = class_character,
                                 num_children = class_integer,
                                 notes = class_list,
                                 citations = class_list,
                                 media_links = class_list
                               ))

class_record_indi <- new_class("class_record_indi", parent = class_record_lin,
                               properties = list(
                                 personal_names = class_list,
                                 sex = new_property(class_character, default = "U"),
                                 facts = class_list,
                                 family_links = class_list,
                                 associations = class_list,
                                 notes = class_list,
                                 citations = class_list,
                                 media_links = class_list
                               ))

class_record_media <- new_class("class_record_media", parent = class_record_lin,
                                properties = list(
                                  file_ref = class_character,
                                  format = class_character,
                                  media_type = class_character,
                                  title = class_character,
                                  notes = class_list,
                                  citations = class_list
                                ))

class_record_sour <- new_class("class_record_sour", parent = class_record_lin,
                               properties = list(
                                 events_recorded = class_character,
                                 date_period = class_character,
                                 jurisdiction_place = class_character,
                                 responsible_agency = class_character,
                                 data_notes = class_list,
                                 originator = class_character,
                                 full_title = class_character,
                                 short_title = class_character,
                                 publication_facts = class_character,
                                 source_text = class_character,
                                 repo_citations = class_list,
                                 notes = class_list,
                                 media_links = class_list
                               ))

class_record_repo <- new_class("class_record_repo", parent = class_record_lin,
                               properties = list(
                                 name = class_character,
                                 address = new_property(new_union(NULL, class_address)),
                                 notes = class_list
                               ))

class_record_note <- new_class("class_record_note", parent = class_record_lin,
                               properties = list(
                                 text = class_character,
                                 citations = class_list
                               ))


