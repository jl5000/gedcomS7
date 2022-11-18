
class_record <- R7::new_class("class_record", abstract = TRUE,
                          properties = list(
                            xref = R7::new_property(
                              R7::class_character),
                            change_date = R7::new_property(R7::new_union(NULL, class_change_date))
                          )
)

class_record_subm <- R7::new_class("class_record_subm", parent = class_record,
                               properties = list(
                                 name = R7::class_character,
                                 address = R7::new_property(R7::new_union(NULL, class_address)),
                                 media_links = R7::class_list,
                                 notes = R7::class_list
                               )
)

class_record_lin <- R7::new_class("class_record_lin", parent = class_record, abstract = TRUE,
                              properties = list(
                                user_reference_numbers = R7::class_character
                              )
)

class_record_famg <- R7::new_class("class_record_famg", parent = class_record_lin,
                               properties = list(
                                 events = R7::class_list,
                                 husb_xref = R7::class_character,
                                 wife_xref = R7::class_character,
                                 chil_xref = R7::class_character,
                                 num_children = R7::class_integer,
                                 notes = R7::class_list,
                                 citations = R7::class_list,
                                 media_links = R7::class_list
                               ))

class_record_indi <- R7::new_class("class_record_indi", parent = class_record_lin,
                               properties = list(
                                 personal_names = R7::class_list,
                                 sex = R7::new_property(R7::class_character, default = "U"),
                                 facts = R7::class_list,
                                 family_links = R7::class_list,
                                 associations = R7::class_list,
                                 notes = R7::class_list,
                                 citations = R7::class_list,
                                 media_links = R7::class_list
                               ))

class_record_media <- R7::new_class("class_record_media", parent = class_record_lin,
                                properties = list(
                                  file_ref = R7::class_character,
                                  format = R7::class_character,
                                  media_type = R7::class_character,
                                  title = R7::class_character,
                                  notes = R7::class_list,
                                  citations = R7::class_list
                                ))

class_record_sour <- R7::new_class("class_record_sour", parent = class_record_lin,
                               properties = list(
                                 events_recorded = R7::class_character,
                                 date_period = R7::class_character,
                                 jurisdiction_place = R7::class_character,
                                 responsible_agency = R7::class_character,
                                 data_notes = R7::class_list,
                                 originator = R7::class_character,
                                 full_title = R7::class_character,
                                 short_title = R7::class_character,
                                 publication_facts = R7::class_character,
                                 source_text = R7::class_character,
                                 repo_citations = R7::class_list,
                                 notes = R7::class_list,
                                 media_links = R7::class_list
                               ))

class_record_repo <- R7::new_class("class_record_repo", parent = class_record_lin,
                               properties = list(
                                 name = R7::class_character,
                                 address = R7::new_property(R7::new_union(NULL, class_address)),
                                 notes = R7::class_list
                               ))

class_record_note <- R7::new_class("class_record_note", parent = class_record_lin,
                               properties = list(
                                 text = R7::class_character,
                                 citations = R7::class_list
                               ))


