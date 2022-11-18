
class_repository_citation <- R7::new_class("class_repository_citation",
                                       properties = list(
                                         xref_repo = R7::class_character,
                                         source_call_number = R7::class_character
                                       ))

class_association <- R7::new_class("class_association")

class_family_link <- R7::new_class("family_links", abstract = TRUE,
                                properties = list(
                                  xref_fam = R7::class_character,
                                  notes = R7::class_list
                                ))


class_child_to_family_link <- R7::new_class("class_child_to_family_link", parent = class_family_link,
                                        properties = list(
                                          pedigree = R7::class_character
                                        ))

class_spouse_to_family_link <- R7::new_class("class_spouse_to_family_link", parent = class_family_link)
