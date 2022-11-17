
class_repository_citation <- new_class("class_repository_citation",
                                       properties = list(
                                         xref_repo = class_character,
                                         source_call_number = class_character
                                       ))

class_association <- new_class("class_association")

class_family_link <- new_class("family_links", abstract = TRUE,
                                properties = list(
                                  xref_fam = class_character,
                                  notes = class_list
                                ))


class_child_to_family_link <- new_class("class_child_to_family_link", parent = class_family_link,
                                        properties = list(
                                          pedigree = class_character
                                        ))

class_spouse_to_family_link <- new_class("class_spouse_to_family_link", parent = class_family_link)
