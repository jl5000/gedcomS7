
class_event_detail <- new_class("class_event_detail",
                          properties = list(
                            type = class_character,
                            date = class_character,
                            place = class_place,
                            address = class_address,
                            agency = class_character,
                            relig_affil = class_character,
                            cause = class_character,
                            notes = class_list,
                            citations = class_list,
                            media_links = class_list
                          ),
                          validator = function(self) {
                            c(
                              chk_input_size(self@type, "@type", 0, 1, 1, 90),
                              chk_input_size(self@date, "@date", 0, 1, 1, 35),
                              chk_input_size(self@place, "@place", 0, 1),
                              chk_input_size(self@address, "@address", 0, 1),
                              chk_input_size(self@agency, "@agency", 0, 1, 1, 120),
                              chk_input_size(self@relig_affil, "@relig_affil", 0, 1, 1, 90),
                              chk_input_size(self@cause, "@cause", 0, 1, 1, 90)
                            )
                          }
)

class_famg_fact <- new_class("class_famg_fact", parent = class_event_detail, 
                       properties = list(
                         husband_age = class_character,
                         wife_age = class_character
                       ),
                       validator = function(self) {
                         c(
                           chk_input_size(self@husband_age, "@husband_age", 0, 1, 2, 13),
                           chk_input_size(self@wife_age, "@wife_age", 0, 1, 2, 13),
                           chk_input_pattern(self@husband_age, "@husband_age", reg_age_at_event()),
                           chk_input_pattern(self@wife_age, "@wife_age", reg_age_at_event())
                         )
                       }
)

class_indi_fact <- new_class("class_indi_fact", parent = class_event_detail, 
                       properties = list(
                         age = class_character
                       ),
                       validator = function(self) {
                         c(
                           chk_input_size(self@age, "@age", 0, 1, 2, 13),
                           chk_input_pattern(self@age, "@age", reg_age_at_event())
                         )
                       }
)

# Individual attributes
caste <- new_class("caste", parent = class_indi_fact,
                   properties = list(caste_name = class_character))
academic_achievement <- new_class("academic_achievement", parent = class_indi_fact,
                                  properties = list(achievement = class_character))
national_id_number <- new_class("national_id_number", parent = class_indi_fact,
                                properties = list(id_number = class_character))
nationality <- new_class("nationality", parent = class_indi_fact,
                         properties = list(nat = class_character))
nobility_title <- new_class("nobility_title", parent = class_indi_fact,
                            properties = list(title = class_character))
num_children <- new_class("num_children", parent = class_indi_fact,
                          properties = list(num_chil = class_integer))
num_relationships <- new_class("num_relationships", parent = class_indi_fact,
                               properties = list(num_rel = class_integer))
occupation <- new_class("occupation", parent = class_indi_fact,
                        properties = list(occu = class_character))
physical_desc <- new_class("physical_desc", parent = class_indi_fact,
                           properties = list(phys_desc = class_character))
property <- new_class("property", parent = class_indi_fact,
                      properties = list(prop = class_character))
religion <- new_class("religion", parent = class_indi_fact,
                      properties = list(relig = class_character))
residence <- new_class("residence", parent = class_indi_fact)
indi_attribute <- new_class("indi_attribute", parent = class_indi_fact,
                            properties = list(attr_descriptor = class_character))

# Individual events
adoption <- new_class("adoption", parent = class_indi_fact,
                      properties = list(
                        famc_xref = class_character,
                        adopting_parent = class_character
                      ))
adult_christening <- new_class("adult_christening", parent = class_indi_fact)
baptism <- new_class("baptism", parent = class_indi_fact)
bar_mitzvah <- new_class("bar_mitzvah", parent = class_indi_fact)
bas_mitzvah <- new_class("bas_mitzvah", parent = class_indi_fact)
birth <- new_class("birth", parent = class_indi_fact,
                   properties = list(famc_xref = class_character))
burial <- new_class("burial", parent = class_indi_fact)
census <- new_class("census", parent = class_indi_fact)
christening <- new_class("christening", parent = class_indi_fact)
confirmation <- new_class("confirmation", parent = class_indi_fact)
cremation <- new_class("cremation", parent = class_indi_fact)
death <- new_class("death", parent = class_indi_fact)
emigration <- new_class("emigration", parent = class_indi_fact)
first_communion <- new_class("first_communion", parent = class_indi_fact)
graduation <- new_class("graduation", parent = class_indi_fact)
immigration <- new_class("immigration", parent = class_indi_fact)
naturalization <- new_class("naturalization", parent = class_indi_fact)
probate <- new_class("probate", parent = class_indi_fact)
retirement <- new_class("retirement", parent = class_indi_fact)
will <- new_class("will", parent = class_indi_fact)
indi_event <- new_class("indi_event", parent = class_indi_fact,
                        properties = list(event_descriptor = class_character))

# Family events
annulment <- new_class("annulment", parent = class_famg_fact)
divorce <- new_class("divorce", parent = class_famg_fact)
divorce_filed <- new_class("divorce_filed", parent = class_famg_fact)
engagement <- new_class("engagement", parent = class_famg_fact)
marr_banns <- new_class("marr_banns", parent = class_famg_fact)
marr_contract <- new_class("marr_contract", parent = class_famg_fact)
marr_license <- new_class("marr_license", parent = class_famg_fact)
marr_settlement <- new_class("marr_settlement", parent = class_famg_fact)
relationship <- new_class("relationship", parent = class_famg_fact)
famg_event <- new_class("will", parent = class_famg_fact,
                        properties = list(event_descriptor = class_character))

