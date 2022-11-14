
event_detail <- new_class("event_detail",
                          properties = list(
                            type = class_character,
                            date = class_character,
                            place = class_any,###
                            address = class_any,###
                            agency = class_character,
                            relig_affil = class_character,
                            cause = class_character,
                            notes = class_list,
                            citations = class_list,
                            media_links = class_list
                          ),
                          validator = function(self) {
                            c(
                              chk_input_size(self@type, "@type", 1, 1, 90),
                              chk_input_size(self@date, "@date", 1, 1, 35),
                              chk_input_size(self@agency, "@agency", 1, 1, 120),
                              chk_input_size(self@relig_affil, "@relig_affil", 1, 1, 90),
                              chk_input_size(self@cause, "@cause", 1, 1, 90)
                            )
                          }
)

famg_fact <- new_class("famg_fact", parent = event_detail,
                       properties = list(
                         husband_age = class_character,
                         wife_age = class_character
                       ),
                       validator = function(self) {
                         c(
                           chk_input_size(self@husband_age, "@husband_age", 1, 2, 13),
                           chk_input_size(self@wife_age, "@wife_age", 1, 2, 13)
                         )
                       }
)

indi_fact <- new_class("indi_fact", parent = event_detail,
                       properties = list(
                         age = class_character
                       ),
                       validator = function(self) {
                         c(
                           chk_input_size(self@age, "@age", 1, 2, 13)
                         )
                       }
)

# Individual attributes
caste <- new_class("caste", parent = indi_fact,
                   properties = list(caste_name = class_character))
academic_achievement <- new_class("academic_achievement", parent = indi_fact,
                                  properties = list(achievement = class_character))
national_id_number <- new_class("national_id_number", parent = indi_fact,
                                properties = list(id_number = class_character))
nationality <- new_class("nationality", parent = indi_fact,
                         properties = list(nat = class_character))
nobility_title <- new_class("nobility_title", parent = indi_fact,
                            properties = list(title = class_character))
num_children <- new_class("num_children", parent = indi_fact,
                          properties = list(num_chil = class_integer))
num_relationships <- new_class("num_relationships", parent = indi_fact,
                               properties = list(num_rel = class_integer))
occupation <- new_class("occupation", parent = indi_fact,
                        properties = list(occu = class_character))
physical_desc <- new_class("physical_desc", parent = indi_fact,
                           properties = list(phys_desc = class_character))
property <- new_class("property", parent = indi_fact,
                      properties = list(prop = class_character))
religion <- new_class("religion", parent = indi_fact,
                      properties = list(relig = class_character))
residence <- new_class("residence", parent = indi_fact)
indi_attribute <- new_class("indi_attribute", parent = indi_fact,
                            properties = list(attr_descriptor = class_character))

# Individual events
adoption <- new_class("adoption", parent = indi_fact,
                      properties = list(
                        famc_xref = class_character,
                        adopting_parent = class_character
                      ))
adult_christening <- new_class("adult_christening", parent = indi_fact)
baptism <- new_class("baptism", parent = indi_fact)
bar_mitzvah <- new_class("bar_mitzvah", parent = indi_fact)
bas_mitzvah <- new_class("bas_mitzvah", parent = indi_fact)
birth <- new_class("birth", parent = indi_fact,
                   properties = list(famc_xref = class_character))
burial <- new_class("burial", parent = indi_fact)
census <- new_class("census", parent = indi_fact)
christening <- new_class("christening", parent = indi_fact)
confirmation <- new_class("confirmation", parent = indi_fact)
cremation <- new_class("cremation", parent = indi_fact)
death <- new_class("death", parent = indi_fact)
emigration <- new_class("emigration", parent = indi_fact)
first_communion <- new_class("first_communion", parent = indi_fact)
graduation <- new_class("graduation", parent = indi_fact)
immigration <- new_class("immigration", parent = indi_fact)
naturalization <- new_class("naturalization", parent = indi_fact)
probate <- new_class("probate", parent = indi_fact)
retirement <- new_class("retirement", parent = indi_fact)
will <- new_class("will", parent = indi_fact)
indi_event <- new_class("indi_event", parent = indi_fact,
                        properties = list(event_descriptor = class_character))

# Family events
annulment <- new_class("annulment", parent = famg_fact)
divorce <- new_class("divorce", parent = famg_fact)
divorce_filed <- new_class("divorce_filed", parent = famg_fact)
engagement <- new_class("engagement", parent = famg_fact)
marr_banns <- new_class("marr_banns", parent = famg_fact)
marr_contract <- new_class("marr_contract", parent = famg_fact)
marr_license <- new_class("marr_license", parent = famg_fact)
marr_settlement <- new_class("marr_settlement", parent = famg_fact)
relationship <- new_class("relationship", parent = famg_fact)
famg_event <- new_class("will", parent = famg_fact,
                        properties = list(event_descriptor = class_character))

