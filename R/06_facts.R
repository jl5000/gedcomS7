
class_event_detail <- R7::new_class("class_event_detail",
                          properties = list(
                            type = R7::class_character,
                            date = R7::class_character,
                            place = class_place,
                            address = class_address,
                            agency = R7::class_character,
                            relig_affil = R7::class_character,
                            cause = R7::class_character,
                            notes = R7::class_list,
                            citations = R7::class_list,
                            media_links = R7::class_list
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

class_famg_fact <- R7::new_class("class_famg_fact", parent = class_event_detail, 
                       properties = list(
                         husband_age = R7::class_character,
                         wife_age = R7::class_character
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

class_indi_fact <- R7::new_class("class_indi_fact", parent = class_event_detail, 
                       properties = list(
                         age = R7::class_character
                       ),
                       validator = function(self) {
                         c(
                           chk_input_size(self@age, "@age", 0, 1, 2, 13),
                           chk_input_pattern(self@age, "@age", reg_age_at_event())
                         )
                       }
)

# Individual attributes
caste <- R7::new_class("caste", parent = class_indi_fact,
                   properties = list(caste_name = R7::class_character))
academic_achievement <- R7::new_class("academic_achievement", parent = class_indi_fact,
                                  properties = list(achievement = R7::class_character))
national_id_number <- R7::new_class("national_id_number", parent = class_indi_fact,
                                properties = list(id_number = R7::class_character))
nationality <- R7::new_class("nationality", parent = class_indi_fact,
                         properties = list(nat = R7::class_character))
nobility_title <- R7::new_class("nobility_title", parent = class_indi_fact,
                            properties = list(title = R7::class_character))
num_children <- R7::new_class("num_children", parent = class_indi_fact,
                          properties = list(num_chil = R7::class_integer))
num_relationships <- R7::new_class("num_relationships", parent = class_indi_fact,
                               properties = list(num_rel = R7::class_integer))
occupation <- R7::new_class("occupation", parent = class_indi_fact,
                        properties = list(occu = R7::class_character))
physical_desc <- R7::new_class("physical_desc", parent = class_indi_fact,
                           properties = list(phys_desc = R7::class_character))
property <- R7::new_class("property", parent = class_indi_fact,
                      properties = list(prop = R7::class_character))
religion <- R7::new_class("religion", parent = class_indi_fact,
                      properties = list(relig = R7::class_character))
residence <- R7::new_class("residence", parent = class_indi_fact)
indi_attribute <- R7::new_class("indi_attribute", parent = class_indi_fact,
                            properties = list(attr_descriptor = R7::class_character))

# Individual events
adoption <- R7::new_class("adoption", parent = class_indi_fact,
                      properties = list(
                        famc_xref = R7::class_character,
                        adopting_parent = R7::class_character
                      ))
adult_christening <- R7::new_class("adult_christening", parent = class_indi_fact)
baptism <- R7::new_class("baptism", parent = class_indi_fact)
bar_mitzvah <- R7::new_class("bar_mitzvah", parent = class_indi_fact)
bas_mitzvah <- R7::new_class("bas_mitzvah", parent = class_indi_fact)
birth <- R7::new_class("birth", parent = class_indi_fact,
                   properties = list(famc_xref = R7::class_character))
burial <- R7::new_class("burial", parent = class_indi_fact)
census <- R7::new_class("census", parent = class_indi_fact)
christening <- R7::new_class("christening", parent = class_indi_fact)
confirmation <- R7::new_class("confirmation", parent = class_indi_fact)
cremation <- R7::new_class("cremation", parent = class_indi_fact)
death <- R7::new_class("death", parent = class_indi_fact)
emigration <- R7::new_class("emigration", parent = class_indi_fact)
first_communion <- R7::new_class("first_communion", parent = class_indi_fact)
graduation <- R7::new_class("graduation", parent = class_indi_fact)
immigration <- R7::new_class("immigration", parent = class_indi_fact)
naturalization <- R7::new_class("naturalization", parent = class_indi_fact)
probate <- R7::new_class("probate", parent = class_indi_fact)
retirement <- R7::new_class("retirement", parent = class_indi_fact)
will <- R7::new_class("will", parent = class_indi_fact)
indi_event <- R7::new_class("indi_event", parent = class_indi_fact,
                        properties = list(event_descriptor = R7::class_character))

# Family events
annulment <- R7::new_class("annulment", parent = class_famg_fact)
divorce <- R7::new_class("divorce", parent = class_famg_fact)
divorce_filed <- R7::new_class("divorce_filed", parent = class_famg_fact)
engagement <- R7::new_class("engagement", parent = class_famg_fact)
marr_banns <- R7::new_class("marr_banns", parent = class_famg_fact)
marr_contract <- R7::new_class("marr_contract", parent = class_famg_fact)
marr_license <- R7::new_class("marr_license", parent = class_famg_fact)
marr_settlement <- R7::new_class("marr_settlement", parent = class_famg_fact)
relationship <- R7::new_class("relationship", parent = class_famg_fact)
famg_event <- R7::new_class("will", parent = class_famg_fact,
                        properties = list(event_descriptor = R7::class_character))

