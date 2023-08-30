
#' Create an individual record object
#' 
#' @inheritParams prop_definitions 
#' @return An S7 object representing a GEDCOM INDIVIDUAL_RECORD.
#' @export
#' @include cls_record.R cls_personal_name.R cls_fact.R cls_non_event.R cls_association.R cls_note.R
#' cls_citation.R cls_media_link.R
class_record_indi <- S7::new_class(
  "class_record_indi", 
  package = "gedcomS7",
  parent = class_record,
  properties = list(
    personal_names = S7::class_list | class_personal_name | S7::class_character,
    sex = S7::new_property(S7::class_character, default = "U"),
    facts = S7::class_list,
    non_events = S7::class_list,
    family_links_as_child = S7::class_list | class_child_family_link | S7::class_character,
    family_links_as_spouse = S7::class_list | class_spouse_family_link | S7::class_character,
    subm_xrefs = S7::class_character,
    associations = S7::class_list,
    alia_xrefs = S7::class_character,
    anci_xrefs = S7::class_character,
    desi_xrefs = S7::class_character,
    note_xrefs = S7::class_character,
    notes = S7::class_list | class_note | S7::class_character,
    citations = S7::class_list | class_citation | S7::class_character,
    media_links = S7::class_list | class_media_link | S7::class_character,
    
    primary_name = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@personal_names) == 0){
          character()
        } else {
          self@personal_names[[1]]@name@full |>
            gsub(pattern = "/", replacement = "")
        }
      }),
    
    all_names = S7::new_property(
      S7::class_character,
      getter = function(self){
        sapply(self@personal_names, \(nm){
          gsub(nm@name@full, pattern = "/", replacement = "")
        }, USE.NAMES = FALSE)
      }),
    
    desc_short = S7::new_property(
      S7::class_character,
      getter = function(self){
        if(length(self@primary_name) == 0){
          name <- "Unnamed individual"
        } else {
          name <- self@primary_name
        }
        paste0("Individual ", self@xref, ", ", name)
      }),
    
    birth_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "BIRT") return(fact@fact_date)
        }
        character()
      }),
    
    birth_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "BIRT") return(fact@fact_location)
        }
        character()
      }),
    
    is_alive = S7::new_property(
      S7::class_logical,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(FALSE)
        }
        TRUE
      }),
    
    death_date = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(fact@fact_date)
        }
        character()
      }),
    
    death_place = S7::new_property(
      S7::class_character,
      getter = function(self){
        for(fact in self@facts){
          if(fact@fact == "DEAT") return(fact@fact_location)
        }
        character()
      }),
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 %s INDI", self@xref),
          sprintf("1 RESN %s", self@restrictions),
          obj_to_ged(self@personal_names, "NAME") |> increase_level(by = 1),
          sprintf("1 SEX %s", self@sex),
          obj_to_ged(self@facts) |> increase_level(by = 1),
          obj_to_ged(self@non_events) |> increase_level(by = 1),
          obj_to_ged(self@family_links_as_child, "FAMC") |> increase_level(by = 1),
          obj_to_ged(self@family_links_as_spouse, "FAMS") |> increase_level(by = 1),
          sprintf("1 SUBM %s", self@subm_xrefs),
          obj_to_ged(self@associations) |> increase_level(by = 1),
          named_vec_to_ged(self@alia_xrefs, "ALIA", "PHRASE") |> increase_level(by = 1),
          sprintf("1 ANCI %s", self@anci_xrefs),
          sprintf("1 DESI %s", self@desi_xrefs),
          self@ids |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_xrefs),
          obj_to_ged(self@notes, "NOTE") |> increase_level(by = 1),
          obj_to_ged(self@citations, "SOUR") |> increase_level(by = 1),
          obj_to_ged(self@media_links, "OBJE") |> increase_level(by = 1),
          obj_to_ged(self@updated) |> increase_level(by = 1),
          obj_to_ged(self@created) |> increase_level(by = 1)
        )
      })
  ),
  validator = function(self){
    c(
      chk_input_size(self@sex, "@sex", 0, 1),
      chk_input_choice(self@sex, "@sex", val_sexes()),
      chk_input_pattern(self@subm_xrefs, "@subm_xrefs", reg_xref(TRUE)),
      chk_input_pattern(self@alia_xrefs, "@alia_xrefs", reg_xref(TRUE)),
      chk_input_pattern(self@anci_xrefs, "@anci_xrefs", reg_xref(TRUE)),
      chk_input_pattern(self@desi_xrefs, "@desi_xrefs", reg_xref(TRUE)),
      chk_input_pattern(self@note_xrefs, "@note_xrefs", reg_xref(TRUE)),
      chk_input_S7classes(self@personal_names, "@personal_names", class_personal_name),
      chk_input_S7classes(self@facts, "@facts", class_fact_indi),
      chk_input_S7classes(self@non_events, "@non_events", class_non_event),
      chk_input_S7classes(self@family_links_as_child, "@family_links_as_child", class_child_family_link),
      chk_input_S7classes(self@family_links_as_spouse, "@family_links_as_spouse", class_spouse_family_link),
      chk_input_S7classes(self@associations, "@associations", class_association),
      chk_input_S7classes(self@notes, "@notes", class_note, ".+"),
      chk_input_S7classes(self@citations, "@citations", class_citation, reg_xref(TRUE)),
      chk_input_S7classes(self@media_links, "@media_links", class_media_link, reg_xref(TRUE))
    )
  }
)
