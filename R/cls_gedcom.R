#' @include cls_validators.R
NULL

#' @include cls_locations.R cls_dates.R
class_gedcom_source <- S7::new_class(
  "class_gedcom_source",
  properties = list(
    product_id = S7::class_character,
    product_name = S7::class_character,
    product_version = S7::class_character,
    business_name = S7::class_character,
    business_address = NULL | class_address,
    phone_numbers = S7::class_character,
    emails = S7::class_character,
    faxes = S7::class_character,
    web_pages = S7::class_character,
    data_name = S7::class_character,
    data_pubdate = NULL | class_date_exact | S7::class_character,
    data_pubtime = NULL | class_time | S7::class_character,
    data_copyright = S7::class_character,
    
    as_ged = S7::new_property(
      S7::class_character,
      getter = function(self){
        c(
          sprintf("0 SOUR %s", self@product_id),
          sprintf("1 VERS %s", self@product_version),
          sprintf("1 NAME %s", self@product_name),
          sprintf("1 CORP %s", self@business_name),
          obj_to_ged(self@business_address) |> increase_level(by = 2),
          sprintf("2 PHON %s", self@phone_numbers),
          sprintf("2 EMAIL %s", self@emails),
          sprintf("2 FAX %s", self@faxes),
          sprintf("2 WWW %s", self@web_pages),
          sprintf("1 DATA %s", self@data_name),
          sprintf("2 DATE %s", datetime_to_val(self@data_pubdate)),
          sprintf("3 TIME %s", datetime_to_val(self@data_pubtime)),
          sprintf("2 COPR %s", self@data_copyright)
        )
      }
    )),
  validator = function(self){
    c(
      chk_input_size(self@product_id, "@product_id", 1, 1, 1),
      chk_input_size(self@product_name, "@product_name", 0, 1, 1),
      chk_input_size(self@product_version, "@product_version", 0, 1, 1),
      chk_input_size(self@business_name, "@business_name", 0, 1, 1),
      chk_input_size(self@business_address, "@business_address", 0, 1),
      chk_input_size(self@phone_numbers, "@phone_numbers", min_val = 1),
      chk_input_size(self@emails, "@emails", min_val = 1),
      chk_input_size(self@faxes, "@faxes", min_val = 1),
      chk_input_size(self@web_pages, "@web_pages", min_val = 1),
      chk_input_size(self@data_name, "@data_name", 0, 1, 1),
      chk_input_size(self@data_pubdate, "@data_pubdate", 0, 1),
      chk_input_size(self@data_pubtime, "@data_pubtime", 0, 1),
      chk_input_size(self@data_copyright, "@data_copyright", 0, 1, 1),
      chk_input_parents(self@product_version, "@product_version", self@product_id, "@product_id"),
      chk_input_parents(self@product_name, "@product_name", self@product_id, "@product_id"),
      chk_input_parents(self@business_name, "@business_name", self@product_id, "@product_id"),
      chk_input_parents(self@data_name, "@data_name", self@product_id, "@product_id"),
      chk_input_parents(self@business_address, "@business_address", self@business_name, "@business_name"),
      chk_input_parents(self@phone_numbers, "@phone_numbers", self@business_name, "@business_name"),
      chk_input_parents(self@emails, "@emails", self@business_name, "@business_name"),
      chk_input_parents(self@faxes, "@faxes", self@business_name, "@business_name"),
      chk_input_parents(self@web_pages, "@web_pages", self@business_name, "@business_name"),
      chk_input_parents(self@data_pubdate, "@data_pubdate", self@data_name, "@data_name"),
      chk_input_parents(self@data_copyright, "@data_copyright", self@data_name, "@data_name"),
      chk_input_parents(self@data_pubtime, "@data_pubtime", self@data_pubdate, "@data_pubdate"),
      chk_input_pattern(self@data_pubdate, "@data_pubdate", reg_date_exact()),
      chk_input_pattern(self@data_pubtime, "@data_pubtime", reg_time())
    )
  }
)

#' @include cls_common.R cls_dates.R cls_record.R
class_gedcomS7 <- S7::new_class(
  "class_gedcomS7",
  properties = list(
    gedcom_version = S7::class_character,
    ext_tags = S7::class_character,
    source = NULL | class_gedcom_source,
    destination = S7::class_character,
    creation_date = NULL | class_date_exact | S7::class_character,
    creation_time = NULL | class_time | S7::class_character,
    subm_uid = S7::class_character,
    gedcom_copyright = S7::class_character,
    default_language = S7::class_character,
    default_place_form = S7::class_character,
    notes = S7::class_list,
    note_uids = S7::class_character,
    
    update_change_dates = S7::new_property(S7::class_logical, default = FALSE),
    add_creation_dates = S7::new_property(S7::class_logical, default = FALSE),
    
    # Records
    subm = S7::class_list,
    indi = S7::class_list,
    fam = S7::class_list,
    sour = S7::class_list,
    repo = S7::class_list,
    media = S7::class_list,
    note = S7::class_list,
    
    uids = S7::new_property( #EVERY UID
      S7::class_list,
      getter = function(self){
        rec_types <- c("indi","fam","sour","repo","media","note","subm")
        rec_uids <- lapply(rec_types, \(rec_type){
          S7::prop(self, rec_type) |> 
            unlist() |> 
            grep(pattern = "^1 UID ", value = TRUE) |> 
            sub(pattern = "^1 UID ", replacement = "") |> 
            unique()
        })
        setNames(rec_uids, rec_types)
        rec_uids
      }
    ),
    
    as_ged = S7::new_property(
      S7::class_character, 
      getter = function(self){
        
        hd <- c(
          "0 HEAD",
          "1 GEDC",
          sprintf("2 VERS %s", self@gedcom_version),
          rep("1 SCHMA", length(self@ext_tags) > 0),
          sprintf("2 TAG %s", self@ext_tags),
          obj_to_ged(self@source) |> increase_level(by = 1),
          sprintf("1 DEST %s", self@destination),
          sprintf("1 DATE %s", date_to_val(self@creation_date)),
          sprintf("2 TIME %s", self@creation_time),
          sprintf("1 SUBM %s", self@xref_subm),
          sprintf("1 COPR %s", self@gedcom_copyright),
          sprintf("1 LANG %s", self@default_language),
          rep("1 PLAC", length(self@default_place_form) > 0),
          sprintf("2 FORM %s", self@default_place_form),
          lst_to_ged(self@notes) |> increase_level(by = 1),
          sprintf("1 SNOTE %s", self@note_uids)
        )
        
        c(
          hd,
          unlist(self@subm),
          unlist(self@indi),
          unlist(self@fam),
          unlist(self@sour),
          unlist(self@repo),
          unlist(self@media),
          unlist(self@note),
          "0 TRLR"
        ) |> unname()
      }
    )
    
  ),
  validator = function(self){
    c(
      chk_input_size(self@gedcom_version, "@gedcom_version", 1, 1),
      chk_input_size(self@ext_tags, "@ext_tags", 0, 0), # extension tags not supported
      chk_input_size(self@source, "@source", 0, 1),
      chk_input_size(self@destination, "@destination", 0, 1, 1),
      chk_input_size(self@creation_date, "@creation_date", 0, 1),
      chk_input_size(self@creation_time, "@creation_time", 0, 1),
      chk_input_size(self@subm_uid, "@subm_uid", 0, 1),
      chk_input_size(self@gedcom_copyright, "@gedcom_copyright", 0, 1, 1),
      chk_input_size(self@default_language, "@default_language", 0, 1, 1),
      #    chk_input_choice(self@default_language, "@default_language", val_languages()),#TODO
      chk_input_size(self@default_place_form, "@default_place_form", 0, 1, 1),
      chk_input_size(self@update_change_dates, "@update_change_dates", 1, 1),
      chk_input_size(self@add_creation_dates, "@add_creation_dates", 1, 1),
      chk_input_parents(self@creation_time, "@creation_time", self@creation_date, "@creation_date"),
      chk_input_pattern(self@gedcom_version,  "@gedcom_version", "^\\d+\\.\\d+\\.\\d+$"),
      chk_input_pattern(self@note_uids, "@note_uids", reg_uuid(TRUE)),
      chk_input_pattern(self@subm_uid, "@subm_uid", reg_uuid(TRUE)),
      chk_input_pattern(self@creation_date, "@creation_date", reg_date_exact()),
      chk_input_pattern(self@creation_time, "@creation_time", reg_time()),
      chk_input_S7classes(self@notes, "@notes", class_note),
      chk_input_S7classes(self@subm, "@subm", class_record_subm),
      chk_input_S7classes(self@indi, "@indi", class_record_indi),
      chk_input_S7classes(self@fam, "@fam", class_record_fam),
      chk_input_S7classes(self@sour, "@sour", class_record_sour),
      chk_input_S7classes(self@repo, "@repo", class_record_repo),
      chk_input_S7classes(self@media, "@media", class_record_media),
      chk_input_S7classes(self@note, "@note", class_record_note)
    )
  }
)


#' Create a new gedcom object
#'
#' @param my_language The primary language in which data will be stored. The language code should
#' adhere to BCP 47.
#'
#' @return A minimal gedcom S7 object.
#' @export
new_gedcomS7 <- function(my_language = "en"){
  
  sour <- class_gedcom_source(product_id = "https://github.com/jl5000/gedcomS7",
                              product_name = "The 'gedcomS7' package for the R language",
                              business_name = "Jamie Lendrum",
                              emails = "jalendrum@gmail.com")
  
  class_gedcomS7(gedcom_version = "7.0.11",
                 source = sour,
                 creation_date = date_exact_current(),
                 default_language = my_language)
}

