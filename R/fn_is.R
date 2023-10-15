
is_indi_rec <- function(record) S7::S7_inherits(record, class_record_indi)
is_famg_rec <- function(record) S7::S7_inherits(record, class_record_fam)
is_sour_rec <- function(record) S7::S7_inherits(record, class_record_sour)
is_repo_rec <- function(record) S7::S7_inherits(record, class_record_repo)
is_media_rec <- function(record) S7::S7_inherits(record, class_record_media)
is_note_rec <- function(record) S7::S7_inherits(record, class_record_note)
is_subm_rec <- function(record) S7::S7_inherits(record, class_record_subm)

is_indi_xref <- function(x, xref) xref %in% x@xrefs[["indi"]]
is_fam_xref <- function(x, xref) xref %in% x@xrefs[["fam"]]
is_sour_xref <- function(x, xref) xref %in% x@xrefs[["sour"]]
is_repo_xref <- function(x, xref) xref %in% x@xrefs[["repo"]]
is_media_xref <- function(x, xref) xref %in% x@xrefs[["media"]]
is_note_xref <- function(x, xref) xref %in% x@xrefs[["note"]]
is_subm_xref <- function(x, xref) xref %in% x@xrefs[["subm"]]

get_record_type <- function(record){
  
  if(S7::S7_inherits(record, class_record_indi)){
    "indi"
  } else if(S7::S7_inherits(record, class_record_fam)){
    "fam"
  } else if(S7::S7_inherits(record, class_record_sour)){
    "sour"
  } else if(S7::S7_inherits(record, class_record_repo)){
    "repo"
  } else if(S7::S7_inherits(record, class_record_media)){
    "media"
  } else if(S7::S7_inherits(record, class_record_note)){
    "note"
  } else if(S7::S7_inherits(record, class_record_subm)){
    "subm"
  } else {
    stop("Unrecognised record")
  }
  
}

check_indi_rec <- function(x, xref) if(!is_indi_xref(x, xref)) stop("The xref is not for an Individual record.")
check_fam_rec <- function(x, xref) if(!is_fam_xref(x, xref)) stop("The xref is not for a Family record.")

