
is_indi_rec <- function(record) S7::S7_inherits(record, IndividualRecord)
is_fam_rec <- function(record) S7::S7_inherits(record, FamilyRecord)
is_sour_rec <- function(record) S7::S7_inherits(record, SourceRecord)
is_repo_rec <- function(record) S7::S7_inherits(record, RepositoryRecord)
is_media_rec <- function(record) S7::S7_inherits(record, MediaRecord)
is_note_rec <- function(record) S7::S7_inherits(record, NoteRecord)
is_subm_rec <- function(record) S7::S7_inherits(record, SubmitterRecord)

is_indi_xref <- function(x, xref) xref %in% x@c_xrefs[["indi"]]
is_fam_xref <- function(x, xref) xref %in% x@c_xrefs[["fam"]]
is_sour_xref <- function(x, xref) xref %in% x@c_xrefs[["sour"]]
is_repo_xref <- function(x, xref) xref %in% x@c_xrefs[["repo"]]
is_media_xref <- function(x, xref) xref %in% x@c_xrefs[["media"]]
is_note_xref <- function(x, xref) xref %in% x@c_xrefs[["note"]]
is_subm_xref <- function(x, xref) xref %in% x@c_xrefs[["subm"]]

get_record_type <- function(record){
  
  if(S7::S7_inherits(record, IndividualRecord)){
    "indi"
  } else if(S7::S7_inherits(record, FamilyRecord)){
    "fam"
  } else if(S7::S7_inherits(record, SourceRecord)){
    "sour"
  } else if(S7::S7_inherits(record, RepositoryRecord)){
    "repo"
  } else if(S7::S7_inherits(record, MediaRecord)){
    "media"
  } else if(S7::S7_inherits(record, NoteRecord)){
    "note"
  } else if(S7::S7_inherits(record, SubmitterRecord)){
    "subm"
  } else {
    stop("Unrecognised record")
  }
  
}

check_indi_rec <- function(x, xref) if(!is_indi_xref(x, xref)) stop("The xref is not for an Individual record.")
check_fam_rec <- function(x, xref) if(!is_fam_xref(x, xref)) stop("The xref is not for a Family record.")

