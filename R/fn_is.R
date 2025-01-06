
is_indi_rec <- function(record) S7::S7_inherits(record, IndividualRecord)
is_fam_rec <- function(record) S7::S7_inherits(record, FamilyRecord)
is_sour_rec <- function(record) S7::S7_inherits(record, SourceRecord)
is_repo_rec <- function(record) S7::S7_inherits(record, RepositoryRecord)
is_media_rec <- function(record) S7::S7_inherits(record, MediaRecord)
is_note_rec <- function(record) S7::S7_inherits(record, NoteRecord)
is_subm_rec <- function(record) S7::S7_inherits(record, SubmitterRecord)

is_indi_xref <- function(x, xref) xref %in% x@records@XREFS[["INDI"]]
is_fam_xref <- function(x, xref) xref %in% x@records@XREFS[["FAM"]]
is_sour_xref <- function(x, xref) xref %in% x@records@XREFS[["SOUR"]]
is_repo_xref <- function(x, xref) xref %in% x@records@XREFS[["REPO"]]
is_media_xref <- function(x, xref) xref %in% x@records@XREFS[["OBJE"]]
is_note_xref <- function(x, xref) xref %in% x@records@XREFS[["SNOTE"]]
is_subm_xref <- function(x, xref) xref %in% x@records@XREFS[["SUBM"]]

get_record_type <- function(record){
  
  if(S7::S7_inherits(record, IndividualRecord)){
    "INDI"
  } else if(S7::S7_inherits(record, FamilyRecord)){
    "FAM"
  } else if(S7::S7_inherits(record, SourceRecord)){
    "SOUR"
  } else if(S7::S7_inherits(record, RepositoryRecord)){
    "REPO"
  } else if(S7::S7_inherits(record, MediaRecord)){
    "OBJE"
  } else if(S7::S7_inherits(record, NoteRecord)){
    "SNOTE"
  } else if(S7::S7_inherits(record, SubmitterRecord)){
    "SUBM"
  } else {
    stop("Unrecognised record")
  }
  
}

check_indi_rec <- function(x, xref) if(!is_indi_xref(x, xref)) stop("The xref is not for an Individual record.")
check_fam_rec <- function(x, xref) if(!is_fam_xref(x, xref)) stop("The xref is not for a Family record.")

