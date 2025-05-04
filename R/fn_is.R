
is_indi_rec <- function(record) S7::S7_inherits(record, IndividualRecord)
is_fam_rec <- function(record) S7::S7_inherits(record, FamilyRecord)
is_sour_rec <- function(record) S7::S7_inherits(record, SourceRecord)
is_repo_rec <- function(record) S7::S7_inherits(record, RepositoryRecord)
is_media_rec <- function(record) S7::S7_inherits(record, MediaRecord)
is_note_rec <- function(record) S7::S7_inherits(record, NoteRecord)
is_subm_rec <- function(record) S7::S7_inherits(record, SubmitterRecord)

is_indi_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Individual"]]]]
is_fam_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Family"]]]]
is_sour_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Source"]]]]
is_repo_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Repository"]]]]
is_media_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Multimedia"]]]]
is_note_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Note"]]]]
is_subm_xref <- function(x, xref) xref %in% x@records@XREFS[[val_record_types()[["Submitter"]]]]

#' @tests
#' expect_error(get_record_type(1), regexp = "Unrecognised record")
#' expect_equal(get_record_type(IndividualRecord()), "INDI")
#' expect_equal(get_record_type(FamilyRecord()), "FAM")
#' expect_equal(get_record_type(SourceRecord()), "SOUR")
#' expect_equal(get_record_type(RepositoryRecord(repo_name = "Lib")), "REPO")
#' expect_equal(get_record_type(SubmitterRecord(subm_name = "subm")), "SUBM")
#' expect_equal(get_record_type(NoteRecord(text = "text")), "SNOTE")
#' expect_equal(get_record_type(MediaRecord(files = MediaFile("file.ext", media_type = "type"))), "OBJE")
get_record_type <- function(record){
  
  if(is_indi_rec(record)){
    val_record_types()[["Individual"]]
  } else if(is_fam_rec(record)){
    val_record_types()[["Family"]]
  } else if(is_sour_rec(record)){
    val_record_types()[["Source"]]
  } else if(is_repo_rec(record)){
    val_record_types()[["Repository"]]
  } else if(is_media_rec(record)){
    val_record_types()[["Multimedia"]]
  } else if(is_note_rec(record)){
    val_record_types()[["Note"]]
  } else if(is_subm_rec(record)){
    val_record_types()[["Submitter"]]
  } else {
    stop("Unrecognised record")
  }
  
}

check_indi_rec <- function(x, xref) if(!is_indi_xref(x, xref)) stop("The xref is not for an Individual record.")
check_fam_rec <- function(x, xref) if(!is_fam_xref(x, xref)) stop("The xref is not for a Family record.")

