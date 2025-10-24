
is_gedcomS7 <- function(x) S7::S7_inherits(x, GedcomS7)

is_indi_rec <- function(record) S7::S7_inherits(record, IndividualRecord)
is_fam_rec <- function(record) S7::S7_inherits(record, FamilyRecord)
is_sour_rec <- function(record) S7::S7_inherits(record, SourceRecord)
is_repo_rec <- function(record) S7::S7_inherits(record, RepositoryRecord)
is_media_rec <- function(record) S7::S7_inherits(record, MediaRecord)
is_note_rec <- function(record) S7::S7_inherits(record, NoteRecord)
is_subm_rec <- function(record) S7::S7_inherits(record, SubmitterRecord)

is_type_xref <- function(x, xref, type){
  check_gedcom_obj(x)
  xref %in% x@records@XREFS[[val_record_types()[[type]]]]
}
is_indi_xref <- function(x, xref) is_type_xref(x, xref, "Individual")
is_fam_xref <- function(x, xref) is_type_xref(x, xref, "Family")
is_sour_xref <- function(x, xref) is_type_xref(x, xref, "Source")
is_repo_xref <- function(x, xref) is_type_xref(x, xref, "Repository")
is_media_xref <- function(x, xref) is_type_xref(x, xref, "Multimedia")
is_note_xref <- function(x, xref) is_type_xref(x, xref, "Note")
is_subm_xref <- function(x, xref) is_type_xref(x, xref, "Submitter")

#' @tests
#' expect_error(get_record_type(1), regexp = "Input is not a gedcomS7 Record")
#' expect_equal(get_record_type(IndividualRecord()), "INDI")
#' expect_equal(get_record_type(FamilyRecord()), "FAM")
#' expect_equal(get_record_type(SourceRecord()), "SOUR")
#' expect_equal(get_record_type(RepositoryRecord(repo_name = "Lib")), "REPO")
#' expect_equal(get_record_type(SubmitterRecord(subm_name = "subm")), "SUBM")
#' expect_equal(get_record_type(NoteRecord(text = "text")), "SNOTE")
#' expect_equal(get_record_type(MediaRecord(files = MediaFile("file.ext", media_type = "type"))), "OBJE")
get_record_type <- function(record){
  stopifnot("Input is not a gedcomS7 Record" = "gedcomS7::Record" %in% class(record))
  parse_line_tag(record@GEDCOM[1])
}

check_gedcom_obj <- function(x) stopifnot("Not a GEDCOM object" = is_gedcomS7(x))
check_indi_rec <- function(x, xref) stopifnot("The xref is not for an Individual record." = is_indi_xref(x, xref))
check_fam_rec <- function(x, xref) stopifnot("The xref is not for a Family record." = is_fam_xref(x, xref))

