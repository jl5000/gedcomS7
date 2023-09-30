
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


