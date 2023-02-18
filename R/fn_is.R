
is_indi_rec <- function(record) S7::S7_inherits(record, class_record_indi)
is_famg_rec <- function(record) S7::S7_inherits(record, class_record_fam)
is_sour_rec <- function(record) S7::S7_inherits(record, class_record_sour)
is_repo_rec <- function(record) S7::S7_inherits(record, class_record_repo)
is_media_rec <- function(record) S7::S7_inherits(record, class_record_media)
is_note_rec <- function(record) S7::S7_inherits(record, class_record_note)

is_indi_xref <- function(x, xref) xref %in% names(x@indi)
is_famg_xref <- function(x, xref) xref %in% names(x@famg)
is_sour_xref <- function(x, xref) xref %in% names(x@sour)
is_repo_xref <- function(x, xref) xref %in% names(x@repo)
is_media_xref <- function(x, xref) xref %in% names(x@media)
is_note_xref <- function(x, xref) xref %in% names(x@note)

is_spouse_link <- function(lnk) class(lnk)[1] == "class_spouse_family_link"
is_child_link <- function(lnk) S7::S7_inherits(lnk, class_child_family_link_biol)
is_birth_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_biol"
is_adop_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_adop"
is_fost_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_fost"

