
is_indi_rec <- function(record) R7::R7_inherits(record, class_record_indi)
is_famg_rec <- function(record) R7::R7_inherits(record, class_record_famg)
is_sour_rec <- function(record) R7::R7_inherits(record, class_record_sour)
is_repo_rec <- function(record) R7::R7_inherits(record, class_record_repo)
is_media_rec <- function(record) R7::R7_inherits(record, class_record_media)
is_note_rec <- function(record) R7::R7_inherits(record, class_record_note)

is_indi_xref <- function(x, xref) xref %in% names(x@indi)
is_famg_xref <- function(x, xref) xref %in% names(x@famg)
is_sour_xref <- function(x, xref) xref %in% names(x@sour)
is_repo_xref <- function(x, xref) xref %in% names(x@repo)
is_media_xref <- function(x, xref) xref %in% names(x@media)
is_note_xref <- function(x, xref) xref %in% names(x@note)