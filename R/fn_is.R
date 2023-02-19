
is_indi_rec <- function(record) S7::S7_inherits(record, class_record_indi)
is_famg_rec <- function(record) S7::S7_inherits(record, class_record_fam)
is_sour_rec <- function(record) S7::S7_inherits(record, class_record_sour)
is_repo_rec <- function(record) S7::S7_inherits(record, class_record_repo)
is_media_rec <- function(record) S7::S7_inherits(record, class_record_media)
is_note_rec <- function(record) S7::S7_inherits(record, class_record_note)
is_subm_rec <- function(record) S7::S7_inherits(record, class_record_subm)

is_indi_uid <- function(x, uid) uid %in% x@uids[["indi"]]
is_fam_uid <- function(x, uid) uid %in% x@uids[["fam"]]
is_sour_uid <- function(x, uid) uid %in% x@uids[["sour"]]
is_repo_uid <- function(x, uid) uid %in% x@uids[["repo"]]
is_media_uid <- function(x, uid) uid %in% x@uids[["media"]]
is_note_uid <- function(x, uid) uid %in% x@uids[["note"]]
is_subm_uid <- function(x, uid) uid %in% x@uids[["subm"]]

is_spouse_link <- function(lnk) class(lnk)[1] == "class_spouse_family_link"
is_child_link <- function(lnk) S7::S7_inherits(lnk, class_child_family_link_biol)
is_birth_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_biol"
is_adop_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_adop"
is_fost_child_link <- function(lnk) class(lnk)[1] == "class_child_family_link_fost"

