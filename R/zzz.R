
.onAttach <- function(libname, pkgname) {
  
    packageStartupMessage(
      "When importing existing GEDCOM files, you should ensure that they ",
      "are error free.\nThis package assumes imported GEDCOM files are valid and ",
      "very few validation checks are carried out.\nSeveral GEDCOM validators are available, ",
      "including an online validator at http://ged-inline.elasticbeanstalk.com/"
    )
    
}

.pkgenv <- new.env(parent=emptyenv())

.pkgenv$gedcom_phys_value_limit <- 248
.pkgenv$gedcom_line_length_limit <- 255

.pkgenv$BOM_UTF8 <- c("ef", "bb", "bf")
.pkgenv$BOM_UTF16_LE <- c("ff", "fe")
.pkgenv$BOM_UTF16_BE <- c("fe", "ff")