
.onAttach <- function(libname, pkgname) {
  
    packageStartupMessage(
      "When importing existing GEDCOM files, you should ensure that they ",
      "are error free.\nThis package assumes imported GEDCOM files are valid and ",
      "very few validation checks are carried out.\nSeveral GEDCOM validators are available, ",
      "including an online validator at https://ged-inline.org/"
    )
    
  if (getRversion() < "4.3.0")
    require(S7)
}

.onLoad <- function(...) {
  S7::methods_register()
}
