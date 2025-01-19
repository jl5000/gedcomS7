filepath="/home/jamie/Documents/R/gedcomS7/tests/testthat/maximal70.ged"
testthat::test_that("test header", {
  testthat::expect_no_error(
    suppressWarnings(read_gedcom(filepath))
  )
})

ged <- read_gedcom(filepath)

for(rec_type in names(ged@records@XREFS)){
  testthat::test_that(paste("test records", rec_type), {
    for(xref in ged@records@XREFS[[rec_type]]){
      testthat::expect_no_error(
        suppressWarnings(pull_record(ged, xref))
      )
    }
  })
}
