
test_that("Function get_families_as_partner() @ L63", {
  sample555 <- read_gedcom(system.file("extdata", "555SAMPLE.GED", package = "gedcomS7"))
  
  expect_equal(get_famg_as_partner(sample555, "@I1@"), c("@F1@", "@F2@"))
  expect_equal(get_famg_as_partner(sample555, "@I2@"), "@F1@")
  expect_equal(get_famg_as_partner(sample555, "@I3@"), character())

  expect_equal(get_famg_as_child(sample555, "@I3@"), c("@F1@", "@F2@"))
  expect_equal(get_famg_as_child(sample555, "@I1@"), character())

  expect_equal(get_indi_partners(sample555, "@I1@"), "@I2@")
  expect_equal(get_indi_partners(sample555, "@I3@"), character())

  expect_error(get_indi_children(sample555, "@I4@"))
  expect_equal(get_indi_children(sample555, "@I1@"), "@I3@")
  expect_equal(get_indi_children(sample555, "@I3@"), character())

  expect_equal(get_indi_parents(sample555, "@I3@"), c("@I1@", "@I2@"))
  expect_equal(get_indi_parents(sample555, "@I1@"), character())

  expect_equal(get_indi_siblings(sample555, "@I3@"), character())

  expect_equal(get_famg_partners(sample555, "@F1@"), c("@I1@", "@I2@"))

  expect_equal(get_famg_children(sample555, "@F1@"), "@I3@")
  expect_equal(get_famg_children(sample555, "@F2@"), "@I3@")
  expect_equal(get_famg_children(sample555, "@F2@", birth_only = TRUE), character())

  expect_equal(get_supporting_recs(sample555, "@I1@"), c("@S1@", "@R1@"))

  expect_equal(get_descendants(sample555, "@I3@"), character())
  expect_equal(get_descendants(sample555, "@I1@"), "@I3@")
  expect_equal(get_descendants(sample555, "@I1@", TRUE), c("@I1@","@I3@"))
  expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE), c("@I2@","@I1@","@I3@"))
  expect_equal(get_descendants(sample555, "@I1@", TRUE, TRUE, TRUE), c("@F1@","@F2@","@I2@","@I1@","@I3@"))

  expect_equal(get_ancestors(sample555, "@I1@"), character())
  expect_equal(get_ancestors(sample555, "@I3@"), c("@I1@","@I2@"))
})
