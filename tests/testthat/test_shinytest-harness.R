context("shinytest-harness")
# This file is for testing the applications in the apps/ directory.
library(shinytest)

test_that("load and run simple simulation", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(
    #relative to testthat root
    testApp('apps/rapidPBPK_pop', compareImages = F)
    # relative to package base
    #testApp('tests/testthat/apps/rapidPBPK_pop', compareImages = F)
  )
})