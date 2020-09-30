library(testthat)

test_that("parent_base_ext separates a full path properly",{
  out <- parent_base_ext("directory/file.extension")
  expect_identical(out, list(parent="directory", base="file", ext="extension"))
})

