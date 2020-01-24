test_that("all_identical works", code = {
  expect_true(all_identical(list(a=1, b=1, c=1, d=1)))
  expect_true(all_identical(list(1, 1, 1)))
  expect_error(all_identical(list(a=1, b=1, c=1, d=2)))
  expect_error(all_identical(list(1, 1, 2)))
})
