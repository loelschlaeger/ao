test_that("check for number works", {
  expect_false(is_number(0))
  expect_true(is_number(1))
  expect_false(is_number(0.5))
  expect_false(is_number("1"))
})
