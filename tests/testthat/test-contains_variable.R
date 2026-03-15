test_that("contains_variable", {
  expect_identical(contains_variable(y~a+b+c+d, "d"), TRUE)
  expect_identical(contains_variable(y~a+b+c+log(d), "d"),TRUE)
  expect_identical(contains_variable(y~a+b+c*d, "d"),TRUE)
  expect_identical(contains_variable(y~a+b+c:d, "d"),TRUE)
  expect_identical(contains_variable(y~a+I(b+c+d),"d"),TRUE)
  expect_identical(contains_variable(y~a+b+cd, "d"), FALSE)
})
