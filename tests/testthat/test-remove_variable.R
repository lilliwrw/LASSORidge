test_that("remove_variable", {
  expect_identical(remove_variable(y~a+b+c+d, "d"), y~a+b+c)
  expect_identical(remove_variable(y~a+b+c+log(d), "d"),y~a+b+c)
  expect_identical(remove_variable(y~a+b+c*d, "d"),y~a+b+c)
  expect_identical(remove_variable(y~a+b+c:d, "d"),y~a+b)
  expect_identical(remove_variable(y~a+I(b+c+d),"d"),y~a)
})
