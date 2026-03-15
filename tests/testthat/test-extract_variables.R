test_that("extract_variables", {
  expect_identical(extract_variables(y~a+b+c+d), c("y", "a", "b", "c", "d"))
  expect_identical(extract_variables(y~a+b+c+log(d)),c("y", "a", "b", "c", "d"))
  expect_identical(extract_variables(y~a+b+c*d, skip_output = TRUE),c("a", "b", "c", "d"))
  expect_identical(extract_variables(y~a+b+c:d, skip_output = TRUE),c("a", "b", "c", "d"))
  expect_identical(extract_variables(y~a+I(b+c+d), skip_output = TRUE),c("a", "b", "c", "d"))
  expect_identical(extract_variables(y~a+b+cd, skip_output = TRUE), c("a", "b", "cd"))
})
