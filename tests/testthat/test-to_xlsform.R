context("kobo generate xlsform test")


test_that("generate xlsform test", {
  expect_output(kobo_to_xlsform(mtcars), "XLS form has been successfully generated")
  expect_output(kobo_to_xlsform(mtcars, n=15), "XLS form has been successfully generated")
})