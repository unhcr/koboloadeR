context("kobo prepare form test")


test_that("prepare form test", {
  expect_output(kobo_prepare_form(), "Survey sheet, ready to be used")
  expect_output(kobo_prepare_form(), "Choices sheet, ready to be used")
  expect_output(kobo_prepare_form(), "Indicator sheet, ready to be used")
  expect_output(kobo_prepare_form(), "Settings sheet, ready to be used")
})
