context("ahp load")


test_that("Project Init", {
  expect_output(kobo_projectinit(), "there's no missing package")
  expect_output(kobo_projectinit(), "Installation completed")
  expect_output(kobo_projectinit(), "Let's create various standard folders and copy some analysis script")
})
