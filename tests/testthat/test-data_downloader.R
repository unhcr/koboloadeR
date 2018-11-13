#context("kobo data downloader test")

#rm(data_123875765466)
#test_that("Retrieve the Data from a Specified Dataset", {
  #Case1: there is no data with 123875765466 id in requested server
  #expect_message(kobo_data_downloader("123875765466"), "No local dataset found")
  #rm(data_123875765466)
  #expect_message(kobo_data_downloader("123875765466"), "Downloading remote file")
  #rm(data_123875765466)
  #expect_message(kobo_data_downloader("123875765466"), "The process has been completed successfully")
  
  #Case2: there is a local data with 123875765466 id. Also, is doesn't exist in requested server
  #expect_message(kobo_data_downloader("123875765466"), "There is no Submissions for this Dataset")
  #expect_message(kobo_data_downloader("123875765466"), "The process has been completed successfully")

#})
#rm(data_123875765466)


