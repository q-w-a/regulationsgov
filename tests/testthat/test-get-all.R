test_that("get_all functions as expected when total elements > 5000", {

  skip("TIME + RATE CONSUMING, TEST IF MAKING CHANGES TO > 5000 SECTION")
  key <- Sys.getenv("DATA_GOV_KEY")
  # skip this test unless you want to use a significant number of requests + are willing to wait
  url <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=09000064846eebaf&page[size]=250&page[number]=1&sort=lastModifiedDate,documentId&api_key=",
                key)

  result <- get_all(url, 88061)
  result2 <- unlist(result)
  links <- result2[grepl("https://api.regulations.gov/v4/comments", result2)]

  expect_equal(length(unique(links)), 88061)

})
