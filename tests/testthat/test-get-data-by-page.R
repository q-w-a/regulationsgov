test_that("get_data_by_page produces expected output", {
  skip_if_no_key()
  key <- check_auth(NULL)
  #key <- "DEMO_KEY"
  url <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=09000064846eebaf&page[size]=250&page[number]=1&api_key=",
                key)
  expect_message(get_data_by_page(url, page_number = 2, quiet = FALSE),
                regexp= "https://api.regulations.gov/v4/comments?filter[commentOnId]=09000064846eebaf&page[size]=250&page[number]=2&api_key=",
                fixed = TRUE)

  result <- get_data_by_page(url, page_number = 2)

  expect_equal(nrow(result$data), 250)

})
