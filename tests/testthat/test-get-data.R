
key <- Sys.getenv("DATA_GOV_KEY")


test_that("get_data produces expected output for documents endpoint", {
  skip_if_no_key()

  # returns nested expected nested list
  url <- paste0("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=",
                key)
  res <- get_data(url)

  expect_false(is.null(res$data))
  expect_false(is.null(res$meta))

  url <- paste0("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=",
                key)
  res_df <- get_data(url,
                     df = TRUE)

  expect_true(is.data.frame(res_df))

  # works for obtaining a single document with attachment links
  url <- paste0("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&include=attachments&api_key=",
                key)
  res_df <- get_data(url,
                     df = TRUE)

  expect_equal(nrow(res_df), 1)

  url <- paste0("https://api.regulations.gov/v4/documents?filter[documentType]=Supporting%20%26%20Related%20Material,Notice&filter[searchTerm]=benzene,runoff&page[number]=1&page[size]=250&api_key=",
                key)
  res_df <- get_data(url, df = TRUE)

  expect_equal(nrow(res_df), 250)


})

test_that("get_data produces expected message when no data can be retrieved", {
  url <- construct_comment_url(agencyId = "CMS",
                               lastModifiedDate = c("2019-02-02 12:00:00",
                                                    "2019-02-02 14:00:00"),
                               sort = "documentId", key = "DEMO_KEY")
  expect_message(get_data(url, df = TRUE), "No data available for the given url.")

})



test_that("get_data produces expected output for comments endpoint", {
  skip_if_no_key()

  url <- paste0("https://api.regulations.gov/v4/comments?filter[docketId]=CMS-2014-0063&api_key=",
                key)
  res <- get_data(url)


  expect_false(is.null(res$data))
  expect_false(is.null(res$meta))

  url <- paste0("https://api.regulations.gov/v4/comments?filter[docketId]=CMS-2014-0063&api_key=",
                key)
  res <- get_data(url,
                  df = TRUE)
  expect_equal(nrow(res), 25)

})

test_that("get_data throws error when invalid url provided", {
  skip_if_no_key()

  url <- paste0("https://api.regulations.gov/v4/comments/FDA-2012-n-0920-0001?include=attachments&api_key=",
                key)
  expect_error(get_data(url),
               "Client error")
})




