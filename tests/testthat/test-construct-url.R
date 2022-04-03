

test_that("construct_document_url function produces valid URL", {

  # check that it works with a single docket ID
  url <- construct_document_url(docketId = "CMS-2014-0063",
                                key = "DEMO_KEY")
  expect_equal(url, "https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&page[number]=1&page[size]=250&api_key=DEMO_KEY")
  #  resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  #  expect_equal(resp$status_code, 200)

  # check that it works with a single document ID
  url <- construct_document_url(documentId = "EOIR-2020-0003-0002",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents/EOIR-2020-0003-0002?api_key=DEMO_KEY")
  #  resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  #  expect_equal(resp$status_code, 200)

  # check that it works with search term, agency ID, and within comment period argument
  url <- construct_document_url(searchTerm = "payments",
                                agencyId = "CMS",
                                withinCommentPeriod = "true",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents?filter[agencyId]=CMS&filter[searchTerm]=payments&filter[withinCommentPeriod]=true&page[number]=1&page[size]=250&api_key=DEMO_KEY")
  #  resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  #  expect_equal(resp$status_code, 200)


})

test_that("check special character handling for construct_document_url", {
  #  skip_if_no_key()

  # check handling of arguments with special characters (&) and the sort argument
  url <- construct_document_url(documentType = "Supporting & Related Material",
                                sort = "postedDate",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents?filter[documentType]=Supporting%20%26%20Related%20Material&page[number]=1&page[size]=250&sort=postedDate&api_key=DEMO_KEY")
  # resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  # expect_equal(resp$status_code, 200)
})

test_that("date range works for construct_document_url", {
  #  skip_if_no_key()
  # check that date range implementation works
  url <- construct_document_url(postedDate = c("2020-01-01", "2020-12-01"),
                                searchTerm = "water",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents?filter[searchTerm]=water&filter[postedDate][ge]=2020-01-01&filter[postedDate][le]=2020-12-01&page[number]=1&page[size]=250&api_key=DEMO_KEY")
  #resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  #expect_equal(resp$status_code, 200)
})

test_that("testing withinCommentPeriod argument of construct_document_url", {
  # skip_if_no_key()

  # check withinCommentPeriod argument
  url <- construct_document_url(withinCommentPeriod = "true",
                                searchTerm = "medical",
                                sort = "postedDate",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents?filter[searchTerm]=medical&filter[withinCommentPeriod]=true&page[number]=1&page[size]=250&sort=postedDate&api_key=DEMO_KEY")
  #  resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  #  expect_equal(resp$status_code, 200)

})

test_that("include attachments works for construct_document_url", {
  url <- construct_document_url(documentId = "CMS-2014-0063-0001",
                                attachments = "true",
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents/CMS-2014-0063-0001?include=attachments&api_key=DEMO_KEY")
})


test_that("collapsing multiple arguments works for construct_document_url", {
  url <- construct_document_url(searchTerm = c("water", "soil"),
                                documentType = c("Supporting & Related Material", "Notice"),
                                key = "DEMO_KEY")
  expect_equal(url,
               "https://api.regulations.gov/v4/documents?filter[documentType]=Supporting%20%26%20Related%20Material,Notice&filter[searchTerm]=water,soil&page[number]=1&page[size]=250&api_key=DEMO_KEY")

  url <- construct_document_url(agencyId = c("CMS", "EPA"),
                                postedDate = c("2020-02-02", "2020-10-02"),
                                key = "DEMO_KEY")
  expect_equal(url,
  "https://api.regulations.gov/v4/documents?filter[agencyId]=CMS,EPA&filter[postedDate][ge]=2020-02-02&filter[postedDate][le]=2020-10-02&page[number]=1&page[size]=250&api_key=DEMO_KEY")
})


test_that("construct_document_url ignores docketId if documentId is provided", {
  url <- construct_document_url(documentId = "NIH-2007-0930-0001",
                         docketId = "NIH-2007-0930",
                         key = "DEMO_KEY")
  expect_equal(url, "https://api.regulations.gov/v4/documents/NIH-2007-0930-0001?api_key=DEMO_KEY")
})

