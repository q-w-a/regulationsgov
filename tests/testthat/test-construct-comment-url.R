test_that("construct_comment_url handles dates correctly", {
  correct <- "https://api.regulations.gov/v4/comments?filter[postedDate][ge]=2020-02-02&filter[postedDate][le]=2020-02-03&filter[agencyId]=CMS,EPA&page[number]=1&page[size]=250&api_key=DEMO_KEY"
  url <- construct_comment_url(
    key = "DEMO_KEY",
    agencyId = c("CMS", "EPA"),
    postedDate = c("2020-02-02", "2020-02-03")
  )
  expect_equal(correct, url)


  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)
})

test_that("construct_comment_url handles sort  correctly", {
  correct <- "https://api.regulations.gov/v4/comments?filter[postedDate][ge]=2020-02-02&filter[postedDate][le]=2020-02-03&filter[agencyId]=FAA&page[number]=1&page[size]=250&sort=documentId&api_key=DEMO_KEY"
  url <- construct_comment_url(
    agencyId = "FAA",
    postedDate = c("2020-02-02", "2020-02-03"),
    sort = "documentId",
    key = "DEMO_KEY"
  )
  expect_equal(correct, url)

  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)

  correct <- "https://api.regulations.gov/v4/comments?filter[agencyId]=CMS&filter[lastModifiedDate][ge]=2019-02-02 12:00:00&filter[lastModifiedDate][le]=2019-02-05 12:00:00&page[number]=1&page[size]=250&sort=lastModifiedDate&api_key=DEMO_KEY"
  url <- construct_comment_url(
    agencyId = "CMS",
    lastModifiedDate = c(
      "2019-02-02 12:00:00",
      "2019-02-05 12:00:00"
    ),
    sort = "lastModifiedDate", key = "DEMO_KEY"
  )
  expect_equal(correct, url)

  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)
})

test_that("construct_comment_url handles commentId argument correctly", {
  correct <- "https://api.regulations.gov/v4/comments/CMS-2020-0088-18990?include=attachments&api_key=DEMO_KEY"
  url <- construct_comment_url(
    commentId = "CMS-2020-0088-18990",
    attachments = "true",
    key = "DEMO_KEY"
  )
  expect_equal(correct, url)

  expect_error(
    construct_comment_url(
      commentId = "CMS-2020-0088-18990",
      attachments = "true",
      sort = "agencyId",
      key = "DEMO_KEY"
    ),
    "Check Arguments: sort"
  )

  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)

  correct <- "https://api.regulations.gov/v4/comments?filter[agencyId]=CMS&filter[lastModifiedDate][ge]=2019-02-02 12:00:00&filter[lastModifiedDate][le]=2019-02-05 12:00:00&page[number]=1&page[size]=250&sort=lastModifiedDate&api_key=DEMO_KEY"
  url <- construct_comment_url(
    agencyId = "CMS",
    lastModifiedDate = c(
      "2019-02-02 12:00:00",
      "2019-02-05 12:00:00"
    ),
    sort = "lastModifiedDate",
    key = "DEMO_KEY"
  )
  expect_equal(correct, url)

  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)

  expect_error(construct_comment_url(
    commentId = "CMS-2020-0088-18990",
    attachments = "true",
    sort = "lastModifiedDate",
    key = "DEMO_KEY"
  ))

  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)


  correct <- "https://api.regulations.gov/v4/comments?filter[postedDate][ge]=2021-01-02&filter[postedDate][le]=2021-01-15&filter[searchTerm]=case%20numbers&page[number]=1&page[size]=250&api_key=DEMO_KEY"

  url <- construct_comment_url(
    searchTerm = "case numbers",
    postedDate = c("2021-01-02", "2021-01-15"),
    key = "DEMO_KEY"
  )

  expect_equal(correct, url)


  # works (this works if the above test works; redundant but need to check that this url is a valid call)
  # expect_error(get_data(url), NA)
})
