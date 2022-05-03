


test_that("get_all_comments produces data frame of comment metadata has
          the correct number of rows and contains download link column", {
  skip_if_no_key()

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(
    endpoint = "document",
    docketId = "CMS-2014-0063",
    test = TRUE
  )
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(
    endpoint = "document",
    docketId = "CMS-2013-0253", test = TRUE
  )
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))
})

test_that("data frame is correct for documentId argument", {
  skip_if_no_key()
  # check functionality with document ID rather than docket ID
  comment_metadata <- get_all_comments(
    endpoint = "document",
    documentId = "CMS-2014-0063-0001", test = TRUE
  )
  expect_equal(nrow(comment_metadata), 3)
})


test_that("data frame is correct for full example", {
  # to save requests
  skip("only run if you can use many requests ")

  result <- get_all_comments(
    endpoint = "document",
    docketId = "FAA-2018-1084"
  )
})

test_that("quiet works and expected message is provided", {
  skip_if_no_key()
  expect_message(
    get_all_comments(
      endpoint = "document",
      documentId = "CMS-2017-0082-0002",
      key = "DEMO_KEY",
      test = TRUE,
      quiet = FALSE
    ),
    "https://api.regulations.gov/v4/comments/CMS-2017-0082-0010"
  )

  expect_message(get_all_comments(
    endpoint = "document",
    documentId = "CMS-2017-0082-0002",
    test = TRUE
  ), NA)
})

test_that("get_all_comments works when number of comments > 500", {
  skip("takes > 2 hours")
  # this will take over 2 hours (>1200 comments)
  res <- get_all_comments(documentId = "CMS-2017-0082-0002")
})


test_that("works for dockets endpoint", {
  skip_if_no_key()
  url <- construct_docket_url(
    agencyId = c("FDA"),
    lastModifiedDate = c(
      "2021-02-22 12:00:00",
      "2021-02-26 12:00:00"
    )
  )

  comments <- get_all_comments(
    endpoint = "docket",
    agencyId = c("FDA"),
    lastModifiedDate = c(
      "2021-02-22 12:00:00",
      "2021-02-26 12:00:00"
    )
  )

  expect_equal(4, nrow(comments))
})

test_that("works for comments endpoint", {
  skip_if_no_key()

  comments <- get_all_comments(
    endpoint = "comment",
    searchTerm = "covid",
    postedDate = c(
      "2021-02-01",
      "2021-02-02"
    )
  )

  expect_equal(nrow(comments), 28)
})

test_that("throws error for invalid endpoint", {
  skip_if_no_key()
  expect_error(
    get_all_comments(endpoint = "Not"),
    "only endpoints available"
  )
})
