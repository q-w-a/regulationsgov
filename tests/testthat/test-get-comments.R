


test_that("extract_meta produces expected output", {
  skip_if_no_key()
  # documents endpoint
  res <- extract_meta("https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0012")
  expect_equal(nrow(res), 1)

  # comments endpoint
  res <- extract_meta("https://api.regulations.gov/v4/documents/HHS-OCR-2018-0002-5313")
  expect_equal(nrow(res), 1)
} )


test_that("data frame of comment metadata has the correct number of rows and contains download link column", {
  skip_if_no_key()

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2014-0063", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2013-0253", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))

  # check functionality with document ID rather than docket ID
  comment_metadata <- get_all_comments(documentId="CMS-2014-0063-0001", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)


})
