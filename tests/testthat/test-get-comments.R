


test_that("get_all_comments produces data frame of comment metadata has
          the correct number of rows and contains download link column", {
  skip_if_no_key()

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2014-0063",
                                       test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2013-0253", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileUrl"]))



})

test_that("data frame is correct for documentId argument", {

  # check functionality with document ID rather than docket ID
  comment_metadata <- get_all_comments(documentId="CMS-2014-0063-0001", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)
})
