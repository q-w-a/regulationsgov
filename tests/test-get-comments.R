

skip_if_no_key <- function() {
  if (identical(Sys.getenv("DATA_GOV_KEY"), "")) {
    skip("No authentication available; skipping the test")
  }
}


test_that("get_data function produces desired output", {

  skip_if_no_key()

  # check with output as nested list
  parsed <- get_data("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=DEMO_KEY")
  expect_false(is.null(parsed$data))

  # check df argument
  parsed <- get_data("https://api.regulations.gov/v4/documents?filter[searchTerm]=toxic&api_key=DEMO_KEY",
                     df = TRUE)
  expect_true(is.data.frame(parsed))
  expect_equal(nrow(parsed), 25)


})


test_that("data frame of comment metadata has the correct number of rows and contains download link column", {
  skip_if_no_key()

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2014-0063", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileFormats"]))

  # check number of rows is correct; one row per comment
  comment_metadata <- get_all_comments(docketId="CMS-2013-0253", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)

  # has download links column
  expect_false(is.null(comment_metadata["fileFormats"]))

  # check functionality with document ID rather than docket ID
  comment_metadata <- get_all_comments(documentId="CMS-2014-0063-0001", test = TRUE)
  expect_equal(nrow(comment_metadata), 3)


})
