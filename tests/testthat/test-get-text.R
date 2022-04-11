

test_that("add text works as expected", {
  skip_if_no_key()
  comment_metadata <- get_all_comments(docketId="CMS-2014-0063",
                                       test = TRUE)
  text <- add_text(comment_metadata)
  expect_equal(ncol(comment_metadata) + 1, ncol(text))
})

test_that("get_text chooses desired element when all=FALSE", {

  text <- extract_text("https://downloads.regulations.gov/FAA-2018-1084-0130/attachment_1.docx")
  expect_equal(text, "I believe this is a Great rule.")

  # correct function is called
  text <- extract_text("https://downloads.regulations.gov/FDA-2008-N-0115-0014/attachment_1.pdf")
  text_pdf <- textreadr::read_pdf("https://downloads.regulations.gov/FDA-2008-N-0115-0014/attachment_1.pdf",
                                  ocr = TRUE)[,"text"] %>%
    paste0(collapse = " ")

  expect_equal(text, text_pdf)
})

test_that("choose_element chooses the correct element", {
  links <- c("https://downloads.regulations.gov/FAA-2018-1084-0130/attachment_1.docx",
    "https://downloads.regulations.gov/FAA-2018-1084-0130/attachment_1.pdf" )
  link <- choose_element(links)
  expect_equal(link, "https://downloads.regulations.gov/FAA-2018-1084-0130/attachment_1.docx")
})


test_that("extract_text works as expected for invalid argument", {

  # returns NA when invalid url provided
  invalid <- extract_text(NA)
  expect_true(is.na(invalid))

  invalid <- extract_text("not a link")
  expect_true(is.na(invalid))


})


test_that("add_text works as expected with all = TRUE", {
  skip("Run only if modifying add_text functions; 38 requests")

  comment_metadata <- get_all_comments(documentId="CMS-2014-0063-0001")
  sub <- comment_metadata[25:26,]
  text <- add_text(sub, all = TRUE)
  expect_false(is.null(text["text"]))

})

test_that("add_text works with document data", {
  skip_if_no_key()
  result <- get_all_documents(docketId ="FDA-2009-N-0501")
  text <- add_text(result)
  expect_false(any(is.na(text["text"])))

})
