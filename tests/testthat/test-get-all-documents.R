

test_that("get_all_documents has expected output with documentId argument", {
  skip_if_no_key()

  result <- get_all_documents(documentId = c("FDA-2012-S-1144-0322",
                                             "NHTSA-2008-0060-0827",
                                             "DOT-OST-2018-0206-0008",
                                             "CMS-2018-0104-0001"))
  expect_equal(nrow(result), 4)

  result <- get_all_documents(docketId ="FDA-2009-N-0501")


})


test_that("get_all_documents has expected output with docketId argument", {
  skip_if_no_key()

  result <- get_all_documents(docketId ="FDA-2009-N-0501")
  expect_equal(nrow(result), 9)

  result <- get_all_documents(docketId = "NIH-2006-0048")

  expect_equal(nrow(result), 1)

})
