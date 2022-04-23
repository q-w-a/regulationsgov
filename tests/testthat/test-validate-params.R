


test_that("validate_params throws correct errors", {
  arg_list <- list(key = NULL,
                   documentId =  c("NIH-2007-0930-0001",
                                   "CMS-2014-0063-0001"))
  expect_error(validate_params(arg_list), "one documentId")



  arg_list <- list(key = NULL,
                   documentId =  c("NIH-2007-0930-0001",
                                   "CMS-2014-0063-0001"),
                   attachments = "false")
  expect_error(validate_params(arg_list),
               "one documentId.*.attachments must be")





  arg_list <- list(key = NULL,
                   documentId =  c("NIH-2007-0930-0001", "CMS-2014-0063-0001"),
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = c("2020-01-10", "2020-10-10", "2020-12-10"),
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)

  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = c("ERRR-01-10", "2020-10-10", "2020-12-10"),
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)

  expect_error(validate_params(arg_list),
               "commentEndDate must be a single character string.*.commentEndDate values must be of the form")


  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = c("2020-10-10", "TEST"),
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)


  expect_error(validate_params(arg_list),
               "commentEndDate values must be of the form")

  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = NULL,
                   documentType = c("Proposed Rule",
                                    "Supporting and Related Material"),
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)


  expect_error(validate_params(arg_list),
               "documentType must contain.*.Check Entries")

  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = NULL,
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = "20XX-10-10",
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)


  expect_error(validate_params(arg_list),
               "postedDate values must be of the form")


  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = NULL,
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = "docketId",
                   page_number = 1,
                   page_size = 250)

  expect_error(validate_params(arg_list),
               "only can sort by one or more of the following")

  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = NULL,
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = c("2020-10-10", "2020-11-10"),
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)


  expect_error(validate_params(arg_list),
               "Check Entries: 2020-10-10, 2020-11-10")



})

test_that("validate_params includes multiple errors", {
  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = NULL,
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = c("2000-10-10 20:00:00", "2020-11-10"),
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = c("XXXX-10-10", "2020-11-10"),
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = NULL,
                   page_number = 1,
                   page_size = 250)
  expect_error(validate_params(arg_list),
               "Check Entries:.*.2000-10-10 20:00:00.*.XXXX-10-10")

  arg_list <- list(key = NULL,
                   documentId =  NULL,
                   attachments = "false",
                   docketId = NULL,
                   agencyId = NULL,
                   commentEndDate = NULL,
                   documentType = NULL,
                   frDocNum = NULL,
                   searchTerm = NULL,
                   postedDate = NULL,
                   lastModifiedDate = NULL,
                   subtype = NULL,
                   withinCommentPeriod = NULL,
                   sort = "agencyId",
                   page_number = 1,
                   page_size = 250)

  expect_error(validate_params(arg_list),
               "attachments must be.*.only can sort.*.Check Entries:.*")



})
