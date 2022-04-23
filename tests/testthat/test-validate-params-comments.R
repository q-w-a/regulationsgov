test_that("validate_params_comments gives correct error for commentId argument issues", {

  args <- list(key = "DEMO_KEY", commentId = "CMS-2020-0088-18990",
    attachments = "true", postedDate = NULL,
    agencyId = c("CMS", "FAA"), commentOnId = NULL,
    lastModifiedDate = NULL,  sort = NULL,
    page_number = 1, page_size = 250)

  expect_error(validate_params_comments(args), "Check Arguments: agencyId")


})
