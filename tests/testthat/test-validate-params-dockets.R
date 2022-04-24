test_that("error for multiple docket ids", {

  args <- list(key = "DEMO_KEY",
               docketId = c("CMS-2016-0034",
                            "FAA-2016-5222"),
               postedDate = NULL,
               agencyId = c("CMS", "FAA"),
               lastModifiedDate = NULL,
               sort = NULL,
               page_number = 1,
               page_size = 250)

  expect_error(validate_params_dockets(args),
               "docketId only accepts")


})

test_that("error for invalid documentType argument", {
  args <- list(key = "DEMO_KEY",
               docketId = NULL,
               postedDate = NULL,
               docketType = "new",
               agencyId = c("CMS", "FAA"),
               lastModifiedDate = NULL,
               sort = NULL,
               page_number = 1,
               page_size = 250)

  expect_error(validate_params_dockets(args),
               "docketType")

  })

