test_that("construct_docket_url works as expected with docketId argument", {

  url <- construct_docket_url(docketId ="CMS-2014-0063",
                       key = "DEMO_KEY")
  correct <- "https://api.regulations.gov/v4/dockets/CMS-2014-0063?api_key=DEMO_KEY"
  expect_equal(url, correct)


  expect_error(construct_docket_url(docketId =c("CMS-2014-0063",
                                                "CMS-2016-0034"),
                                    key = "DEMO_KEY"),
               "docketId only accepts one docketId")


})

test_that("construct_docket_url works as expected when docketId not provided", {
  url <- construct_docket_url(agencyId = c("CMS", "EPA"),
                              lastModifiedDate =  c("2020-01-02 12:00:00",
                                                    "2020-02-02 12:00:00" ),
                              key = "DEMO_KEY")
  correct <- "https://api.regulations.gov/v4/dockets?filter[agencyId]=CMS,EPA&filter[lastModifiedDate][ge]=2020-01-02 12:00:00&filter[lastModifiedDate][le]=2020-02-02 12:00:00&page[number]=1&page[size]=250&api_key=DEMO_KEY"
  expect_equal(url, correct)

  url <- construct_docket_url(docketType = "Nonrulemaking",
                              lastModifiedDate =  c("2021-01-02 12:00:00",
                                                    "2021-02-02 12:00:00"),
                              key = "DEMO_KEY")
  correct <- "https://api.regulations.gov/v4/dockets?filter[docketType]=Nonrulemaking&filter[lastModifiedDate][ge]=2021-01-02 12:00:00&filter[lastModifiedDate][le]=2021-02-02 12:00:00&page[number]=1&page[size]=250&api_key=DEMO_KEY"
  expect_equal(url, correct)


  url <- construct_docket_url(lastModifiedDate =  c("2021-01-02 12:00:00", "2021-10-02 12:00:00"),
                              searchTerm = "Medicaid Medicare",
                              key = "DEMO_KEY")
  correct <-  "https://api.regulations.gov/v4/dockets?filter[searchTerm]=Medicaid%20Medicare&filter[lastModifiedDate][ge]=2021-01-02 12:00:00&filter[lastModifiedDate][le]=2021-10-02 12:00:00&page[number]=1&page[size]=250&api_key=DEMO_KEY"
  expect_equal(url, correct)

})


test_that("construct_docket_url handles multiple sort arguments", {
  url <- construct_docket_url(lastModifiedDate =  c("2021-01-02 12:00:00", "2021-10-02 12:00:00"),
                              searchTerm = "Medicaid Medicare",
                              sort = c("title","lastModifiedDate"),
                              key = "DEMO_KEY")
  correct <- "https://api.regulations.gov/v4/dockets?filter[searchTerm]=Medicaid%20Medicare&filter[lastModifiedDate][ge]=2021-01-02 12:00:00&filter[lastModifiedDate][le]=2021-10-02 12:00:00&page[number]=1&page[size]=250&sort=title,lastModifiedDate&api_key=DEMO_KEY"

  expect_equal(correct, url)

  })
