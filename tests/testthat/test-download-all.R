test_that("download_all works as  expected", {
  skip("Downloading required for these tests")

   urls <- c("https://downloads.regulations.gov/FDA-2009-N-0501-0009/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0009/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0008/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0008/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0005/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0005/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0001/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0001/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0012/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0012/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0010/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0010/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0011/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0011/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0007/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0007/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0006/content.pdf,https://downloads.regulations.gov/FDA-2009-N-0501-0006/content.htm",
   "https://downloads.regulations.gov/FDA-2009-N-0501-0006/content.doc")

   expect_message(download_all(urls[1:4], dest = "../../test/", quiet = TRUE), NA)
   files <- list.files("../../test/")
   expect_equal(length(files), 8)

   expect_true(any(grepl("htm", files )))

   download_all(urls[1:3], dest = "../../test2", format = "pdf", quiet = TRUE)

   files2 <- list.files("../../test2/")
   expect_equal(length(grep("pdf", files2)), 3)

})
