

test_that("extract_meta produces expected output", {
  skip_if_no_key()
  # documents endpoint
  res <- extract_meta("https://api.regulations.gov/v4/documents/FDA-2009-N-0501-0012")
  expect_equal(nrow(res), 1)

  # comments endpoint
  res <- extract_meta("https://api.regulations.gov/v4/documents/HHS-OCR-2018-0002-5313")
  expect_equal(nrow(res), 1)
} )
