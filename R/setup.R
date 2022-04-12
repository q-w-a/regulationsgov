utils::globalVariables(c("."))

#' Set Data.gov Key as an Environmental Variable
#'
#' This function provides a way to store the Data.gov API key as an
#' environmental variable for convenience when working with this package.
#' If you run this function, you will not have to supply the API key
#' every time you use a function that retrieves data from Regulations.gov.
#' Otherwise, you will need to supply the API key when needed through
#' the `key` argument.
#'
#' @param key valid Data.gov API key obtained \href{https://open.gsa.gov/api/regulationsgov/#getting-started}{here}.
#' @importFrom magrittr %>%
#' @importFrom purrr map reduce
#' @importFrom utils URLencode
#' @export
#' @examples
#' \dontrun{
#' set_datagov_key("PUT_KEY_HERE")
#' }
set_datagov_key <- function(key) {
  # to see if the key supplied is valid
  if ( !is.character(key) || nchar(key) != 40) {
    stop('INVALID KEY:
    Valid Data.gov API keys for accessing the Regulations.gov
    API are a character string of length 40.
    Go to the https://open.gsa.gov/api/regulationsgov/#getting-started to obtain a valid key.')
  }

  Sys.setenv(DATA_GOV_KEY = key)

  if (!identical(Sys.getenv("DATA_GOV_KEY"), "")) {
    message('SUCCESS:
  DATA_GOV_KEY environment variable now set.
  You do not have to use the key argument for the functions in this package.')
  }
}




#' Function for Skipping Tests if Authorization is Unavailable

skip_if_no_key <- function() {
  if (identical(Sys.getenv("DATA_GOV_KEY"), "")) {
    testthat::skip("No authentication available; skipping the test")
  }
}

