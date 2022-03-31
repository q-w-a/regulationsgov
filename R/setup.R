
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "filt", "value", "term"))

#not working

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




#' Verify Key is Available
#'
#' This function checks if the user has the API key set up or passed as an argument
#'
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}
check_auth <- function(key) {
  if (identical(Sys.getenv("DATA_GOV_KEY"), "") && is.null(key)) {
    stop("Valid API key needed. Obtain one at https://open.gsa.gov/api/regulationsgov/#getting-started")
  }
  else {
    if (!is.null(key)) return(key)
    else return(Sys.getenv("DATA_GOV_KEY"))
  }
}





#' Find Element in a Nested List with the Given Name
#'
#' Find the value of the element with the given name in
#' a deeply nested list. Based off of advice
#' [here](https://stackoverflow.com/questions/58400176/r-find-object-by-name-in-deeply-nested-list).
#' @param nested_list a nested list, such as the type returned by fromJSON
#' @param search_for a character string that is the name of the element we want to find in `nested_list`
#' @importFrom utils hasName
#'
find_element <- function(nested_list, search_for) {
  # if the element being searched for is found, return the value under this name
  if (hasName(nested_list, search_for)) {
    return(nested_list[[search_for]])
  }
  else if (is.list(nested_list)) {
    for (obj in nested_list) {
      # check each list in nested_list recursively
      result <- Recall(obj, search_for)
      if (!is.null(result)) return(result)
    }
  }
  else {
    return(NULL)
  }
}



#' Function for Skipping Tests if Authorization is Unavailable

skip_if_no_key <- function() {
  if (identical(Sys.getenv("DATA_GOV_KEY"), "")) {
    testthat::skip("No authentication available; skipping the test")
  }
}

