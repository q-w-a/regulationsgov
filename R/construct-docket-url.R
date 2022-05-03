


#' Create a URL for the Dockets Endpoint
#'
#' Takes the parameters available for the documents endpoint of the regulations.gov API, as
#' described [here](https://open.gsa.gov/api/regulationsgov/#searching-for-documents) under the
#' documents section under API calls, and constructs a URL that can be used to retrieve the
#' data specified. To understand how these parameters relate to the data, it may help to go the
#' the regulations.gov search function and go to a [specific docket](https://www.regulations.gov/docket/CMS-2014-0063)
#' or [document](https://www.regulations.gov/document/CMS-2014-0063-0001) and look at the details section.
#'
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' with the `docketId` argument.
#' @param docketId character string representing one valid docket ID, for example, "CMS-2014-0063".
#' @param agencyId character string representing a government agency, for example, "CMS" or "EPA" or a character
#' vector of multiple agencies
#' @param docketType the type of docket, valid types are "Rulemaking" and "Nonrulemaking"
#' @param searchTerm character string representing a search term to filter the results for or a character
#' vector with multiple search terms to filter for.
#' @param lastModifiedDate a character vector of length 1, representing a single posted date to filter for,
#'  or a vector of length 2, representing a date range beginning with the first element and ending at the second.
#'  Each element must be in the format yyyy-MM-dd HH:mm:ss.
#' @param sort character string representing which value you want to sort the results by, supported variables are
#'  "title", "docketId", "lastModifiedDate"
#' @param page_number character string representing which page to retrieve the data; useful when
#' output is on multiple pages. Valid values are between 1 and 20. Default value is 1.
#' @param page_size character string representing how many elements should be on each page. Valid values
#' are between 5 and 250. The default value is 250.
#' @export
#' @examples
#' url <- construct_docket_url(
#'   agencyId = c("CMS", "EPA"),
#'   lastModifiedDate = c("2020-01-02 12:00:00", "2020-02-02 12:00:00"), key = "DEMO_KEY"
#' )
#' url <- construct_docket_url(docketId = "CMS-2014-0063-0001", key = "DEMO_KEY")
construct_docket_url <- function(key = NULL,
                                 docketId = NULL,
                                 agencyId = NULL,
                                 docketType = NULL,
                                 searchTerm = NULL,
                                 lastModifiedDate = NULL,
                                 sort = NULL,
                                 page_number = 1,
                                 page_size = 250) {

  # get arguments as a named list
  arguments <- as.list(environment())

  # check that given arguments are valid
  validate_params_dockets(arguments)

  # retrieve key
  key <- check_auth(key)

  # set base url
  base <- "https://api.regulations.gov/v4/dockets"

  if (!is.null(docketId)) {
    # no need for multiple pages for single document
    url <- paste0(base, "/", docketId, "?api_key=", key)
  } else {
    # collapse all arguments
    url <- make_url(arguments, base, key)
  }
  return(url)
}
