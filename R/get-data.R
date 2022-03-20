

#' Acquire Data for the Given Url
#'
#' @param url a url containing an API call
#' @param df logical, \code{TRUE} if you want the output as a data frame.
#' Default value is \code{FALSE}
#' @importFrom httr config GET
#'
#' @examples
#'\dontrun{
#'get_data("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=DEMO_KEY")
#'}
get_data <- function(url, df = FALSE) {

  resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))

  # check if type is as expected
  if (httr::http_type(resp) != "application/vnd.api+json") {
    httr::http_status(resp) %>%
      paste(collapse = "\n") %>%
      message()
    stop("API failed to return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"),
    simplifyVector = FALSE)

  if (df) {
    tryCatch( {
      parsed <- map(parsed$data, get_df) %>%
        reduce(bind_rows) },
      error = function(cnd) {
        message("Conversion to data frame failed; returning nested list")
      }
    )
  }
  parsed
}

#' Acquire Data for the Given Page and Url
#'
#' @param url a url containing an API call
#' @param page_number page number to obtain data for, default value is 1
#'
#' @examples
#' \dontrun{
#' get_data_by_page("https://api.regulations.gov/v4/documents?
#' filter[docketId]=CMS-2014-0063&page[size]=250&page[number]=1&api_key=DEMO_KEY",
#' page_number = 2)
#' }
get_data_by_page <- function(url, page_number = 1) {
  # set page number to given page
  for_page <- paste0("page[number]=", page_number, "&sort")
  url_for_page <- gsub("page\\[number\\](.*)sort", for_page, url)
  # print(url_for_page)
  get_data(url_for_page)
}


#' Convert Element of Nested List from jsonlite::fromJSON into Data Frame
#'
#' Can be used iteratively in conjunction with \code{bind_rows} to convert the
#'  entire nested list into a data frame.
#' @param element a named nested list
#' @return the element converted to a data frame
get_df <- function(element) {
  element %>%
    unlist() %>%
    t() %>%
    as.data.frame() %>%
    dplyr::rename_with(~gsub("attributes.", "", .x, fixed = TRUE))
}
