

#' Acquire Data for the Given Url
#'
#' @param url a url containing a valid API call
#' @param df logical, \code{TRUE} if you want the output as a data frame.
#' Default value is \code{FALSE}
#' @param quiet logical with default `TRUE`, passed to [httr::RETRY()].
#' If `TRUE` this function does not print messages when [httr::RETRY()] has
#' to retry getting a request due to hitting a rate limit.
#' @param ... additional arguments to pass to [httr::RETRY()]
#' @importFrom httr config GET
#' @export
#' @examples
#'\dontrun{
#'get_data("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=DEMO_KEY")
#'}
get_data <- function(url, df = FALSE, quiet = TRUE,...) {

  # try insistently to get response for given url
  # don't retry on errors 400, 403 since do not represent
  # errors relating to too many requests, so the issue will
  # not be resolved by retrying
  resp <- httr::RETRY(verb = "GET",
                      url = url,
                      config = config(ssl_verifypeer = FALSE),
                      pause_min = 4,
                      pause_base = 2,
                      pause_cap = 3010,
                      max_times = 50,
                      terminate_on = c(400,403),
                      quiet = quiet,
                      ...)

  # check if type is as expected
  if (httr::http_type(resp) != "application/vnd.api+json") {
    httr::http_status(resp) %>%
      paste(collapse = "\n") %>%
      message()
    stop("API failed to return json", call. = FALSE)
  }

  # display the specific information about the error
  else if (httr::http_error(resp)) {
    error_message <- httr::http_status(resp) %>%
      paste(collapse = "\n")
    stop(error_message,
      call. = FALSE)
  }

  # obtain content of request
  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text"),
    simplifyVector = TRUE,
    flatten = TRUE)

  if (df) {
    parsed <- parsed$data %>%
      convert_df()
  }
  parsed
}

#' Acquire Data for the Given Page and Url
#'
#' Get data for the given page number. Used for iterating over pages when
#' this is necessary to obtain all the data.
#'
#' @param url a url containing an API call
#' @param page_number page number to obtain data for, default value is 1
#' @param quiet logical; FALSE if you want detailed information printed to the console,
#' TRUE otherwise. FALSE is recommended for longer calls to monitor the progress.
#' @examples
#' \dontrun{
#' get_data_by_page("https://api.regulations.gov/v4/documents?
#' filter[docketId]=CMS-2014-0063&page[size]=250&page[number]=1&api_key=DEMO_KEY",
#' page_number = 2)
#' }
#' @keywords internal
get_data_by_page <- function(url, page_number = 1, quiet = TRUE) {
  # set page number to given page
  for_page <- paste0("page[number]=", page_number, "&")
  url_for_page <- gsub("page\\[number\\]=[0-9]+&", for_page, url, perl = TRUE)
  if (!quiet) message("Page ", page_number, ": ", url_for_page)
  get_data(url_for_page, quiet = quiet)
}



#' Get All the Elements When Iteration Over Pages May Be Necessary
#'
#' Get all elements corresponding to a given url, since the API allows
#' at most 20 pages, \code{page[number]} parameter of the url, each of
#' which can contain at most 250 elements, the \code{page[size]} parameter.
#' If there are less than 250 elements, then no iteration is needed because
#' all elements are on the first page. Otherwise, multiple API calls will
#' be needed to obtain all the results.
#'
#' @param url a valid API url that includes the \code{page[size]} and \code{page[number]}
#' parameters.
#' @param quiet logical; FALSE if you want detailed information printed to the console,
#' TRUE otherwise. FALSE is recommended for longer calls to monitor the progress.
#' @return a nested list containing all elements corresponding to the given url.
#' Functionality is not yet implemented for obtaining all elements when there are
#' more than 5000 elements.
#' @examples
#' \dontrun{iterate_over_pages("https://api.regulations.gov/v4/comments?filter
#' [commentOnId]=09000064816e1a41&page[size]=250&page[number]=1&
#' sort=lastModifiedDate,documentId&api_key=DEMO_KEY")
#' }
#' @keywords internal
iterate_over_pages <- function(url, quiet = TRUE) {

  first <- get_data_by_page(url, page_number = 1, quiet = quiet)

  if (!quiet) message("Number of Elements is: ",
                      first$meta$totalElements)

  if (is.null(first$meta$totalElements) || first$meta$totalElements <= 250) {
    pages <- first
  }

  else if (first$meta$totalElements > 250 && first$meta$totalElements <= 5000 ) {
    # since we can have 250 elements on each page, set number of pages to get all elements
    end <- floor(first$meta$totalElements /250) + 1
    pages <- map(1:end, ~get_data_by_page(page_number = .x, url = url, quiet = quiet))
  }
  else{
    message("There are ", first$meta$totalElements,
            " comments. ")
    pages <- get_all(url, first$meta$totalElements, quiet = quiet)
  }
  return(pages)
}


#' Extract All Elements When there are More than 5000
#'
#' This function uses the procedure outlined [here](https://open.gsa.gov/api/regulationsgov/#searching-for-documents) under the section
#' *Retrieve all comments for a docket where number of comments is greater than 5000*
#' @param url that you want to obtain all data for, where there are more than 5000 elements. This url
#' *must* be sorted by lastModifiedDate and documentId. For example,
#' \code{"https://api.regulations.gov/v4/comments?filter[commentOnId]=09000064846eebaf&page[size]=250&page[number]=1&sort=lastModifiedDate,documentId&api_key=DEMO_KEY"}
#' @param num_elements number of elements associated with the given `url` (totalElements)
#' @param quiet logical; FALSE if you want the urls to be printed as this function iterates. Default value is
#' TRUE, where the urls are not printed to the console.
#' @keywords internal
get_all <- function(url, num_elements, quiet = TRUE) {

  n <- num_elements

  # 20 pages maximum, 250 elements per page
  iterations <- floor(n / (250*20)) + 1

  results <- map(1:20, ~get_data_by_page(page_number = .x,
                                            url = url,
                                         quiet = quiet)$data)
  # extract the date in the last element
  last_date <- find_element(results[[20]],
                            "lastModifiedDate") %>%
    unlist(use.names = FALSE)

  last_date <- last_date[length(last_date)]
  last_date <- convert_time(last_date)

  for (i in 2:(iterations)) {

    url_split <- strsplit(url, "page[size]",
                          fixed= TRUE) %>%
      unlist()
    new_url <- paste0(url_split[1],
                      "filter[lastModifiedDate][ge]=",
                      last_date,
                      "&page[size]",
                      url_split[2])
    url_split <- strsplit(url, "sort") %>%
      unlist()

    if (!quiet) message(new_url)

    pages <- map(1:20, ~get_data_by_page(page_number = .x,
                                           url = new_url,
                                         quiet = quiet)$data)

    last_date <- find_element(pages[[20]],
                              "lastModifiedDate") %>%
      unlist(use.names = FALSE)

    last_date <- last_date[length(last_date)]
    last_date <- convert_time(last_date)

    results <- append(results, pages)
  }
  return(results)
}


#' Convert Date of the form '2020-07-22T18:28:40Z' to Eastern Time
#' @param date of the form '2020-07-22T18:27:52Z'
#' @return date of the form '2020-07-22 14:28:40'
#' @keywords internal
convert_time <- function(date) {
  date <- gsub("T", " ", date)
  time <- strptime(date,
                   format = "%Y-%m-%d %T",
                   tz = "UTC") %>%
    as.POSIXct() %>%
    format(tz = "America/New_York")

  return(time)

}



#' Helper Function to Convert get_data output to data frame
#'
#' This function is for use with the `df=TRUE` argument with
#' \code{\link{get_data}}. It converts the nested list containing
#' the data into a data frame.
#'
#' @param parsed_data output from `jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = TRUE,flatten = TRUE)`
#' in the get_data function.
#'
#' @return a data frame of 1 row, where each column represents
#' data a field of information from the nested list.
#'
#' @keywords internal
convert_df <- function(parsed_data) {
  if (purrr::is_empty(parsed_data)) {
    message("No data available for the given url.")
    return(NULL)
  }
  else if (!is.data.frame(parsed_data)) {

  parsed_data <- parsed_data %>%
      unlist(recursive = FALSE) %>%
      t()  %>%
      as.data.frame()
  }
  parsed_data %>%
    select(!dplyr::contains("display")) %>%
    rename_with(~gsub("attributes\\.",
                      "",
                      .x))
}





