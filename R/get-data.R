

#' Acquire Data for the Given Url
#'
#' @param url a url containing a valid API call
#' @param df logical, \code{TRUE} if you want the output as a data frame.
#' Default value is \code{FALSE}
#' @importFrom httr config GET
#' @export
#' @examples
#'\dontrun{
#'get_data("https://api.regulations.gov/v4/documents?filter[docketId]=CMS-2014-0063&api_key=DEMO_KEY")
#'}
get_data <- function(url, df = FALSE) {

  #resp <- httr::GET(url, config = config(ssl_verifypeer=FALSE))
  resp <- httr::RETRY(verb = "GET",
                      url = url,
                      config = config(ssl_verifypeer = FALSE),
                      pause_min = 4,
                      pause_base = 2,
                      pause_cap = 3010,
                      max_times = 50,
                      terminate_on = 400)
  # check if type is as expected
  if (httr::http_type(resp) != "application/vnd.api+json") {
    httr::http_status(resp) %>%
      paste(collapse = "\n") %>%
      message()
    stop("API failed to return json", call. = FALSE)
  }
  else if (httr::http_error(resp)) {
    error_message <- httr::http_status(resp) %>%
      paste(collapse = "\n")
    stop(error_message,
      call. = FALSE)
  }

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
#' @return a nested list containing all elements corresponding to the given url.
#' Functionality is not yet implemented for obtaining all elements when there are
#' more than 5000 elements.
#' @examples
#' \dontrun{iterate_over_pages("https://api.regulations.gov/v4/comments?filter
#' [commentOnId]=09000064816e1a41&page[size]=250&page[number]=1&
#' sort=lastModifiedDate,documentId&api_key=DEMO_KEY")
#' }
iterate_over_pages <- function(url) {
  first <- get_data_by_page(url, page_number = 1)

  if (is.null(first$meta$totalElements)) {
    # message("no pages")
    pages <- first
  }

  else if (first$meta$totalElements <= 250) {
    #message("Number of elements is only ", first$meta$totalElements)
    # all elements will be on the first page
    pages <- first
  }

  else if (first$meta$totalElements > 250 && first$meta$totalElements <= 5000 ) {
   # message("Number of elements is  ", first$meta$totalElements, " so iterating over pages")

    # since we can have 250 elements on each page, set number of pages to get all elements
    end <- floor(first$meta$totalElements /250) + 1
    # print(end)
    pages <- map(1:end, ~get_data_by_page(page_number = .x, url=url))
  }
  else{
    message("There are ", first$meta$totalElements,
            " comments. The time for this request for detailed information is expected to be at least ",
            (first$meta$totalElements/500), " hours.")
  #  message("not yet implemented; call will take over an hour due to API rate limit")
    pages <- get_all(url, first$meta$totalElements)
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
get_all <- function(url, num_elements) {

  n <- num_elements

  # 20 pages, 250 elements per page
  iterations <- floor(n / (250*20)) + 1

  results <- map(1:20, ~get_data_by_page(page_number = .x,
                                            url = url)$data)
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

    message(new_url)

    pages <- map(1:20, ~get_data_by_page(page_number = .x,
                                           url = new_url)$data)

    last_date <- find_element(pages[[20]],
                              "lastModifiedDate") %>%
      unlist(use.names = FALSE)

    last_date <- last_date[length(last_date)]
    last_date <- convert_time(last_date)
    # message(last_date)

    results <- append(results, pages)
  }
  return(results)
}


#' Convert Date of the form '2020-07-22T18:28:40Z' to Eastern Time
#' @param date of the form '2020-07-22T18:27:52Z'
#' @return date of the form '2020-07-22 14:28:40'
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
convert_df <- function(parsed_data) {

  if (!is.data.frame(parsed_data)) {

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





