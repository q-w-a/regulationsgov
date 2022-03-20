
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


#' Create a URL for the Documents Endpoint
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
#'   \code{\link{set_datagov_key}}
#' @param documentId character string representing a valid document ID, for example, "CMS-2014-0063-0001"
#' @param docketId character string representing a valid docket ID, for example, "CMS-2014-0063"
#' @param agencyId character string representing a government agency, for example, "CMS" or "EPA"
#' @param commentEndDate character vector of length 1, representing a single comment end date, or length
#'  2, representing a date range starting with the first element of the vector and ending at the second element
#'  of the vector. The format of each date must be yyyy-MM-dd, for example, "2020-12-01"
#' @param documentType character string that is one of the following categories:
#'  "Notice", "Rule", "Proposed Rule", "Supporting & Related Material", "Other"
#' @param frDocNum character string representing the Federal Register Number, for example, "2014-10228"
#' @param searchTerm character string representing a search term to filter the results for
#' @param postedDate character vector of length 1, representing a single posted date, or a vector of length
#'  2, representing a date range beginning with the first element and ending at the second. Each element
#'  must be in the format yyyy-MM-dd.
#' @param lastModifiedDate a character vector of length 1, representing a single posted date to filter for,
#'  or a vector of length 2, representing a date range beginning with the first element and ending at the second.
#'  Each element must be in the format yyyy-MM-dd.
#' @param subtype character string representing a document subtype, for example, "Petitions for Exemption"
#' @param withinCommentPeriod "true" if you only want results that are still open for comment, otherwise leave
#'  this argument null.
#' @param sort character string representing which value you want to sort the results by, supported variables are
#'  "commentEndDate", "postedDate", "lastModifiedDate", "documentId" and "title"
#' @param page_number character string representing which page to retrieve the data; useful when
#' output is on multiple pages. Valid values are
#' @param page_size character string representing how many elements should be on each page. Valid values
#' are between 5 and 250.
#' @export
construct_document_url <- function(
  key = NULL,
  documentId = NULL,
  docketId = NULL,
  agencyId = NULL,
  commentEndDate = NULL,
  documentType = NULL,
  frDocNum = NULL,
  searchTerm = NULL,
  postedDate = NULL,
  lastModifiedDate = NULL,
  subtype = NULL,
  withinCommentPeriod = NULL,
  sort = NULL,
  page_number = 1,
  page_size = 250) {
  # get arguments as a named list
  arguments <- as.list(environment())
  # return(arguments)
  # return(arguments)
  key <- check_auth(key)
  base <- "https://api.regulations.gov/v4/documents"
  if (!is.null(documentId)) {
    # no need for multiple pages for single document
    url <- paste0(base, "/", documentId, "?api_key=", key)
  }
  else {
    not_filt <- c("sort", "page_number", "page_size")
    filters <- unlist(arguments) %>%
      dplyr::as_tibble(rownames = "term") %>%
      dplyr::filter(term != "key") %>% # put search term last
      dplyr::mutate(term = factor(term,
                                  levels = c(.data$term[!term == "sort"],
                                             "sort"))) %>%
      dplyr::arrange(term) %>%
      mutate(value = URLencode(value, reserved = TRUE)) %>%
      mutate(filt = ifelse(!(term %in% not_filt),
                           paste0("filter[", term, "]=", value),
                           paste0(term, "=", value)),
             filt =  gsub( "page_size",
                           "page[size]",
                           filt,
                           fixed = TRUE),
             filt = gsub("page_number",
                         "page[number]",
                         filt,
                         fixed = TRUE),
             filt = gsub("[postedDate1]",
                         "[postedDate][ge]",
                         filt,
                         fixed = TRUE),
             filt = gsub("[postedDate2]",
                         "[postedDate][le]",
                         filt,
                         fixed = TRUE)) %>%
      dplyr::pull(filt) %>%
      paste0(collapse="&")
    url <- paste0(base, "?", filters, "&api_key=", key)

  }
  return(url)

}



# test



#' Get All Comments for a Specific Docket or Document
#'
#' This function automates the process of obtaining the metadata,
#' including download links for attachments for
#' all comments corresponding to a specific document
#' or docket ID as multiple API calls will be necessary.
#'
#' @param docketId character string; the docket ID for the comments needed. If you're providing a document
#' ID with the \code{document_id} argument, this can be left \code{NULL}.
#' @param documentId character string; the document ID for the comments needed.
#' @param test logical; to test if the output is as expected without using up many API calls.
#' @return a data frame containing the comment metadata, including the download links
#' @importFrom dplyr bind_rows mutate
#' @examples
#' \dontrun{
#' # retrieve all comments for docket CMS-2014-0063
#' comments_CMS_2014_0063 <- get_all_comments(docketId = "CMS-2014-0063")
#' }
get_all_comments <- function(docketId = NULL, documentId = NULL, test = FALSE) {
  url <- ifelse(!is.null(documentId),
                construct_document_url(documentId = documentId),
                construct_document_url(docketId = docketId))

  docs <- iterate_over_pages(url)
  objids <- map(docs, ~find_element(.x, "objectId"))
  # print(objids)
  # extract all comments for each document; each link corresponds to one document
  comments_per_doc_links <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=",
                                   unlist(objids),
                                   "&page[size]=250&page[number]=1&sort=lastModifiedDate,documentId&api_key=",
                                   Sys.getenv("DATA_GOV_KEY"))
  #print(comments_per_doc_links)
  res <- map(comments_per_doc_links, iterate_over_pages)
  res <- unlist(res, recursive = TRUE, use.names = FALSE)

  # extract only the comment links
  comment_links <- res[grepl("https://api.regulations.gov/v4/comments", res)]

  # for testing
  if (test) {
    comment_links <- comment_links[1:3]
  }

  # retrieve the comment metadata, including download links, for all comments
  r <- map(comment_links, extract_comment_meta)

  result <- tryCatch(
    do.call("bind_rows", r),

    error = function(cnd) {
      message(cnd)
      r
    }
  )
  result
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



#' Extract Metadata for Specific Comments
#'
#' @param comment_link a link of the form https://api.regulations.gov/v4/comments/COMMENT_ID_HERE for which
#'   detailed data will be obtained.
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#' @return a data frame containing the metadata for the comment ID, including, if present
#'   the attachment links for that comment.
#' @importFrom rlang .data
#' @keywords internal
#'  utils::globalVariables(".")
#' @examples
#' \dontrun{
#' extract_comment_meta("https://api.regulations.gov/v4/comments/COMMENT_ID_HERE")
#' }
extract_comment_meta <- function(comment_link, key = NULL) {
  key <- check_auth(key)
  print(comment_link)
  api_link <- paste0(comment_link, "?include=attachments&api_key=", key)
  #api_link <- paste0(comment_link, "?include=attachments&api_key=DEMO_KEY")

  #  print(api_link)
  parsed <- get_data(api_link)
  fileFormats <- find_element(parsed, "fileFormats")


  metadata <- parsed$data$attributes

  if (is.null(fileFormats)) {
    comment_meta_df <- metadata %>%
      unlist(recursive = FALSE) %>% as.data.frame()
    comment_meta_df$fileFormats <- rep(NA, nrow(comment_meta_df))
  }

  else {
    # combine comma-separated download links into a single column to enable compatibility
    # across comment metadata with differing numbers of attachment links
    fileFormats <- unlist(fileFormats)
    fileFormats <- fileFormats[grepl("http", fileFormats)] %>%
      paste0(collapse = ",") %>%
      gsub('"', "", .)

    comment_meta_df <- metadata %>%
      unlist(recursive = FALSE) %>%
      as.data.frame()

    # add download link information
    comment_meta_df$fileFormats <- fileFormats
  }

  # remove unnecessary display columns
  comment_meta_df <- comment_meta_df[,!grepl("display", names(comment_meta_df))]

  # add comment ID
  comment_meta_df$cid <- parsed$data$id

  # return data frame of metadata for a single comment ID
  return(comment_meta_df)
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
#' @examples
#' \dontrun{iterate_over_pages("https://api.regulations.gov/v4/comments?filter
#' [commentOnId]=09000064816e1a41&page[size]=250&page[number]=1&
#' sort=lastModifiedDate,documentId&api_key=DEMO_KEY")
#' }
iterate_over_pages <- function(url) {
  first <- get_data_by_page(url, page_number = 1)

  if (is.null(first$meta$totalElements)) {
    message("no pages")
    pages <- first
  }

  else if (first$meta$totalElements <= 250) {
    message("Number of elements is only ", first$meta$totalElements)
    pages <- first
  }

  else if (first$meta$totalElements > 250 && first$meta$totalElements <= 5000 ) {
    message("Number of elements is  ", first$meta$totalElements, " so iterating over pages")
    # since we can have 250 elements on each page, set number of pages to get all elements
    end <- floor(first$meta$totalElements /250) + 1
    # print(end)
    pages <- map(1:end, ~get_data_by_page(page_number = .x, url=url))
  }
  else{
    pages = "too many"
  }
  return(pages)
}





