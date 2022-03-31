


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
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @param documentId character string representing a valid document ID, for example, "CMS-2014-0063-0001"
#' @param attachments character string "true" if you want the download links included for a document,
#' otherwise leave as NULL. Note the API only provides functionality for obtaining download link if you provide a document ID
#' with the `documentId` argument.
#' @param docketId character string representing a valid docket ID, for example, "CMS-2014-0063"
#' @param agencyId character string representing a government agency, for example, "CMS" or "EPA" or a character
#' vector of multiple agencies.
#' @param commentEndDate character vector of length 1, representing a single comment end date, or length
#'  2, representing a date range starting with the first element of the vector and ending at the second element
#'  of the vector. The format of each date must be yyyy-MM-dd, for example, "2020-12-01"
#' @param documentType character string that is one of the following categories, or, if multiple document types
#' are desired, a character vector containing a subset of these categories:
#'  "Notice", "Rule", "Proposed Rule", "Supporting & Related Material", "Other"
#' @param frDocNum character string representing the Federal Register Number, for example, "2014-10228"
#' @param searchTerm character string representing a search term to filter the results for or a character
#' vector with multiple search terms to filter for.
#' @param postedDate character vector of length 1, representing a single posted date, or a vector of length
#'  2, representing a date range beginning with the first element and ending at the second. Each element
#'  must be in the format yyyy-MM-dd.
#' @param lastModifiedDate a character vector of length 1, representing a single posted date to filter for,
#'  or a vector of length 2, representing a date range beginning with the first element and ending at the second.
#'  Each element must be in the format yyyy-MM-dd.
#' @param subtype character string representing a document subtype, for example, "Petitions for Exemption" or
#' "Request for Comments"
#' @param withinCommentPeriod "true" if you only want results that are still open for comment, otherwise leave
#'  this argument null.
#' @param sort character string representing which value you want to sort the results by, supported variables are
#'  "commentEndDate", "postedDate", "lastModifiedDate", "documentId" and "title"
#' @param page_number character string representing which page to retrieve the data; useful when
#' output is on multiple pages. Valid values are
#' @param page_size character string representing how many elements should be on each page. Valid values
#' are between 5 and 250.
#' @export
#' @examples
#' \dontrun{
#' url <- construct_document_url(agencyId = c("CMS", "EPA"),
#' postedDate = c("2020-02-02", "2020-10-02"),
#' key = "DEMO_KEY")
#' url <- construct_document_url(documentId = "CMS-2014-0063-0001",
#' attachments = "true",
#' key = "DEMO_KEY")}
construct_document_url <- function(
  key = NULL,
  documentId = NULL,
  attachments = NULL,
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
  # retrieve key
  key <- check_auth(key)
  base <- "https://api.regulations.gov/v4/documents"
  if (!is.null(documentId)) {
    # no need for multiple pages for single document
    url <- paste0(base, "/", documentId, "?api_key=", key)

    if (!is.null(attachments)) {
      url <- paste0(base, "/", documentId, "?include=attachments&api_key=", key)
    }
  }
  else {
    arguments <- map(names(arguments),
                     ~collapse_values(.,arguments=arguments)) %>%
      unlist(recursive = FALSE)
    not_filt <- c("sort", "page_number", "page_size")
    filters <- unlist(arguments) %>%
      dplyr::as_tibble(rownames = "term") %>%
      dplyr::filter(term != "key") %>%
      # order terms so sort term is last
      dplyr::mutate(term = factor(term,
                                  levels = c(.data$term[!term == "sort"],
                                             "sort"))) %>%
      dplyr::arrange(term) %>%
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

#' Collapse Argument Values
#'
#' Collapse values to a comma separated string when a list
#' argument contains multiple values that need to be included in
#' the url.
#'
#' @param x name of element in the named list arguments
#' @param arguments named list containing arguments passed to the
#'  \code{\link{construct_document_url}} function
 collapse_values <- function(x, arguments) {
   if (!is.null(arguments[[x]]) &
       !(x %in% c("postedDate",
                  "lastModifiedDate"))) {
     args <- unlist(arguments[x])
     args <- tryCatch( {args %>% URLencode(reserved=TRUE)},
                       error = function(e) {
                         return(args)
                       })
     arguments[x] <- paste0(unlist(args), collapse = ',')
     return(arguments[x])
   }
  else {
    return(arguments[x])
  }
 }
