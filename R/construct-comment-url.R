utils::globalVariables(c("term", "value", "filt"))

#' Create a URL for the Comments Endpoint
#'
#' Takes the parameters available for the comments endpoint of the regulations.gov API, as
#' described [here](https://open.gsa.gov/api/regulationsgov/) under the
#' comments section under API calls, and constructs a URL that can be used to retrieve the
#' data specified. To understand how these parameters relate to the data, it may help to go the
#' the regulations.gov search function and go to a [specific docket](https://www.regulations.gov/docket/CMS-2014-0063)
#' or [document](https://www.regulations.gov/document/CMS-2014-0063-0001) and look at the associated comments.
#'
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @param commentId a comment ID that uniquely identifies the comment.
#' @param attachments character string "true" if you want the download links included for a comment,
#' otherwise leave as NULL. Note the API only provides functionality for obtaining download link if you provide a comment ID
#' with the `commentId` argument. You can see this under the Comment ID Section at the page for any
#' specific comment (see the [example here](https://www.regulations.gov/comment/ED-2011-OSERS-0012-0231)).
#' @param postedDate character vector of length 1, representing a single posted date, or a vector of length
#'  2, representing a date range beginning with the first element and ending at the second. Each element
#'  must be in the format yyyy-MM-dd.
#' @param agencyId character string representing a government agency, for example, "CMS" or "EPA" or a character
#' vector of multiple agencies
#' @param searchTerm character string representing a search term to filter the results for or a character
#' vector with multiple search terms to filter for.
#' @param commentOnId character vector containing the `objectId`s of the documents for which you want
#' to obtain comments.
#' @param lastModifiedDate a character vector of length 1, representing a single posted date to filter for,
#'  or a vector of length 2, representing a date range beginning with the first element and ending at the second.
#'  Each element must be in the format yyyy-MM-dd HH:mm:ss.
#' @param sort character string representing which value you want to sort the results by, supported variables are
#' postedDate, lastModifiedDate and documentId.
#' @param page_number character string representing which page to retrieve the data; useful when
#' output is on multiple pages. Valid values are between 1 and 20. Default value is 1.
#' @param page_size character string representing how many elements should be on each page. Valid values
#' are between 5 and 250. The default value is 250.
#' @export
#' @examples
#' construct_comment_url(
#'   agencyId = c("CMS", "EPA"),
#'   postedDate = c("2020-02-02", "2020-10-02"),
#'   key = "DEMO_KEY"
#' )
#' construct_comment_url(
#'   searchTerm = "case numbers",
#'   postedDate = c("2021-01-02", "2021-01-15"),
#'   key = "DEMO_KEY"
#' )
construct_comment_url <- function(key = NULL,
                                  commentId = NULL,
                                  attachments = NULL,
                                  postedDate = NULL,
                                  agencyId = NULL,
                                  searchTerm = NULL,
                                  commentOnId = NULL,
                                  lastModifiedDate = NULL,
                                  sort = NULL,
                                  page_number = 1,
                                  page_size = 250) {

  # get arguments as a named list
  arguments <- as.list(environment())
  # retrieve key
  key <- check_auth(key)

  # check that given arguments are valid
  validate_params_comments(arguments)


  # set base url
  base <- "https://api.regulations.gov/v4/comments"

  if (!is.null(commentId)) {
    # no need for multiple pages for single document
    url <- paste0(base, "/", commentId, "?api_key=", key)

    if (!is.null(attachments)) {
      url <- paste0(
        base, "/", commentId,
        "?include=attachments&api_key=", key
      )
    }
  } else {
    url <- make_url(arguments, base, key)
  }
  return(url)
}

make_url <- function(arguments, base, key) {
  arguments <- map(
    names(arguments),
    ~ collapse_values(., arguments = arguments)
  ) %>%
    unlist(recursive = FALSE)

  not_filt <- c("sort", "page_number", "page_size")

  filters <- unlist(arguments) %>%
    dplyr::as_tibble(rownames = "term") %>%
    dplyr::filter(term != "key") %>%
    # order terms so sort term is last
    dplyr::mutate(term = factor(term,
      levels = c(
        .data$term[!term == "sort"],
        "sort"
      )
    )) %>%
    dplyr::arrange(term) %>%
    mutate(
      filt = ifelse(!(term %in% not_filt),
        paste0("filter[", term, "]=", value),
        paste0(term, "=", value)
      ),
      filt = gsub("page_size",
        "page[size]",
        filt,
        fixed = TRUE
      ),
      filt = gsub("page_number",
        "page[number]",
        filt,
        fixed = TRUE
      ),
      filt = gsub("[postedDate1]",
        "[postedDate][ge]",
        filt,
        fixed = TRUE
      ),
      filt = gsub("[postedDate2]",
        "[postedDate][le]",
        filt,
        fixed = TRUE
      ),
      filt = gsub("[lastModifiedDate1]",
        "[lastModifiedDate][ge]",
        filt,
        fixed = TRUE
      ),
      filt = gsub("[lastModifiedDate2]",
        "[lastModifiedDate][le]",
        filt,
        fixed = TRUE
      )
    ) %>%
    dplyr::pull(filt) %>%
    paste0(collapse = "&")

  url <- paste0(base, "?", filters, "&api_key=", key)
  return(url)
}
