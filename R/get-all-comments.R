

#' Get All Comments for a Specific Docket or Document
#'
#' This function automates the process of obtaining the metadata,
#' including download links for attachments for
#' all comments corresponding to a specific document
#' or docket ID as multiple API calls will be necessary.
#' @param endpoint which endpoint you want the url to be based on. The
#' options are "document", "comment", and "docket". Note that this will be
#' the endpoint your parameters will be applied to (e.g. the posted date of
#' the comment versus the posted date of the document).
#' @param ... arguments passed to [construct_document_url()],
#'  [construct_comment_url()],  [construct_docket_url()].
#' @param key the API key passed in the function call; this may be NULL if the user has
#' chosen to set up the key as an environmental variable instead with the function
#' [set_datagov_key()]. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @param test logical; to test if the output is as expected without using up many API calls.
#' @param quiet logical; FALSE if you want to see the progress of the function as it
#' acquires detailed information for each comment. This is recommended if you are obtaining
#' more than 500 comments, since this call will take over an hour to run due to rate
#' limits for the API.
#' @return a data frame containing the comment metadata, including the download links
#' @importFrom dplyr bind_rows mutate
#' @export
#' @examples
#' \dontrun{
#' # retrieve all comments for docket CMS-2014-0063
#' comments_CMS_2014_0063 <- get_all_comments(endpoint = "document",
#' docketId = "CMS-2014-0063", test = TRUE)
#'
#' # retrieve all comments for document NIH-2007-0930-0001
#' comment_metadata <- get_all_comments(endpoint = "document",
#' documentId="NIH-2007-0930-0001")
#'
#' # retrieve all comments for documents last modified between 2021-04-01 12:00:00 and
#' 2021-04-02 12:00:00 with search term 'privacy'
#' comments <- get_all_comments(endpoint = "document",
#' lastModifiedDate = c("2021-04-01 12:00:00", "2021-04-02 12:00:00"),
#' quiet = FALSE, searchTerm = "privacy")
#' }
get_all_comments <- function(endpoint,
                             ...,
                             key = NULL,
                             quiet = TRUE,
                             test = FALSE) {
  key <- check_auth(key)
  if (!endpoint %in% c("document", "docket", "comment")) {
    stop("Invalid Input",
    "\nThe only endpoints available are 'document', 'docket', and 'comment.'",
    call. =  FALSE)
  }

  if (endpoint == "document") {
    url <- construct_document_url(..., key = key)
    if (!quiet) {
      message("URL constructed based on given arguments: ",
              url, "\n") }
    docs <- iterate_over_pages(url, quiet = quiet)
    comment_links <- get_comment_links(docs,
                                       key = key,
                                       quiet = quiet)
  }

  else if (endpoint == "docket") {
    url <- construct_docket_url(..., key = key)
    if (!quiet) {
      message("URL constructed based on given arguments: ",
              url, "\n") }
    docs <- iterate_over_pages(url, quiet = quiet)
    docketids <- purrr::map(docs, ~.x["id"]) %>%
      unlist()

    #docketids <- paste0(docketids, collapse = ",")
    url <- construct_document_url(docketId = docketids,
                                  key = key)
    docs <- iterate_over_pages(url,
                               quiet = quiet)
    comment_links <- get_comment_links(docs,
                                       key = key,
                                       quiet = quiet)
  }

  else if (endpoint == "comment") {
    url <- construct_comment_url(..., key = key)
    if (!quiet) {
      message("URL constructed based on given arguments: ",
              url, "\n") }
    comments <- iterate_over_pages(url, quiet = quiet)
    commentids <- purrr::map(comments, ~.x["id"]) %>%
      unlist(use.names = FALSE)
    comment_links <- paste0("https://api.regulations.gov/v4/comments/",
                            commentids)
  }

  # for testing
  if (test) {
    comment_links <- comment_links[1:3]
  }

  # if interactive session, check if the user wants to proceed
  # when number of elements is high and run time will also be high

  # avoid hitting rate limit by slowing iteration if there are more than 500 comments
  # since rate limit is 500 per hour
  if (length(comment_links) > 500) {
    if (interactive()) {
      continue <- check_continue(length(comment_links))
      if (!continue) return(NULL)
    }
    # iterate with a delay of 7.2 seconds for each request to
    # avoid hitting the 500 per hour rate limit
    extract_meta_slow <- purrr::slowly(extract_meta,
                                       rate = purrr::rate_delay(7.2))
  }
  else {
    # if there are less than 500 links the function does not need
    # to be slowed down
    extract_meta_slow <- extract_meta
  }

  # retrieve the comment metadata, including download links, for all comments
  r <- map(comment_links, ~{if (!quiet) message(.x);
    extract_meta_slow(.x, key = key);})

  result <- tryCatch(
    bind_rows(r),

    error = function(cnd) {
      message(cnd)
      r
    }
  )
  result
}


#' Get Comment Links
#'
#' Extract objectIds from nested list, collect comments corresponding
#' to each `objectId`, and return the links of comment links obtained so
#' detailed comment-specific metadata can be obtained.
#'
#' @param docs a nested list containing data from a valid API call
#' containing documents and their corresponding objectIds
#' @param key valid API key
#' @param quiet logical; TRUE if you want messages printed,
#' FALSE otherwise
#' @keywords internal
get_comment_links <- function(docs, key, quiet = TRUE) {
  objids <- purrr::map(docs, ~find_element(.x, "objectId")) %>%
    unlist() %>%
    unique()

  if (!quiet) {
    message("\nNumber of Object IDs:", length(objids), "\n") }


  # extract all comments for each document; each link corresponds to one document
  comments_per_doc_links <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=",
                                   unlist(objids),
                                   "&page[size]=250&page[number]=1&sort=lastModifiedDate,documentId&api_key=",
                                   key)

  res <- map(comments_per_doc_links, ~iterate_over_pages(.x,
                                                         quiet = quiet))
  res <- unlist(res, recursive = TRUE, use.names = FALSE)

  # extract only the comment links
  comment_links <- res[grepl("https://api.regulations.gov/v4/comments", res)]

  return(comment_links)

}

#' Check if the User Wants to Continue
#'
#' In an interactive session, check if the user wants to continue when
#' the number of requests means the call will take an extended amount of
#' time (over 500 requests means the call will take over an hour due to
#' the 500 per hour request limit).
#' @param num_elements of elements associated with the url
check_continue <-  function(num_elements) {
  message("There are ",
                    num_elements,
                    " elements.\nDue to the rate limits for the regulations.gov API,",
                    "\nthis will take ",
                    num_elements/500,
                    " hours to complete.")

  response <- readline(prompt = "Do you want to continue (Y/N)? ")

  if (tolower(response) %in% c("y", "yes")) {
    return(TRUE)
  }
  else {
    message("Exiting the function.")
    return(FALSE)
  }
}




