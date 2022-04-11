

#' Get All Comments for a Specific Docket or Document
#'
#' This function automates the process of obtaining the metadata,
#' including download links for attachments for
#' all comments corresponding to a specific document
#' or docket ID as multiple API calls will be necessary.
#'
#' @param docketId character string; the docket ID for the comments needed. If you're providing a document
#' ID with the \code{document_id} argument, this should be left \code{NULL}.
#' @param documentId character string; the document ID for the comments needed.
#' @param key your regulations.gov API key. You can leave this NULL if the key has
#' been set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#'   Obtain a key at [here](https://open.gsa.gov/api/regulationsgov/#getting-started).
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
#' comments_CMS_2014_0063 <- get_all_comments(docketId = "CMS-2014-0063", test = TRUE)
#' }
get_all_comments <- function(docketId = NULL, documentId = NULL, test = FALSE, key = NULL, quiet = TRUE) {
  key <- check_auth(key)
  url <- ifelse(!is.null(documentId),
                construct_document_url(documentId = documentId,
                                       key = key),
                construct_document_url(docketId = docketId,
                                       key = key))

  docs <- iterate_over_pages(url)

  objids <- find_element(docs, "objectId")


  # extract all comments for each document; each link corresponds to one document
  comments_per_doc_links <- paste0("https://api.regulations.gov/v4/comments?filter[commentOnId]=",
                                   unlist(objids),
                                   "&page[size]=250&page[number]=1&sort=lastModifiedDate,documentId&api_key=",
                                   key)

  res <- map(comments_per_doc_links, iterate_over_pages)
  res <- unlist(res, recursive = TRUE, use.names = FALSE)

  # extract only the comment links
  comment_links <- res[grepl("https://api.regulations.gov/v4/comments", res)]

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
    extract_meta_slow <- purrr::slowly(extract_meta,
                                       rate = purrr::rate_delay(7.2))
  }
  else {
    extract_meta_slow <- extract_meta
  }

  # retrieve the comment metadata, including download links, for all comments
  r <- map(comment_links, ~{if (!quiet) message(.x); extract_meta_slow(.x, key = key);})

  result <- tryCatch(
    bind_rows(r),

    error = function(cnd) {
      message(cnd)
      r
    }
  )
  result
}



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




