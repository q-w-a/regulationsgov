

#' Get Document Metadata for Given Documents or Docket
#'
#' @param endpoint string that represents which endpoint you want the
#' url to be based on. The options are "document" and "docket". Note that this will be
#' the endpoint your parameters will be applied to (e.g. the posted date of
#' the document versus the posted date of the docket). The default value
#' is "document."
#' @param ... arguments passed to [construct_document_url()] or
#' [construct_docket_url()], whichever function corresponds to the endpoint provided.
#' @param quiet logical; FALSE if you want to see the progress of the function as it
#' acquires detailed information for each comment.
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @return a data frame
#' @export
#' @examples
#' \dontrun{
#' # get all document data for documents associated with docket FDA-2009-N-0501
#' result <- get_all_documents(docketId = "FDA-2009-N-0501")
#' # get all document data for document IDs
#' result <- get_all_documents(documentId = c(
#'   "FDA-2012-S-1144-0322",
#'   "NHTSA-2008-0060-0827", "DOT-OST-2018-0206-0008", "CMS-2018-0104-0001"
#' ))
#' }
get_all_documents <- function(endpoint = "document",
                              ...,
                              quiet = TRUE,
                              key = NULL) {
  key <- check_auth(key)
  if (!endpoint %in% c("document", "docket")) {
    stop("Invalid Input",
      "\nThe only endpoints available are 'document' and 'docket'",
      call. =  FALSE
    )
  }
  args <- list(...)
  if (!is.null(args[["documentId"]])) {
    documentId <- args[["documentId"]]
  } else if (endpoint == "document") {
    validate_params(list(...))
    documents <- construct_document_url(...,
      key = key
    ) %>%
      iterate_over_pages()
    documentId <- documents$data$id
  } else if (endpoint == "docket") {
    validate_params_dockets(list(...))
    url <- construct_docket_url(...,
      key = key
    )
    if (!quiet) {
      message(
        "URL constructed based on given arguments: ",
        url, "\n"
      )
    }
    docs <- iterate_over_pages(url, quiet = quiet)
    docketids <- purrr::map(docs, ~ ifelse("data" %in% names(.x),
      .x$data["id"],
      .x["id"]
    )) %>%
      unlist()

    if (length(docketids) == 0) {
      message("No data to retrieve.")
      return(NULL)
    }


    url <- construct_document_url(
      docketId = docketids,
      key = key
    )
    docs <- iterate_over_pages(url,
      quiet = quiet
    )
    documentId <- purrr::map(docs, ~ ifelse("data" %in% names(.x),
      .x$data["id"],
      .x["id"]
    )) %>%
      unlist()
  }

  # if there are more than 500 links, slow the iteration to
  # avoid hitting the rate limit
  if (length(documentId) > 500) {
    if (interactive()) {
      continue <- check_continue(length(documentId))
      if (!continue) {
        return(NULL)
      }
    }
    extract_meta_slow <- purrr::slowly(extract_meta,
      rate = purrr::rate_delay(7.2)
    )
  }
  # otherwise iterate at the normal speed
  else {
    extract_meta_slow <- extract_meta
  }

  # obtain information for all document IDs
  documents <- map(
    documentId,
    ~ paste0(
      "https://api.regulations.gov/v4/documents/",
      .x
    ) %>%
      extract_meta_slow(key = key)
  )

  # bind rows for all individual documents into one data frame
  result <- tryCatch(
    {
      bind_rows(documents)
    },
    error = function(e) {
      documents <- map(documents, ~ mutate(
        .x,
        across(
          everything(),
          as.character
        )
      ))
      bind_rows(documents)
    }
  )
  return(result)
}
