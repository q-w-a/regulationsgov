

#' Get Document Metadata for Given Documents or Docket
#'
#' @param docketId character string; the docket ID for the documents needed. If you're providing a document
#' ID with the \code{document_id} argument, this should be left \code{NULL}.
#' @param documentId character string or character vector containing the document ID
#' or multiple document IDs for which metadata is desired.
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @return a data frame
#' @export
#' @examples
#' \dontrun{
#' # get all document data for documents associated with docket FDA-2009-N-0501
#' result <- get_all_documents(docketId ="FDA-2009-N-0501")
#' # get all document data for document IDs
#' result <- get_all_documents(documentId = c("FDA-2012-S-1144-0322",
#'  "NHTSA-2008-0060-0827", "DOT-OST-2018-0206-0008", "CMS-2018-0104-0001"))
#' }
get_all_documents <- function(docketId = NULL, documentId = NULL, key = NULL) {
  key <- check_auth(key)

  if (!is.null(docketId)) {
    documents <- iterate_over_pages(construct_document_url(docketId = docketId,
                                                           key = key))

    documentId <- documents$data$id
  }

  documents <- map(documentId,
                   ~paste0("https://api.regulations.gov/v4/documents/",
                           .x) %>%
                     extract_meta(key = key))

  result <- tryCatch( {
    bind_rows(documents)

    },
    error = function(e) {
      documents <- map(documents, ~mutate(.x,
                             across(everything(),
                                    as.character)))
      bind_rows(documents)
    }
  )

}



