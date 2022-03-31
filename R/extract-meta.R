
#' Extract Metadata for Specific Documents or Comments
#'
#' @param link a link of the form https://api.regulations.gov/v4/comments/ID_HERE for
#' the comment or document ID for which detailed data will be obtained.
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @return a data frame containing the metadata for the comment or document ID, including, if present
#'   the attachment links.
#' @importFrom rlang .data
#' @importFrom dplyr select contains across everything
#' @keywords internal
#'  utils::globalVariables(".")
#' @examples
#' \dontrun{
#' # get comment metadata for a specific comment
#' extract_meta("https://api.regulations.gov/v4/comments/COMMENT_ID_HERE")
#' # get comment metadata for a specific document
#' extract_meta("https://api.regulations.gov/v4/documents/DOCUMENT_ID_HERE")
#' }
extract_meta <- function(link, key = NULL) {
  key <- check_auth(key)

  api_link <- paste0(link, "?include=attachments&api_key=", key)
  parsed <- get_data(api_link)
  metadata <- parsed$data$attributes

  # check if nested list contains download url
  file_url_exists <- find_element(parsed, "fileUrl")

  comment_meta_df <- metadata %>%
    unlist(recursive = FALSE) %>%
    t() %>%
    as.data.frame()

  # adding all download urls as a single comma separated column
  # for access in downstream analyses
  if (!is.null(file_url_exists)) {
    # if there is NOT an included list, download urls
    # are in the fileFormats list within the metadata
    # nested list
    if (is.null(parsed$included)) {
      m <- metadata$fileFormats %>%
        unlist()
      urls <- m[names(m) == "fileUrl"]
    }
    # if there is an included list, must map over this nested list
    # to retrieve all download urls
    else if (!is.null(parsed$included)) {
      urls <- purrr::map(parsed$included,
                         ~find_element(.x,
                                       "fileUrl")) %>%
        unlist()
    }

    # combine all urls into a single string
    file_urls <- urls %>%
      paste0(collapse=",")

    # print(urls)

    comment_meta_df$fileUrl <- file_urls
  }
  # remove unnecessary columns
  comment_meta_df <- comment_meta_df %>%
    dplyr::select(!dplyr::contains("display")) %>%
    dplyr::select(!dplyr::contains("fileFormats")) %>%
    dplyr::mutate(across(everything(),
                         unlist))

  # add ID
  comment_meta_df$cid <- parsed$data$id

  # return data frame of metadata for a single comment ID
  return(comment_meta_df)
}











