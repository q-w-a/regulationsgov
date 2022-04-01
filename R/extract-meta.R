
#' Extract Metadata for Specific Documents or Comments
#'
#' @param link a link of the form https://api.regulations.gov/v4/comments/COMMENT_ID_HERE for
#' the comment endpoint or a link of the form https://api.regulations.gov/v4/comments/DOCUMENT_ID_HERE
#' for the document endpoint. Detailed information will be obtained on the document or comment of the
#' given ID.
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}. You can use "DEMO_KEY" for a very limited number of calls if needed.
#' @return a data frame containing the metadata for the comment or document ID, including, if present
#'   the attachment links.
#' @importFrom rlang .data
#' @importFrom dplyr select contains across everything rename_with mutate
#' @keywords internal
#'  utils::globalVariables(".")
#' @examples
#' \dontrun{
#' # get comment metadata for a specific comment
#' result_comment <- extract_meta("https://api.regulations.gov/v4/comments/NOAA-NMFS-2013-0095-0007")
#' # get comment metadata for a specific document
#' result_doc <- extract_meta("https://api.regulations.gov/v4/documents/NIH-2006-0048-0001")
#' }
extract_meta <- function(link, key = NULL) {
  # set to NULL to create binding
  . <- NULL
  key <- check_auth(key)

  api_link <- paste0(link, "?include=attachments&api_key=", key)
  parsed <- get_data(api_link)

  #metadata <- parsed$data$attributes

  # check if nested list contains download url
 # file_url_exists <- find_element(parsed, "fileUrl")

  comment_meta_df <- parsed %>%
    unlist(recursive = TRUE) %>%
    t()  %>%
    as.data.frame()


  # remove unnecessary columns and clean column names
  comment_meta_df <- comment_meta_df %>%
    select(!dplyr::contains("display")) %>%
    rename_with(~gsub("fileFormats\\.|attributes\\.|included\\.",
                             "", .x)) %>%
    rename_with(~gsub(".", "_", .x,
                             fixed = TRUE)) %>%
    mutate(dplyr::across(dplyr::everything(),
                  ~paste0(unlist(.x),
                          collapse = ','))) %>%
    mutate(fileUrl = paste0(
      select(., contains("fileUrl")),
                            collapse = ",")) %>%
    select(-dplyr::matches("fileUrl[[:digit:]]")) %>%
    select(-which(dplyr::all_of(.) == ""))

  # add ID
  comment_meta_df$cid <- parsed$data$id

  # return data frame of metadata for a single comment ID
  return(comment_meta_df)
}







