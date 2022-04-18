
utils::globalVariables("result")


#' Download All Files
#'
#' Download all the files corresponding to the given file URLS
#' of documents or comments.
#'
#' @param data_frame a data frame of the form created by [get_all_documents()]
#' or [get_all_comments()]
#' @param dest destination directory where you want the files to be downloaded.
#' Paths can be relative or absolute.
#' @param format a character vector of the file formats to download.
#' The default is `NULL` in which case all files are downloaded.
#' @param ... additional parameters to be passed to [utils::download.file()]
#' @export
download_all <- function(data_frame, dest, format = NULL, ...) {

  if (is.data.frame(data_frame)) {
    urls <- result[["fileUrl"]]
  }
  else if (is.character(urls)) {
    urls <- data_frame
  }
  else{
    stop("data_frame argument is in an invalid format. Supply a data frame or character
         vector containing the file urls.")
  }

  # add trailing / if not present
  if (substr(dest,
             nchar(dest),
             nchar(dest)) != "/") {
    dest <- paste0(dest, "/")
  }

  # create directory if it doesn't exist
  if (!dir.exists(dest)) {
    dir.create(dest)
  }

  # extract all individual urls from character
  # vector, since some entries contain multiple urls
  urls <- strsplit(urls, ",") %>%
    unlist(use.names = FALSE)

  if (!is.null(format)) {
    format <- paste0(format, collapse = "|")
    urls <- urls[grep(format, urls)]
    if (length(urls) == 0) {
      stop("There are no files of the given formats.")
    }
  }

  # split the urls by / to extract name
  names <- strsplit(urls, "/")

  # create names for files based on ID and file format
  names <- purrr::map_chr(names, ~{
    l <- unlist(.x)
    n <- length(l)
    paste0(l[(n-1):n],
         collapse = "_")})

  # download files
  purrr::pwalk(list(url =urls,
                   name =names),
              function(url, name) {
                utils::download.file(url = url,
                             destfile = paste0(dest, name,
                                                    collapse = ""),
                             method = "auto",
                             ...)} )

}


