

#' Obtain Text from Download Links and Add to Data Frame
#'
#' @param df data frame produced by \code{\link{get_all_comments}} or \code{\link{get_all_documents}}
#' @param all logical; TRUE if you want to include all attachments for each comment,
#' FALSE if you want to include only one. The default value is FALSE. If you choose
#' TRUE, note that the attachments may include duplicated text, so this should be considered
#' for further analysis.
#' @return data frame with all columns of `df` as well as an additional column `text` that
#' contains the text from the attachment in the download link for that commend ID (if present).
#' @export
#' @examples
#' \dontrun{
#' result <- get_all_documents(documentId = c("FDA-2012-S-1144-0322",
#' "NHTSA-2008-0060-0827", "DOT-OST-2018-0206-0008", "CMS-2018-0104-0001"))
#' resul <- add_text(result)
#' }
add_text <- function(df, all = FALSE) {
  error <- ""
  if (!requireNamespace("textreadr", quietly = TRUE)) {
    error <- paste0(error, "Install \"textreadr\" to use this function.") }
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    error <- paste0(error, "Install \"tesseract\" to use this function.") }

  if (error != "") {
    stop(error,
         call. = FALSE)
  }
  if (!"fileUrl" %in% names(df)) {
    stop("fileUrl is not a column in the data frame")
  }
  text <- purrr::map_chr(df[,"fileUrl"], ~get_text(.x, all))
  df <- cbind(df, text)
  return(df)
}

#' Get Text for Given Link
#' @param link from fileUrl column; may be multiple links comma-separated.
#' @param all logical; TRUE if you want to include all attachments for each comment,
#' FALSE if you want to include only one. The default value is FALSE. If you choose
#' TRUE, note that the attachments may include duplicated text, so this should be considered
#' for further analysis.
#' @keywords internal
get_text <- function(link, all) {
  links <- strsplit(link, ",", fixed = TRUE) %>%
    unlist()
  if (length(links) == 1) {
    text <- extract_text(link)
  }
  else {
    if (all) {
      text <- purrr::map_chr(links,
                             extract_text) %>%
        paste0(collapse = " ")
      message(class(text))
    }
    else {
      link <- choose_element(links)
      text <- extract_text(link)
    }
  }
  return(text)
}

#' Choose Link Based on Format
#' @param links character vector of links
#' @keywords internal
choose_element <- function(links) {

  docs <- grepl("doc", links)
  pdfs <- grepl("pdf", links)

  if (any(docs)) {
    link <- links[which(docs)][1]
  }
  else if (any(pdfs)) {
    link <- links[which(pdfs)][1]
  }
  else {
    link <- links[1]
  }
  return(link)
}

#' Extract Text from Given Link
#' @param link a single download link for a document or comment. For example,
#' "https://downloads.regulations.gov/FDA-2012-S-1144-0322/attachment_1.pdf".
#' @return text extracted from the given download link
#' @keywords internal
extract_text <- function(link) {
  tryCatch( {
    if (is.na(link)) {
      text <- NA
    }
    else if (!(grepl("pdf", link, fixed = TRUE))) {
      text <- textreadr::read_document(link,
                                       combine = TRUE)
    }
    else {
      text <- textreadr::read_pdf(link, ocr = TRUE)[,"text"] %>%
        paste0(collapse = " ")
    }
  },
  error=function(e) {
    message("Attachments for ",
            link,
            " could not be obtained.")
    text <- NA
  })
  if (is.null(text)) {
    return(NA)
  }
  else {
    return(text)
  }
}

