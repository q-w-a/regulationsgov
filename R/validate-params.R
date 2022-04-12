




#' Validate the Parameters Passed to the construct_document_url Function
#'
#' Ensure the arguments the construct_document_url function receives
#' aren't invalid. This helps to prevent making failed API calls.
#'
#' @param arg_list named list containing arguments to the [construct_document_url()] function.
validate_params <- function(arg_list) {

  err <- ""

  # check there is only one documentId
  if (length(arg_list[["documentId"]]) > 1) {
    err <- paste0(err,
                  "\ndocumentId only accepts one documentId")
  }

  # check that attachments is NULL or 'true'
  if (!is.null(arg_list[["attachments"]]) && arg_list[["attachments"]] != "true")   {
    err <- paste0(err,
                  "\nattachments must be 'true' or NULL")
  }

  # check commentEndDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["commentEndDate"]])) {
    err <- paste0(check_date(arg_list[["commentEndDate"]],
                             "commentEndDate"))
  }

  # check all documentTypes are one of the provided values
  if (!is.null(arg_list[["documentType"]])) {
    valid_types <- c("Notice",
                     "Rule",
                     "Proposed Rule",
                     "Supporting & Related Material",
                     "Other")

    invalid <- !(arg_list[["documentType"]] %in% valid_types)

    if (any(invalid)) {
      err <- paste0(err,
                    '\ndocumentType must contain one or more of the following:\n',
                    '"Notice",  "Rule", "Proposed Rule", "Supporting & Related Material", "Other"',
                    "\nCheck Entries: ",
                    paste0(arg_list[["documentType"]][invalid],
                           collapse = ", "))
    }
  }

  # check postedDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["postedDate"]])) {
    err <- paste0(check_date(arg_list[["postedDate"]],
                             "postedDate"))
  }

  # check lastModifiedDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["lastModifiedDate"]])) {
    err <- paste0(check_date(arg_list[["lastModifiedDate"]],
                             "lastModifiedDate"))
  }

  # withinCommentPeriod must be NULL or "true"
  if (!is.null(arg_list[["withinCommentPeriod"]]) && arg_list[["withinCommentPeriod"]] != "true")   {
    err <- paste0(err,
                  "withinCommentPeriod must be 'true' or NULL")
  }


  # check sort is one or more of accepted values
  if (!is.null(arg_list[["sort"]])) {
    valid_types <- c("commentEndDate",
                     "postedDate",
                     "lastModifiedDate",
                     "documentId",
                     "title")

    invalid <- !(arg_list[["sort"]] %in% valid_types)
    if (any(invalid)) {
      err <- paste0(err,
                    '\nonly can sort by one or more of the following:\n',
                    '"commentEndDate, "postedDate", "lastModifiedDate", "documentId", "title"',
                    "\nCheck Entries: ",
                    paste0(arg_list[["sort"]][invalid],
                           collapse = ", "))
        }
    }


  if (err != "") {
    stop(err, call. = FALSE)
  }

}



#' Helper function to check validity of date parameters
#'
#' @param dates date vector or single string to be checked
#' @param param name of the parameter
#' @return error message; empty string if no issues found
check_date <- function(dates, param) {
  err <- ""
  if (length(dates) > 2) {
    err <- paste0(err,
                  "\n",
                  param,
                  " must be a single character string or a character vector of length 2")
  }

  if (param != "lastModifiedDate") {
    invalid <- !grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d",
                      dates)
    if (any(invalid)) {
      err <- paste0(err,
                    "\n", param,
                    " values must be of the form 'yyyy-MM-dd'",
                    "\nCheck Entries: ",
                   paste0(dates[invalid], collapse = ", "))
    }
  }
  else {
    invalid <- !grepl("\\d\\d\\d\\d-\\d\\d-\\d\\d \\d\\d:\\d\\d:\\d\\d",
                      dates)
    if (any(invalid)) {
      err <- paste0(err,
                    "\n", param,
                    " values must be of the form 'yyyy-MM-dd HH:mm:ss'",
                    "\nCheck Entries: ",
                    paste0(dates[invalid], collapse = ", "))
    }
  }

  return(err)
}
