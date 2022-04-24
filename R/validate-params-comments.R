globalVariables(c("arg"))



#' Validate the Parameters Passed to the `construct_comments_url` Function
#'
#' Ensure the arguments the `construct_comments_url` function receives
#' aren't invalid. This helps to prevent making failed API calls.
#'
#' @param arg_list named list containing arguments to the [construct_comment_url()] function.
#' @keywords internal
validate_params_comments <- function(arg_list) {

  err <- ""

  # check there is only one commentId
  if (!is.null(arg_list[["commentId"]])) {
    if (length(arg_list[["commentId"]]) > 1) {
      err <- paste0(err,
                    "\ncommentId only accepts one commentId")
    }
    # check no invalid arguments are provided
    res <- check_non_id(arg_list, "commentId")
    if (!is.null(res))  err <- paste0(err, res)
  }
  # check that attachments is NULL or 'true'
  if (!is.null(arg_list[["attachments"]]) && arg_list[["attachments"]] != "true")   {
    err <- paste0(err,
                  "\nattachments must be 'true' or NULL")
  }


  # check postedDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["postedDate"]])) {
    err <- paste0(err, check_date(arg_list[["postedDate"]],
                             "postedDate"))
  }

  # check lastModifiedDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["lastModifiedDate"]])) {
    err <- paste0(err, check_date(arg_list[["lastModifiedDate"]],
                             "lastModifiedDate"))
  }

  # check sort is one or more of accepted values
  if (!is.null(arg_list[["sort"]])) {
    valid_types <- c( "postedDate",
                     "lastModifiedDate",
                     "documentId")

    invalid <- !(arg_list[["sort"]] %in% valid_types)
    if (any(invalid)) {
      err <- paste0(err,
                    '\nonly can sort by one or more of the following:\n',
                    '"postedDate", "lastModifiedDate", "documentId"',
                    "\nCheck Entries: ",
                    paste0(arg_list[["sort"]][invalid],
                           collapse = ", "))
    }
  }
  if (err != "") {
    stop(err, call. = FALSE)
  }
}

#' Check if Invalid Arguments Are Provided When ID Argument is Provided
#'
#' Because the structure of the url is different if the user provides an id,
#' more specifically a "docketId", "documentId", or "commentId", this function
#' checks if the user has supplied additional arguments that can't be included into
#' the valid API call, and constructs an error accordingly.
#'
#' @param arg_list named list containing arguments to the one of the `*_construct_url` functions.
#' @param id which type of ID to check ("docketId", "documentId", or "commentId")
#' @keywords internal
check_non_id <- function(arg_list, id) {
  # check no invalid arguments are provided
  null_vals <- purrr::map_lgl(arg_list,
                              purrr::is_null) %>%
  dplyr::as_tibble(rownames = "arg") %>%
  dplyr::filter(!(arg %in% c("key", id,
                             "attachments", "page_number",
                             "page_size")) & value == FALSE)
  if (nrow(null_vals) > 0) {
    res <- paste0( "\n If ", id, " is provided, the only other argument",
                  "\nthat is valid is the attachments argument.\n",
                  "Check Arguments: ",
                  paste0(null_vals["arg"], collapse = "\n"))
    return(res)
  }
  else {
    return(NULL)
  }
}
