
#' Validate the Parameters Passed to the construct_docket_url Function
#'
#' Ensure the arguments the `construct_document_url` function receives
#' aren't invalid. This helps to prevent making failed API calls.
#'
#' @param arg_list named list containing arguments to the [construct_docket_url()] function.
#' @keywords internal
validate_params_dockets <- function(arg_list) {

  err <- ""

  # check there is only one docketId
  if (!is.null(arg_list[["docketId"]])) {
    if (length(arg_list[["docketId"]]) > 1) {
      err <- paste0(err,
                    "\ndocketId only accepts one docketId")
    }
    # check no invalid arguments are provided
    res <- check_non_id(arg_list, "docketId")
    if (!is.null(res)) err <- paste0(err, res)
  }

  if (!is.null(arg_list[["docketType"]])) {
    if (!arg_list[["docketType"]] %in% c("Rulemaking", "Nonrulemaking")) {
      err <- paste0(err, "docketType must be one of the following:\n'Rulemaking', 'Nonrulemaking'")
    }

  }

  # check lastModifiedDate is less than or equal to length 2 and dates are of the correct format
  if (!is.null(arg_list[["lastModifiedDate"]])) {
    err <- paste0(err, check_date(arg_list[["lastModifiedDate"]],
                                  "lastModifiedDate"))
  }

  # check sort is one or more of accepted values
  if (!is.null(arg_list[["sort"]])) {
    valid_types <- c( "title",
                      "lastModifiedDate",
                      "docketId")

    invalid <- !(arg_list[["sort"]] %in% valid_types)
    if (any(invalid)) {
      err <- paste0(err,
                    '\nOnly can sort by one or more of the following:\n',
                    '"title", "lastModifiedDate", "docketId"',
                    "\nCheck Entries: ",
                    paste0(arg_list[["sort"]][invalid],
                           collapse = ", "))
    }
  }
  if (err != "") {
    stop(err, call. = FALSE)
  }
}
