
#' Verify Key is Available
#'
#' This function checks if the user has the API key set up or passed as an argument
#'
#' @param key the API key passed in the function call; this may be NULL if the user has
#'   chosen to set up the key as an environmental variable instead with the function
#'   \code{\link{set_datagov_key}}
#' @keywords internal
check_auth <- function(key) {
  if (identical(Sys.getenv("DATA_GOV_KEY"), "") && is.null(key)) {
    stop("Valid API key needed. Obtain one at https://open.gsa.gov/api/regulationsgov/#getting-started")
  }
  else {
    if (!is.null(key)) return(key)
    else return(Sys.getenv("DATA_GOV_KEY"))
  }
}





#' Find Element in a Nested List with the Given Name
#'
#' Find the value of the element that contains the given string in its name in
#' a nested list. Based off of advice
#' [here](https://stackoverflow.com/questions/58400176/r-find-object-by-name-in-deeply-nested-list).
#' @param nested_list a nested list, such as the type returned by fromJSON
#' @param search_for a character string that is the name of the element we want to find in `nested_list`
#' @return any values of the list where the name contains the substring `search_for`
#' @importFrom utils hasName
#' @keywords internal
#' @export
#' @examples {
#' # create list of data frames for example
#' df_nest <- list(Y = data.frame(x.test = c(1,2,3)),
#' Z = data.frame(w = c("a","b","c")))
#'
#' # extract element containing 'test'
#' find_element(df_nest, "test")
#'
#' # create nested list for example
#' list_nest <- list(Y = c(1,2,3),
#' z = list(x.test = c(1,2,3),
#' y = c("no", "yes")))
#' find_element(list_nest, "test")
#' }
find_element <- function(nested_list, search_for) {
  # if the element being searched for is found, return the value under this name
  index <- grep(search_for, names(nested_list))
  if (length(index) > 0) {
    return(nested_list[index])
  }
  else if (is.list(nested_list)) {
    for (obj in nested_list) {
      # check each list in nested_list recursively
      result <- Recall(obj, search_for)
      if (!is.null(result)) return(result)
    }
  }
  else {
    return(NULL)
  }
}


