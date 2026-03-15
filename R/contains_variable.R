#' Check if a formula contains a variable
#'
#' \code{contains_variable} checks, if a variable is contained in a formula.
#' \code{contains_variable_internal} skips input validation.
#'
#' @param formula A model formula
#' @param variable The name of the variable to check for (as character vector of
#' length 1)
#'
#' @return \code{TRUE} or \code{FALSE}
#'
#' @export
#'
#' @examples
#' contains_variable(y~a+b+c+d, "d")
#' contains_variable(y~a+b+c+log(d), "d")
#' contains_variable(y~a+b+c*d, "d")
#' contains_variable(y~a+b+c, "d")
#'
contains_variable <- function(formula, variable) {
  # Input validation
  stopifnot("'formula' must have type language" = typeof(formula)=="language",
            "'variable' must be a character vector" = is.character(variable),
            "'variable' must have length 1" = length(variable) == 1)

  return(contains_variable_internal(formula, variable))
}

#' @rdname contains_variable
#' @export
contains_variable_internal <- function(formula, variable) {
  variable_found <- FALSE

  # For each component of the expression, check:
  # If it is a call, does it contain the variable
  # If it is a symbol, is it the variable
  for (i in 1:length(formula)) {
    if (i==1) next

    # Recursive search of the variable in terms
    if (is.call(formula[[i]])) {
      variable_found <- variable_found || contains_variable_internal(formula[[i]], variable)

    } else if (is.symbol(formula[[i]])) {
      if (as.character(formula[[i]]) == variable) {
        variable_found <- TRUE
      }
    }
  }

  # Return the result
  return(variable_found)
}
