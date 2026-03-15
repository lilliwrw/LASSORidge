#' Extracts all variables in a formula
#'
#' \code{extract_variables} returns all variables contained in the formula as
#' character vector.
#'
#' @param formula A model formula
#' @param skip_output The output variable of the formula is not returned.
#' (default: TRUE)
#'
#' @return A character vector of the contained variable names.
#'
#' @export
#'
#' @examples
#' extract_variables(y~a+b+c+d)
#'
extract_variables <- function(formula, skip_output = FALSE) {
  # Input validation
  stopifnot("'formula' must have type language" = typeof(formula)=="language")

  if (as.character(formula[[1]]) == "~" && skip_output) {
    return(unique(extract_variables_internal(formula[[3]])))
  } else {
    return(unique(extract_variables_internal(formula)))
  }
}

#' @rdname extract_variables
#' @export
extract_variables_internal <- function(formula) {
  variables_found <- character(0)

  # For each component of the expression, check:
  # If it is a call, add all variables in the terms
  # If it is a symbol, add the symbol
  # Skip function names
  for (i in 1:length(formula)) {
    if (i==1) next

    # Recursive search of variables in terms
    if (is.call(formula[[i]])) {
      variables_found <- c(variables_found, extract_variables_internal(formula[[i]]))

    } else if (is.symbol(formula[[i]])) {
        variables_found <- c(variables_found, as.character(formula[[i]]))
    }
  }

  # Return the result
  return(variables_found)
}
