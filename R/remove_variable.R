#' Remove a variable from a formula
#'
#' \code{remove_variable} removes a variable from a formula and returns the
#' remaining formula as if the chosen variable would be set to 0. (Intercept is
#' kept, i.e. \code{remove_variable(y~a+b, "a")} returns \code{y~b}, not
#' \code{y~0+b})
#' \code{remove_variable_internal} skips input validation.
#'
#' @param formula A model formula
#' @param variable The name of the variable to remove (as character vector of
#' length 1)
#'
#' @return The model formula without the variable
#'
#' @export
#'
#' @examples
#' remove_variable(y~a+b+c+d, "d")
#' remove_variable(y~a+b+c+log(d), "d")
#' remove_variable(y~a+b+c*d, "d")
#' remove_variable(y~a+b+c:d, "d")
#'
remove_variable <- function(formula, variable) {
  # Input validation
  stopifnot("'formula' must have type language" = typeof(formula)=="language",
            "'variable' must be a character vector" = is.character(variable),
            "'variable' must have length 1" = length(variable) == 1)

  return(remove_variable_internal(formula, variable))
}

#' @rdname remove_variable
#' @export
remove_variable_internal <- function(formula, variable) {
  variable_found <- integer(0)

  # For each component of the expression, check:
  # If it is a call, remove the variable in each term recursively. Then clean up
  #   the formula.
  # If it is a symbol and equal to the variable to remove, mark the symbol for
  #   deletion
  for (i in 1:length(formula)) {
    if (i==1) next

    # Recursive removal of the variable in terms
    if (is.call(formula[[i]])) {
      new_formula <- remove_variable_internal(formula[[i]], variable)

      if (as.character(formula[[i]][[1]]) %in% c("I")) {
        if (contains_variable(formula[[i]],variable)) {
          variable_found <- c(variable_found, i)
        }
      }

      if (length(new_formula)==1) {
        variable_found <- c(variable_found, i)
      } else if (length(new_formula)==2 && length(formula[[i]]) != 2) {
        if (as.character(new_formula[[1]]) %in% c(":")) {
          variable_found <- c(variable_found, i)
        } else {
          new_formula <- new_formula[[2]]
        }
      }
      formula[[i]] <- new_formula

      # Mark the variable for removal
    } else if (is.symbol(formula[[i]])) {
      if (as.character(formula[[i]]) == variable) {
        variable_found <- c(variable_found, i)
      }
    }
  }

  # Delete found occurrences of the variable and empty operators
  return(formula[!(1:length(formula) %in% variable_found)])
}




################################################################################
# Legacy                                                                       #
################################################################################
# # A structured way to handle model formulae
# #
# custom_formula <- function(formula) {
#   stopifnot("formula has to be a formula object" = "formula" %in% class(formula))
#   formula <- deparse(formula)
#   formula <- stringr::str_remove_all(formula, "[:space:]")
#   formula <- (stringr::str_split(formula, "~"))[[1]]
#   stopifnot("formula must contain exactly one ~" = length(formula)==2)
#
#   output <- formula[1]
#   term <- custom_term(formula[2])
#
#   structure(term,
#             output = output,
#             class = "custom_formula")
# }
#
#
# # @rdname custom_formula
# custom_term <- function(formula, terms_placeholden = NULL, placeholder_string = NULL) {
#   if (is.null(terms_placeholden)) {
#     placeholder_string <- NULL
#     while (is.null(placeholder_string)) {
#       str <- paste0(sample(letters, 10, replace = TRUE), collapse = "")
#       if (!(str %in% formula)) {
#         placeholder_string <- str
#       }
#     }
#   }
#
#   # Detect elementary terms
#   if (!stringr::str_detect(formula, "[\\(\\)\\^\\+\\-\\:\\%\\*\\/]")) {
#     if (stringr::str_detect(formula, "^[:digit:]+$")) {
#       t <- structure(formula,
#                      ct2 = NULL,
#                      operator = NULL,
#                      elementary = TRUE,
#                      binary = FALSE,
#                      wrapped_by = "",
#                      included_variables = character(),
#                      class = "custom_term")
#     } else {
#       if (formula %in% names(terms_placeholden)) {
#         return(terms_placeholden[[formula]])
#       }
#       t <- structure(formula,
#                      ct2 = NULL,
#                      operator = NULL,
#                      elementary = TRUE,
#                      binary = FALSE,
#                      wrapped_by = "",
#                      included_variables = formula,
#                      class = "custom_term")
#     }
#     return(t)
#   }
#
#   # Handle brackets and function calls
#   while (stringr::str_detect(formula, "[\\.[:alnum:]\\_]*\\([^\\(]*\\)")) {
#     wrapper <- ""
#
#     # Handle function calls
#     if (stringr::str_detect(formula, "[\\.[:alnum:]\\_]+\\([^\\(]*\\)")) {
#       # Extract whole function call
#       function_call <- stringr::str_extract(formula, "(?<![\\.[:alnum:]\\_])[\\.[:alnum:]\\_]+\\([^\\(]*\\)")
#       term <- stringr::str_split_1(function_call, "[\\(\\)]")
#
#       # Which function is called
#       wrapper <- term[1]
#
#       # Term, that is the argument of the function called
#       term <- custom_term(term[2], terms_placeholden, placeholder_string)
#       term$wrapped_by <- wrapper
#
#       # Generate placeholder, insert into the string and store
#       plc_hld <- paste0(placeholder_string, length(terms_placeholden)+1)
#       terms_placeholden <- append(terms_placeholden, list(plc_hld = term))
#       stringr::str_replace(formula, function_call, plc_hld)
#
#       # Handle brackets
#     } else {
#       # Extract whole bracket
#       in_brackets <- stringr::str_extract(formula, "\\([^\\(]*\\)")
#       term <- stringr::str_split_1(in_brackets, "[\\(\\)]")
#
#       # Generate term in the bracket
#       term <- custom_term(term[2], terms_placeholden, placeholder_string)
#
#       # Generate placeholder, insert into the string and store
#       plc_hld <- paste0(placeholder_string, length(terms_placeholden)+1)
#       terms_placeholden <- append(terms_placeholden, list(plc_hld = term))
#       stringr::str_replace(formula, in_brackets, plc_hld)
#     }
#   }
#   # From here on formula is bracket-free -- hooray!
#
#
#
#
#
#   print(formula)
#
#   structure(ct1,
#             ct2 = ct2,
#             operator = op,
#             elementary = TRUE,
#             binary = TRUE,
#             wrapped_by = "",
#             included_variables = included_variables,
#             class = "custom_term")
# }
