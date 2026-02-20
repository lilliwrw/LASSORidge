#' Extract LASSO coefficients
#'
#' Returns coefficient estimates from a fitted LASSO model.
#'
#' @param object A fitted \code{lasso_model} object.
#' @param lambda_index Index of lambda value to extract. Default last.
#'
#' @returns Numeric vector of coefficients.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5, sd=2), 20, 5)
#' y <- rnorm(20, sd=2)
#' fit <- lasso(X, y)
#' coef_lasso(fit)
coef_lasso <- function(object, lambda_index = NULL) {
  if(!inherits(object, "lasso_model"))
    stop("Not a lasso_model object")

  beta <- object$beta

  if(!is.matrix(beta)) { #falls Vektor in Matrix umwandeln
    beta <- matrix(beta, ncol = 1)  # Spalte = 1 Lambda
  }

  if(is.null(lambda_index)) lambda_index <- ncol(beta)

  beta[, lambda_index, drop = FALSE]
}
