#' Extract LASSO coefficients
#'
#' Returns coefficient estimates from a fitted LASSO model.
#'
#' @param object A fitted \code{lasso_model} object.
#' @param lambda_index Index of lambda value to extract. Defaults to the last one.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A numeric matrix containing the coefficient estimates for the selected lambda value.
#'          The matrix has one column corresponding to the chosen lambda, and one row per predictor.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5, sd=2), 20, 5)
#' y <- rnorm(20, sd=2)
#' fit <- lasso(X, y)
#' coef(fit)
coef.lasso_model <- function(object, lambda_index = NULL,...) {
  if(!inherits(object, "lasso_model")) stop("Not a lasso_model object") #S3- Methode für Klasse lasso_model

  beta <- object$beta #Koeffizienten

  if(!is.matrix(beta)) { #falls Vektor in Matrix umwandeln
    beta <- matrix(beta, ncol = 1)  # Spalte = 1 Lambda
  }

  if(is.null(lambda_index)) lambda_index <- ncol(beta) #letztes lambda typischerweise das kleinste

  #Ausgabe
  beta[, lambda_index, drop=FALSE]
}
