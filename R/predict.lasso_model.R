#' Predict using a LASSO model
#'
#' Generates predictions for new data using a fitted LASSO model.
#'
#' @param object A fitted \code{lasso_model} object.
#' @param X_new Numeric matrix of new predictors.
#' @param lambda_index Index of lambda value to use. Default last.
#'
#' @returns Numeric vector of predictions.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5, sd=2), 20, 5)
#' y <- rnorm(20, sd=2)
#' fit <- lasso(X, y)
#' X_new <- matrix(rnorm(10*5), 10, 5)
#' predict(fit, X_new)
predict.lasso_model <- function(object, X_new, lambda_index = NULL) {
  if(!inherits(object, "lasso_model"))
    stop("Not a lasso_model object")

  beta <- coef(object, lambda_index)
  std <- object$standardization

  if(!is.null(std)) {
    X_new <- scale(X_new,
                   center = std$X_means,
                   scale  = std$X_scales)
  }
  as.numeric(X_new %*% beta)
}
