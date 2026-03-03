#' Predict method for LAR models
#'
#' S3 method to make predictions from a LAR model.
#' Can predict for a specific step or the full coefficient path.
#' Automatically deals with standardized coefficients if the model was standardized.
#'
#' @param object An object of class `"lar_model"`.
#' @param newx Numeric matrix of new predictors.
#' @param step Optional integer specifying which step to use for prediction.
#'   If `NULL` (default), predictions are returned for all steps.
#' @param ... Additional arguments (currently unused).
#'
#' @return Numeric vector of predictions if `step` is given,
#'   or numeric matrix (nrow(newx) x ncol(beta)) if step = NULL.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(30 * 5), 30, 5)
#' beta_true <- c(2, -1.5, 0, 0, 0)
#' y <- X %*% beta_true + rnorm(30, sd = 0.5)
#' fit <- lar(X, y)
#'
#' # Predict for training data using all steps
#' pred_all <- predict(fit, X)
#'
#' # Predict for training data using step 3
#' pred_step3 <- predict(fit, X, step = 3)
predict.lar_model <- function(object, newx, step = NULL, ...) {
  beta <- object$beta #Koeffizienten extrahieren

  #Spaltenanzahl prüfen
  if (ncol(newx) != nrow(beta))stop("Number of columns of newx must match number of variables in model")

  #Vorhersage für einen bestimmten Schritt
  if (!is.null(step)) {
    if (step < 1 || step > ncol(beta))stop("`step` must be between 1 and the number of steps in the path")
    return(as.vector(newx %*% beta[, step]))
  }

  #Vorhersage für alle Schritte
  preds <- newx %*% beta
  return(preds)
}
