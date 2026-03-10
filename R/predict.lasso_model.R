#' Predict using a LASSO model
#'
#' Generates predictions for new data using a fitted LASSO model.
#'
#' @param object A fitted \code{lasso_model} object.
#' @param X_new Numeric matrix of new predictors.
#' @param lambda_index Index of lambda value to use. Default last.
#' @param ... Additional arguments (ignored).
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
predict.lasso_model <- function(object, X_new, lambda_index = NULL, ...) {
  #S3 Methode, Objekt muss Klasse lasso_model haben
  if(!inherits(object, "lasso_model")) stop("Not a lasso_model object")

<<<<<<< HEAD
=======
  #letzte Lambda-Spalte
  if (is.null(lambda_index)) lambda_index <- length(object$lambda_seq)

>>>>>>> 8100ff99e0d923920b1051ad321b552291231392
  #kein generisches coef() verwenden, sondern coef.lasso_model
  beta <- coef.lasso_model(object, lambda_index) #Koeffizientenmatrix für gewünschte lambda-Spalte

  std <- object$standardization
  if(!is.null(std)) { #prüfen ob Modell standartisiert wurde, wenn ja Prädikatoren ebenfalls standartisieren
    X_new <- scale(X_new,
                   center = std$X_means,
                   scale  = std$X_scales)
  }
<<<<<<< HEAD
  as.numeric(X_new %*% beta) #Vorhersage als numerischen Vektor zurückgeben
=======

  if(!is.null(std)) {
    pred <- as.numeric(X_new %*% beta)
    pred <- pred + std$y_mean  #Intercept hinzufügen
  } else {
    pred <- as.numeric(X_new %*% beta)
  }

  return(pred) #Vorhersage zurückgeben
>>>>>>> 8100ff99e0d923920b1051ad321b552291231392
}
