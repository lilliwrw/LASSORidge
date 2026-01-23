#' Cross-Validation for LASSO
#'
#' Performs K-fold (default 5-fold) cross-validation to select the optimal lambda.
#'
#' @param X Numeric matrix of predictors (n x p), centered and optionally scaled.
#' @param y Numeric response vector of length n, centered.
#' @param n_folds Number of folds for cross-validation, default 5.
#' @param n_lambda Number of lambda values to evaluate, default 100.
#' @param lambda_min_ratio Ratio of min lambda to max lambda, default 0.01.
#' @param tol Convergence tolerance for lasso_cd, default 1e-6.
#' @param max_iter Maximum iterations for lasso_cd, default 1000.
#'
#' @returns A list with:
#' \describe{
#'   \item{lambda_seq}{Lambda sequence evaluated.}
#'   \item{cv_err}{Vector of mean CV errors for each lambda.}
#'   \item{lambda_opt}{Lambda value with minimal CV error.}
#'   \item{beta_opt}{Coefficients at lambda_opt.}
#' }
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50*5), nrow=50)
#' y <- rnorm(50)
#' std <- standardize_data(X, y)
#' cv <- lasso_cv(std$X, std$y, n_folds=5)
lasso_cv <- function(X, y, n_folds=5, n_lambda=100, lambda_min_ratio=0.01,
                     tol=1e-6, max_iter=1000) {
  n <- nrow(X)
  lambda_seq <- lambda_sequence(X, y, n_lambda, lambda_min_ratio)
  cv_err <- numeric(length(lambda_seq)) #speichert mittleren Corss-Validation Fehler pro Lambda

  #Folds zufällig aufteilen
  set.seed(1)  #immer gleiche Aufteilung, Ergebnis reproduzierbar
  folds <- sample(rep(1:n_folds, length.out = n))

  for(i in seq_along(lambda_seq)) {
    lambda <- lambda_seq[i]
    err_folds <- numeric(n_folds) #soll Validierungsfehler für Folf k speichern

    for(k in 1:n_folds) {
      train_idx <- which(folds != k) #Beobachtungen nicht in Fold k
      val_idx   <- which(folds == k) #Beobachtungen in Fold k, also Fold k

      X_train <- X[train_idx, , drop=FALSE] #Matrixstruktur behalten für %*%
      y_train <- y[train_idx]
      X_val   <- X[val_idx, , drop=FALSE]
      y_val   <- y[val_idx]

      #bestimme beta für lambda durch LASSO mit Trainingsdaten
      est <- lasso_cd(X_train, y_train, lambda, tol, max_iter)

      #Vorhersage für Validierungsdaten
      y_pred <- X_val %*% est$beta
      err_folds[k] <- mean((y_val - y_pred)^2)
    }

    cv_err[i] <- mean(err_folds) #Validierungsfehler gemittelt für jedes lambda
  }

  #Bestimmung des optimalen lambda, zugehörige beta bestimmen
  lambda_opt <- lambda_seq[which.min(cv_err)]
  beta_opt   <- lasso_cd(X, y, lambda_opt, tol, max_iter)$beta

  #Ausgabe
  list(lambda_seq = lambda_seq,
    cv_err     = cv_err,
    lambda_opt = lambda_opt,
    beta_opt   = beta_opt)
}
