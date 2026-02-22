#' Cross Validation for LASSO or Ridge
#'
#' Searchs an optimal regularization parameter for the LASSO or Ridge Estimation
#'
#' @param X Numeric matrix of predictors (n x p), should be standardized.
#' @param y Numeric response vector of length n, should be centered.
#' @param M Positive natural number (Number of Unterteilungen von den Beobachtungen???)
#' @param method character vector ("lasso" or "ridge")
#'
#' @returns A list with:
#' \describe{
#'   \item{lambda_opt}{The regularization parameter with the lowest cv fault from all parameter through lambda_sequence?}
#'   \item{cv_values}{CV faults for all tested regularization parameters.}
#'   \item{lambda_seq}{All testet regularization parameters.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 100
#' p <- 20
#' X <- matrix(rnorm(n*p), n, p)
#' beta_true <- c(3, -2, 1.5, rep(0, p-3))
#' y <- X %*% beta_true + rnorm(n)
#' lambda_CV(X,y,method="lasso")
#' lambda_CV(X,y,method="ridge")
#'
lambda_CV <- function(X, y, M = 5, method){
  M <- as.integer(M)
  if(M <= 0) stop("M musst be a positive number")
  stopifnot("method musst bei lasso or ridge" = (method %in% c("lasso","ridge")))

  n <- nrow(X)
  fold_index <- sample(rep(1:M, length.out = n), #length out führt dazu, dass immer n zugordnet werden könnnen, auch wenn M gar kein Teiler von n ist (dann die vorderen Indexmengen etwas größer, d.h. nicht exkat gleich groß, reicht aber)
                       size = n, replace = FALSE)

  std <- standardize_data(X,y)
  X <- std$X
  y <- std$y
  #lambda_seq <- lambda_sequence(X,y) #! vllt. doch andere lambda Sequenz für ridge
  if(method == "lasso"){
  #  std <- standardize_data(X, y)
    lambda_seq <- lambda_sequence(std$X, std$y)
  } else if(method == "ridge"){
    lambda_seq <- lambda_sequence_ridge(X,y)
  }

  cv <- numeric(length(lambda_seq))
  for (l in seq_along(lambda_seq)){
    res <- 0
    for (m in 1:M){
      X_train <- X[fold_index != m, , drop = FALSE]
      X_test <- X[fold_index == m, , drop =  FALSE]
      y_train <- y[fold_index != m]
      y_test <- y[fold_index == m]
      if(method == "lasso"){
       # std <- standardize_data(X_train, y_train)

        beta <- lasso_cd(X_train, y_train, lambda_seq[l])$beta
        intercept <- 0
      }

      if(method == "ridge"){
       # ridge_data <- ridge(X_train, y_train, lambda_seq[l]) #Name?
      #  beta <- ridge_data$coefficients
       # intercept <- ridge_data$intercept
        beta <- ridge_core(X_train, y_train, lambda_seq[l]) #!irgendwie Verschiebung um 10er Stelle in Beispielen (vllt. um p?)
        intercept <- 0 #stimmt das hier? ist es standartisiert?
      }
      y_pred <- intercept + X_test %*% beta

      res <- res + sum((y_test - y_pred)^2)

      #Alternativ (nahe an der Definition)
      #res <- res + sum(sapply(which(fold_index == m), function(i){
      #  (y[i] - (intercept + sum(beta*X[i,])))^2
       # (y[i] - (intercept + beta %*% X[i,]))^2
       #(y[i] - (beta[1] + sum(beta[-1]*X[i,])))^2 #das ist L(Yi,f schlange von Xi)
        #etwas Verwirrung, aber bei Standartisierung ist wohl beta0 =0, d.h. die erste Komponente wird meistens weggelassen (sogenanntes intercept)
      #}))
    }
    cv[l] <- res/n
  }
  return(list(
    lambda_opt = lambda_seq[which.min(cv)],
    cv_values = cv,
    lambda_seq = lambda_seq
  ))
}
