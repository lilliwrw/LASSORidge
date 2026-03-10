#' Cross Validation for LASSO or Ridge
#'
#' Searching an optimal regularization parameter for the LASSO or Ridge Estimation
#'
#' @param X Numeric matrix of predictors (n x p).
#' @param y Numeric response vector of length n.
#' @param M Positive number, should be an integer, but it will be converted to an integer as long as possible.
#' @param method character vector ("lasso" or "ridge")
#'
#' @returns A list with:
#' \describe{
#'   \item{lambda_opt}{The regularization parameter with the lowest cv fault from all parameter through lambda_sequence}
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
#' lambda_cv(X,y,method="lasso")
#' lambda_cv(X,y,method="ridge")
#'
lambda_cv <- function(X, y, M = 5, method){
  # input checks
  M <- as.integer(M)
  if(M <= 0) stop("M must be a positive number")
  stopifnot("method must bei lasso or ridge" = (method %in% c("lasso","ridge")))
  if (!is.matrix(X)) {
    X <- tryCatch(as.matrix(X), error = function(e) {
      stop("X must be a matrix or coercible to a matrix.")
    })
  }
  if (!is.numeric(X)) stop("X must be numeric")
  if (!is.numeric(y)) stop("y must be numeric")
  if (anyNA(X) || anyNA(y))stop("NA in Data not allowed")

  n <- nrow(X)
  p <- ncol(X)
  if (length(y) != n)stop("Length of y must match number of rows of X")
  if (M > n)  stop("M must be smaller or equal (leave-one-out cross validation) to nrow(X)")

  # Index for spliting the data into K roughly equal-sized parts
  ## random assignment, but if M is no divisor of n, the first parts are a bit bigger
  n <- nrow(X)

  fold_index <- sample(rep(1:M, length.out = n),
                       size = n, replace = FALSE)

  # Determining th lambdas to be tested, depending on the method
  ## Standardize for the lambda sequences, in standardize_data also checking the inputs X and y
  if(method == "lasso"){
    std_all <- standardize_data(X, y)
    lambda_seq <- lambda_sequence(std_all$X, std_all$y)
  } else if(method == "ridge"){
    std_all <- ridge_standardizeData(X, y)
    lambda_seq <- lambda_sequence_ridge(std_all$Xs)
  }

  # Determining the CV_error for every lambda
  cv <- numeric(length(lambda_seq))
  for (l in seq_along(lambda_seq)){
    res <- 0

    # iteration through the parts, each part is one-time test area
    for (m in 1:M){

      # Assigning the data, whether train or test data, to the fold index
      X_train <- X[fold_index != m, , drop = FALSE]
      X_test <- X[fold_index == m, , drop =  FALSE]
      y_train <- y[fold_index != m]
      y_test <- y[fold_index == m]

      # algorithm depending on the method
      ## determination of the coefficients (beta) for LASSO or Ridge
      if(method == "lasso"){
        # Standardization on training data
        std_train <- standardize_data(X_train, y_train)
        X_train_scaled <- std_train$X
        y_train_centered <- std_train$y

        # Standardize test data with training parameters
        X_test_scaled <- scale(X_test, center = std_train$X_means, scale = std_train$X_scales)
        beta <- lasso_cd(X_train_scaled, y_train_centered, lambda_seq[l])$beta
      }
      if(method == "ridge"){
        # Standardization on training data
        std_train <- ridge_standardizeData(X_train, y_train)
        X_train_scaled <- std_train$Xs
        y_train_centered <- std_train$ys

        # Standardize test data with training parameters
        X_test_scaled <- scale(X_test, center = std_train$X_means, scale = std_train$X_sds)
        beta <- ridge_core(X_train_scaled, y_train_centered, lambda_seq[l])
      }

      # Back transformation from standardization to original data
      y_pred <- std_train$y_mean + as.vector(X_test_scaled %*% beta)

      # cv_errror for m and summation over all m
      res <- res + sum((y_test - y_pred)^2)
    }

    # final cv_error for a lambda
    cv[l] <- res/n
  }

  return(list(
    # lambda_opt is that lambda, where the cv_error is minimal
    lambda_opt = lambda_seq[which.min(cv)],
    cv_values = cv,
    lambda_seq = lambda_seq
  ))
}
