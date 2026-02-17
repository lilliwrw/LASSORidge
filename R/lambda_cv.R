#' Cross Validation for LASSO or Ridge
#'
#' Searchs an optimal regularization parameter for the LASSO or Ridge Estimation
#'
#' @param X Numeric matrix of predictors (n x p), should be standardized.
#' @param y Numeric response vector of length n, should be centered.
#' @param M Positive natural number (Number of Unterteilungen von den Beobachtungen???)
#' @param e 1 or 2 for LASSO or Ridge estimation ?? (wie bei mapply)
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
#' @examples ????
#'
lambda_CV <- function(X,y,M = 5, e){
  M <- as.integer(M)
  if(M <= 0) stop("M musst be a positive number")
  if(!e %in% c(1,2)) stop("e must be 1 (lasso) or 2 (ridge)")
  #beim Rest muss ich mal gucken, wie die anderen das jetzt mit dem Standartisieren gelöst haben


  n <- nrow(X)
  #prüfen X,Y standartisiert richtige Grööße etc.
  #eher bei bsp, oder?  set.seed(1) # statt mit 1 evtl. mit seed arbeiten (wenn NULL, dann "echtere" Zufallszahl)
  fold_index <- sample(rep(1:M, length.out = n), #length out führt dazu, dass immer n zugordnet werden könnnen, auch wenn M gar kein Teiler von n ist (dann die vorderen Indexmengen etwas größer, d.h. nicht exkat gleich groß, reicht aber)
                       size = n, replace = FALSE)
  lambda_seq <- lambda_sequence(X,y) #warum mit x und y?
  #cv <- rep(0,length(lambda_seq)) #muss ja theoretisch nicht..
  for (l in seq_along(lambda_seq)){ #geht das ? Kein integer Vektor?
    res <- 0
    for (m in 1:M){
      X_train <- X[fold_index != m,]
      X_test <- X[fold_index == m,]
      y_train <- y[fold_index != m]
      y_test <- y[fold_index == m]
      if(e==1){
        beta <- lasso_cd(X_train, y_train, lambda_seq[l])$beta #ist das jetzt ein Vektor?
      }
      if(e==2){
        beta <- ridge_cd(X_train, y_train, lambda_seq[l])$beta #heißt die Funktion so?
      }
      y_pred <- beta[1] + X_test %*% beta[-1] #Skalarprodukt Name der Variable?

      res <- res + sum((y_test - y_pred)^2)

      #Alternativ (nahe an der Definition)
      #res <- res + sum(sapply(which(fold_index == m), function(i){
      #  (y[i] - (beta[1] + sum(beta[-1]*X[i,])))^2 #das ist L(Yi,f schlange von Xi)
      #}))
    }
    cv[l] <- 1/n*res
  }
  return(list(
    lambda_opt = lambda_seq[which.min(cv)],
    cv_values = cv,
    lambda_seq = lambda_seq
  ))
}
