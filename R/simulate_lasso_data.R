#' Simulate LASSO data
#'
#' Generates synthetic data for LASSO regression.
#'
#' @param n Number of observations (rows of X)
#' @param p Number of predictors (columns of X)
#' @param n_active Number of non-zero coefficients in beta
#' @param beta_values Optional numeric vector for non-zero coefficients (length = n_active)
#' @param sigma Standard deviation of Gaussian noise (default 1)
#' @param seed Optional random seed for reproducibility
#' @importFrom stats rnorm runif
#'
#' @returns A list with:
#' \describe{
#'   \item{X}{Design matrix (n x p)}
#'   \item{y}{Response vector (length n)}
#'   \item{beta}{True coefficient vector (length p)}
#' }
#' @export
#'
#' @examples
#' data <- simulate_lasso_data(n=50, p=10, n_active=3, sigma=1, seed=1)
#' str(data)
simulate_lasso_data <- function(n, p, n_active, beta_values=NULL, sigma=1, seed=NULL) {
  if(!is.null(seed)) set.seed(seed) #Reprudierzierbarkeit der Ergebnisse
  beta <- rep(0, p) #alle Koeffizienten 0

  if(is.null(beta_values))beta_values <- runif(n_active, 1, 3) #zufällige Werte zwischen 1 und 3
  beta[1:n_active] <- beta_values #die ersten n_active Koeffizienten auf erzeugt Werte setzen
  X <- matrix(rnorm(n*p), nrow=n, ncol=p)
  y <- as.vector(X %*% beta + rnorm(n, mean=0, sd=sigma)) #Vector+ Gaussian noise
  #Ausgabe
  list(X = X, y = y, beta = beta)
}
