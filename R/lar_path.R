#' Compute LAR coefficient path
#'
#' Computes the full Least Angle Regression (LAR) coefficient path.
#'
#' @param X Numeric predictor matrix (standardized)
#' @param y Numeric response vector (centered)
#' @param max_steps Maximum number of LAR steps (default ncol(X))
#'
#' @return A list containing:
#' \describe{
#' \item{beta_path}{Matrix of coefficient estimates (columns = steps)}
#' \item{active_sets}{List of active index sets}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(30 * 5), 30, 5)
#' beta_true <- c(2, -1.5, 0, 0, 0)
#' y <- X %*% beta_true + rnorm(30, sd = 0.5)
#' data_std <- standardize_lar(X, y)
#' X_std <- data_std$X
#' y_std <- data_std$y
#' fit <- lar_path(X_std, y_std)
lar_path <- function(X, y, max_steps = ncol(X)) {

  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  beta <- rep(0, p)
  r <- y #y-Xbeta=y (wg. beta=0)

  beta_path <- matrix(0, nrow = p, ncol = max_steps+1)
  beta_path[,1] <- beta #Spalte 1 als 'Startpunkt' (mit 0 als Eintrag)

  #Aktive Menge (zu Beginn leere Menge)
  active <- integer(0)
  active_sets <- list(active)

  for (k in seq_len(max_steps)) {

    #Residuum und Korrelationen
    c_vec <- drop(crossprod(X, r))
    C_max <- max(abs(c_vec)) #größte absoulute Relation

    if (C_max < .Machine$double.eps) break #wenn C_max=0 haben Lösung erreicht

    #neue Variablen hinzufügen, die mit größter Korrelation
    new_active <- which(abs(c_vec) == C_max)
    active <- union(active, new_active)

    #equiangular_direction()
    eq <- equiangular_direction(X, active, c_vec)

    #step_size_gamma()
    gamma_info <- step_size_gamma(X, eq$u, c_vec, active)
    gamma <- gamma_info$gamma
    next_index <- gamma_info$next_index

    #updata_coefficients()
    upd <- update_coefficients(beta, active, eq$w, gamma, r, eq$u)
    beta <- upd$beta
    r <- upd$r

    #an richtiger Stelle in Matrix speichern
    beta_path[,k + 1] <- beta
    active_sets[[k + 1]] <- active

  }

  #Ausgabe
  list(
    beta_path = beta_path, #Spalten = Schritte, Zeilen = Variablen
    active_sets = active_sets
  )
}
