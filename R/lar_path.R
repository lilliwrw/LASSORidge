#' Compute LAR coefficient path
#'
#' Computes the full coefficient path for the LAR algorithm given a
#' standardized predictor matrix \eqn{X} and centered response vector \eqn{y}.
#'
#' The function iteratively adds predictors to the active set, moves
#' along the equiangular direction, updates coefficients, and stores
#' the coefficient path until \code{max_steps} or convergence.
#'
#' @param X Numeric predictor matrix of dimension \eqn{n \times p} (standardized)
#' @param y Numeric response vector of length \eqn{n} (centered)
#' @param max_steps Maximum number of LAR steps (default = \code{ncol(X)})
#'
#' @return A list containing:
#' \describe{
#' \item{beta_path}{Matrix of dimension \eqn{p \times (K+1)} of coefficient estimates. Each column corresponds to a LAR step, first column is zero.}
#' \item{active_sets}{List of length \eqn{K+1} containing the indices of active predictors at each step.}
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
    #Korrelation neu berechnen
    c_vec <- compute_correlations(X, r)
    C_max <- max(abs(c_vec)) #größte absoulute Relation
    if (C_max < .Machine$double.eps) break #wenn C_max=0 haben Lösung erreicht

    # Bei Start: alle Variablen mit maximaler Korrelation aktivieren
    if(length(active) == 0){
      active <- which(abs(c_vec) == C_max)
    }

    #equiangular_direction()
    eq <- equiangular_direction(X, active, c_vec)

    #step_size_gamma()
    gamma_info <- step_size_gamma(X, eq$u, c_vec, active)
    gamma <- gamma_info$gamma
    next_index <- gamma_info$next_index

    #update_coefficients()
    upd <- update_coefficients(beta, active, eq$w, gamma, r, eq$u)
    beta <- upd$beta
    r <- upd$r

    #neue Variablen hinzufügen, die mit größter Korrelation
    if(!is.na(next_index)) active <- union(active, next_index)

    #an richtiger Stelle in Matrix speichern
    beta_path[,k + 1] <- beta
    active_sets[[k + 1]] <- active

    #Abbruch, wenn keine neue Variable hinzukommt
    if(is.na(next_index)) break
  }

  #Ausgabe
  list(
    beta_path = beta_path, #Spalten = Schritte, Zeilen = Variablen
    active_sets = active_sets #Aktive Variablen pro Schritt
  )
}
