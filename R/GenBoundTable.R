
#' Correlation Bounds Table
#'
#' Generate a Table Lower (below the diagonal) and upper (above the diagonal)
#' correlation bounds.
#'
#' @param n Sample size for the simulation data
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param digits the decimal place for the correlation
#'
#' @return A matrix with correlation bounds.
#'
#'
#' @examples
#' X1 <- function(n) rnorm(n, mean = 0, sd = 1)
#' X2 <- function(n) rpois(n, lambda = 2)
#' X3 <- function(n) runif(n, min = 0, max = 1)
#' GenBoundTable(10000,list(X1,X2,X3),digits=3)
#'
#' @export
GenBoundTable <- function(n, lst, digits = 4) {
  pair_bounds <- Compute.PairBounds(lst)
  nvars <- length(lst)
  bounds_table <- matrix(1, nrow = nvars, ncol = nvars)

  for (i in seq_len(nvars)) {
    for (j in seq_len(nvars)) {
      if (i < j) {
        bounds_table[i, j] <- round(pair_bounds$up_bdd[i, j], digits)
      } else if (i > j) {
        bounds_table[i, j] <- round(pair_bounds$low_bdd[i, j], digits)
      }
    }
  }

  varnames <- paste0("X", seq_len(nvars))
  dimnames(bounds_table) <- list(varnames, varnames)
  return(bounds_table)
}


