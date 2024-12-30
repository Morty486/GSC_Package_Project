
#' Title
#'
#' @param n
#' @param lst
#' @param digits
#'
#' @return
#' @export
#'
#' @examples
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


