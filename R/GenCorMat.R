#' Correlation within Bounds
#'
#' Generate a correlation matrix that correlations are feasible within bounds
#'
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param round the decimal we want to keep
#' @param max_tries the maximum iteration for generating correlation matrix
#'
#' @return a correlation matrix
#'
#' @importFrom stats cor runif
#'
#'
#'
#' @examples
#' X <- function(n) rnorm(n, mean = 0, sd = 1)
#' Y <- function(n) rpois(n, lambda = 2)
#' Z <- function(n) runif(n, min = 0, max = 1)
#' GenCorMat(list(X,Y,Z))
#'
#' A <- function(n) rbinom(n, size = 1, prob = 0.5)
#' B <- function(n) rexp(n, rate = 1)
#' C <- function(n) rbinom(n, size = 20, prob = 0.6)
#' GenCorMat(list(A, B))
#'
#' @export
GenCorMat <- function(lst, round = 2, max_tries = 100) {
  nvars <- length(lst)
  if (!(nvars == 2 || nvars == 3)) {
    stop("This function can only support 2 or 3 variables.")
  }

  # 1) Compute feasible pairwise bounds
  bounds <- GenCorSeqSort::Compute.PairBounds(lst)

  # 2) Try up to max_tries times
  for (attempt in seq_len(max_tries)) {
    # (A) Build an identity correlation matrix
    cor_mat <- diag(1, nvars)

    # (B) Fill random correlations in [lower, upper] for off-diagonals
    for (i in seq_len(nvars - 1)) {
      for (j in (i + 1):nvars) {
        lo <- bounds$low_bdd[i, j]
        hi <- bounds$up_bdd[i, j]

        if (is.na(lo) || is.na(hi)) {
          stop("Feasible bounds contain NA; cannot pick a random correlation.")
        }
        if (lo > hi) {
          stop(sprintf("Inverted feasible range for pair (%d, %d): lower=%.2f > upper=%.2f",
                       i, j, lo, hi))
        }

        # Pick & round
        corr_ij <- round(runif(1, lo, hi), round)
        # Clamp if rounding drifts out of [lo, hi]
        if (corr_ij < lo) corr_ij <- lo
        if (corr_ij > hi) corr_ij <- hi

        cor_mat[i, j] <- corr_ij
        cor_mat[j, i] <- corr_ij
      }
    }

    # (C) Single try(...) block with all checks
    try({
      # --- 1) Always validate correlation ---
      GenCorSeqSort::Validate.Correlation(cor_mat, bounds)

      # If only 2 variables => if we succeed here, return
      if (nvars == 2) {
        return(cor_mat)
      }

      # --- 2) Otherwise, for 3 variables, also do tri-variate checks ---
      l       <- GenCorSeqSort::Find.Order(lst, cor_mat)
      cor_vec <- l[[7]]   # the re-ordered correlations
      pv      <- l[[6]]   # partial-sorting proportions
      low_bd  <- l[[3]]   # re-ordered lower bounds
      up_bd   <- l[[4]]   # re-ordered upper bounds
      ord     <- l[[1]]   # ordering of variables

      GenCorSeqSort::Check.TriBounds(cor_vec, pv, low_bd, up_bd, ord, TRUE)

      # If we get here => no error => pass => return
      return(cor_mat)

    }, silent = TRUE)

    # If an error occurs in any step, we do not hit 'return(cor_mat)',
    # so the loop just continues silently.
  }

  # If all attempts fail, stop:
  stop(paste("Could not find a correlation matrix passing the required checks",
             "within", max_tries, "tries."))
}
