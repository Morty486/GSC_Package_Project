#' Correlation within Bounds
#'
#' Generate a correlation matrix that correlations are feasible within bounds
#'
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param round the decimal we want to keep
#' @param max_tries the maximum iteration for generating correlation matrix
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' X <- function(n) rnorm(n, mean = 0, sd = 1)
#' Y <- function(n) rpois(n, lambda = 2)
#' Z <- function(n) runif(n, min = 0, max = 1)
#' GenCorMat(list(X,Y,Z)))
#'
#'
GenCorMat <- function(lst, round = 2, max_tries = 100) {
  if (!(length(lst) == 2 || length(lst) == 3)) {
    stop("This function can only support 2 or 3 variables.")
  }

  bounds <- Compute.PairBounds(lst)  # Compute pairwise bounds for the variables

  for (attempt in seq_len(max_tries)) {
    cor_mat <- diag(1, length(lst))

    # Generate random correlations within bounds
    for (i in seq_len(length(lst) - 1)) {
      for (j in (i + 1):length(lst)) {
        lower_ij <- bounds$low_bdd[i, j]
        upper_ij <- bounds$up_bdd[i, j]

        if (is.na(lower_ij) || is.na(upper_ij)) {
          stop("Feasible bounds contain NA; cannot pick a random correlation.")
        }
        if (lower_ij > upper_ij) {
          stop(paste("Inverted feasible range for pair (", i, ",", j, "):",
                     "lower =", lower_ij, ">", "upper =", upper_ij))
        }

        # Generate correlation within bounds
        corr_ij <- round(runif(1, min = lower_ij, max = upper_ij), round)
        if (corr_ij < lower_ij) corr_ij <- lower_ij
        if (corr_ij > upper_ij) corr_ij <- upper_ij

        cor_mat[i, j] <- corr_ij
        cor_mat[j, i] <- corr_ij
      }
    }

    # Validate correlation matrix with Validate.Correlation
    try({
      Validate.Correlation(cor_mat, bounds)  # Ensure matrix is valid
    }, silent = TRUE)

    # Perform Check.TriBounds validation
    try({
      l <- Find.Order(lst, cor_mat)  # Use Find.Order to get inputs for Check.TriBounds
      cor <- l[[7]]
      pv <- l[[6]]
      low_bdd <- l[[3]]
      up_bdd <- l[[4]]
      ord <- l[[1]]

      # Pass the matrix through Check.TriBounds
      Check.TriBounds(cor, pv, low_bdd, up_bdd, ord, TRUE)

      # If both validations pass, return the matrix
      return(cor_mat)
    }, silent = TRUE)
  }

  # Stop if a valid matrix cannot be found after max_tries
  stop(paste("Could not find a correlation matrix passing both validations",
             "within", max_tries, "tries."))
}
