
#' Title
#'
#' @param n
#' @param lst
#' @param cor_mat
#'
#' @return
#' @export
#'
#' @examples
GenCorDataMulti1.1 = function(n, lst, cor_mat) {
  if (!nrow(cor_mat) == length(lst)) {
    stop("Dimension of correlation matrix does not match the number of variables! \n")
  }

  sim = sapply(lst, function(a) { a(n) })
  n.var = length(lst)
  cor_mat_input = cor_mat
  pairbounds = Compute.PairBounds(lst)
  Validate.Correlation(cor_mat, pairbounds)

  prop_matrix = matrix(0, n.var, n.var)
  prop_matrix[upper.tri(prop_matrix)] = Compute.SortProp(cor_mat, pairbounds)

  low_bdd = pairbounds$low_bdd
  up_bdd = pairbounds$up_bdd

  n_start = 1
  for (i in 1:(n.var - 1)) {
    for (j in (i + 1):n.var) {
      # Probabilistic rounding for sorting proportion
      sort_ratio = prop_matrix[i, j]
      n_end = floor(sort_ratio * n)
      if (runif(1) < (sort_ratio * n - n_end)) {
        n_end = n_end + 1
      }

      if ((n_start + n_end) > n) {
        err = paste(c("After fixing the front correlations, ",
                      "the correlation between ", as.character(i),
                      " and ", as.character(j),
                      " is out of the bounds of this method"), collapse = "")
        stop(err)
      }

      sim[, j] = Rank.Sort(sim[, i], sim[, j], n_start:(n_start + n_end), cor_mat[i, j])
    }

    n_start = n_start + max(floor(n * prop_matrix[i, (i + 1):n.var]))

    # Update correlation matrix
    if ((i + 1) <= (n.var - 1)) {
      for (x in (i + 1):(n.var - 1)) {
        for (y in (x + 1):n.var) {
          cor_mat[x, y] = cor_mat[x, y] -
            min(prop_matrix[i, x], prop_matrix[i, y]) *
            ifelse((cor_mat[i, x] * cor_mat[i, y] > 0), up_bdd[x, y], low_bdd[x, y])
        }
      }
    }

    # Update proportion we need to sort
    prop_matrix[upper.tri(prop_matrix)] = Compute.SortProp(cor_mat, pairbounds)
  }

  l = list(sim, round(cor(sim), 4), round(cor_mat_input, 4))
  names(l) = c("sim_data", "gen_cor", "spec_cor")
  l
}



