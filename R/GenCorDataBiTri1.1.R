#' Generate bivariate or trivariate data with specified marginal distributions as well as a specified correlation structure
#'
#' This function generates bivariate or trivariate mixed simulation data given the specified marginal distributions and
#'  correlation structure.
#'
#' Generate random samples from the intended marginal distributions independently. When there are two variables,
#'  X and Y to introduce a specified correlation, a calculated proportion of data (X and Y) will be sorted or
#'  reversely sorted simultaneously  based on Demirtas (2019).
#'  When there are three variables, X, Y and Z, to introduce a specified correlation structure, variables will be sorted
#'  sequentially after reordering. According to the sorting ratios calculated by \code{\link{Compute.SortProp}}
#'  for all three pairs (XY, XY, XZ), reorder X, Y and Z via \code{\link{CorOrder.ToVar}}.
#'  Keep the first variable unchanged and sort the second and the third variable accordingly via \code{\link{Rank.Sort}}.
#'  Then keep the modified second variable unchanged and sort the the modified third variable. The given correlation structure
#'  may not be feasible if it is out of the pairwise bounds or trivariate bounds which is checked by \code{\link{Validate.Correlation}}
#'  and \code{\link{Check.TriBounds}}.
#'
#' @param n  Sample size for the simulation data
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param cor_mat  Specified correlation matrix
#'
#'
#' @return A list is returned which contains matrices of simulated data, generated correlations and specified correlations.
#'   \item{\code{sim_data}}{n by (2 or 3) matrix. Each column corresponds to a variable, and each row is one random sample.}
#'   \item{\code{gen_cor}}{Correlation matrix calculated from the simulated data}
#'   \item{\code{spec_cor}}{Specified correlation matrix}
#'
#' @references Demirtas (2019), Inducing Any Feasible Level of Correlation to
#'             Bivariate Data With Any Marginals, The American Statistician 73.3 (2019): 273-277
#'
#'
#'
#'
#' @importFrom stats cor
#'
#' @examples
#'  f1 = function(n){rnorm(n)}
#'  cor_mat = matrix(c(1,.49,.1, .49, 1, -.4, .1, -.4, 1), nrow = 3)
#'  GenCorDataBiTri(10^5, list(f1,f1,f1), cor_mat)
#'
#'
#'
#' @export
GenCorDataBiTri1.1 = function(n, lst, cor_mat,row.method=1) {
  if (!(length(lst) == 2 || length(lst) == 3)) {
    stop("This sorting method only can be applied to 2 or 3 variables")
  }
  if (!nrow(cor_mat) == length(lst)) {
    stop("Dimension of correlation matrix does not match the number of variables! \n")
  }

  pairbounds = Compute.PairBounds(lst)
  Validate.Correlation(cor_mat, pairbounds)
  prop_vec = Compute.SortProp(cor_mat, pairbounds)


  # Function to compute probabilities for floor and ceiling
  get_probabilities <- function(x) {
    decimal_part <- x - floor(x)  # Get the decimal part
    return(c(1 - decimal_part, decimal_part))  # Probabilities for floor and ceiling
  }


  # Define a function to adjust n_sort based on probabilities
  adjust_n_sort <- function(n_sort, probabilities) {
    # Use mapply to adjust values
    n_sort_adjusted <- mapply(function(val, prob) {
      chosen_function <- sample(c("floor", "ceiling"), 1, prob = prob)
      # Apply the sampled function
      if (chosen_function == "floor") {
        return(floor(val))
      } else {
        return(ceiling(val))
      }
    }, n_sort, probabilities)

    return(n_sort_adjusted)
  }



  if (length(lst) == 2) {



    n_sort <- n * prop_vec
    # Apply the function to each value in n_sort to get probabilities
    probabilities <- lapply(n_sort, get_probabilities)

    n_sort_adjusted <- mapply(function(val, prob) {
      chosen_function <- sample(c("floor", "ceiling"), 1, prob = prob)
      # Sample based on probabilities
      if (chosen_function == "floor") {
        return(floor(val))
      } else {
        return(ceiling(val))
      }
    }, n_sort, probabilities)

    n_sort = n_sort_adjusted


    sim = sapply(lst, function(a){a(n)})
    if (cor_mat[upper.tri(cor_mat)] > 0) {
      sim[, 1][1:n_sort] = sort(sim[, 1][1:n_sort])
      sim[, 2][1:n_sort] = sort(sim[, 1][1:n_sort])
    } else{
      sim[, 1][1:n_sort] = sort(sim[, 1][1:n_sort])
      sim[, 2][1:n_sort] = rev(sort(sim[, 1][1:n_sort]))
    }
    ord <- 1:2
  }

  if (length(lst) == 3) {

    l = Find.Order(lst, cor_mat)
    ord = l[[1]]
    srt_lst = l[[2]]
    srt_low_bdd = l[[3]]
    srt_up_bdd = l[[4]]
    srt_name = l[[5]]
    srt_pv = l[[6]]
    srt_cor = l[[7]]

    Check.TriBounds(srt_cor, srt_pv, srt_low_bdd, srt_up_bdd, ord)

    sim = sapply(srt_lst, function(a){a(n)})
    if (row.method == 2){
      sim = sapply(srt_lst, function(a){a(10*n)})
    }

    s1 = srt_cor[1]*srt_cor[2] > 0
    s2 = srt_cor[3] < 0
    s3 = abs(srt_low_bdd[3]) < srt_up_bdd[3]

    if (row.method == 1){
      probabilities1 <- lapply(srt_pv[1]*n, get_probabilities)
      probabilities2 <- lapply(srt_pv[2]*n, get_probabilities)
      n1 = adjust_n_sort(srt_pv[1]*n, probabilities1)
      n2 = adjust_n_sort(srt_pv[2]*n, probabilities2)
    }else if (row.method == 2){
      n1 = floor(srt_pv[1]*n*10)
      n2 = floor(srt_pv[2]*n*10)
    }else {
      stop("Please Choose the Correct Method!")
    }


    if (all(s1,s2,s3) || all(!s1, !s2, !s3)) {
      print("Sort without overlap")
      n3 = floor(srt_pv[3]*n)
      if (row.method == 2){
        n3 = floor(srt_pv[3]*n*10)
      }
      sim[, 2] = Rank.Sort(sim[, 1], sim[, 2], 1:n1, srt_cor[1])
      sim[, 3] = Rank.Sort(sim[, 1], sim[, 3], (n1 + 1):(n1 + n2), srt_cor[2])
      sim[, 3] = Rank.Sort(sim[, 2], sim[, 3], (n1 + n2 + 1):(n1 + n2+ n3), srt_cor[3])
    }else{
      print("Sort with overlap")
      # The remain correlation need to achieve for the last one
      rem = srt_cor[3] - srt_pv[1]*ifelse((srt_cor[1]*srt_cor[2] > 0), srt_up_bdd[3], srt_low_bdd[3])
      n3 = ifelse(rem >= 0, floor(n*(rem/srt_up_bdd[3])), floor(n*(rem/srt_low_bdd[3])))
      if (row.method == 2){
        n3 = ifelse(rem >= 0, floor(10*n*(rem/srt_up_bdd[3])), floor(10*n*(rem/srt_low_bdd[3])))
      }
      sim[, 2] = Rank.Sort(sim[, 1], sim[, 2], (1:n1), srt_cor[1])
      sim[, 3] = Rank.Sort(sim[, 1], sim[, 3], 1:n2, srt_cor[2])
      sim[, 3] = Rank.Sort(sim[, 2], sim[, 3], (n2 + 1):(n2 + n3), rem)
    }

  }

  colnames(sim) = as.character(ord)
  sim = sim[, order(colnames(sim))]
  if (row.method == 2){
    sim = sim[sample(1:nrow(sim), n, replace = FALSE), ]
  }
  re = list(sim, round(cor(sim),4), round(cor_mat,4))
  names(re) = c("sim_data", "gen_cor", "spec_cor")
  re
}


