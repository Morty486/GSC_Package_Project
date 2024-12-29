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
#' @seealso \code{\link{Rank.Sort}}, \code{\link{Validate.Correlation}}, \code{\link{Compute.SortProp}}, \code{\link{Compute.PairBounds}}, \code{\link{CorOrder.ToVar}}, \code{\link{Check.TriBounds}}
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
