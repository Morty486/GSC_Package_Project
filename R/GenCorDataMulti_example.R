#' Generate multivariate data with specified marginal distributions as well as a specified correlation structure
#'
#' This function generates multivariate mixed simulation data (n by k) given the specified marginal distributions and correlation structure.
#'
#' Generate n random samples from the intended k marginal distributions independently. To introduce a specified correlation structure for these random samples,
#' sort (k-1) sequences of numbers sequentially. In this function, the sequential sorting is achieved by keeping i th sequence of numbers unchanged and sorting
#' (i+1) th to k th sequences of numbers accordingly for i from 1 to (k-1).
#' For example, if we have 4 random variables X, Y, Z, and Q, and the input order is (X, Y, Z, Q), first we keep the sequence for X unchanged and sort Y, Z and Q accordingly.
#' Then we keep Y unchanged and sort Z, Q accordingly. Finally, we keep Z unchanged and sort Q accordingly. The input order will affect the range of feasible correlation structures
#' which can be achieved by this method. For higher dimensions (> 3), it is hard to calculate the conditional bounds for this method.
#' If k <= 3, we recommend to use \code{\link{GenCorData}} function.
#'
#'
#'
#' @param n  Sample size for the simulation data
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param cor_mat  Specified correlation matrix
#'
#'
#' @return A list is returned which contains matrices of simulated data, generated correlations and specified correlations.
#'   \item{\code{sim_data}}{n by k matrix. Each column corresponds to a variable, and each row is one random sample.}
#'   \item{\code{gen_cor}}{Correlation matrix calculated from the simulated data}
#'   \item{\code{spec_cor}}{Specified correlation matrix}
#'
#' @references Demirtas (2019), Inducing Any Feasible Level of Correlation to
#'             Bivariate Data With Any Marginals, The American Statistician 73.3 (2019): 273-277
#'
#' @seealso \code{\link{Rank.Sort}}, \code{\link{Validate.Correlation}}, \code{\link{Compute.SortProp}}, \code{\link{Compute.PairBounds}}
#'
#'
#'
#' @examples
#' f1 = function(n){rnorm(n)}
#' cor_mat = matrix(
#' c(1,.1,.2,.3,.1,1,.4,.2,.2,.4,1,.2,.3,.2,.2,1),
#' nrow = 4)
#' GenCorDataMulti(10^5, list(f1,f1,f1,f1), cor_mat)
#'
#' @importFrom stats cor
#'
#' @export
