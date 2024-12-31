

#' Computing Correlation
#'
#' This is a function for the correlation output
#'
#' @param n the sample size for the simulated data
#' @param lst A list of functions which generate data under specified marginal distributions separately
#' @param cor_mat Specified correlation matrix
#' @param meth method of correlation
#'
#' @return A vector containing correlation
#' @export
#'
#' @examples
#' library(doParallel)
#' library('GenCorSeqSort')
#' library('foreach')
#' time1=Sys.time()    #### time at the starting
#' cl <- makeCluster(detectCores()-1)  #### Creating cluster
#' clusterExport(cl, varlist = c("mean.gen.cor"))  ### using user defined function inside the cluster
#' clusterEvalQ(cl, library(GenCorSeqSort))  #### Using the package
#' registerDoParallel(cl)  #### cluster registration
#'
#' result <- foreach(i = 1:1000, .combine = c) %dopar% {
#'   mean.gen.cor(n = 10^3, lst = list(f1, f2, f3), cor_mat = cor_mat, meth = 'pearson')
#' }
#' stopCluster(cl)   ####stopping cluster
#' cl=result
#' time2=Sys.time()   #### time at the end of the computation
#' Computation.Time=time2-time1
#' corxy=mean(cl[seq(from=1,to=300,by=3)])  ### corXY comes from the first element of the 3 length output
#' corxz=mean(cl[seq(from=2,to=300,by=3)])   ### similarly corXZ is the second element of the 3 length output
#' coryz=mean(cl[seq(from=3,to=300,by=3)])
#'
#' cat('The average emperical correlation between X and Y is',corxy)
#' cat('The average emperical correlation between X and Z is',corxz)
#' cat('The average emperical correlation  between Y and Z is',coryz)
#'
#' cat(' The total computation time :',Computation.Time)
#'
#'
#'
#'
GenCor=function(n,lst,cor_mat,meth){ #n=no of simulation #cor_mat=correlation matrix #meth=method of correlation

  sim.data=GenCorDataMulti(n,lst,cor_mat)$sim_data #### Simulating
  cor12=cor(sim.data[,1],sim.data[,2],method=meth)  ### correlation between X and Y
  cor13=cor(sim.data[,1],sim.data[,3],method=meth)  ### Correlation between X and Z
  cor23=cor(sim.data[,2],sim.data[,3],method=meth)  ### Correlation  between Y and Z

  return(c(cor12,cor13,cor23))
}

