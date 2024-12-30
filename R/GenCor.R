

#' Title
#'
#' @param n
#' @param lst
#' @param cor_mat
#' @param meth
#'
#' @return
#' @export
#'
#' @examples
GenCor=function(n,lst,cor_mat,meth){ #n=no of simulation #cor_mat=correlation matrix #meth=method of correlation

  sim.data=GenCorDataMulti(n,lst,cor_mat)$sim_data #### Simulating
  cor12=cor(sim.data[,1],sim.data[,2],method=meth)  ### correlation between X and Y
  cor13=cor(sim.data[,1],sim.data[,3],method=meth)  ### Correlation between X and Z
  cor23=cor(sim.data[,2],sim.data[,3],method=meth)  ### Correlation  between Y and Z

  return(c(cor12,cor13,cor23))
}

