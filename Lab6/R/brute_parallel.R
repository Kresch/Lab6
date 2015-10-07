#'
#' Brute force algorithm for the knapsack problem using parallelizing
#'  @name Brute force parallel
#'  @param x is a 2dim matrix containing the weights and values
#'  @param W is the capacity of the knapsack
#'  @return a list with the _value_ of the optimally packed knapsack and the _elements_ that gives this value.
#'  @description uses brute force, i.e. tests all combinations and finds the one with max value under the restriction total weight<W.
#'  @references https://en.wikipedia.org/wiki/Knapsack_problem
#' 

set.seed(42)
n <- 16
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )
library(parallel)

brute_force_knapsack_para<-function(x, W){
  cores<-parallel::detectCores()
  stopifnot(is.data.frame(x))
  stopifnot(dim(x)[2]==2)
  stopifnot(names(x)==c("w","v"))
  stopifnot(all(x$w>0)&&all(x$v>0))
  n<-length(x$w)
  #create matrix of bin reps.
  bin_mat<-parallel::mclapply(c(1:2^n),function(x){as.integer(intToBits(x)[1:n])},mc.cores=cores)
  #where we only have the bits that are needed to define a number of max 2^n
  weights<-simplify2array(parallel::mclapply(bin_mat,function(y){y%*%x$w},mc.cores=cores))
  values<-simplify2array(parallel::mclapply(bin_mat,function(y){y%*%x$v},mc.cores=cores))
  #returns a list of matrices with 1 element that is the weight of combo i.
  values[weights>W]<-0
  max1_index<-which.max(values)
  
  #booleans used as 0s and 1s.
  index_elements<-bin_mat[[max1_index]]
  
  #a list now
  elements<-c(c(1:n)*index_elements)
  
  elements<-elements[elements>0]
 
  return(list(value=round(values[max1_index]),elements=elements))
}

