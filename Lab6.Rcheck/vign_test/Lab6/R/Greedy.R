#'
#' Greedy heuristic for the knapsack problem
#' 
#' @name Greedy heuristic
#'  @param x is a 2dim matrix containing the weights and values
#'  @param W is the capacity of the knapsack
#'  @return a list with the _value_ of the optimally packed knapsack and the _elements_ that gives this value.
#'  @description Uses greedy heuristic to solve knapsack problem, i.e. orders x by ratio v/w and 
#'    picks up the first lines until the knapsack is full (or almost)
#'  @references https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#' 


set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    w=sample(1:4000, size = n, replace = TRUE),
    v=runif(n = n, 0, 10000)
  )

greedy_knapsack<-function(x, W){
  stopifnot(is.data.frame(x))
  stopifnot(dim(x)[2]==2)
  stopifnot(names(x)==c("w","v"))
  stopifnot(all(x$w>0)&&all(x$v>0))
  n<-length(x$w)
  x$ratio <- x$v/x$w
  x$index <- 1:length(x$v)
  x <- x[order(x$ratio,decreasing = TRUE),]
  value <- 0
  weight <- 0
  i <- 1
  elements <- c()
  while(weight<=W){
    weight <- weight + x$w[i]
    if(weight<= W){
      value <- value + x$v[i]
      elements <- c(elements, x$index[i])
    }
    i <- i+1
    stopifnot(i<=length(x$v))
  }
  return(list(value=round(value),elements=elements))
}

Rprof("greedy_knapsack.out")
y<-greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)
Rprof(NULL)
summaryRprof("greedy_knapsack.out")
