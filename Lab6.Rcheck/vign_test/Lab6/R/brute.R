#'
#' Brute force algorithm for the knapsack problem
#'  @name Brute force
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


brute_force_knapsack<-function(x, W){
        stopifnot(is.data.frame(x))
        stopifnot(dim(x)[2]==2)
        stopifnot(names(x)==c("w","v"))
        stopifnot(all(x$w>0)&&all(x$v>0))
        n<-length(x$w)
        #create matrix of bin reps.
        bin_mat<-sapply(c(1:2^n),function(x){as.integer(intToBits(x)[1:n])})
        #where we only have the bits that are needed to define a number of max 2^n
        
        
        #now we can matrix-multiplicate this with the vector of weights and
        #for those that satisfy weights<W we calculate the values 
        values<-c()
        find_W<-function(col){
                OK_weights<-c()
                weight<-t(x$w)%*%col
                if (weight<=W){
                        values<-c(values,t(x$v)%*%col)
                } else { values<-c(values,0)}
                return(values)
        }
        m<-apply(bin_mat,2,find_W)
        #we have the values and want the max and which combination of items corresponds to 
        #this maxima.
        index_max_value<-which.max(m)
        index_elements<-bin_mat[,index_max_value]
        elements<-c(c(1:n)*index_elements)
        elements<-elements[elements>0]
        #the last couple of rows might be done simpler? but doesnt affect runtime.
        return(list(value=round(max(m)),elements=elements))
        
}

Rprof("brute.out", line.profiling = TRUE, memory.profiling = TRUE)
y<-brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000)
Rprof(NULL)
summaryRprof("brute.out")



