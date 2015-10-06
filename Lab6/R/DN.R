#'
#' Dynamic programming algorithm for the knapsack problem
#' 
#'  @param x is a 2dim matrix containing the weights and values
#'  @param W is the capacity of the knapsack
#'  @return a list with the _value_ of the optimally packed knapsack and the _elements_ that gives this value.
#'  @description Uses DP to find optimal value and elements, i.e. divides the problem into subproblems and solve each one, memoizes it and solve the whole problem by using that
#'  @references https://en.wikipedia.org/wiki/Knapsack_problem
#' 


set.seed(42)
n <- 2000
knapsack_objects <-
        data.frame(
                w=sample(1:4000, size = n, replace = TRUE),
                v=runif(n = n, 0, 10000)
        )
#v-> values stored in array
#w->weights stored in array
#n->number of distinct items
#W-> capacity in weight
#we will use the apply-family rather than for-loops.

x<-data.frame(w=c(1,1,1,1,2,2,3),v=c(1,1,2,3,1,3,5))
W=7
knapsack_dynamic<-function(x,W){
        lapply(w,function(x){stopifnot(is.integer(x))})
        w<-x$w
        v<-x$v
        n<-nrow(x)
        m<-matrix(rep(0,times=(n+1)*(W+1)),nrow=n+1)
        #weights as columns. items as rows
        for (i in 2:(n+1)){
                for (j in 2:(W+1)){
                        if (w[i-1]<=j){                                       #we could use
                                m[i,j]<-max(m[i-1,j],m[i-1,j-w[i-1]]+v[i-1])  #this part
                        } else{                                               #as a function 
                                m[i,j]<-m[i-1,j]                              #and call
                        }                                                     #with an apply
                }
        }
        value<-round(m[nrow(m),ncol(m)])
        #we have our m-matrix. Next is to find the way back to see what elements 
        #constitutes this solution.
        
        #!!!! I didnt solve how to find the way back. This code is taken and adapted from
        #http://www.statsblogs.com/2012/10/30/reinforcement-learning-in-r-an-introduction-to-dynamic-programming/
        #Dont know if we can use that?
        amount = rep(0, length(w))
        a = m[nrow(m), ncol(m)]
        j = length(w)
        Y = W
        
        while(a > 0) {
          while(m[j+1,Y+1] == a) {
            j = j - 1
          }
          j = j + 1
          amount[j] = 1
          Y = Y - w[j]
          j = j - 1
          a = m[j+1,Y+1]
        }
        elements<-amount*c(1:length(amount))
        elements<-elements[elements>0]
        return(list(value=value,elements=elements))
}




