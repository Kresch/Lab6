## ----echo=FALSE----------------------------------------------------------
n <- 16
knapsack_objects <-
data.frame(
w=sample(1:4000, size = n, replace = TRUE),
v=runif(n = n, 0, 10000)
)
iter=0
times<-c()
while (iter<1000){
  time<-proc.time()
  brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
  times<-c(times,proc.time()-time)
  iter<-iter+1
}

## ------------------------------------------------------------------------
mean(times)

