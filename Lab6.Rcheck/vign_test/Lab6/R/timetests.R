#testing times
# set.seed(42)
# n <- 500
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# iter<-0
# times<-c()
# while (iter<50){
#   time<-proc.time()
#   knapsack_dynamic(x = knapsack_objects, W = 2000)
#   times<-c(times,proc.time()-time)
#   iter=iter+1
# }
# mean(times)