## ----include=FALSE-------------------------------------------------------
library(Lab6)
library(parallel)

## ------------------------------------------------------------------------
set.seed(42)
n <- 16
knapsack_objects <-
        data.frame(
                w=sample(1:4000, size = n, replace = TRUE),
                v=runif(n = n, 0, 10000)
        )
system.time(brute_force_knapsack(knapsack_objects,2000))

## ------------------------------------------------------------------------
set.seed(42)
n <- 500
knapsack_objects <-
        data.frame(
                w=sample(1:4000, size = n, replace = TRUE),
                v=runif(n = n, 0, 10000)
        )
system.time(knapsack_dynamic(knapsack_objects,2000))

## ------------------------------------------------------------------------
n <- 1000000
knapsack_objects <-
        data.frame(
                w=sample(1:4000, size = n, replace = TRUE),
                v=runif(n = n, 0, 10000)
        )
  
system.time(greedy_knapsack(knapsack_objects,W=2000))

