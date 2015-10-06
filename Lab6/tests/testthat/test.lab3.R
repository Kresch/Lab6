library(stringr)

test_that("Output is the same for BF and DP", {
  expect_equal(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500),knapsack_dynamic(brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)))
}
          )