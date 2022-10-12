suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


testthat:: test_that("not a data frame", {
  expect_error(dynamic_knapsack(TRUE,10))
  expect_error(dynamic_knapsack(x <- c(2,3,4) ,500))
  expect_error(dynamic_knapsack(x <- c(2,1,1,1,1,1) ,5))
  expect_error(dynamic_knapsack(x <- c(2,3,4) ,TRUE))
  expect_error(dynamic_knapsack(x <- FALSE ,3))



})


testthat:: test_that("expecting value", {
  expect_success(expect_type(dynamic_knapsack(x = knapsack_objects[1:8,],W=3500), "list"))
  expect_success(expect_type(dynamic_knapsack(x = knapsack_objects[1:8,],W=2000), "list"))

})


testthat:: test_that("not a numeric", {
  expect_error(dynamic_knapsack(x = knapsack_objects[1:8,],W="2000"))
  expect_error(dynamic_knapsack(x = knapsack_objects[1:8,],W=NULL))

})


test_that("object name is given accordingly", {
  expect_silent(nu <- dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(nu, c("value", "elements"))
})


