#' Brute without para
#' @description max weight with values.
#' @param x is a data frame
#' @param W is a integer
#'
#' @return will return a list
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' brute_without_para(x = knapsack_objects[1:8,], W = 3500)

brute_without_para <- function(x, W) {
  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  stopifnot(W>= 0)

  stopifnot(all(sort(names(x)) == c("v", "w")))
  stopifnot(all(x$v > 0))
  stopifnot(all(x$w > 0))


  n <- length(x$v | x$w)
  selected_elements <- c()
  value <- c()
  for (i in 1:(2^n)) {
    items <- as.vector(which(intToBits(i)==1))
    for (j in 1:length(items)) {
      selected_weight <- knapsack_objects$w[items][j]
    }
    if(selected_weight < W){
      for (k in 1:length(items)) {
        selected_value <- knapsack_objects$v[items][k]
      }
      value <- append(value, selected_value)
      selected_elements <- append(selected_elements, items)
    }
  }


  max_value <- max(selected_value)
  ind <- which(value == max_value)
  ele <- selected_elements[ind]
  return(output <- list(value = round(max_value), elements = ele))
}


