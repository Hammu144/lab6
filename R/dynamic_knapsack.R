#' Dynamic knapsack
#' @description calulating possible weight capicity we can put in bag with help of discreat problem.
#' @param x data frame
#' @param W integer
#'
#' @return list
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
knapsack_dynamic <- function(x, W){

  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  stopifnot(W>= 0)

  weight_vec <- x$w
  value_vec <- x$v
  main_matrix <- matrix(-1, nrow = length(weight_vec) + 1, ncol = W +1)
  main_matrix[1, ] <- 0
  main_matrix[ , 1] <- 0


  filtr <- function(i,j){
    weight <- weight_vec[i - 1]
    if(i == 0 || j <= 0){
      main_matrix[i, j] <<- 0
      return(0)
    }
    if (main_matrix[i-1, j] == -1){
      main_matrix[i-1, j] <<-  filtr(i-1,j)
    }

    if(weight > j || j - weight <= 0 ){
      main_matrix[i, j] <<-  main_matrix[i-1, j]
    }
    else{

      if(main_matrix[i, j - weight] == -1){
        main_matrix[i, j - weight] <<-  filtr(i, j - weight_vec[i - 1])}

      main_matrix[i, j] <<- max(main_matrix[i-1, j], main_matrix[i -1, j - weight] + value_vec[i - 1])
    }
  }

  res <-  filtr(length(weight_vec)+1, W+1)



  index <- c()
  j <- W + 1
  i <- length(weight_vec)+1

  while ( i > 1) {
    if(main_matrix[i, j] != main_matrix[i-1, j]){
      index <- append(index, i-1)
      j <-  j - weight_vec[i-1]
    }
    i = i-1
  }
  return(list(value = round(res) , elements = index))
}

