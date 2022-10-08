#' Brute force knapsack
#' @description Implement knapsack by appling brute force which is actually we obtained
#' by making subset and then witht help help of parallel we are saving time.
#' @param x is a data frame
#' @param W given max weight
#' @param parallel logical value
#'
#' @return a list
#' @export
#'
#' @examples
#' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,parallel=FALSE)
brute_force_knapsack <-function(x, W, parallel = FALSE){

  stopifnot(is.data.frame(x))
  stopifnot(is.numeric(W))
  stopifnot(W>= 0)

  stopifnot(sort(names(x)) == c("v", "w"))
  stopifnot(x$v > 0)
  stopifnot(x$w > 0)



  num_of_obj <- length(x$v | x$w)
  no_of_sub_set <- 2^(num_of_obj)


  weight_vec <- x$w
  value_vec <- x$v


  no_of_cores <- parallel::detectCores()
  no_of_cores <- 2
  clust <- parallel::makeCluster(no_of_cores)


  if(parallel == FALSE){


    filtration <- function(x){
      bin_value <- intToBits(unlist(x))[1:num_of_obj]
      weight_sum <- sum(weight_vec[which(bin_value  == 1)])
      value_sum <- sum(value_vec[which(bin_value  == 1)])
      return(list(bin_value  = bin_value , weight_sum = weight_sum, value_sum = value_sum))
    }
    new_matrix <- sapply(1:no_of_sub_set, filtration)

  }else{

    filtration <- function(x){
      bin_value <- intToBits(unlist(x))[1:num_of_obj]
      weight_sum <- sum(weight_vec[which(bin_value  == 1)])
      value_sum <- sum(value_vec[which(bin_value  == 1)])
      return(list(bin_value  = bin_value , weight_sum = weight_sum, value_sum = value_sum))
    }

    new_matrix <- parallel::parSapply(clust, 1:no_of_sub_set, filtration)
    parallel::stopCluster(clust)
  }

  value_sum <- as.numeric( new_matrix["value_sum", ])
  weight_sum <- as.numeric( new_matrix["weight_sum", ])
  bin_value <-  new_matrix["bin_value", ]


  max_value <- max(value_sum[which(weight_sum <= W)])
  value_sum[which(weight_sum > W)] <- 0
  ith_max_value <- which(value_sum == max_value)[1]
  final_list <- x[bin_value[[ith_max_value]] == 1, ]


  return(list(value = max_value,  elements = as.numeric(row.names(final_list))))
}


