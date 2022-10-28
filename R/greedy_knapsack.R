#' Greedy Knapsack
#' @description knapsack problem being greedy check every possiblity of values.
#' @param x is a data frame
#' @param W given max weight
#'
#' @return a list
#' @export
#'
#' @examples
#' #' set.seed(42)
#' n <- 2000
#' knapsack_objects <-data.frame(
#'   w=sample(1:4000, size = n, replace = TRUE),
#'   v=runif(n = n, 0, 10000)
#' )
#' greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)

greedy_knapsack <-
  function(x, W){
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(W))
    stopifnot(W>= 0)

    stopifnot(all(sort(names(x)) == c("v", "w")))
    stopifnot(all(x$v > 0))
    stopifnot(all(x$w > 0))


    n <- length(x$v | x$w)
    weight_vec <- x$w
    value_vec <- x$v


    ratio <-  value_vec /  weight_vec
    sorting <- sort(ratio, decreasing = TRUE)
    e = rep(0, n)
    current =  W


    for(i in  sorting){

      num <- which( sorting == i)[1]

      if (weight_vec[num] <= current){

        e[num] <- 1
        current <-   current - weight_vec[num]
      }else{
        break
      }

    }

    elements <- which(e == 1)
    value <- sum(value_vec[elements])




    if (length(elements) != n){
      n_sorting <- sort(value_vec, decreasing = TRUE)

      e = rep(0, n)
      current = W


      for(i in n_sorting){

        num <- which(value_vec == i)[1]
        if (weight_vec[num] <= current){

          e[num] <- 1
          current <- current - weight_vec[num]
        }else{
          break
        }
      }
      elements_2 <- which(e == 1)
      value_2 <- sum(value_vec[elements_2])


      if (value_2 > value){
        value <- value_2
        elements <- elements_2
      }
    }

    return(result <- list(value = value, elements = elements))

  }

