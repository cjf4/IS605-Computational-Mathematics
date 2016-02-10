library(nnet)

#A is 3x3 coefficient matrix, B are the contraints
#function solves for vector X 

upper_tri_solver <- function(A, b) {
  n <- nrow(A)
  for(k in 1:(n-1)) {
    max_index <- which.is.max(abs(A[,k])) 
    if (max_index != k) {
      A1 <- A[k,]
      A2 <- A[max_index,]
      b1 <- b[k]
      b2 <- b[max_index]
      A[k,] <- A2
      A[max_index,] <- A1
      b[k] <- b2
      b[max_index] <- b1
    }
    #i == row
    for (i in (k+1):n) {
      #multiplier = negative -1/2 first time
      multiplier <- A[i,k]/A[k,k]
      #j == col
      A[i,k] <- 0
      for (j in (k+1):n) {
        A[i,j] <- A[i,j] - multiplier * A[k,j]
      }
      b[i] <- b[i] - multiplier * b[k]
    }
  }
  
  x <- vector(mode="numeric",length=n)
  x[n] <- b[n]/A[n,n]
  x[n-1] <- (b[n-1] - (A[n-1,n] * x[n])) / A[n-1,n-1]
  x[n-2] <- (b[n-2] - (A[n-2,n-1] * x[n-1]) - (A[n-2,n] * x[n])) / A[n-2,n-2]
  
  return(x)
}

matrix_elements <- c(1,2,-1,1,-1,-2,3,5,4)
A <- matrix(matrix_elements, nrow=3, ncol=3)
b <- c(1,2,6)

upper_tri_solver(A, b)