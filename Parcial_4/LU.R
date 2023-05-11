lu <- function(A) {
  n <- nrow(A)
  L <- diag(n)
  U <- matrix(0, nrow = n, ncol = n)
  
  for(k in 1:(n-1)) {
    for(i in (k+1):n) {
      L[i, k] <- A[i, k] / A[k, k]
      
      for(j in (k+1):n) {
        A[i, j] <- A[i, j] - L[i, k]*A[k, j]
      }
      A[i, k] <- L[i, k]
    }
  }
  
  for(i in 1:n) {
    U[i, i:n] <- A[i, i:n]
  }
  
  
  return(list(L = L, U = U))
}

A <- matrix(rnorm(9), nrow = 3)
lu(A)
