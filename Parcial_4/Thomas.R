thomas <- function(A, b) {
  # A: matriz tridiagonal de coeficientes
  # b: Vector columna de términos independientes
  
  if(!is.matrix(A)) { stop("El objeto A no es una matriz") }
  
  n <- nrow(A)
  
  if(n != ncol(A))  { stop("La matriz A no es cuadrada") }
  if(n != nrow(b))  { stop("La dimension del vector b no corresponde") }
  
  if(all(A[, -1][which(!diag(-1, ncol(A))) != 0] == 0) &
     all(A[,  1][which(!diag( 0, ncol(A))) != 0] == 0) &
     all(A[, -2][which(!diag( 2, ncol(A))) != 0] == 0) ) {
        stop("La matriz A no es triangular")
  }
  
  c <- rep(0, n)
  d <- rep(0, n)
  
  c[1] <- A[1, 2] / A[1,1]
  d[1] <- b[1] / A[1, 1]
  
  for(i in 2:(n-1)) {
    temp <- A[i, i] - A[i, i-1] * c[i-1]
    c[i] <- A[i, i+1] / temp
    d[i] <- (b[i] - A[i, i-1] * d[i-1]) / temp
  }
  
  temp <- A[n, n] - A[n, n-1] * c[n-1]
  d[n] <- (b[n] - A[n, n-1] * d[n-1]) / temp
  
  
  x <- rep(0, n)
  
  x[n] <- d[n]
  for(i in (n-1):1) {
    x[i] <- d[i] - c[i] * x[i+1]
  }
  
  return(x)
}

m <- matrix(c(c( 0.8, -0.4,  0), 
              c(-0.4,  0.8, -0.4),
              c( 0,   -0.4, 0.8)), byrow = TRUE, ncol = 3, nrow = 3)
m
b <- matrix(c(41, 25, 105), ncol = 1)
b

thomas(m, b)

