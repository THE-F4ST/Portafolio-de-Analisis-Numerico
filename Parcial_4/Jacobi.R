jacobi <- function(A, b, x0, max_iter, tol = 0.05) {
  # A: matriz dominante de coeficientes
  # b: vector columna de valores independientes
  # x0: Vector columna de una solución inicial
  # max_iter: Cantidad máxima de iteraciones a realizar
  # tol: Error de tolerancia establecido
  
  if(!is.matrix(A)) { stop("El objeto A no es una matriz") }
  
  n <- nrow(A)
  
  if(n != ncol(A))  { stop("La matriz A no es cuadrada") }
  if(n != nrow(d)) { stop("La dimension del vector d no corresponde") }
  if(n != nrow(x0)) { stop("La dimension del vector x0 no corresponde") }
  if(max_iter <= 0) { stop("max_iter debe ser un numero positivo")}
  
  for(i in 1:n) {
    sum_row <- sum(abs(A[i, ]))
    A_ii_abs <- abs(A[i, i])
    if(A_ii_abs < sum_row - A_ii_abs) { stop("La matriz A no es dominante") }
  }
  
  
  # Inicia algoritmo
  x <- x0
  x_n <- x
  for(k in 1:max_iter) {
    for(i in 1:n) {
      x_n[i] <- (b[i] - sum(A[i, -i] * x[-i])) / A[i, i]
    }
    
    if(max(abs(A %*% x - b)) < tol) {
      break
    }
    
    x <- x_n
  }
  
  return(x_n)
}

m <- matrix(c(1, -1, 1, 2), byrow = TRUE, ncol = 2, nrow = 2)
m
b <- matrix(c(5, 3), ncol = 1)
b
x0 <- matrix(c(1, 1), ncol = 1)
x0

jacobi(m, b, x0, 20)
