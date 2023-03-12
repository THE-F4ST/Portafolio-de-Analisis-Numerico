polNewton <- function(x_vec, y_vec) {
  n <- length(y_vec)
  if(n != length(x_vec)) { 
    print("La longitud de los vectores difiere")
    return(NA)
  }
  
  y <- matrix(y_vec, ncol = 1)
  
  m <- matrix(0, ncol = n-1, nrow = n)
  m <- cbind(y, m)
  
  for(i in 2:n) {
    k <- 1
    for(j in 2:n) {
      if(j > i) { break }
      m[i, j] <- (m[i, j-1] - m[i-1, j-1]) / (x_vec[i] - x_vec[i-k])  
      k <- k + 1
    }
  }
  
  polinomio <- ''
  for(i in 1:n) {
    polinomio <- paste(polinomio, ' +   (', m[i,i], ')')
    j <- 1
    termino <- ''
    while(j < i) {
      termino <- paste(termino, '(x - (', x_vec[j], '))')
      j <- j + 1
    }
    polinomio <- paste(polinomio, termino)
  }
  
  cat('\n', polinomio, '\n')
  return(m)
}

x_vec <- c(1,2,3,4)
y_vec <- c(2,-1,0,1)
M <- polNewton(x_vec, y_vec)
M
z <- paste(z, "0")




