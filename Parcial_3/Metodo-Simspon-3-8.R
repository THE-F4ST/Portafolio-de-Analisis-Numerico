simpson_3_8 <- function(f, a, b, n) {
  if(is.function(f) == FALSE) {
    stop("Argumento f no es una funcion")
  }
  if(b < a) {
    stop("Intervalo incorrecto")
  } 
  if(n < 0) {
    stop("Argumento n no es entero positivo")
  }
  
  h <- (b-a)/n
  Sum <- f(a) + f(b)
  
  for(i in 2:(n-1)) {
    if(i%%3 == 0) {
      Sum <- Sum + 2*f(a + i*h)
    } else {
      Sum <- Sum + 3*f(a + i*h)
    }
  }
  
  return((3*h*Sum)/8) 
}

f_x <- function(x) {
  return(1/(1+x^2)) 
}

simpson_3_8(f_x, -1, 1, 12)
