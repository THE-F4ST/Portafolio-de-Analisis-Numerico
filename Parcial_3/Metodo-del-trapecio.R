metodoTrapecio <- function(f, a, b, n) {
  
  if(!is.function(f)) {
    stop("El argumento f no es una funcion")
  }
  if(b < a) {
    stop("El intervalo es incorrecto")
  }
  if(n < 0) {
    stop("El argumento n debe ser un numero natural")
  }
  
  x_i <- a
  h <- (b - a)/n
  
  Trapecio <- f(x_i)
  
  for(i in 1:(n - 1)) {
    x_i <- x_i + h
    Trapecio <- Trapecio + 2*f(x_i) 
  }
  x_i <- x_i + h
  Trapecio <- Trapecio + f(x_i)
  return((h/2)*Trapecio)
}

f_x <- function(x) {
  return(1/(x^2))
}

metodoTrapecio(f_x, 2, 4, 100)
  