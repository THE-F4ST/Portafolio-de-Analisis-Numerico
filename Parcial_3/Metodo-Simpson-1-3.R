simpson_1_3.functional <- function(f, a, b, n) {
  if(!is.function(f)) {
    stop("El argumento f no es una funcion")
  }
  if(b < a) {
    stop("El intervalo es incorrecto")
  }
  if(n < 0) {
    stop("El argumento n debe ser un numero natural")
  }
  
}

simpson_1_3.vectorial <- function(x_vec, y_vec) {
  n <- length(y_vec)
  if(length(x_vec) != n) {
    stop("Longitudes de vectores no corresponden")
  }
  
  h <- x_vec[2] - x_vec[1]
  
  I <- y_vec[1]
  for(i in 2:(n - 1)) {
    if(i%%2 == 0) {
      I <- I + 4*y_vec[i]
    } else {
      I <- I + 2*y_vec[i]
    }
  }
  I <- I + y_vec[n]
  return((h/2)*I)
}

simpson_1_3.vectorial(seq(from = 0, to = 0.8, by = 0.2),
                      c(3.5, 3.1, 3, 2.8, 2.6))
