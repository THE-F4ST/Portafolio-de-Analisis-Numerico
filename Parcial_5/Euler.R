fun_x_y <- function(x, y) {
  return(0.7*y - x^2 + 1)
}

euler <- function(f_x_y, a, b, y0, n) {
  h <- (b - a)/n
  x0 <- a
  
  x_vec <- c(a)
  y_vec <- c(y0)
  for(i in 1:n) {
    y1 <- y0 + h*f_x_y(x0, y0)
    x0 <- a + i*h
    y0 <- y1
    x_vec[i+1] <- x0
    y_vec[i+1] <- y0
  }
  return(list(x_vec, y_vec))
}

P <- euler(fun_x_y, 1, 2, 1, 14)

plot(P[[1]], P[[2]], 
     xlim = c(-0.1, 2.2), ylim = c(-0.1, 2.2), xlab = "x", ylab = "y")
abline(h = 0)
abline(v = 0)
