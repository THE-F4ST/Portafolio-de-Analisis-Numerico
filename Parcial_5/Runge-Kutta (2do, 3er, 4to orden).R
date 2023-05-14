fun_x_y <- function(x, y) {
  return(4*exp(0.8*x) - 0.5*y)
}

RK_2 <- function(f_x_y, a, b, y0, n) {
  h <- (b - a)/n
  x0 <- a
  
  x_vec <- c(a)
  y_vec <- c(y0)
  for(i in 1:n) {
    k1 <- fun_x_y(x0, y0)
    k2 <- fun_x_y(x0 + (h/2), y0 + (k1*h/2))
    y1 <- y0 + h*k2
    x0 <- a + i*h
    y0 <- y1
    x_vec[i+1] <- x0
    y_vec[i+1] <- y0
  }
  return(list(x_vec, y_vec))
}


RK_3 <- function(f_x_y, a, b, y0, n) {
  h <- (b - a)/n
  x0 <- a
  
  x_vec <- c(a)
  y_vec <- c(y0)
  for(i in 1:n) {
    k1 <- fun_x_y(x0, y0)
    k2 <- fun_x_y(x0 + (h/2), y0 + (k1*h/2))
    k3 <- fun_x_y(x0 + h, y0 - k1*h + 2*k2*h)
    y1 <- y0 + (k1 + 4*k2 + k3)*h/6
    x0 <- a + i*h
    y0 <- y1
    x_vec[i+1] <- x0
    y_vec[i+1] <- y0
  }
  return(list(x_vec, y_vec))
}


RK_4 <- function(f_x_y, a, b, y0, n) {
  h <- (b - a)/n
  x0 <- a
  
  x_vec <- c(a)
  y_vec <- c(y0)
  for(i in 1:n) {
    k1 <- h*f_x_y(x0, y0)/2
    k2 <- h*f_x_y(x0 + (h/2), y0 + k1)/2
    k3 <- f_x_y(x0 + (h/2), y0 + k2)/2
    k4 <- h*f_x_y(x0 + h, y0 + 2*k3)/2
    y1 <- y0 + (k1 + 2*k2 + 2*k3 + k4)/3
    x0 <- a + i*h
    y0 <- y1
    x_vec[i+1] <- x0
    y_vec[i+1] <- y0
  }
  return(list(x_vec, y_vec))
}

P2 <- RK_2(fun_x_y, 0, 2, 2, 10)
P3 <- RK_2(fun_x_y, 0, 2, 2, 10)
P4 <- RK_2(fun_x_y, 0, 2, 2, 10)


plot(P2[[1]], P2[[2]], xlab = "x", ylab = "y", type = "l", 
     col="green", lwd = 3)
abline(h = 0, v = 0)
legend(x = "bottomright", 
       legend = c("Orden 2", "Orden 3", "Orden 4"),
       fill = c("green", "blue", "red"),
       title = "Runge-Kutta")
lines(P3[[1]], P3[[2]], col = "blue", lwd = 3)
lines(P4[[1]], P4[[2]], col = "red", lwd = 3)

