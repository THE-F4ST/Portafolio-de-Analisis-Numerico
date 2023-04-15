gauss_lagendr3 <- function(f, a, b) {
  w <- c(8/9, 5/9, 5/9)
  x <- c(0, sqrt(3/5), -sqrt(3/5))
  Sum <- 0
  for(i in 1:3) {
    Sum <- Sum + w[i]*f((a-b)*x[i]/2 + (a+b)/2)
  }
  
  return((b-a)*Sum/2)
}

f_x <- function(x) {
  return(cos(x)/sqrt(x)) 
}

gauss_lagendr3(f_x, 0, 1)