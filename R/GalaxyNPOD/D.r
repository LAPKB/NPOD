source("prob.r")

D <- function(theta_parameter, y, t=NULL, sigma, PYL) {
  
  N <- length(y[, 1])
  D_components = -N

  for (i in 1:N) {
    D_components <- D_components + prob(y[i,], t[i,], theta_parameter, sigma) / PYL[i]
  }
  return(D_components)
}