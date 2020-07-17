prob <- function(y, t, theta, sigma, individual) {

  j <- length(y)
  z <- matrix(0, 1, 10)

  m <- mu(theta, t, individual)
  z <- (y - m) ^ 2

  prob <- (1 / (sqrt(2 * pi) * sigma)) ^ (j) * exp(-(1 / (2 * (sigma) ^ (2))) * sum(z))
  return(prob)
}