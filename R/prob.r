prob <- function(y, theta, i, sigma, individuals) {

  j <- length(y)
  z <- matrix(0, 1, 10)

  m <- mu(theta, i, individuals)
  z <- (y - m) ^ 2

  prob <- (1 / (sqrt(2 * pi) * sigma)) ^ (j) * exp(-(1 / (2 * (sigma) ^ (2))) * sum(z))
  return(prob)
}