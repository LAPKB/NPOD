prob <- function(y, t, theta, sigma, individual, m) {
  #TODO: remove t, theta and individual from this function, those values are only used to calculate
  # mu, instead of that we just need to receive mu
  j <- length(y)
  z <- matrix(0, 1, j)
  if (missing(m)) {
    m <- mu(theta, t, individual)
  }
  z <- (y - m) ^ 2
  pr <- rep(0, j)
  for (i in 1:j) {

    pr[i] <- (1 / (sqrt(2 * pi) * sigma[i])) * exp(-(1 / (2 * (sigma[i]) ^ (2)) * z[i]))

  }
  prob <- prod(pr)
  #  prob <- (1 / (sqrt(2 * pi) * sigma)) ^ (j) * exp(-(1 / (2 * (sigma) ^ (2))) * sum(z))
  return(prob)
}

multi_prob <- function(y, t, theta, sigma, individuals, m) {
  if (missing(m)) {
    m <- multi_mu(theta, t, individuals)
  }
  #Mu shouldn't be here
  N <- length(y)
  K <- length(theta)
  prob <- matrix(0, N, K)

  #TODO: vectorize this
  for (i in 1:N) {
    for (l in 1:K) {
      prob[i, l] <- prob(y = y[[i]], sigma = sigma[[i]], m = m[[i, l]])
    }
  }
  prob
}