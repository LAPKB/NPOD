prob <- function(y, t, theta, sigma, individual, m) {
  #TODO: remove t, theta and individual from this function, those values are only used to calculate
  # mu, instead of that we just need to receive mu

  # if (theta <= 0) {
  #   print("Negative theta")
  #   return(0)
  # }
  j <- length(y)
  z <- matrix(0, 1, j)
  if (missing(m)) {
    m <- mu(theta, t, individual)
  }

  if (length(y) != length(m)) {
    warning("Observed and predicted sizes don't match")
    print(length(y))
    print(length(m))
    print(y)
    print(m)
    print("----")
    stop()
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
  print("Multi prob")
  #Mu shouldn't be here
  N <- length(y)
  K <- tryCatch({
    value <- length(theta[1,])
  }, error = function(e) {
    length(theta)
  })
  mprob <- matrix(0, N, K)

  # IF ANY ELEMENT IN THETA IS NEGATIVE ALL THE PROB ARE ZERO ??????????
  if (sum(theta <= 0) >= 1) {
    return(mprob)
  }

  if (missing(m)) {
    t1 <- system.time({
      m <- multi_mu(theta, t, individuals)
    })
    print(t1)
  }

  #TODO: vectorize this
  for (i in 1:N) {
    for (l in 1:K) {
      mprob[i, l] <- prob(y = y[[i]], sigma = sigma[[i]], m = m[[i, l]])
    }
  }
  return(mprob)
}