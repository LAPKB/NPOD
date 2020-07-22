PSI_2 <- function(y, t=NULL, theta, sigma) {

  N <- length(y[, 1])
  K <- length(theta[1,])
  psi <- matrix(0, N, K)

  for (i in 1:N) {
    for (l in 1:K) {
      psi[i, l] <- prob(y[i,], t[i,], theta[, l], sigma)
    }
  }
  return(psi)
}