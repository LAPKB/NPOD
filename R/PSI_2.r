#source("prob.r")

PSI_2 <- function(y, t, theta, sigma) {
  print("PSI")


  library(bigstatsr)


  N <- length(y)
  K <- tryCatch({
    value <- length(theta[1,])
  }, error = function(e) {
    length(theta)
  })
  psi <- matrix(0, N, K)

  psi <- multi_prob(y, t, theta, sigma)

  return(psi)
}