source("simulation2.r")

initial_data <- function(K) {
  theta_0 <- matrix(rep(0, 5 * K), nrow = 5)
  #Simulation needs to be implemented to continue
  ans <- simulation()
  t <- ans$t
  y <- ans$y
  true_theta <- ans$true_theta
  sigma <- 0.005
  a <- c(0.4999,0.4999,0.4999,0.4999,0.0001)
  b <- c(1.9999,1.9999,1.9999,1.9999,99.9999)
  
  for (l in 1:K) {
    theta_0[1, l] <- a[1] + (l - runif(1)) * ((b[1] - a[1]) / K)
    theta_0[2, l] <- a[2] + (l - runif(1)) * ((b[2] - a[2]) / K)
    theta_0[3, l] <- a[3] + (l - runif(1)) * ((b[3] - a[3]) / K)
    theta_0[4, l] <- a[4] + (l - runif(1)) * ((b[4] - a[4]) / K)
    theta_0[5, l] <- a[5] + (l - runif(1)) * ((b[5] - a[5]) / K)

  }

  return(list("y" = y, "t" = t, "sigma" = sigma, "theta_0" = theta_0, "true_theta" = true_theta))
}