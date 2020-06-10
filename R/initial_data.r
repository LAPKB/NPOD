initial_data <- function(K) {
  theta_0 <- matrix(rep(0, 2 * K), nrow = 2)
  #Simulation needs to be implemented to continue
  # [t,y,true_theta] = simulation
  sigma <- 0.005
  a <- 1
  b <- 4
  c <- 0.1
  d <- 1

  for (l in 1:K) {
    theta_0[1, l] <- a + (l - runif(1)) * ((b - a) / K)
    theta_0[2, l] <- c + (l - runif(1)) * ((d - c) / K)

  }

  return(list("y" = y, "t" = t, "sigma" = sigma, "theta_0" = theta_0, "true_theta" = true_theta))
}