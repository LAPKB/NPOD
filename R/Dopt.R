setwd("R")
source("PSI_2.r")
source("burke.R")

Dopt <- function(y, t, theta_0, theta_F, theta_d, sigma) {
  old_theta <- theta_0
  count <- 1
  F0 <- -10 ^ (30)
  F1 <- 2 * F0

  library(neldermead)
  option <- optimset(MaxFunEvals = 2000000000, TolX = 1e-14, MaxIter = 5, TolFun = 1e-14)
  while (abs(F1 - F0) > theta_F) {
    F0 <- F1

    P = PSI_2(y, t, old_theta, sigma)
    ans <- burke(p)
    lam <- ans$lambda
    ind = (lam > 0.00000001) & (lam > (max(lam) / 1000))
    ## WORK IN PROGRESS
  }
}