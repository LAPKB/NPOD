setwd("R")
source("PSI_2.r")
source("burke.R")
source("D.r")
source("prune.r")

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
    ind <- (lam > 0.00000001) & (lam > (max(lam) / 1000))
    inb_theta <- old_theta[, ind]

    P = PSI_2(y, t, inb_theta, sigma)
    ans <- burke(P)
    F1 <- ans$obj
    lam <- ans$lambda

    ind2 <- lam > max(lam) / 100
    new_w <- lam[a] / sum(lam[a])
    new_theta <- inb_theta[, ind2]

    if (abs(F1 - F0) <= theta_F) {
      break
    }

    K <- length(new_theta[1,])
    pyl <- P * new_w
    nc_theta <- new_theta

    library(parallel)

    inputs <- 1:K
    numCores <- detectCores()
    cl <- makeCluster(numCores)
    parfor <- function(l) {
      Dtheta <- function(.theta) { D(.theta, y, t, sigma, pyl) }
      fun <- function(.theta_parameter) {-1 * Dtheta(.theta_parameter) }
      cand_theta <- fminsearch(fun, new_theta[, l], options)

      #this is the return value of parfor
      nc_theta <- prune(new_theta, cand_theta, theta_d)
    }
    results <- parLapply(cl, inputs, parfor)
    #How do we need to process the results???

    old_theta <- nc_theta

    count <- count + 1


  }

  return(list("count" = count, "theta" = new_theta, "w" = new_w, "LogLikelihood" = F1))
}