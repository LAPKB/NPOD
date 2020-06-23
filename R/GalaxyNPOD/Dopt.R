Dopt <- function(y, t, theta_0, theta_F, theta_d, sigma,a,b) {
  old_theta <- theta_0
  count <- 1
  F0 <- -10 ^ (30)
  F1 <- 2 * F0

  library(neldermead)
  options <- optimset(MaxFunEvals = 2000000000, TolX = 1e-14, MaxIter = 5, TolFun = 1e-14)
  while (abs(F1 - F0) > theta_F) {
    
    F0 <- 0
    inb_F <-0

    P <- PSI_2(y, t, old_theta, sigma)
    print(F1)
    print(F0)
    ans1 <- burke(P)
    lam1 <- ans1$lambda
    ind <- (lam1 > 0.00000001) & (lam1 > (max(lam1) / 1000))
    inb_theta <- matrix(old_theta[,ind],nrow = nrow(old_theta),ncol = length(old_theta[,ind]))
    print(inb_theta)
    
    P <- PSI_2(y, t, inb_theta, sigma)
    print(F1)
    print(F0)
    ans2 <- burke(P)
    print(F1)
    print(F0)
    inb_F <- ans2$fobj
    lam2 <- ans2$lambda

    ind2 <- (lam2 > (max(lam2)/1000))
    new_w <- lam2[ind2] / sum(lam2[ind2])
    new_theta <- matrix(inb_theta[,ind2],nrow = nrow(inb_theta),ncol = length(inb_theta[,ind2]))

    F0<-F1
    F1=inb_F
    print(F1)
    print(F0)
    
      if (abs(F1 - F0) <= theta_F) {
        break
        }

    K <- length(new_theta[1,])
    pyl <- P %*% new_w
      
    for (l in 1:K) {
      Dtheta <- function(.theta) { D(.theta, y, t, sigma, pyl) }
      fun <- function(.theta_parameter) {-1 * Dtheta(.theta_parameter) }
      cand_theta <- fminsearch(fun, new_theta[,l], options)
      
      new_theta <- prune(new_theta, cand_theta$optbase$xopt, theta_d,a,b)
      print(new_theta)
      }

    old_theta <- new_theta

    count <- count + 1
    }

  return(list("count" = count, "theta" = new_theta, "w" = new_w, "LogLikelihood" = F1))
  }
