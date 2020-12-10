Dopt <- function(y, t, theta_0, theta_F, theta_d, sigma, a, b) {
  print("dopt")
  old_theta <- theta_0
  count <<- 1
  F0 <- -10 ^ (30)
  F1 <- 2 * F0

  ind1 <- c(0)
  ind2 <- c(0)

  options <- optimset(MaxFunEvals = 2000000000, TolX = 1e-14, MaxIter = 10, TolFun = 1e-14)
  #  old_F <-c()
  new_F <- c()
  #  old_F[count] <- F1
  new_F[count] <- F0
  new_F[count + 1] <- F1
  #while (abs(F1 - F0) > theta_F) {
  while (abs(new_F[count + 1] - new_F[count]) > theta_F) {

    #old_F[count] <-new_F[count] 


    P1 <- PSI_2(y, t, old_theta, sigma)
    ans1 <- burke(P1)
    lam1 <- ans1$lambda
    ind1 <- (lam1 > 0.00000001) & (lam1 > (max(lam1) / 1000))
    inb_theta <- tryCatch({
      matrix(old_theta[, ind1], nrow = nrow(old_theta), ncol = ncol(old_theta[, ind1]))
    }, error = function(e) {
      matrix(old_theta[, ind1], byrow = T, ncol = sum(ind1))
    })
    # inb_theta <- matrix(old_theta[, ind1], nrow = nrow(old_theta), ncol = ncol(old_theta[, ind1]))
    # inb_theta <- matrix(old_theta[,ind1])
    print(inb_theta)

    lambda <- c()
    fobj <- c()

    P2 <- PSI_2(y, t, inb_theta, sigma)
    ans2 <- burke(P2)

    #updating of F1
    new_F[count + 2] <- ans2$fobj
    lam2 <- ans2$lambda

    ind2 <- (lam2 > (max(lam2) / 1000))
    new_w <- lam2[ind2] / sum(lam2[ind2])
    new_theta <- tryCatch({
      matrix(inb_theta[, ind2], nrow = nrow(inb_theta), ncol = ncol(inb_theta[, ind2]))
    }, error = function(e) {
      matrix(inb_theta[, ind2], byrow = T, ncol = sum(ind2))
    })
    # new_theta <- matrix(inb_theta[ind2])
    print(new_theta)
    print("START")
    print(ans2)
    print("END")


    if (abs(new_F[count + 2] - new_F[count + 1]) <= theta_F) {
      break
    }
    #print(abs(new_F[count+2] - new_F[count+1]))
    K <- length(new_theta[1,])
    pyl <- P2 %*% new_w
    #TODO: Ask Alona, the size of new_theta is increasing, 1:k is fixed, is not testing all supp points.
    for (l in 1:K) {
      #Dtheta <- function(.theta) { D(.theta, y, t, sigma, pyl, individuals) }
      multi_Dtheta <- function(.theta) {-1 * multi_D(.theta, y, t, sigma, pyl) }
      # fun <- function(.theta_parameter) {-1 * Dtheta(.theta_parameter) }
      cand_theta <- fminsearch(multi_Dtheta, new_theta[, l], options)
      print(cand_theta)

      new_theta <- prune(new_theta, cand_theta$optbase$xopt, theta_d, a, b)
      print(new_theta)
    }

    old_theta <- new_theta

    count <- count + 1
    print("Counter: ")
    print(count)
  }

  return(list("count" = count, "theta" = new_theta, "w" = new_w, "LogLikelihood" = new_F[length(new_F)]))
}
