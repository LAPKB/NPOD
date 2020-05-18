prob <- function (y, t, theta, sigma){
    j <- length(y)
    dim <- length(theta)
    z <- matrix(0, 1, 10)

    for (i in 1:j) {
       m <- mu(theta, t[i])
       z[i] <- (y[j]-m)^2
    }
  prob = (1/(sqrt(2*pi)*sigma))^(j)*exp(-(1/(2*(sigma)^(2)))*sum(z))  
  return(prob)
}