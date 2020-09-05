rm(list = ls())
setwd("R/GalaxyNPOD")
#source("initial_data.r")
source("Dopt.R")
source("PSI_2.r")
source("D.r")
source("burke.R")
source("prune.r")
source("mu.r")
source("prob.r")
library(neldermead)

#ans <- initial_data(10)
y <- matrix(c(9.1720, 9.3500, 9.4830, 9.5580, 9.7750, 10.2270, 10.4060, 16.0840, 16.1700, 18.4190, 18.5520, 18.6000, 18.9270, 19.0520, 19.0700, 19.3300, 19.3430, 19.3490, 19.4400, 19.4730, 19.5290, 19.5410, 19.5470, 19.6630, 19.8460, 19.8560, 19.8630, 19.9140, 19.9180, 19.9730, 19.9890, 20.1660, 20.1750, 20.1790, 20.1960, 20.2150, 20.2210, 20.4150, 20.6290, 20.7950, 20.8210, 20.8460, 20.8750, 20.9860, 21.1370, 21.4920, 21.7010, 21.8140, 21.9210, 21.9600, 22.1850, 22.2090, 22.2420, 22.2490, 22.3140, 22.3740, 22.4950, 22.7460, 22.7470, 22.8880, 22.9140, 23.2060, 23.2410, 23.2630, 23.4840, 23.5380, 23.5420, 23.6660, 23.7060, 23.7110, 24.1290, 24.2850, 24.2890, 24.3660, 24.7170, 24.9900, 25.6330, 26.9600, 26.9950, 32.0650, 32.7890, 34.2790), nrow = 82, ncol = 1)
length(y)
t <- NULL
sigma <- 2.08

theta_0 <- matrix(c(14.5688, 9.4113, 28.3659), nrow = 1, ncol = 3)
#theta_0 <- matrix(8+30*runif(3),nrow = 1,ncol = 3)
#true_theta <- ans$true_theta
a <- 8
b <- 38

theta_F <- 1e-2
theta_d <- 1e-4
# Start the clock!
ptm <- proc.time()
ans <- Dopt(y, t, theta_0, theta_F, theta_d, sigma, a, b)
# Stop the clock
proc.time() - ptm

count <- ans$count
theta <- ans$theta
w <- ans$w
logLikelihood <- ans$LogLikelihood

P <- PSI_2(y, t, theta, sigma)
PYL <- P %*% w

Dfun <- function(.theta) { D(.theta, y, t, sigma, PYL) }
nDfun <- function(.theta) {(-1) * Dfun(.theta) }
x = fminbnd(nDfun, x0 = 9, xmin = 8, xmax = 38)
max_value = Dfun(x$optbase$xopt)
h <- Vectorize(Dfun)
curve(h, 0, 40)

#The function
stem <- function(x, y, pch = 16, linecol = 1, clinecol = 1, ...) {
  if (missing(y)) {
    y = x
    x = 1:length(x)
  }
  plot(x, y, pch = pch, ...)
  for (i in 1:length(x)) {
    lines(c(x[i], x[i]), c(0, y[i]), col = linecol)
  }
  lines(c(x[1] - 2, x[length(x)] + 2), c(0, 0), col = clinecol)
}

stem(theta, w)