setwd("R")
source("initial_data.r")
source("Dopt.R")
source("PSI_2.r")
source("D.r")
source("burke.R")
source("prune.r")
source("mu.r")

ans <- initial_data(10)
y <- ans$y
t <- ans$t
sigma <- ans$sigma
theta_0 <- ans$theta_0
true_theta <- ans$true_theta
a<-ans$a
b<-ans$b

theta_F <- 10e-2
theta_d <- 10e-4

ans <- Dopt(y, t, theta_0, theta_F, theta_d, sigma,a,b)

count <- ans$count
theta <- ans$theta
w <- ans$w
logLikelihood <- ans$logLikelihood

# P <- PSI_2(y, t, thera, sigma)
# PYL <- P * w

# Dfun <- function(.theta_parameter) { D(.theta_parameter, y, t, sigma, PYL) }
# K <- seq(from = 1.5, to = 3.5, by = 0.1)
# V <- seq(from = 0.3, to = 0.5, by = 0.05)
# Z <- matrix(rep(0, length(K) * length(V)), nrow = length(K))
# for (i in 1:length(K)) {
#   for (j in 1:length(V)) {
#     Z[i, j] = Dfun(c(K[i], V[i]))
#   }
# }

#Plots
