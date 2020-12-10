rm(list = ls())
setwd("R")
source("NPOD.R")
source("runs/inits.R")

theta_0 <- theta_0_1comp()

pkdata_file <- "data/data_1comp.csv"
sim_file <- ""
model <- function(theta,t){
    (20/theta[2])*exp(-theta[1]*t)
}
params <- vector(mode = "list", length = 2) # K V
params[[1]] <- c(0.4, 0.4) # min
params[[2]] <- c(2, 2) #max
# population_data <- read_csv(population_file)
individuals <- seq(from=1, to=20)

ans <- NPOD(sim_file, pkdata_file, params, individuals, model=model, c0=0, c1=0, c2=0.5, theta_0=theta_0)
