rm(list = ls())
# setwd("R")
#devtools::install(args="--no-multiarch")
# source("NPOD.R")
library(NPOD)
pkdata_file <- "inst/data/data_1comp_neely.csv"
sim_file <- ""
model <- function(theta,t){
    #t<c(0.5,1,2,3,4,5,...)
    #this equation assumes a dose of 500mg, an infusion of 0.5h and a time vector like c(0.5,...)
    x05<-(500/(0.5*theta[1]*theta[2]))*(1-exp(-theta[1]*t[1]))
    val <- (x05)*exp(-theta[1]*(t[-1]-0.5))
    return(c(x05,val))
}
params <- vector(mode = "list", length = 2) # K V
params[[1]] <- c(0.001, 50) # min
params[[2]] <- c(2, 250) #max
# population_data <- read_csv(population_file)
individuals <- seq(from=1, to=51)
# theta_0<-matrix(unlist(read.csv("test.csv")),nrow=2)
t1<- system.time({
    ans <- NPOD(sim_file, pkdata_file, params, individuals, model=model, c0=0, c1=0.1, size_theta0= 100)
})




