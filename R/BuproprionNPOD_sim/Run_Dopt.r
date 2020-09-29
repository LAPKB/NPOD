rm(list = ls())
setwd("R/BuproprionNPOD_sim")
#source("initial_data.r")
source("Dopt.R")
source("PSI_2.r")
source("D.r")
source("burke.R")
source("prune.r")
source("mu.r")
source("prob.r")


library(ospsuite) # PK-Sim R toolbox
library(readr)
library(ggplot2)
library(tibble)
library(tidyr)
library(neldermead)
library(DiceDesign)
library(readxl)

#this statement not needed on LAPKB machine
# initPKSim("C:/Users/alona.kryshchenko/Dropbox (CSUCI)/For Alan/SummerGrant/Bupropion with R-Toolbox/PK-Sim 9.0.144")


### Load the individuals into a list of objects holding their individual characteristics
population_data <- read_csv("Normal_cleareance_pop.csv")

# Convert population_data into IndividualCharacteristics objects used in PK-Sim simulation
# Initialize list of individuals
number_of_individuals <- length(population_data$IndividualId)

individuals_old <- vector(mode = "list", length = number_of_individuals)
for (i in 1:number_of_individuals) {
  # Process population
  #if (population_data$Population[i] == "European") {
  their_population <- HumanPopulation$European_ICRP_2002
  #} else if (population_data$Population[i] == "BlackAmerican") {
  #   their_population <- HumanPopulation$BlackAmerican_NHANES_1997
  # } else if (population_data$Population[i] == "MexicanAmerican") {
  #   their_population <- HumanPopulation$MexicanAmericanWhite_NHANES_1997
  # } else if (population_data$Population[i] == "Asian") {
  #   their_population <- HumanPopulation$Asian_Tanaka_1996
  # }
  # Process gender
  if (population_data$Gender[i] == "FEMALE") {
    their_gender <- Gender$Female
  } else if (population_data$Gender[i] == "MALE") {
    their_gender <- Gender$Male
  }
  # Create individual characteristics
  # Every individual's age will be set to 40 years, the average of the age range given, i.e. 25-55 years
  individual_chars <- createIndividualCharacteristics(species = Species$Human, population = their_population, gender = their_gender,
                                                      weight = population_data$`Weight [kg]`[i], weightUnit = "kg",
                                                      height = population_data$`Height [m]`[i], heightUnit = "m",
                                                      age = population_data$`Age (years, rounded to integer (except for <1))`[i], ageUnit = "year(s)")
  # Add to list
  individuals_old[[i]] <- individual_chars
}

pkdata <- read.csv("data.csv")
time <- vector(mode = "list", length = number_of_individuals)
y_old <- vector(mode = "list", length = number_of_individuals)
sigma_old <- vector(mode = 'list', length = number_of_individuals)
for (i in 1:number_of_individuals) {
  time[[i]] <- pkdata[, 1][pkdata[, i + 1] != 999] * 60
  y_old[[i]] <- pkdata[, i + 1][pkdata[, i + 1] != 999]
  sigma_old[[i]] <- (0.5 * 1.65 + 0.1 * y_old[[i]]) #* 0.0000000000000000001
}

ind <- c(0)
for (i in 1:number_of_individuals) { ind[i] = length(y_old[[i]]) != 0 }
#TODO: 32 should not be hardcoded
t <- time[as.logical(ind)]
individuals <- individuals_old[as.logical(ind)]
y <- y_old[as.logical(ind)]
sigma <- sigma_old[as.logical(ind)]

characteristics <- individuals

indiv <- c()
#transform all the ind char to ind (createIndividual)
for (i in 1:length(characteristics)) {
  indiv[[i]] <- createIndividual(characteristics[[i]])
}

#Create population.csv
col.names <- indiv[[1]]$distributedParameters$paths
df <- as.data.frame(t(list(indiv[[1]]$distributedParameters$values)[[1]]))
colnames(df) <- col.names
for (ind in indiv[-(1:1)]) {
  aux <- as.data.frame(t(list(ind$distributedParameters$values)[[1]]))
  colnames(aux) <- col.names
  df <- rbind(df, aux)
}
ids <- data.frame(IndividualId = as.character(1:nrow(df)))
df <- cbind(ids, df)
write.csv(df, "test.csv", row.names = F)

# data <- c("POPDATA DEC_11\n", "#ID,EVID,TIME,DUR,DOSE,ADDL,II,INPUT,OUT\n")
# for (i in length(y)) {
#   for (j in length(y[[i]])) {
#     data <- append(data, paste0(i, ",0,", t[[i]][[j]], ",0,.,.,.,1,", y[[i]][[j]]))
#   }
# }

#ans <- initial_data(10)
# y <- ans$y
# t <- ans$t
#sigma <- 25
#c0(0.5)min(y)+c1(0.1)y_ij


#true_theta <- ans$true_theta
# a <- c(0.4, 180, 0.9)
# b <- c(0.6, 220, 1)

a <- c(0.9)
b <- c(1.1)

theta_0 <- a + t(runif.faure(10, 2)$design) * (b - a)
theta_0 <- theta_0[1,]

theta_F <- 10e-2
theta_d <- 10e-4

####### TEST BLOCK ####### REMOVE BEFORE RUNNING
# t1 <- system.time({
#   m <- multi_mu(theta, t, individuals)
# })
# psi <- multi_prob(y, t, theta, sigma, individuals, m)
# ans <- burke(psi)
##### END TEST BLOCK #####
# error <- c()
# for (i in 1:number_of_individuals) {

#   sub <- c()
#   for (j in 1:length(theta[1, ])) {
#     sub <- append(sub, (m[[i, j]] - y[[i]]) ^ 2)
#   }
#   error[[i]] <- max(sub)
# }




ans <- Dopt(y, t, theta_0, theta_F, theta_d, sigma, a, b, individuals)

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