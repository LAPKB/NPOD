rm(list = ls())
setwd("R/BuproprionNPOD_pop")
#source("initial_data.r")
source("Dopt.R")
source("PSI_2.r")
source("D.r")
source("burke.R")
source("prune.r")
source("mu.r")



library(ospsuite) # PK-Sim R toolbox
library(readr)
library(ggplot2)
library(tibble)
library(tidyr)
library(neldermead)
library(DiceDesign)
library(readxl)

#this statement not needed on LAPKB machine
initPKSim("C:/Users/alona.kryshchenko/Dropbox (CSUCI)/For Alan/SummerGrant/Bupropion with R-Toolbox/PK-Sim 9.0.144")


### Load the individuals into a list of objects holding their individual characteristics
population_data <- read_csv("bupropion baseline demographics - to share - converted to metric units.csv")

# Convert population_data into IndividualCharacteristics objects used in PK-Sim simulation
# Initialize list of individuals
number_of_individuals <- length(population_data$ID)

individuals_old <- vector(mode = "list", length = number_of_individuals)
for (i in 1:number_of_individuals) {
  # Process population
  if (population_data$Population[i] == "European") {
    their_population <- HumanPopulation$European_ICRP_2002
  } else if (population_data$Population[i] == "BlackAmerican") {
    their_population <- HumanPopulation$BlackAmerican_NHANES_1997
  } else if (population_data$Population[i] == "MexicanAmerican") {
    their_population <- HumanPopulation$MexicanAmericanWhite_NHANES_1997
  } else if (population_data$Population[i] == "Asian") {
    their_population <- HumanPopulation$Asian_Tanaka_1996
  }
  # Process gender
  if (population_data$Sex[i] == "Female") {
    their_gender <- Gender$Female
  } else if (population_data$Sex[i] == "Male") {
    their_gender <- Gender$Male
  }
  # Create individual characteristics
  # Every individual's age will be set to 40 years, the average of the age range given, i.e. 25-55 years
  individual_chars <- createIndividualCharacteristics(species = Species$Human, population = their_population, gender = their_gender,
                                                      weight = population_data$`Weight (kg)`[i], weightUnit = "kg",
                                                      height = population_data$`Height (cm)`[i], heightUnit = "cm",
                                                      age = 40, ageUnit = "year(s)")
  # Add to list
  individuals_old[[i]] <- individual_chars
}

Bupropion150PKdata <- read.csv("Bupropion150PKdata.csv")
time <- vector(mode = "list", length = number_of_individuals)
y_old <- vector(mode = "list", length = number_of_individuals)
sigma_old <- vector(mode = 'list', length = number_of_individuals)
for (i in 1:number_of_individuals) {
  time[[i]] <- Bupropion150PKdata[, 1][Bupropion150PKdata[, i + 1] != 999] * 60
  y_old[[i]] <- Bupropion150PKdata[, i + 1][Bupropion150PKdata[, i + 1] != 999]
  sigma_old[[i]] <- 0.5 * 1.65 + 0.1 * y_old[[i]]
}

ind <- c(0)
for (i in 1:33) { ind[i] = length(y_old[[i]]) != 0 }
t <- time[as.logical(ind)]
individuals <- individuals_old[as.logical(ind)]
y <- y_old[as.logical(ind)]
sigma <- sigma_old[as.logical(ind)]

data <- c("POPDATA DEC_11\n", "#ID,EVID,TIME,DUR,DOSE,ADDL,II,INPUT,OUT\n")
for (i in length(y)) {
  for (j in length(y[[i]])) {
    data <- append(data, paste0(i, ",0,", t[[i]][[j]], ",0,.,.,.,1,", y[[i]][[j]]))
  }
}

#ans <- initial_data(10)
# y <- ans$y
# t <- ans$t
#sigma <- 25
#c0(0.5)min(y)+c1(0.1)y_ij


#true_theta <- ans$true_theta
a <- c(0.01, 0.01, 0.01)
b <- c(100, 1.1, 100)

theta_0 <- a + t(runif.faure(10, 3)$design) * (b - a)

theta_F <- 10e-2
theta_d <- 10e-4

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
