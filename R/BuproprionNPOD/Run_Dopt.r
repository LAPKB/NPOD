setwd("R")
source("initial_data.r")
source("Dopt.R")
source("PSI_2.r")
source("D.r")
source("burke.R")
source("prune.r")
source("mu.r")

library(DiceDesign)
library(readxl)

### Load the individuals into a list of objects holding their individual characteristics
population_data <- read_csv("bupropion baseline demographics - to share - converted to metric units.csv")

# Convert population_data into IndividualCharacteristics objects used in PK-Sim simulation
# Initialize list of individuals
number_of_individuals <- length(population_data$ID)

individuals <- vector(mode = "list", length = number_of_individuals)
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
  individuals[[i]] <- individual_chars
}

Bupropion150PKdata <- read.csv("Bupropion150PKdata.csv")
time<-vector(mode = "list", length = number_of_individuals)
y<-vector(mode = "list", length = number_of_individuals)
for (i in 1:number_of_individuals){
  time[[i]]<- Bupropion150PKdata[,1][Bupropion150PKdata[,i+1]!=999]*60
    y[[i]]<-Bupropion150PKdata[,i+1][Bupropion150PKdata[,i+1]!=999]
  }

#ans <- initial_data(10)
# y <- ans$y
# t <- ans$t
sigma <- 25
#true_theta <- ans$true_theta
a<-c(0.5,0.5,0.5,0.5,0)
b<-c(2,2,2,2,100)

theta_0 <- a+t(runif.faure(1000,5)$design)*(b-a)

theta_F <- 10e-2
theta_d <- 10e-4

ans <- Dopt(y, t, theta_0, theta_F, theta_d, sigma,a,b,individuals)

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
