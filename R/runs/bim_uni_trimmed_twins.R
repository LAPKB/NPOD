rm(list = ls())
setwd("R")
source("NPOD.R")
library(ospsuite) # PK-Sim R toolbox

population_file <- "data/demographics_bm_trim_twins.csv"
theta_0 <-  matrix(c(1.571292659, 1.684000469, 1.897188772, 1.334296808, 3.507619028, 1.965186011, 1.654506763, 2.959986382, 1.651944217, 1.684047026, 1.994727369, 1.749076709, 1.944705938, 2.022695536, 1.878534566, 0.122129378, 0.104367722, 0.134260968, 0.063060173, 0.066073962, 0.116931539, 0.104979233, 0.116084543, 0.073854101, 0.097529239, 0.083336113, 0.116909087, 0.147036329, 0.126209817, 0.136675505), nrow = 1)
pkdata_file <- "data/obs_twins.csv"
sim_file <- "data/sim.pkml"
params <- vector(mode = "list", length = 2)
params[[1]] <- c(0.01) # min
params[[2]] <- c(5) #max
population_data <- read_csv(population_file)
number_of_individuals <- length(population_data$IndividualId)
individuals <- vector(mode = "list", length = number_of_individuals)
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
                                                        weight = population_data$`Organism.Weight..kg.`[i], weightUnit = "kg",
                                                        height = population_data$`Organism.Height..dm.`[i], heightUnit = "dm",
                                                        age = population_data$`Organism.Age..year.s..`[i], ageUnit = "year(s)")
    # Add to list
    individuals[[i]] <- individual_chars
    }

    #All the population functions might receive the population object and a the theta matrix
    population_functions <- c(function(.population, .theta, index = NULL) {
    pop_theta = c()
    for (i in 1:length(.theta[1, ])) {
        pop_theta <- append(pop_theta, rep(.theta[1, i], number_of_individuals))
    }
    .population$setParameterValues("Liver Enzyme|Reference concentration", pop_theta)
}
)



ans <- NPOD(sim_file, pkdata_file, params, individuals, population_functions, c0=0, c1=0.00003, theta_0=theta_0)
