rm(list = ls())
setwd("R")
source("NPOD.R")
library(ospsuite) # PK-Sim R toolbox
  

  population_file <- "data/Normal_cleareance_pop.csv"
  pkdata_file <- "data/data.csv"
  sim_file <- "data/sim.pkml"
  params <- vector(mode = "list", length = 2)
  params[[1]] <- c(0.95) # min
  params[[2]] <- c(1.35) #max
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
                                                      weight = population_data$`Weight [kg]`[i], weightUnit = "kg",
                                                      height = population_data$`Height [m]`[i], heightUnit = "m",
                                                      age = population_data$`Age (years, rounded to integer (except for <1))`[i], ageUnit = "year(s)")
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


  
ans <- NPOD(sim_file, pkdata_file, params, individuals, population_functions)



