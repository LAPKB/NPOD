rm(list = ls())
setwd("R")
source("NPOD.R")
library(ospsuite) # PK-Sim R toolbox
  #   population_file <- "Normal_cleareance_pop.csv"
  pkdata_file <- "data/obs_sim2.csv"
  sim_file <- "data/sim2.pkml"
  params <- vector(mode = "list", length = 2)
  params[[1]] <- c(0.1) # min
  params[[2]] <- c(10) #max
  #   population_data <- read_csv(population_file)
  number_of_individuals <- 100
  individuals <- vector(mode = "list", length = number_of_individuals)
  for (i in 1:number_of_individuals) {
    individual_chars <- createIndividualCharacteristics(species = Species$Human, population = HumanPopulation$European_ICRP_2002, gender = Gender$Male,
                                                      weight = 73, weightUnit = "kg",
                                                      height = 1.76, heightUnit = "m",
                                                      age = 30, ageUnit = "year(s)")
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
