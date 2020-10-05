rm(list = ls())
setwd("R/BuproprionNPOD_sim")
source("Run_Dopt.r")
library(ospsuite) # PK-Sim R toolbox

run <- "sim"
# run <- "bup"

## PARAMETER DEFINITION ##
if (run == "sim") {
  sim_param <- F
  population_file <- "Normal_cleareance_pop.csv"
  pkdata_file <- "data.csv"
  sim_file <- "sim.pkml"
  params <- vector(mode = "list", length = 2)
  params[[1]] <- c(0.1) # min
  params[[2]] <- c(10) #max
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
} else if (run == "bup") {
  sim_param <- T
  population_file <<- "bupropion baseline demographics - to share - converted to metric units.csv"
  pkdata_file <<- "Bupropion150PKdata.csv"
  sim_file <<- "PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml"
  param_min <<- c(0.1, 150, 0.1)
  param_max <<- c(10, 300, 10)
}
ans <- run_dopt(sim_file, pkdata_file, params, sim_param, individuals)
## END PARAMETER DEFINITION ##
