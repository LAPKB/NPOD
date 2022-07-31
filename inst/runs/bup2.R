rm(list = ls())
setwd("inst")
library(NPOD)
library(ospsuite) # PK-Sim R toolbox
population_file <- "data/bupropion baseline demographics - to share - converted to metric units.csv"
pkdata_file <- "data/Bupropion150PKdata_fixed.csv"
sim_file <- "data/PO SR 150 mg bupropion to human - Connarn et al 2017 - table - scaling factors.pkml"
params <- vector(mode = "list", length = 2)
params[[1]] <- c(0, 0, 0) # min
params[[2]] <- c(10, 1000, 1000) # max
population_data <- readr::read_csv(population_file)
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
  individual_chars <- createIndividualCharacteristics(
    species = Species$Human, population = their_population, gender = their_gender,
    weight = population_data$`Weight (kg)`[i], weightUnit = "kg",
    height = population_data$`Height (cm)`[i], heightUnit = "cm",
    age = 40, ageUnit = "year(s)"
  )
  individuals[[i]] <- individual_chars
}





parameter_paths <- c("Applications|PO 150 mg - human|SR PO 150 mg - FDA table|ScaleFactorX",
                     "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|ScaleFactorY",
                     "Liver and Intestinal CL|Reference concentration")

population_functions <- get_population_functions(parameter_paths,number_of_individuals)







ans <- NPOD(sim_file, pkdata_file, params, individuals, population_functions, c1 = 0.03, c2 = 0.01) # c1=0.03, c2=0.05,

