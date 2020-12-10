rm(list = ls())
setwd("R")
source("NPOD.R")
library(ospsuite) # PK-Sim R toolbox
 population_file <- "data/bupropion baseline demographics - to share - converted to metric units.csv"
  pkdata_file <- "data/Bupropion150PKdata.csv"
  sim_file <- "data/PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml"
  params <- vector(mode = "list", length = 2)
  params[[1]] <- c(0.1, 150, 0.1) # min
  params[[2]] <- c(10, 600, 10) #max
  population_data <- read_csv(population_file)
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
    individual_chars <- createIndividualCharacteristics(species = Species$Human, population = their_population, gender = their_gender,
                                                      weight = population_data$`Weight (kg)`[i], weightUnit = "kg",
                                                      height = population_data$`Height (cm)`[i], heightUnit = "cm",
                                                      age = 40, ageUnit = "year(s)")
    individuals[[i]] <- individual_chars
  }


  population_functions <- c(function(.population, .theta, .index = NULL) {
    .population$setParameterValues("Liver and Intestinal CL|Reference concentration", rep(.theta[3, .index], length(.population$allIndividualIds)))
  }
  )


  simulation_functions <- c(function(.simulation, .theta, .index = NULL) {
    .scale_dissolution_profile <- function(simulation, scaling_factors = c(1, 1)) {
      dissolution_data_path = "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|Fraction (dose)"
      if (all(scaling_factors > 0) && (!identical(scaling_factors, c(1, 1)))) {
        dissolution_data_parameter <- getParameter(dissolution_data_path, simulation)
        dissolution_data_formula <- dissolution_data_parameter$formula
        dissolution_data_all_points <- dissolution_data_formula$allPoints

        numPoints <- length(dissolution_data_all_points)

        # Initialize vectors to hold values of table
        times <- rep(0, numPoints)
        fractions <- rep(0, numPoints)

        # Read in points
        for (i in 1:numPoints) {
          times[i] <- dissolution_data_all_points[[i]]$x
          fractions[i] <- dissolution_data_all_points[[i]]$y
        }

        points <- array(c(times, fractions), dim = c(numPoints, 2))
        # print(points)

        # Apply scaling factor to table
        new_x <- points[, 1] * scaling_factors[1]
        new_y <- points[, 2] * scaling_factors[2]

        # print(new_x)
        # print(new_y)

        dissolution_data_formula$setPoints(new_x, new_y)
      } else {
        if (!all(scaling_factors > 0)) {
          print("Negative scaling factor is not allowed.")
        }
        return(NULL)
      }
    }
    .scale_dissolution_profile(.simulation, c(.theta[1, .index], .theta[2, .index]))
  })

  
ans <- NPOD(sim_file, pkdata_file, params, individuals, population_functions, simulation_functions)
