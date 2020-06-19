source("prob.r")

PSI_2 <- function(y, t, theta, sigma) {

  library(readr)
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

  N <- length(y[, 1])
  K <- length(theta[1,])
  psi <- matrix(0, N, K)

  for (i in 1:N) {
    for (l in 1:K) {
      psi[i, l] <- prob(y[i,], t[i,], theta[, l], sigma, individuals[[i]])
    }
  }
  return(psi)
}