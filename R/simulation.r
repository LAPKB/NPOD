# Load necessary libraries and functions
library(ospsuite)   # PK-Sim R toolbox
library(readr)  
source("mix_norm.r")
source("apply_and_simulate.R")

# createIndividualCharacteristics() and createIndividual() require at least PK-Sim v9.0.119
# so we need to use the portable version of PK-Sim
# Download the portable version and initialize the path to the folder here
initPKSim("C:/Users/alona.kryshchenko/Dropbox (CSUCI)/For Alan/SummerGrant/Bupropion with R-Toolbox/PK-Sim 9.0.144")



### Load the individuals into a list of objects holding their individual characteristics
population_data <- read_csv("bupropion baseline demographics - to share - converted to metric units.csv")

# Convert population_data into IndividualCharacteristics objects used in PK-Sim simulation
# Initialize list of individuals
number_of_individuals <- length(population_data$ID)
individuals <- vector(mode = "list", length = number_of_individuals)
for (i in 1:number_of_individuals){
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
  individual_chars <- createIndividualCharacteristics(species = Species$Human, population = their_population,gender = their_gender,
                                                      weight = population_data$`Weight (kg)`[i], weightUnit = "kg",
                                                      height = population_data$`Height (cm)`[i], heightUnit = "cm",
                                                      age = 40, ageUnit = "year(s)")
  # Add to list
  individuals[[i]] <- individual_chars
}

### Set scaling factors, which must be strictly positive
x_scaling_factor <- 2+0.5*rnorm(number_of_individuals)
y_scaling_factor <- 0.5+0.15*rnorm(number_of_individuals)
# Set up the vector as required
scaling_factors <- matrix(c(x_scaling_factor, y_scaling_factor),nrow = 2,ncol = number_of_individuals)

y1<-mix_norm(number_of_individuals,c(1,2),c(0.05,0.05),0.8)
y2<-mix_norm(number_of_individuals,c(0.7,1),c(0.05,0.05),0.5)
y3<-mix_norm(number_of_individuals,c(0.5,1),c(0.05,0.05),0.2)

### Read in the physiological parameter changes
# Note that the csv should have the headers [Path,Value,Base_unit]
# Also note that these physiological parameter changes override any physiological parameters defined in creating the individual
default_par_values <- read_csv("PKSim_pars_defaults.csv")
# I copy it over so that I still have the default parameter values available

par_values<-vector(mode = "list", length = number_of_individuals)

for(i in 1:number_of_individuals){
  par_values[[i]] <- default_par_values
# and modify the parameter values as follows, NOTE: Values are in base units
par_values[[i]]$Value[1] <- y1[i]
par_values[[i]]$Value[2] <- y2[i]
par_values[[i]]$Value[3] <- y3[i]
}

# Start a fresh simulation
simFilePath <- file.path(getwd(), paste0("PO SR 150 mg bupropion to human - Connarn et al 2017 - table.pkml"), fsep=.Platform$file.sep)
sim <- loadSimulation(simFilePath)

resultsTime<-vector(mode = "list", length = number_of_individuals)
resultsValues<-vector(mode = "list", length = number_of_individuals)

for (i in 1:number_of_individuals){
  simulationResults <- apply_and_simulate(simulation = sim, 
                                          individual_chars = individuals[[i]],
                                          scaling_factors = scaling_factors[,i],
                                          dissolution_data_path = "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|Fraction (dose)",
                                          par_values = par_values[[i]])
  resultsPath <- simulationResults$allQuantityPaths[[1]]
  resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
  resultsTime[[i]] <- resultsData$data$Time
  resultsValues[[i]] <- resultsData$data$`Organism|PeripheralVenousBlood|Bupropion human 1st order|Plasma (Peripheral Venous Blood)`
  
}
