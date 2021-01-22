rm(list = ls())
setwd("R")
source("NPOD.R")
library(ospsuite) # PK-Sim R toolbox
  population_file <- "data/bim_uni_pop.csv"
  pkdata_file <- "data/data_bim_uni.csv"
  sim_file <- "data/sim.pkml"
  params <- vector(mode = "list", length = 2)
  params[[1]] <- c(0.01) # min
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
                                                      weight = population_data$`Weight (kg)`[i], weightUnit = "kg",
                                                      height = population_data$`Height (m)`[i], heightUnit = "m",
                                                      age = population_data$`Age (yr)`[i], ageUnit = "year(s)")
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
original <-c(0.146738996, 0.123293797, 3.170923764, 1.979605999, 0.094835319, 1.878534566, 0.108820894, 1.664082432, 2.761443638, 2.287738879,
                    1.897188772, 0.097035077, 0.082756603, 0.086893976, 2.156157535, 1.735379708, 3.847188377, 1.994727369, 1.531496725, 0.102180699,
                    2.880964641, 0.073854101, 1.275426390, 0.108035295, 2.363376581, 0.058876530, 1.651944217, 0.161762432, 1.850065187, 0.133951076,
                    2.373573413, 0.126209817, 0.085673368, 2.586016226, 0.173492127, 0.093789337, 0.113822807, 0.136675505, 1.593700177, 1.880331976,
                    1.212728388, 0.063362327, 0.116909087, 3.507619028, 2.200653328, 0.120567410, 1.749076709, 2.329988218, 0.110024431, 0.063060173,
                    0.134447024, 0.083234464, 3.296153545, 0.234944034, 0.134260968, 1.666189990, 0.117223628, 0.100846702, 2.616496816, 0.139876350, 
                    1.966206983, 2.814812391, 1.516704054, 0.064970702, 0.096454675, 1.334296808, 3.098095951, 1.839557095, 0.089748738, 1.654506763, 
                    0.089800378, 0.094357268, 0.124507898, 0.093186748, 0.101272563, 2.959986382, 0.083584167, 2.329923151, 1.478419200, 0.122879261, 
                    1.450412959, 1.924533902, 0.089720051, 0.065944486, 0.074500112, 2.400025537, 1.623319666, 0.069790582, 0.104777191, 0.142460888,
                    0.078014023, 1.993290360, 0.135619964, 0.123392348, 1.327600402, 2.577439989, 0.160399272, 1.571292659, 2.203917519, 1.573226289, 
                    0.066073962, 2.081737686, 0.084480469, 1.601815667, 2.573740598, 2.492078269, 2.032550129, 0.101991266, 0.098959164, 0.083511758, 
                    1.598018991, 0.115760403, 2.441406605, 0.089998514, 1.684047026, 0.084991178, 1.723409162, 1.529295859, 0.118728247, 2.249698147,
                    0.122704478, 0.116931539, 1.992246518, 2.073065166, 0.108431000, 1.232511342, 2.531792589, 1.084853160, 2.217082683, 2.136590466, 
                    2.479191796, 0.168109114, 0.114148445, 0.146531275, 0.123083234, 0.099705230, 1.808488675, 1.273822394, 1.493883115, 0.146671258, 
                    0.084866643, 2.372305577, 0.097529239, 0.122129378, 0.119630258, 1.741349377, 0.107479869, 0.093433982, 2.022695536, 0.075270096, 
                    0.065651944, 0.121109817, 1.682462298, 2.356857090, 1.565529003, 3.592710077, 1.685880899, 1.848148250, 0.077200107, 3.179568090, 
                    2.857667209, 0.126723019, 0.110541788, 3.017453382, 0.082663184, 2.149355970, 1.300242155, 2.597365515, 1.684000469, 1.944705938, 
                    2.177347540, 1.919464244, 0.124326593, 0.089576950, 0.129333766, 0.094497707, 0.147036329, 1.319235805, 0.083336113, 0.093260834, 
                    2.040276806, 1.575005782, 0.116084543, 0.057651493, 0.100180177, 0.135309086, 0.104367722, 1.965186011, 0.091088188, 2.525448909, 
                    0.108618042, 0.109611659, 0.104979233, 1.783574019, 0.134260195, 1.688358128, 1.877636412, 1.822295191, 0.080955547, 2.294027773)
  sol <- c(8.75125, 5.980148, 0.881125, 1.859468, 0.01712812, 3.638867, 1.804616, 5.96146, 4.376912, 2.994895, 0.7049, 0.9930357)
  w <- c(0.01598739, 0.01826494, 0.01329514, 0.18437462, 0.32117277, 0.04730599, 0.09605593, 0.01622064, 0.04145555, 0.09786423, 0.01650442, 0.13149838)
  twins_sol <- c(1.88125, 1.569375, 1.335469, 0.1269531, 1.686328, 0.1074609, 0.1464453, 0.09771484, 1.65709, 0.1366992, 0.117207, 3.508848, 2.963066,
                 1.964092, 1.652217, 0.1318262, 1.749678, 0.07334961, 1.9446, 2.022568, 1.895869, 0.1025879, 0.06360352, 0.1220801, 1.99333, 0.0830957)
  twins_w <- c(0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.06666667, 0.03333319, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.10000000,
               0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333333, 0.03333347, 
               0.06666667, 0.03333333, 0.03333333, 0.03333333)

t1<- system.time({
 ans <- NPOD(sim_file, pkdata_file, params, individuals, population_functions, c0=0.5, c1=0.1)
})


