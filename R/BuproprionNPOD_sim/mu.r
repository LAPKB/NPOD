# mu <- function(theta, t, individual) {
#   print("mu")

#   # theta - vector of parameter values (the dim is the same as number of model parameters)
#   # t - vertor of measurement times
#   # individual - Individual Characteristics of the subject

#   library(ospsuite) # PK-Sim R toolbox
#   source("set_Sim_Times.r")
#   source("apply_and_simulate.R")
#   ### Set scaling factors, which must be strictly positive
#   x_scaling_factor <- theta[1]
#   y_scaling_factor <- theta[2]
#   # Set up the vector as required
#   scaling_factors <- c(x_scaling_factor, y_scaling_factor)
#   par_values = list()

#   #par_values <- vector(mode = "list", length = number_of_individuals)

#   ### Read in the physiological parameter changes
#   # Note that the csv should have the headers [Path,Value,Base_unit]
#   # Also note that these physiological parameter changes override any physiological parameters defined in creating the individual
#   # default_par_values <- read_csv("PKSim_pars_defaults.csv")
#   # I copy it over so that I still have the default parameter values available
#   # par_values <- default_par_values
#   #par_values$Path[1] <- 'Organism|Plasma protein scale factor' #path to the parameters probably can be made as input
#   #par_values$Path[2] <- 'Organism|Skin|Peripheral blood flow fraction'
#   par_values$Path[1] <- 'Liver and Intestinal CL|Reference concentration'

#   # modify the parameter values as follows, NOTE: Values are in base units
#   par_values$Value[1] <- theta[3]
#   #par_values$Value[2] <- theta[4]
#   #par_values$Value[3] <- theta[5]

#   ### Apply the above changes and then simulate with individual #i
#   #we need a fresh simulation
#   simFilePath <- file.path(getwd(), paste0("PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml"), fsep = .Platform$file.sep)
#   sim <- loadSimulation(simFilePath)

#   # Note that the dissolution data path is unique to the simulation but it will have following format:
#   # "Applications|Administration Protocol|Formulation|Fraction (dose)"

#   set_Sim_Times(sim, t)

#   # ## Load individual
#   # individual <- createIndividual(individual)

#   # ## Apply the distributed parameters of the individual to the simulation
#   # setParameterValuesByPath(individual$distributedParameters$paths,
#   #                                individual$distributedParameters$values,
#   #                                simulation = sim)

#   # simulationResults <- runSimulation(simulation = sim)
#   simulationResults <- apply_and_simulate(simulation = sim,
#                                             individual_chars = individual,
#                                             scaling_factors = scaling_factors,
#                                             dissolution_data_path = "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|Fraction (dose)",
#                                             par_values = par_values)
#   simulationResults$allQuantityPaths
#   resultsPath <- simulationResults$allQuantityPaths[[1]]
#   resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
#   resultsTime <- resultsData$data$Time
#   resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Bupropion human 1st order|Plasma (Peripheral Venous Blood)`
#   #ind = c(2:length(resultsValues))
#   return(resultsValues[2:length(resultsValues)])
# }

# theta <- c(2, 0.5, 2, 1, 0.5)
# t <- c(60,120,180,240)
# y <- mu(theta, t, individuals[[1]])
# plot(c(0,t), y)

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



multi_mu <- function(theta, t, individuals) {
  print("Multi Mu")
  #Ok, I have a problem, theta[2] and theta[3] are parameters that modify the simulation
  #so... in order to simulate all the posibilities, it seems that I need to simulate K times
  #Where K <- length(theta[1,])


  sim <- loadSimulation("sim.pkml")

  #If we know that all the parameters belongs to the population
  #We can just replicate the population and do one big simulation.

  lines = read_lines(file("test.csv"))
  lines = c(lines[1], rep(lines[-1], length(theta)))
  write_lines(lines, "population.csv")
  # pop <- read.csv("population.csv")
  # pop$IndividualId <- as.character((1:(length(lines) - 1)))
  # write.csv(pop, "population.csv")


  population <- loadPopulation("population.csv")
  pop_size <- length(population$allIndividualIds)
  time_points <- length(unique(t)[[1]])

  #Set the times
  sim$outputSchema$clear() #first clear default
  sim$outputSchema$addTimePoints(unlist(t)) #add times

  m <- matrix(rep(list(), pop_size), nrow = 100, ncol = length(theta))
  t1 <- system.time({
    # for (k in 1:length(theta)) {
    pop_theta = c()
    for (i in 1:length(theta)) {
      pop_theta <- append(pop_theta, rep(theta[i], 100))
    }
    #this line sets the ith theta[3] to all the subjects in the population
    population$setParameterValues("Liver Enzyme|Reference concentration", pop_theta)
    # setParameterValuesByPath('Liver Enzyme|Reference concentration',
    #                              theta[k],
    #                              simulation = sim)

    # # This line scales the dissolution profile inside the model using the two scaling factors
    # .scale_dissolution_profile(sim, c(theta[1, k], theta[2, k]))

    # Run the simulation
    res <- runSimulation(simulation = sim, population = population)

    #Extract the results
    resPath <- res$allQuantityPaths[[1]]
    resData <- getOutputValues(res, quantitiesOrPaths = resPath)

    for (sub in 1:(100 * length(theta))) {
      # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
      # Filter the non-needed concentrations (not all 't' are needed)
      j <- ((sub - 1) %% 100) + 1
      k <- ((sub - 1) %/% 100) + 1
      m[[j, k]] <- resData$data[resData$data[1] == sub][-(1:22)] #[resData$data[resData$data[1] == sub][(12:22)] %in% t[[sub]]]
    }

    # Restore the dissolution profile
    # TODO: improve this
    # sim <- loadSimulation("sim.pkml")
    # sim$outputSchema$clear() #first clear default
    # sim$outputSchema$addTimePoints(unlist(t)) #add times

    # }
  })

  return(m)
}

mu <- function(theta, t, individual_chars) {
  #Ok, I have a problem, theta[2] and theta[3] are parameters that modify the simulation
  #so... in order to simulate all the posibilities, it seems that I need to simulate K times
  #Where K <- length(theta[1,])


  sim <- loadSimulation("sim.pkml")
  individual <- createIndividual(individual_chars)

  setParameterValuesByPath(individual$distributedParameters$paths,
                                 individual$distributedParameters$values,
                                 simulation = sim)
  setParameterValuesByPath("Liver Enzyme|Reference concentration",
                                 theta,
                                 simulation = sim)
  #If we know that all the parameters belongs to the population
  #We can just replicate the population and do one big simulation.

  # lines = read_lines(file("test.csv"))
  # lines = c(lines[1], rep(lines[-1], length(theta)))
  # write_lines(lines, "population.csv")
  # pop <- read.csv("population.csv")
  # pop$IndividualId <- as.character((1:(length(lines) - 1)))
  # write.csv(pop, "population.csv")


  # population <- loadPopulation("population.csv")
  # pop_size <- length(population$allIndividualIds)

  #Set the times
  sim$outputSchema$clear() #first clear default
  sim$outputSchema$addTimePoints(unlist(t)) #add times

  simulationResults <- runSimulation(simulation = sim)

  resultsPath <- simulationResults$allQuantityPaths[[1]]
  resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
  resultsTime <- resultsData$data$Time
  resultsValues <- resultsData$data$"Organism|PeripheralVenousBlood|Drug A|Plasma (Peripheral Venous Blood)"
  #ind = c(2:length(resultsValues))
  return(resultsValues)

  # m <- matrix(rep(list(), pop_size), nrow = 100, ncol = 10)
  # t1 <- system.time({
  #   # for (k in 1:length(theta)) {
  #   pop_theta = c()
  #   for (i in 1:length(theta)) {
  #     pop_theta <- append(pop_theta, rep(theta[i], 100))
  #   }
  #   #this line sets the ith theta[3] to all the subjects in the population
  #   population$setParameterValues("Liver Enzyme|Reference concentration", pop_theta)
  #   # setParameterValuesByPath('Liver Enzyme|Reference concentration',
  #   #                              theta[k],
  #   #                              simulation = sim)

  #   # # This line scales the dissolution profile inside the model using the two scaling factors
  #   # .scale_dissolution_profile(sim, c(theta[1, k], theta[2, k]))

  #   # Run the simulation
  #   res <- runSimulation(simulation = sim, population = population)

  #   #Extract the results
  #   resPath <- res$allQuantityPaths[[1]]
  #   resData <- getOutputValues(res, quantitiesOrPaths = resPath)

  #   for (sub in 1:1000) {
  #     # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
  #     # Filter the non-needed concentrations (not all 't' are needed)
  #     j <- ((sub - 1) %% 100) + 1
  #     k <- ((sub - 1) %/% 100) + 1
  #     m[[j, k]] <- resData$data[resData$data[1] == sub][-(1:22)] #[resData$data[resData$data[1] == sub][(12:22)] %in% t[[sub]]]
  #   }

  #   # Restore the dissolution profile
  #   # TODO: improve this
  #   # sim <- loadSimulation("sim.pkml")
  #   # sim$outputSchema$clear() #first clear default
  #   # sim$outputSchema$addTimePoints(unlist(t)) #add times

  #   # }
  # })

}

