mu <- function(theta, t, individual) {
  print("mu")

  # theta - vector of parameter values (the dim is the same as number of model parameters)
  # t - vertor of measurement times
  # individual - Individual Characteristics of the subject

  library(ospsuite) # PK-Sim R toolbox
  source("set_Sim_Times.r")
  source("apply_and_simulate.R")
  ### Set scaling factors, which must be strictly positive
  x_scaling_factor <- theta[1]
  y_scaling_factor <- theta[2]
  # Set up the vector as required
  scaling_factors <- c(x_scaling_factor, y_scaling_factor)
  par_values = list()

  #par_values <- vector(mode = "list", length = number_of_individuals)

  ### Read in the physiological parameter changes
  # Note that the csv should have the headers [Path,Value,Base_unit]
  # Also note that these physiological parameter changes override any physiological parameters defined in creating the individual
  # default_par_values <- read_csv("PKSim_pars_defaults.csv")
  # I copy it over so that I still have the default parameter values available
  # par_values <- default_par_values
  #par_values$Path[1] <- 'Organism|Plasma protein scale factor' #path to the parameters probably can be made as input
  #par_values$Path[2] <- 'Organism|Skin|Peripheral blood flow fraction'
  par_values$Path[1] <- 'Liver and Intestinal CL|Reference concentration'

  # modify the parameter values as follows, NOTE: Values are in base units
  par_values$Value[1] <- theta[3]
  #par_values$Value[2] <- theta[4]
  #par_values$Value[3] <- theta[5]

  ### Apply the above changes and then simulate with individual #i
  #we need a fresh simulation
  simFilePath <- file.path(getwd(), paste0("PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml"), fsep = .Platform$file.sep)
  sim <- loadSimulation(simFilePath)

  # Note that the dissolution data path is unique to the simulation but it will have following format:
  # "Applications|Administration Protocol|Formulation|Fraction (dose)"

  set_Sim_Times(sim, t)

  simulationResults <- apply_and_simulate(simulation = sim,
                                            individual_chars = individual,
                                            scaling_factors = scaling_factors,
                                            dissolution_data_path = "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|Fraction (dose)",
                                            par_values = par_values)
  simulationResults$allQuantityPaths
  resultsPath <- simulationResults$allQuantityPaths[[1]]
  resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
  resultsTime <- resultsData$data$Time
  resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Bupropion human 1st order|Plasma (Peripheral Venous Blood)`
  #ind = c(2:length(resultsValues))
  return(resultsValues[2:length(resultsValues)])
}

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
  #Ok, I have a problem, theta[2] and theta[3] are parameters that modify the simulation
  #so... in order to simulate all the posibilities, it seems that I need to simulate K times
  #Where K <- length(theta[1,])


  sim <- loadSimulation("PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml")
  population <- loadPopulation("population.csv")
  pop_size <- length(population$allIndividualIds)

  #Set the times
  sim$outputSchema$clear() #first clear default
  sim$outputSchema$addTimePoints(unlist(t)) #add times

  res <- c()
  t1 <- system.time({
    for (k in 1:length(theta[1, ])) {
      #this line sets the ith theta[1] to all the subjects in the population
      population$setParameterValues("Liver and Intestinal CL|Reference concentration", rep(theta[1, k], pop_size))

      # This line scales the dissolution profile inside the model using the two scaling factors
      old_diss_prof <- .scale_dissolution_profile(sim, c(theta[2, k], theta[3, k]))

      # Run the simulation
      res[[k]] <- runSimulation(simulation = sim, population = population)

      # Restore the dissolution profile
      # TODO: improve this
      sim <- loadSimulation("PO SR 150 mg bupropion to human - Connarn et al 2017 - table - June 2.pkml")
      sim$outputSchema$clear() #first clear default
      sim$outputSchema$addTimePoints(unlist(t)) #add times

    }
  })

  #TODO:
  #* Filter the non-needed concentrations (not all 't' are needed)
  #* Extract the concentrations from the res list and create the m matrix needed in multi_prob

}

