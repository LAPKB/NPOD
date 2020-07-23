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
  ind = c(1:length(resultsValues))
  return(resultsValues)
}

# theta <- c(2, 0.5, 2, 1, 0.5)
# t <- c(60,120,180,240)
# y <- mu(theta, t, individuals[[1]])
# plot(c(0,t), y)
