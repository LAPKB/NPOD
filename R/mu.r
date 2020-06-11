mu <- function(theta, i) {
  library(ospsuite) # PK-Sim R toolbox

  source("apply_and_simulate.R")
  ### Set scaling factors, which must be strictly positive
  x_scaling_factor <- theta[1]
  y_scaling_factor <- theta[2]
  # Set up the vector as required
  scaling_factors <- c(x_scaling_factor, y_scaling_factor)
  par_values = list()


  ### Read in the physiological parameter changes
  # Note that the csv should have the headers [Path,Value,Base_unit]
  # Also note that these physiological parameter changes override any physiological parameters defined in creating the individual
  # default_par_values <- read_csv("PKSim_pars_defaults.csv")
  # I copy it over so that I still have the default parameter values available
  # par_values <- default_par_values
  par_values$Path[1] <- 'Organism|Plasma protein scale factor' #path to the parameters probably can be made as input
  par_values$Path[2] <- 'Organism|Skin|Peripheral blood flow fraction'
  par_values$Path[3] <- 'Liver and Intestinal CL|Reference concentration'

  # modify the parameter values as follows, NOTE: Values are in base units
  par_values$Value[1] <- theta[3]
  par_values$Value[2] <- theta[4]
  par_values$Value[3] <- theta[5]



  ### Apply the above changes and then simulate with individual #i
  #we need a fresh simulation
  simFilePath <- file.path(getwd(), paste0("PO SR 150 mg bupropion to human - Connarn et al 2017 - table.pkml"), fsep = .Platform$file.sep)
  sim <- loadSimulation(simFilePath)

  # Note that the dissolution data path is unique to the simulation but it will have following format:
  # "Applications|Administration Protocol|Formulation|Fraction (dose)"

  simulationResults <- apply_and_simulate(simulation = sim,
                                            individual_chars = individuals[[i]],
                                            scaling_factors = scaling_factors,
                                            dissolution_data_path = "Applications|PO 150 mg - human|SR PO 150 mg - FDA table|Fraction (dose)",
                                            par_values = par_values)
  simulationResults$allQuantityPaths
  resultsPath <- simulationResults$allQuantityPaths[[1]]
  resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
  resultsTime <- resultsData$data$Time
  resultsValues <- resultsData$data$`Organism|PeripheralVenousBlood|Bupropion human 1st order|Plasma (Peripheral Venous Blood)`

  return(resultsValues)
}

theta <- c(2, 0.5, 2, 1, 0.5)
i <- 1
y <- mu(theta, i)
plot(resultsTime[[1]], y)

