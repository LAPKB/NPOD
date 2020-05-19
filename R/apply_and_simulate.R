#####
#
# File Name:    simulation()
# Author:       Moriah Pellowe
#
# This script will simulate a given individual, given scaling factors, and given physiological parameter changes
# 1) the PK-Sim simulation
# 2) the demographic data of the individual_chars,
# 3) the scaling factors applied to the dissolution profile, [x_scaling_factor, y_scaling_factor] and
# 4) the dissolution data path with format "Applications|Administration Protocol|Formulation|Fraction (dose)"
# 4) the PK-Sim parameters to be updated as an array with rows [path, units, new_value] as a result of read_csv {library(readr)
#
#####

apply_and_simulate <- function(simulation, 
                               individual_chars = NULL, 
                               scaling_factors = c(1,1), dissolution_data_path = NULL, 
                               par_values= NULL){
    
    ## Apply individual to simulation
    if (!is.null(individual_chars)){
        ## Load individual
        individual <- createIndividual(individual_chars)
        
        ## Apply the distributed parameters of the individual to the simulation
        setParameterValuesByPath(individual$distributedParameters$paths, 
                                 individual$distributedParameters$values,
                                 simulation = simulation)
        # test that it worked
        number_of_distributed_parameters <- length(individual$distributedParameters$paths)
        for (i in 1:number_of_distributed_parameters) {
            test_parameter <- getParameter(individual$distributedParameters$paths[i], simulation)
            value_after <- as.numeric(test_parameter$value)
            if (individual$distributedParameters$values[i]!=value_after){
                print(i)
                print("Unsuccessful in setting parameter.")
                return(NULL)
            }
        }
    }
    
    
    ## Apply scaling factors to dissolution profile
    if (all(scaling_factors>0) && (!identical(scaling_factors, c(1,1)))) {
        dissolution_data_parameter <- getParameter(dissolution_data_path, simulation)
        dissolution_data_formula <- dissolution_data_parameter$formula
        dissolution_data_all_points <- dissolution_data_formula$allPoints
        
        numPoints <- length(dissolution_data_all_points)
        
        # Initialize vectors to hold values of table
        times <- rep(0,numPoints)
        fractions <- rep(0, numPoints)
        
        # Read in points
        for (i in 1:numPoints){
            times[i] <- dissolution_data_all_points[[i]]$x
            fractions[i] <- dissolution_data_all_points[[i]]$y
        }
        
        points <- array(c(times, fractions), dim=c(numPoints, 2))
        # print(points)
        
        # Apply scaling factor to table
        new_x <- points[,1]*scaling_factors[1]
        new_y <- points[,2]*scaling_factors[2]
        
        # print(new_x)
        # print(new_y)
        
        dissolution_data_formula$setPoints(new_x, new_y)    
    } else {
        if (!all(scaling_factors>0)) {
            print("Negative scaling factor is not allowed.")
            return(NULL)
        }
    }

    
    
    ## Apply PK-Sim parameters as prescribed in list of lists
    if (!is.null(par_values)){
        setParameterValuesByPath(par_values$Path,
                                 par_values$Value,
                                 simulation = simulation)
    }
    
    results <- runSimulation(simulation = simulation)
    return(results)
}