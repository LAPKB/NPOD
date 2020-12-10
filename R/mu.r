.rep_population <- function(lines, times) {
    row <- c(lines[1], rep(lines[-1], times))
    res <- c(lines[1])
    for (i in 1:(length(lines[-1]) * times)) {
      res <- c(res, toString(c(sprintf('"%i"', i), strsplit(row[(i + 1)], ",")[[1]][-1])))
    }
    print(length(res))
    return(res)
  }

multi_mu <- function(theta, t) {
  print("Multi Mu")

  n_ind <- length(t)
  if(simulator=="PKSIM"){ 
    sim <- loadSimulation(sim_file)
    if (length(simulation_functions) > 0) {
      file.copy("test.csv", "population.csv")
    } else {
      lines = read_lines(file("test.csv"))
      lines = .rep_population(lines, length(theta[1,]))
      write_lines(lines, "population.csv")
    }
    population <- loadPopulation("population.csv")
    pop_size <- length(population$allIndividualIds)
    #Set the times
    sim$outputSchema$clear() #first clear default
    sim$outputSchema$addTimePoints(unique(unlist(t))) #add times
  }
  
  time_points <- length(unique(unlist(t))) #We might be able to get this number from the result
  if (!(0 %in% unique(unlist(t)))) {
    time_points = time_points + 1
  }



  m <- matrix(rep(list(), n_ind * length(theta[1,])), nrow = n_ind, ncol = length(theta[1,]))
  if(simulator == "EQ"){
    for (k in 1:length(theta[1, ])) {
      for (sub in 1:n_ind) {
        # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
        # Filter the non-needed concentrations (not all 't' are needed)
        m[[sub, k]] <- model(theta[,k],t[[sub]])
        #  resData$data[resData$data[1] == sub][-(1:(2 * time_points))][resData$data[resData$data[1] == sub][((time_points + 1):(2 * time_points))] %in% t[[sub]]]

      }
    }
  }else if (length(simulation_functions) > 0) {
    t1 <- system.time({
      for (k in 1:length(theta[1, ])) {
        #this line sets the ith theta[3] to all the subjects in the population
        #population$setParameterValues("Liver and Intestinal CL|Reference concentration", rep(theta[3, k], pop_size))
        for (popfun in population_functions) {
          popfun(population, theta, k)
        }
        for (simfun in simulation_functions) {
          simfun(sim, theta, k)
        }
        # setParameterValuesByPath('Liver and Intestinal CL|Reference concentration',
        #                          theta[3, k],
        #                          simulation = sim)

        # # This line scales the dissolution profile inside the model using the two scaling factors
        # .scale_dissolution_profile(sim, c(theta[1, k], theta[2, k]))

        # Run the simulation
        res <- runSimulation(simulation = sim, population = population)

        #Extract the results
        resPath <- res$allQuantityPaths[[1]]
        resData <- getOutputValues(res, quantitiesOrPaths = resPath)
        for (sub in 1:n_ind) {
          # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
          # Filter the non-needed concentrations (not all 't' are needed)

          m[[sub, k]] <- resData$data[resData$data[1] == sub][-(1:(2 * time_points))][resData$data[resData$data[1] == sub][((time_points + 1):(2 * time_points))] %in% t[[sub]]]

        }

        # Restore the dissolution profile
        # TODO: improve this
        sim <- loadSimulation(sim_file)
        sim$outputSchema$clear() #first clear default
        sim$outputSchema$addTimePoints(unlist(t)) #add times

      }
    })
  } else {
    t1 <- system.time({
      # for (k in 1:length(theta)) {
      # pop_theta = c()
      # for (i in 1:length(theta[1, ])) {
      #   pop_theta <- append(pop_theta, rep(theta[1, i], 100))
      # }
      #this line sets the ith theta[3] to all the subjects in the population
      # population$setParameterValues("Liver Enzyme|Reference concentration", pop_theta)
      for (popfun in population_functions) {
        popfun(population, theta, 0)
      }
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

      for (sub in 1:(n_ind * length(theta[1, ]))) {
        # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
        # Filter the non-needed concentrations (not all 't' are needed)
        j <- ((sub - 1) %% n_ind) + 1
        k <- ((sub - 1) %/% n_ind) + 1
        #resData$data[resData$data[1] == sub] is of size 3*time_points the first third is the id, the second third are the times and the third third is the data
        m[[j, k]] <- resData$data[resData$data[1] == sub][-(1:(2 * time_points))][resData$data[resData$data[1] == sub][((time_points + 1):(2 * time_points))] %in% t[[j]]]
        #[resData$data[resData$data[1] == sub][(12:22)] %in% t[[sub]]]
      }

      # Restore the dissolution profile
      # TODO: improve this
      # sim <- loadSimulation("sim.pkml")
      # sim$outputSchema$clear() #first clear default
      # sim$outputSchema$addTimePoints(unlist(t)) #add times

      # }
    })
  }
  return(m)
}

cached_mu <- memoise(multi_mu)

# mu <- function(theta, t, individual_chars) {
#   #Ok, I have a problem, theta[2] and theta[3] are parameters that modify the simulation
#   #so... in order to simulate all the posibilities, it seems that I need to simulate K times
#   #Where K <- length(theta[1,])


#   sim <- loadSimulation("sim.pkml")
#   individual <- createIndividual(individual_chars)

#   setParameterValuesByPath(individual$distributedParameters$paths,
#                                  individual$distributedParameters$values,
#                                  simulation = sim)
#   setParameterValuesByPath("Liver Enzyme|Reference concentration",
#                                  theta,
#                                  simulation = sim)
#   #If we know that all the parameters belongs to the population
#   #We can just replicate the population and do one big simulation.

#   # lines = read_lines(file("test.csv"))
#   # lines = c(lines[1], rep(lines[-1], length(theta)))
#   # write_lines(lines, "population.csv")
#   # pop <- read.csv("population.csv")
#   # pop$IndividualId <- as.character((1:(length(lines) - 1)))
#   # write.csv(pop, "population.csv")


#   # population <- loadPopulation("population.csv")
#   # pop_size <- length(population$allIndividualIds)

#   #Set the times
#   sim$outputSchema$clear() #first clear default
#   sim$outputSchema$addTimePoints(unlist(t)) #add times

#   simulationResults <- runSimulation(simulation = sim)

#   resultsPath <- simulationResults$allQuantityPaths[[1]]
#   resultsData <- getOutputValues(simulationResults, quantitiesOrPaths = resultsPath)
#   resultsTime <- resultsData$data$Time
#   resultsValues <- resultsData$data$"Organism|PeripheralVenousBlood|Drug A|Plasma (Peripheral Venous Blood)"
#   #ind = c(2:length(resultsValues))
#   return(resultsValues)

#   # m <- matrix(rep(list(), pop_size), nrow = 100, ncol = 10)
#   # t1 <- system.time({
#   #   # for (k in 1:length(theta)) {
#   #   pop_theta = c()
#   #   for (i in 1:length(theta)) {
#   #     pop_theta <- append(pop_theta, rep(theta[i], 100))
#   #   }
#   #   #this line sets the ith theta[3] to all the subjects in the population
#   #   population$setParameterValues("Liver Enzyme|Reference concentration", pop_theta)
#   #   # setParameterValuesByPath('Liver Enzyme|Reference concentration',
#   #   #                              theta[k],
#   #   #                              simulation = sim)

#   #   # # This line scales the dissolution profile inside the model using the two scaling factors
#   #   # .scale_dissolution_profile(sim, c(theta[1, k], theta[2, k]))

#   #   # Run the simulation
#   #   res <- runSimulation(simulation = sim, population = population)

#   #   #Extract the results
#   #   resPath <- res$allQuantityPaths[[1]]
#   #   resData <- getOutputValues(res, quantitiesOrPaths = resPath)

#   #   for (sub in 1:1000) {
#   #     # Return format m[i,l] where i->1..Nsub and l->1..size(theta)
#   #     # Filter the non-needed concentrations (not all 't' are needed)
#   #     j <- ((sub - 1) %% 100) + 1
#   #     k <- ((sub - 1) %/% 100) + 1
#   #     m[[j, k]] <- resData$data[resData$data[1] == sub][-(1:22)] #[resData$data[resData$data[1] == sub][(12:22)] %in% t[[sub]]]
#   #   }

#   #   # Restore the dissolution profile
#   #   # TODO: improve this
#   #   # sim <- loadSimulation("sim.pkml")
#   #   # sim$outputSchema$clear() #first clear default
#   #   # sim$outputSchema$addTimePoints(unlist(t)) #add times

#   #   # }
#   # })

# }

