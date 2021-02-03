
# library(memoise)
# library(readr)
# library(ggplot2)
# library(tibble)
# library(tidyr)
# library(neldermead)
# library(DiceDesign)
# library(readxl)

# source("Dopt.R")
# source("PSI_2.r")
# source("D.r")
# source("burke.R")
# source("prune.r")
# source("mu.r")
# source("prob.r")

#'@export
NPOD <- function(sim_file, pkdata_file, params, individuals, population_functions=c(), simulation_functions=c(), noise=F, model=NULL, c0=0.5, c1=0.1, c2=0, ncycles=Inf, theta_0=NULL,size_theta0=NULL, cache_folder_name=NULL) {

  #TODO: WIP global variables.
  sim_file <<- sim_file
  pkdata_file <<- pkdata_file
  params <- params
  individuals <- individuals
  simulation_functions <<- simulation_functions
  population_functions <<- population_functions
  ncycles <<- ncycles

  if(is.null(model)){
    simulator <<- "PKSIM"
  } else if(is.function(model)){
    simulator <<- "EQ"
    model <<- model
  } else {
    stop()
  }
  npod_cache <<- cachem::cache_mem()
  if(!is.null(cache_folder_name)){
    npod_cache <<- cachem::cache_disk(cache_folder_name)
  }
  cached_mu <<- memoise::memoise(multi_mu, cache=npod_cache)
  
  # sum2 <- memoise::memoise(function(x){
  #   print(x)
  #   x+2
  # }, cache = npod_cache)

  ## Error global variables
  n_err <<- 0
  err_log <<- rep(list(), 5)


  ### Load the individuals into a list of objects holding their individual characteristics
  # population_data <- read_csv(pop_file)

  # # Convert population_data into IndividualCharacteristics objects used in PK-Sim simulation
  # # Initialize list of individuals
  # number_of_individuals <- length(population_data$IndividualId)
  number_of_individuals <- length(individuals)
  pkdata <- read.csv(pkdata_file)
  
  time <- vector(mode = "list", length = number_of_individuals)
  y_old <- vector(mode = "list", length = number_of_individuals)
  sigma_old <- vector(mode = 'list', length = number_of_individuals)
  add_noise <- function(vector, sigma0, sigma1, sigma2){
    noise_add <- vector + rnorm(length(vector),0,sigma1)
    noise_mul <- vector * rnorm(length(vector),0,sigma2)
    return(sigma2 + noise_add + noise_mul)
  }
  #Extract this as parameters
  #Create another parameter to control the noise injection
  # c0<- 0.5
  # c1<- 0.1
  for (i in 1:number_of_individuals) {
    time[[i]] <- pkdata[, 1][pkdata[, i + 1] != 999] * 1.0 #* 60
    if(noise){
      y_old[[i]] <- add_noise(pkdata[, i + 1][pkdata[, i + 1] != 999], c2, c0, c1)
    } else {
      y_old[[i]] <- pkdata[, i + 1][pkdata[, i + 1] != 999]
    }
  }
  min_y <- min(unlist(y_old))
  for (i in 1:number_of_individuals) {
     sigma_old[[i]] <- (c0 * min_y + c1 * y_old[[i]] + c2)  
  }

  ind <- c(0)
  for (i in 1:number_of_individuals) { ind[i] = length(y_old[[i]]) != 0 }
  t <- time[as.logical(ind)]
  individuals <- individuals[as.logical(ind)]
  y <- y_old[as.logical(ind)]
  sigma <- sigma_old[as.logical(ind)]

  
  if(simulator=="PKSIM"){

  
    characteristics <- individuals

    indiv <- c()
    #transform all the ind char to ind (createIndividual)
    for (i in 1:length(characteristics)) {
      indiv[[i]] <- createIndividual(characteristics[[i]])
    }

    #Create population.csv
    col.names <- indiv[[1]]$distributedParameters$paths
    df <- as.data.frame(t(list(indiv[[1]]$distributedParameters$values)[[1]]))
    colnames(df) <- col.names
    for (ind in indiv[-(1:1)]) {
      aux <- as.data.frame(t(list(ind$distributedParameters$values)[[1]]))
      colnames(aux) <- col.names
      df <- rbind(df, aux)
    }
    ids <- data.frame(IndividualId = as.character(1:nrow(df)))
    df <- cbind(ids, df)
    write.csv(df, "test.csv", row.names = F)
  }
  # data <- c("POPDATA DEC_11\n", "#ID,EVID,TIME,DUR,DOSE,ADDL,II,INPUT,OUT\n")
  # for (i in length(y)) {
  #   for (j in length(y[[i]])) {
  #     data <- append(data, paste0(i, ",0,", t[[i]][[j]], ",0,.,.,.,1,", y[[i]][[j]]))
  #   }
  # }

  #ans <- initial_data(10)
  # y <- ans$y
  # t <- ans$t
  #sigma <- 25
  #c0(0.5)min(y)+c1(0.1)y_ij


  #true_theta <- ans$true_theta
  # a <- c(0.4, 180, 0.9)
  # b <- c(0.6, 220, 1)

  a <- params[[1]]
  b <- params[[2]]

  if(is.null(size_theta0)){
    size_theta0<- number_of_individuals
  }

  if(is.null(theta_0)){
    if (length(a) == 1) {
      theta_0 <- a + t(DiceDesign::runif.faure(size_theta0, 2)$design) * (b - a)
      # theta_0 <- a + t(runif.faure(500, 2)$design) * (b - a)
      theta_0 <- matrix(theta_0[1,], ncol = length(theta_0[1,]))
    } else {
      # theta_0 <- a + t(runif.faure(500, length(a))$design) * (b - a)
      theta_0 <- a + t(DiceDesign::runif.faure(size_theta0, length(a))$design) * (b - a)
    }
  # theta_0 <- matrix(c(0.146738996, 0.123293797, 3.170923764, 1.979605999, 0.094835319, 1.878534566, 0.108820894, 1.664082432, 2.761443638, 2.287738879, 1.897188772, 0.097035077, 0.082756603, 0.086893976, 2.156157535, 1.735379708, 3.847188377, 1.994727369, 1.531496725, 0.102180699, 2.880964641, 0.073854101, 1.27542639, 0.108035295, 2.363376581, 0.05887653, 1.651944217, 0.161762432, 1.850065187, 0.133951076, 2.373573413, 0.126209817, 0.085673368, 2.586016226, 0.173492127, 0.093789337, 0.113822807, 0.136675505, 1.593700177, 1.880331976, 1.212728388, 0.063362327, 0.116909087, 3.507619028, 2.200653328, 0.12056741, 1.749076709, 2.329988218, 0.110024431, 0.063060173, 0.134447024, 0.083234464, 3.296153545, 0.234944034, 0.134260968, 1.66618999, 0.117223628, 0.100846702, 2.616496816, 0.13987635, 1.966206983, 2.814812391, 1.516704054, 0.064970702, 0.096454675, 1.334296808, 3.098095951, 1.839557095, 0.089748738, 1.654506763, 0.089800378, 0.094357268, 0.124507898, 0.093186748, 0.101272563, 2.959986382, 0.083584167, 2.329923151, 1.4784192, 0.122879261, 1.450412959, 1.924533902, 0.089720051, 0.065944486, 0.074500112, 2.400025537, 1.623319666, 0.069790582, 0.104777191, 0.142460888, 0.078014023, 1.99329036, 0.135619964, 0.123392348, 1.327600402, 2.577439989, 0.160399272, 1.571292659, 2.203917519, 1.573226289, 0.066073962, 2.081737686, 0.084480469, 1.601815667, 2.573740598, 2.492078269, 2.032550129, 0.101991266, 0.098959164, 0.083511758, 1.598018991, 0.115760403, 2.441406605, 0.089998514, 1.684047026, 0.084991178, 1.723409162, 1.529295859, 0.118728247, 2.249698147, 0.122704478, 0.116931539, 1.992246518, 2.073065166, 0.108431, 1.232511342, 2.531792589, 1.08485316, 2.217082683, 2.136590466, 2.479191796, 0.168109114, 0.114148445, 0.146531275, 0.123083234, 0.09970523, 1.808488675, 1.273822394, 1.493883115, 0.146671258, 0.084866643, 2.372305577, 0.097529239, 0.122129378, 0.119630258, 1.741349377, 0.107479869, 0.093433982, 2.022695536, 0.075270096, 0.065651944, 0.121109817, 1.682462298, 2.35685709, 1.565529003, 3.592710077, 1.685880899, 1.84814825, 0.077200107, 3.17956809, 2.857667209, 0.126723019, 0.110541788, 3.017453382, 0.082663184, 2.14935597, 1.300242155, 2.597365515, 1.684000469, 1.944705938, 2.17734754, 1.919464244, 0.124326593, 0.08957695, 0.129333766, 0.094497707, 0.147036329, 1.319235805, 0.083336113, 0.093260834, 2.040276806, 1.575005782, 0.116084543, 0.057651493, 0.100180177, 0.135309086, 0.104367722, 1.965186011, 0.091088188, 2.525448909, 0.108618042, 0.109611659, 0.104979233, 1.783574019, 0.134260195, 1.688358128, 1.877636412, 1.822295191, 0.080955547, 2.294027773), nrow=1)
  # theta_0 <- matrix(c(1.571292659, 1.684000469, 1.897188772, 1.334296808, 3.507619028, 1.965186011, 1.654506763, 2.959986382, 1.651944217, 1.684047026, 1.994727369, 1.749076709, 1.944705938, 2.022695536, 1.878534566, 0.122129378, 0.104367722, 0.134260968, 0.063060173, 0.066073962, 0.116931539, 0.104979233, 0.116084543, 0.073854101, 0.097529239, 0.083336113, 0.116909087, 0.147036329, 0.126209817, 0.136675505), nrow = 1)
  }
  
  theta_0



  theta_F <- 10e-2
  theta_d <- 10e-4

  ####### TEST BLOCK ####### REMOVE BEFORE RUNNING
  # load("ans.Rdata")
  # t1 <- system.time({
  # m <- cached_mu(ans$theta, t)
  # })

  # # # return(m)
  # w<-c(0.122129378, 0.104367722, 0.134260968, 0.063060173, 0.066073962, 0.116931539, 0.104979233, 0.116084543, 0.073854101, 0.097529239, 0.083336113, 0.116909087, 0.147036329, 0.126209817, 0.136675505)
  # plot(NULL, xlim=c(0, 1500), ylim=c(0, 1.3))
  # for(l in 1:length(y)){
  #   if (l>15) {
  #   lines(t[[l]], y[[l]], col = "blue")
  #   points(t[[l]], y[[l]], col = "blue")
  #   text(t[[l]][[10]]+50, y[[l]][[10]],sprintf("%f",w[l-15]), cex=.5)
  #   }
    
  # }




  # plot(c(0, max(unlist(t))), c(0, max(unlist(y))), col = "white", xlab = "Time (m)", ylab = "Concentration")
  # #plot(c(0, 1.5), c(0, 1.5), type = "l", col = "black", xlab = "Observed", ylab = "Predicted")
  # total_wavg = c()
  # for (l in 1:length(y)) {
  #   lines(t[[l]], y[[l]], col = "#40687A")
  #   points(t[[l]], y[[l]], col = "#40687A")
  #   wavg = rep(0, length(m[[l, 1]])) #matrix(rep(list(), length(ans$w)), nrow = 22, ncol = 1)
  #   for (sup in 1:length(ans$w)) {
  #     wavg = wavg + m[[l, sup]] * ans$w[sup]
  #   }
  #   lines(t[[l]], wavg, col = "red")
  #   points(t[[l]], wavg, col = "red")
  #   #points(y[[l]], wavg, pch = 15)
  #   total_wavg[[l]] = wavg
  # }
  # data <- data.frame(x = unlist(y), y = unlist(total_wavg))
  # line <- lm(formula = y ~ x, data = data)
  # sub <- 3

  # # Libraries
  # library(ggplot2)

  # # # Create data
  # data1 <- data.frame(x = ans$theta[1,], y = ans$w)
  # data2 <- data.frame(x = ans$theta[2,], y = ans$w)
  # data3 <- data.frame(x = ans$theta[3,], y = ans$w)
  # # # data2 <- data.frame(x = c(1.571292659, 1.684000469, 1.897188772, 1.334296808, 3.507619028, 1.965186011, 1.654506763, 2.959986382, 1.651944217, 1.684047026, 1.994727369, 1.749076709, 1.944705938, 2.022695536, 1.878534566, 0.122129378, 0.104367722, 0.134260968, 0.063060173, 0.066073962, 0.116931539, 0.104979233, 0.116084543, 0.073854101, 0.097529239, 0.083336113, 0.116909087, 0.147036329, 0.126209817, 0.136675505), y = rep(1/30,30))


  # # # # Plot
  # p1 <- ggplot(data1, aes(x = x, y = y)) +
  # geom_point() +
  # geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  # ggtitle("Marginals - theta[1] (Scalefx)") +
  # xlab("Liver Enzyme|Reference concentration") +
  # ylab("Weight")

  # p2 <- ggplot(data2, aes(x = x, y = y)) +
  # geom_point() +
  # geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  # ggtitle("Marginals - theta[2] (Scalefy)") +
  # xlab("Liver Enzyme|Reference concentration") +
  # ylab("Weight")

  # p3 <- ggplot(data3, aes(x = x, y = y)) +
  # geom_point() +
  # geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  # ggtitle("Marginals - theta[3] (Liver intestinal CL ref)") +
  # xlab("Liver Enzyme|Reference concentration") +
  # ylab("Weight")

  # gridExtra::grid.arrange(p1, p2, p3, ncol = 3)

  # p2 <- ggplot(data2, aes(x = x, y = y)) +
  # geom_point() +
  # geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  # ggtitle("Marginals - Optimal density") +
  # xlab("Liver Enzyme|Reference concentration") +
  # ylab("Weight")

  # gridExtra::grid.arrange(p1, p2, ncol = 2)
  # # plot(t[[sub]], m[[sub, 6]], col = "white")

  # n <- 21
  # s <- 2.346697
  # error <- qnorm(0.975) * s / sqrt(n)


  # # psi <- multi_prob(y, t, theta_0, sigma, individuals, m)
  # # ans <- burke(psi)
  ##### END TEST BLOCK #####
  # error <- c()
  # for (i in 1:number_of_individuals) {

  #   sub <- c()
  #   for (j in 1:length(theta[1, ])) {
  #     sub <- append(sub, (m[[i, j]] - y[[i]]) ^ 2)
  #   }
  #   error[[i]] <- max(sub)
  # }




  ans <- Dopt(y, t, theta_0, theta_F, theta_d, sigma, a, b)

  count <- ans$count
  theta <- ans$theta
  w <- ans$w
  logLikelihood <- ans$logLikelihood
  return(ans)
}

posterior <- function(res){
  psi <- ans$PSI #psi[j,i] jth subject, ith support point
  

  nsub <- nrow(psi)
  nspp <- ncol(psi)
  w <- matrix(ans$w, nrow=nspp)
  post <- matrix(0, nsub, nspp)

  py <- psi %*% w
  
  for(j in 1:nsub){
    for(i in 1:nspp){
      post[j,i] = psi[j,i] * w[i] / py[j]
    }
  }
  return(post)

}

posterior_observations <- function(ans, posterior){
  pkdata <- read.csv(pkdata_file)
  number_of_individuals <- ncol(pkdata) - 1
  time <- vector(mode = "list", length = number_of_individuals)
  y_old <- vector(mode = "list", length = number_of_individuals)


  for (i in 1:number_of_individuals) {
    time[[i]] <- pkdata[, 1][pkdata[, i + 1] != 999] * 1.0 #* 60
    y_old[[i]] <- pkdata[, i + 1][pkdata[, i + 1] != 999]
  }
  ind <- c(0)
  for (i in 1:number_of_individuals) { ind[i] = length(y_old[[i]]) != 0 }
  t <- time[as.logical(ind)]
  y <- y_old[as.logical(ind)]

  simulator<<-"PKSIM"
  sim_file<<-sim_file
  simulation_functions<<-c()
  population_functions<<-population_functions
  t1 <- system.time({
  m <- cached_mu(ans$theta, t)
  })

  plot(c(0, max(unlist(y))), c(0, max(unlist(m))), type = "l", col = "black", xlab = "Observed", ylab = "posterior")
  total_wavg = c()
  for (l in 1:length(y)) {
    wavg = rep(0, length(nrow(m))) 
    for (sup in 1:length(ncol(m))) {
      wavg = wavg + m[[l, sup]] * posterior[l, sup]
    }
    points(y[[l]], wavg, pch = 15)
    total_wavg[[l]] = wavg
  }
  data <- data.frame(x = unlist(y), y = unlist(total_wavg))
  line <- lm(formula = y ~ x, data = data)

}
#'@export
correlation <- function(res, individuals, params = c('Age', 'Weight', 'Height')){
  #TODO: make this general
  #P(x/Y) = post
  #Gji<-c(xi,paramsj,prob[j,i]/nrow(prob[j,i]))
  #g[j=1,i=1] -> (5, 40, 80, 180, 0.3) j -> subject & i -> spp
  #g[j=2,i=1] -> (5, 4,3 72, 167, 0.28)
  #g[j=1,i=2] -> (8, 40, 80, 180, 0.36) 
  #cov[i,j] = sum((xi- mx)(yj - my) * post[j,i]/nrow(post))
  post<-posterior(res)
  nrvs<-nrow(res$theta)

  # cyp_info<-matrix(c(1,1,1,1,1,1,2,1,2,1,3,4,1,3,4,3,1,1,1,1,1,5,5,5,4,5,4,1,1,6,5,1,1,2,2,2,2,2,2,3,4,1,2,2,1,2,2,2,2,2,2,2,2,2,2,2,2,1,2,1,2,2,2,2), ncol=2)

  pop<-matrix(rep(0,(length(params)+nrvs+1+0)*(length(individuals)*ncol(res$theta))), ncol=(length(params)+nrvs+1+0))
  for(i in 1:(length(individuals)*ncol(res$theta))){
    sub <- ((i - 1) %% length(individuals)) + 1
    spp <- ((i - 1) %/% length(individuals)) + 1

    pop[i,1]<-post[sub,spp]
    for(n in 1:nrvs){ #inserting the rvs
      pop[i,n+1]<-res$theta[n,spp]
    }
    
    for(n in 1:length(params)){ #inserting the subject parameters
      pop[i,n+nrvs+1]<-individuals[[sub]][[tolower(params[n])]]$'value'
    }

    # for(n in 1:2){ #inserting the subject parameters
    #   pop[i,n+nrvs+1+length(params)]<-cyp_info[sub,n]
    # }


  }
  g<-as.data.frame(pop[,2:(length(params)+nrvs+1+0)])
  names(g) <- c(sprintf("theta%s",seq(1:nrvs)), params)#, c("CYP2B6", "CYP2C19"))
  weighted_corr <- cov.wt(g, wt = pop[,1], cor = TRUE)
  corr_matrix <- weighted_corr$cor
  return(corr_matrix)
}

obs_pred_plot <- function(ans,pkdata_file, sim_file, population_functions){
  ans<-readRDS("bup2_ans_fixed.rds")
  theta<-ans$theta
  w<-ans$w
  pkdata <- read.csv(pkdata_file)
  number_of_individuals <- ncol(pkdata) - 1
  time <- vector(mode = "list", length = number_of_individuals)
  y_old <- vector(mode = "list", length = number_of_individuals)


  for (i in 1:number_of_individuals) {
    time[[i]] <- pkdata[, 1][pkdata[, i + 1] != 999] * 1.0 #* 60
    y_old[[i]] <- pkdata[, i + 1][pkdata[, i + 1] != 999]
  }
  ind <- c(0)
  for (i in 1:number_of_individuals) { ind[i] = length(y_old[[i]]) != 0 }
  t <- time[as.logical(ind)]
  y <- y_old[as.logical(ind)]

  simulator<<-"PKSIM"
  sim_file<<-sim_file
  simulation_functions<<-c()
  population_functions<<-population_functions
  t1 <- system.time({
  m <- cached_mu(theta, t)
  })
  # plot(c(0, max(unlist(t))), c(0, max(unlist(y))), col = "white", xlab = "Time (m)", ylab = "Concentration")
  plot(c(0, max(unlist(y))), c(0, max(unlist(m))), type = "l", col = "black", xlab = "Observed", ylab = "Predicted")
  total_wavg = c()
  for (l in 1:length(y)) {
    # lines(t[[l]], y[[l]], col = "#40687A")
    # points(t[[l]], y[[l]], col = "#40687A")
    wavg = rep(0, length(m[[l, 1]])) #matrix(rep(list(), length(ans$w)), nrow = 22, ncol = 1)
    for (sup in 1:length(w)) {
      wavg = wavg + m[[l, sup]] * w[sup]
    }
    # lines(t[[l]], wavg, col = "red")
    # points(t[[l]], wavg, col = "red")
    points(y[[l]], wavg, pch = 15)
    total_wavg[[l]] = wavg
  }
  data <- data.frame(x = unlist(y), y = unlist(total_wavg))
  line <- lm(formula = y ~ x, data = data)
}

spaghetti_plot_w <-function(){
  plot(c(0, max(unlist(t))), c(0, pmax(max(unlist(y)), max(unlist(m)) )), col = "white", xlab = "Time (m)", ylab = "Concentration")
  #plot(c(0, 1.5), c(0, 1.5), type = "l", col = "black", xlab = "Observed", ylab = "Predicted")
  total_wavg = c()
  for (l in 1:length(y)) {
    lines(t[[l]], y[[l]], col = "#40687A")
    points(t[[l]], y[[l]], col = "#40687A")
    wavg = rep(0, length(m[[l, 1]])) #matrix(rep(list(), length(ans$w)), nrow = 22, ncol = 1)
    for (sup in 1:length(w)) {
      wavg = wavg + m[[l, sup]] * w[sup]
    }
    lines(t[[l]], wavg, col = "red")
    points(t[[l]], wavg, col = "red")
    #points(y[[l]], wavg, pch = 15)
    total_wavg[[l]] = wavg
  }
  # data <- data.frame(x = unlist(y), y = unlist(total_wavg))
  # line <- lm(formula = y ~ x, data = data)
}

spaghetti_plot <-function(){
  library(ggplot2)
  library(purrr)
  library(ggdark)
  # plot(c(0, max(unlist(t))), c(0, pmax(max(unlist(y)), max(unlist(m)) )), col = "white", xlab = "Time (m)", ylab = "Concentration")
  # #plot(c(0, 1.5), c(0, 1.5), type = "l", col = "black", xlab = "Observed", ylab = "Predicted")
  # total_wavg = c()
  # for (l in 1:length(y)) {
    
  #   lines(t[[l]], y[[l]], col = "#40687A")
  #   points(t[[l]], y[[l]], col = "#40687A")
  #   wavg = rep(0, length(m[[l, 1]])) #matrix(rep(list(), length(ans$w)), nrow = 22, ncol = 1)
  #   for (sup in 1:length(w)) {
  #     lines(t[[l]], m[[l, sup]], col = "red")
  #     points(t[[l]],m[[l, sup]], col = "red")
  #   }
    
  # }
  #This seems to be too nested and causing a stack overflow.
  maxw<-max(w)
  minw<-min(w)
  normw <- (w-minw)/(maxw-minw)
  reduce(1:length(y), function(x,i){ 
    reduce(1:length(w), function(y, j){
        y + geom_line(data = data.frame(t=t[[i]], y=m[[i, j]], normw=normw[j]), aes(x=t, y=y), alpha = normw[j])#, color=normw), size = 0.6, alpha = normw[j])
    },
    .init=x + geom_line(data = data.frame(t=t[[i]], y=y[[i]]), aes(x=t, y=y, color="red"), size=1)
    )  
  },
  .init=ggplot()
  ) + dark_theme_linedraw() +
  theme(panel.grid.major = element_line(color = "grey50")) +
  theme(panel.grid.minor = element_line(color = "grey50"))#+ scale_color_gradient(low="blue", high="red")
   #+ scale_color_gradientn(colours = rainbow(length(w))) +
  

  # data <- data.frame(x = unlist(y), y = unlist(total_wavg))
  # line <- lm(formula = y ~ x, data = data)
}

marginals_plot <- function(ans){
  library(ggplot2)

  # # Create data
  # data1 <- data.frame(theta = original, y = rep(1/length(original), length(original)))
  # data2 <- data.frame(theta= sol, y = w)
  data2 <- data.frame(theta= twins_sol, y = twins_w)
  # data3 <- data.frame(x = ans$theta[3,], y = ans$w)
  # initials <- data.frame(x=c(1), y=c(0.2))
  # # data2 <- data.frame(x = c(1.571292659, 1.684000469, 1.897188772, 1.334296808, 3.507619028, 1.965186011, 1.654506763, 2.959986382, 1.651944217, 1.684047026, 1.994727369, 1.749076709, 1.944705938, 2.022695536, 1.878534566, 0.122129378, 0.104367722, 0.134260968, 0.063060173, 0.066073962, 0.116931539, 0.104979233, 0.116084543, 0.073854101, 0.097529239, 0.083336113, 0.116909087, 0.147036329, 0.126209817, 0.136675505), y = rep(1/30,30))


  # # # Plot
  p1 <- ggplot(data1,aes(x=theta,y=y)) +
  geom_col(alpha = 0.3, width=0.0001, color="red", fill="red") +
  geom_col(data=data2, alpha = 0.3, width=0.0001, color="lightblue", fill="lightblue") +
  dark_theme_linedraw() +
  geom_vline(xintercept=c(weighted.mean(original, rep(1/length(original), length(original))),weighted.mean(sol, w)),col=c("red","lightblue"), linetype=2)+
  theme(panel.grid= element_line(color = "grey30")) +
  ggtitle("Marginals - Liver Enzyme|Reference concentration") +
  xlab("Liver Enzyme|Reference concentration") +
  ylab("Weight") + coord_trans(x="log10")

  p2 <- ggplot(data2,aes(x=x,y=y)) +

  geom_col(alpha = 0.5, width=0.1) +
  dark_theme_linedraw() +
  geom_vline(xintercept=c(1,weighted.mean(ans$theta[2, ], ans$w)),col=c("red","blue"))+
  theme(panel.grid= element_line(color = "grey30")) +
  ggtitle("Marginals - theta[2] (Scalefy)") +
  xlab("theta[2] (Scalefy)") +
  ylab("Weight")

  p3 <- ggplot(data3,aes(x=x,y=y)) +

  geom_col(alpha = 0.5, width=0.1) +
  dark_theme_linedraw() +
  geom_vline(xintercept=c(1,weighted.mean(ans$theta[3, ], ans$w)),col=c("red","blue"))+
  theme(panel.grid= element_line(color = "grey30")) +
  ggtitle("Marginals - theta[3] (theta[3] (Liver Enzyme|Reference concentration))") +
  xlab("theta[1] (theta[3] (Liver Enzyme|Reference concentration))") +
  ylab("Weight")


  gridExtra::grid.arrange(p1, p2, p3, ncol = 3) 
}

# P <- PSI_2(y, t, theta, sigma)
# PYL <- P * w

# Dfun <- function(.theta_parameter) { D(.theta_parameter, y, t, sigma, PYL) }
# K <- seq(from = 1.5, to = 3.5, by = 0.1)
# V <- seq(from = 0.3, to = 0.5, by = 0.05)
# Z <- matrix(rep(0, length(K) * length(V)), nrow = length(K))
# for (i in 1:length(K)) {
#   for (j in 1:length(V)) {
#     Z[i, j] = Dfun(c(K[i], V[i]))
#   }
# }

#Plots

# wmean = 0

# for (i in 1:ncol(theta)){
#   wmean = wmean + (theta[2,i]/theta[3,i])*w[i]
# }

# plot(c(0, max(unlist(t))), c(0, max(unlist(m))), col = "white", xlab = "Time (m)", ylab = "Concentration")
# color<-c("red", "blue", "black")
# for(i in 1:nrow(m)){
#   for(sup in 1:3){
#     lines(t[[i]], m[[i, sup]], col = color[sup])
#     points(t[[i]],m[[i, sup]], col = color[sup])
#   }
# }
