set_Sim_Times <- function(simulation,times){
  simulation$outputSchema$clear() #first clear default
  simulation$outputSchema$addTimePoints(as.numeric(times)) #add times
  
}