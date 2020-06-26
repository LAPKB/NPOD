set_Sim_Times <- function(simulation,timePoints){
  
  #function to simulate only at specific times
  
  clearOutputIntervals(simulation) #first clear default
  OutputSchema$addTimePoints(timePoints)
  #for(i in times){ #now add new "intervals" for each sample time
    
    #addOutputInterval(simulation = simulation, startTime = i, endTime = i+1, resolution = 1)
  #}
}