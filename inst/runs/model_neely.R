model <- function(theta,t){
    #this equation assumes a dose of 500mg, an infusion of 0.5h and a time vector like c(0.5,...)
    x05<-(500/(theta[1]*theta[2]))*(1-exp(-theta[1]*t[1]))
    val <- (x05/theta[2])*exp(-theta[1]*(t[-1]-0.5))
    return(c(x05,val))
}