prune <- function(theta, theta_plus, theta_d, a, b){

    #R parameters are passed by value, not need to copy

    dist = Inf

    for( this_col in 1:ncol(theta)){
        new_dist = sum(abs(theta_plus - theta[,this_col])/(b-a))
        dist = min(dist, new_dist)
        #print(dist)
    }
    up = sign(pmin(theta_plus-a))
    down = sign(pmin(b-theta_plus))
    if(dist>theta_d && up > -1  && down > -1 ){
        theta = cbind(theta, theta_plus)
    }

    return(theta)

}