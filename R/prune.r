prune <- function(theta, theta_plus, theta_d){
    a = c(1,0.1)
    b = c(4, 1)

    #R parameters are passed by value, not need to copy

    dist = Inf

    for( col in 1:nrow(theta)){
        new_dist = sum(abs(theta_plus - theta[,col]/(b-a)))
        dist = min(dist, new_dist)
    }
    up = sign(pmin(theta_plus-a))
    down = sign(pmin(b-theta_plus))
    if(dist>theta_d && up > -1  && down > -1 ){
        theta = cbind(theta, theta_plus)
    }

    return(theta)

}