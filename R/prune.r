prune <- function(theta, theta_plus, theta_d, a, b) {
  #a = c(1,0.1)
  #b = c(4, 1)

  #R parameters are passed by value, not need to copy

  dist = Inf

  for (this_col in 1:ncol(theta)) {
    new_dist = sum(abs(theta_plus - matrix(theta[, this_col])) / matrix(b - a))
    dist = min(dist, new_dist)
  }
  up = sign(min(theta_plus - matrix(a)))
  down = sign(min(matrix(b) - theta_plus))
  if (dist > theta_d && up == 1 && down == 1) {
    theta = cbind(theta, theta_plus)
  }

  return(theta)

}