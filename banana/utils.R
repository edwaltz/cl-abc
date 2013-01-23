# Untils for "banana" shaped distribution example.
rbanana <- function(par, b=B, scale=1) {
  # Simulate from the standard banana distribution.
  #
  # Args:
  #   par - unknown parameters for likelihood;
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(par)
  n <- nrow(par)
  data <- matrix(0, nrow=n, ncol=p)  # simulated data
  data[, 1] <- par[, 1]+rnorm(n, sd=10)
  data[, 2] <- par[, 2]-b*(data[, 1]-par[, 1])^2+100*b+rnorm(n)
  data[, -c(1, 2)] <- par[, -c(1, 2)]+matrix(rnorm(n*(p-2)), nrow=n, ncol=p-2)
  return(data)
}

rbanana.rot <- function(par, b=B, scale=1) {
  # Simulate from the rotated banana distribution.
  #
  # Args:
  #   par - unknown parameters for likelihood;
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(par)
  n <- nrow(par)
  data <- matrix(0, nrow=n, ncol=p)  # simulated data
  rotate.mat <- matrix(c(cos(pi/4), -sin(pi/4), cos(pi/4), sin(pi/4)), nrow=2)
  data[, 1] <- rnorm(n, sd=10)
  data[, 2] <- -b*data[, 1]^2+100*b+rnorm(n)
  data[, 1:2] <- data[, 1:2]%*%rotate.mat+par[, 1:2]
  data[, -c(1, 2)] <- par[, -c(1, 2)]+matrix(rnorm(n*(p-2)), nrow=n, ncol=p-2)
  return(data)
}