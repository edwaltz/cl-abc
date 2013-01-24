# Untils for "banana" shaped distribution example.
rbanana <- function(par, ndim, b, scale=1) {
  # Simulate from the standard banana distribution.
  #
  # Args:
  #   par - unknown parameters for likelihood;
  #   ndim - number of dimensions for summary statistics;
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(par)
  if (p>ndim) {
    stop("The dim of sumstat must be higher than that of par.")
  }
  n <- nrow(par)
  data <- matrix(0, nrow=n, ncol=ndim)  # simulated data
  data[, 1] <- par[, 1]+rnorm(n, sd=10)
  data[, 2] <- par[, 2]-b*(data[, 1]-par[, 1])^2+100*b+rnorm(n)
  if (ndim>2) {
    data[, -c(1, 2)] <- mvrnorm(n, rep(0, ndim-2), diag(ndim-2))
  }
  if (p>2) {
    data[, 3:p]  <- par[, 3:p] + data[, 3:p]
  }
  return(data)
}

rbanana.rot <- function(par, ndim, b, scale=1) {
  # Simulate from the rotated banana distribution.
  #
  # Args:
  #   par - unknown parameters for likelihood;
  #   ndim - number of dimensions for summary statistics;
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(par)
  if (p>ndim) {
    stop("The dim of summstat must be higher than that of par.")
  }
  n <- nrow(par)
  data <- matrix(0, nrow=n, ncol=ndim)  # simulated data
  rotate.mat <- matrix(c(cos(pi/4), -sin(pi/4), cos(pi/4), sin(pi/4)), nrow=2)
  data[, 1] <- rnorm(n, sd=10)
  data[, 2] <- -b*data[, 1]^2+100*b+rnorm(n)
  data[, 1:2] <- data[, 1:2]%*%rotate.mat+par[, 1:2]
  if (ndim>2) {
    data[, -c(1, 2)] <- mvrnorm(n, rep(0, ndim-2), diag(ndim-2))
  }
  if (p>2) {
    data[, 3:p]  <- par[, 3:p] + data[, 3:p]
  }
  return(data)
}