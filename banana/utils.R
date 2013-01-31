# Untils for "banana" shaped distribution example.
library(abc)
library(MASS)

pmin <- -100
pmax <- 100

clabc.step <- function(num, p, obs, h, rlik, type="paire", ...) {
  # ABC for 1 obs.
  #
  # Args:
  #   num - number of samples form the prior of the parameters;
  #   p - dimension of the parameter;
  #   obs - observed summary statistic;
  #   h - threshold;
  #   rlik - function generate random points from the specific likelihood;
  #   type - full & pair.
  #
  # Return:
  #   The sample points.
  
  op <- options(warn=(-1))  # suppress warnings
  ptm.final <- proc.time()  # time record
  obs.val <- as.vector(obs)
  d <- length(obs.val)  # dimension for summary statistic
  n <- num*h  # number of parameter points
  
  ret <- list()
  ret$prior <- matrix(runif(num*p, pmin, pmax), nrow=num)
  
  
  if (type=="full") {
    sim <- matrix(0, nrow=num, ncol=d)  # simulated data
    rlik(...)
    ret$par <- abc(obs.val, ret$prior, sim, h, "rejection")$unadj.values
    ret$prior <- ret$par[sample(n, num, TRUE), ]+
      mvrnorm(num, rep(0, p), var(ret$par)*(4/((p+2)*n))^(2/(p+4)))
    gc()
  } else if (type=="pair") {
    order <- combn(d, 2)  # order of the composite likelihood
    for (ind in 1:(d*(d-1)/2)) {
      rlik(...)
      ret$par <- abc(obs.val[order[, ind]], ret$prior, sim[, order[, ind]], h, 
                     "rejection")$unadj.values
      ret$prior <- ret$par[sample(n, num, TRUE), ]+
        mvrnorm(num, rep(0, p), var(ret$par)*(4/((p+2)*n))^(2/(p+4)))
      gc()
    }
  }
  
  # Finalize the running.
  cost.final <- proc.time()-ptm.final
  print(cost.final["elapsed"])
  options(op)
  return(ret)
}

rbanana <- function(b, scale=1) {
  # Simulate from the standard banana distribution.
  #
  # Args:
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(ret$prior)
  ndim <- ncol(sim)  # number of dimensions for summary statistics
  if (p>ndim) {
    stop("The dim of sumstat must be higher than that of par.")
  }
  n <- nrow(ret$prior)
  sim[, 1] <- ret$prior[, 1]+rnorm(n, sd=10)
  sim[, 2] <- ret$prior[, 2]-b*(sim[, 1]-ret$prior[, 1])^2+100*b+rnorm(n)
  if (ndim>2) {
    sim[, -c(1, 2)] <- mvrnorm(n, rep(0, ndim-2), diag(ndim-2))
  }
  if (p>2) {
    sim[, 3:p]  <- ret$prior[, 3:p] + sim[, 3:p]
  }
}

rbanana.rot <- function(b, scale=1) {
  # Simulate from the rotated banana distribution.
  #
  # Args:
  #   b - bananacity;
  #   scale - the scale of the marginal distribution.
  #
  # Returns:
  #   Simulation for the banana likelihood.
  
  p <- ncol(ret$prior)
  ndim <- ncol(sim)  # number of dimensions for summary statistics
  if (p>ndim) {
    stop("The dim of summstat must be higher than that of par.")
  }
  n <- nrow(ret$prior)
  sim <- matrix(0, nrow=n, ncol=ndim)  # simulated data
  rotate.mat <- matrix(c(cos(pi/4), -sin(pi/4), cos(pi/4), sin(pi/4)), nrow=2)
  sim[, 1] <- rnorm(n, sd=10)
  sim[, 2] <- -b*sim[, 1]^2+100*b+rnorm(n)
  sim[, 1:2] <- sim[, 1:2]%*%rotate.mat+ret$prior[, 1:2]
  if (ndim>2) {
    sim[, -c(1, 2)] <- mvrnorm(n, rep(0, ndim-2), diag(ndim-2))
  }
  if (p>2) {
    sim[, 3:p]  <- ret$prior[, 3:p] + sim[, 3:p]
  }
}

adj.margin <- function(join, margin) {
  # Marginal adjustment for composite likelihood ABC.
  #
  # Args:
  #   join - the unadjusted joint distribution;
  #   margin - the marginal distribution.
  #
  # Returns:
  #   The adjusted joint distribution.
  
  adj <- join
  for (ind in 1:ncol(join)) {
    adj[order(join[, ind]) ,ind] <- sort(margin[, ind])
  }
  return(adj)
}