# Composite Likelihood ABC
library(abc)
library(MASS)

pmin <- -100
pmax <- 100

sim.prior <- function(data, num) {
  # Simulate from prior estimated by the standard kernel density estimatior
  # 
  # Args:
  #   data - points to be estimated
  #   num - number of simulation points of the prior
  #
  # Returns:
  #   A list of points simulated from the estimated prior and corresponding bandwidth matrix.

  n <- nrow(data)
  d <- ncol(data)
  H <- var(data)*(4/((d+2)*n))^(2/(d+4))
  pos <- sample(n, num, TRUE, rep(1/n, n))
  ret.sim <- data[pos, ]+mvrnorm(num, rep(0, d), H)
  return(ret.sim)
}

clabc <- function(obs, prior, sim, h) {
  # Composite Likelihood ABC.
  #
  # Args:
  #   obs - observed summary statistic;
  #   prior - prior of the parameters;
  #   sim - simulation for the component of the composite likelihood;
  #   h - threshold;
  #
  # Returns:
  #   A list of the abc estimation of parameters par, simulated prior of the 
  #   estimated distribution from the par and the corresponding bandwidth matrix.

  ret <- list()
  ret$par <- abc(obs, prior, sim, h, "rejection")$unadj.values
  ret$sim <- sim.prior(ret$par, nrow(prior))
  return(ret)
}

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
  prior <- matrix(runif(num*p, pmin, pmax), nrow=num)
  ret <- list()
  
  if (type=="full") {
    sim <- rlik(prior, ndim=d, ...)
    ret <- clabc(obs, prior, sim, h)
    prior <- ret$prior
  } else if (type=="pair") {
    order <- combn(d, 2)  # order of the composite likelihood
    for (ind in 1:(d*(d-1)/2)) {
      sim <- rlik(prior, ndim=d, ...)
      ret <- clabc(obs.val[order[, ind]], prior, sim[, order[, ind]], h)
      prior <- ret$prior
    }
  }

  # Finalize the running.
  cost.final <- proc.time()-ptm.final
  print(cost.final["elapsed"])
  options(op)
  return(ret)
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