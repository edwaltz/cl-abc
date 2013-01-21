# Composite Likelihood ABC
library(abc)

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
  res.sim <- data[pos, ]+mvrnorm(num, rep(0, d), H)
  return(res.sim)
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

  par <- abc(obs, prior, sim, h, "rejection")$unadj.values
  res.sim <- sim.prior(par, nrow(prior))
  return(list(par=par, prior=res.sim))
}

clabc.step <- function(prior, obs, h, rlik, type="paire") {
  # ABC for 1 obs.
  #
  # Args:
  #   prior - prior of the parameters;
  #   obs - observed summary statistic;
  #   h - threshold;
  #   rlik - function generate random points from the specific likelihood;
  #   type - full & pair.
  #
  # Return:
  #   The sample points.
  
  op <- options(warn=(-1))  # suppress warnings
  ptm.final <- proc.time()  # time record
  temp <- prior
  res <- list()
  
  if (type=="full") {
    sim <- rlik(temp)
    res <- clabc(obs, temp, sim, h)
    temp <- res$prior
  } else if (type=="pair") {
    order <- combn(length(obs), 2)  # order of the composite likelihood
    for (ind in 1:(p*(p-1)/2)) {
      sim <- rlik(temp)
      res <- clabc(obs[order[, ind]], temp, sim[, order[, ind]], h)
      temp <- res$prior
    }
  }

  # Finalize the running.
  cost.final <- proc.time()-ptm.final
  print(cost.final["elapsed"])
  options(op)
  return(list(par=res$par, prior=res$prior))
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