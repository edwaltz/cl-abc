# Composite Likelihood ABC

simPriorKernel <- function(data, num) {
  # Simulate from prior estimated by the standard kernel density estimatior
  # 
  # Args:
  #   data - points to be estimated
  #   num - number of simulation points of the prior
  #
  # Returns:
  #   A list of points simulated from the estimated prior and corresponding bandwidth matrix.
  n <- nrow(data)
  p <- ncol(data)
  H <- var(data)*(4/((p+2)*n))^(2/(p+4))
  pos <- sample(n, num, TRUE, rep(1/n, n))
  sim <- data[pos, ]+mvrnorm(num, rep(0, p), H)
  return(list(prior=sim, bandwidth=H))
}

shiftPoints <- function(data) {
  # Shift data for shrinkage kernel estimation
  k <- 1  # scale of the bandwidth
  n <- nrow(data)
  p <- ncol(data)
  h <- k*(4/((p+2)*n))^(1/(p + 4))
  a <- sqrt(1-h^2)
  mu <- colMeans(data)
  res <- t(a*t(data)+(1-a)*mu)  # shifted samples
  return(list(shifted=res, bandwidth=h))
}

simPriorShrinkage <- function(data, num) {
  # Simulate from prior estimated by the shrinkage kernel density estimatior.
  # 
  # Args:
  #   data - points to be estimated;
  #   num - number of simulation points of the prior.
  #
  # Returns:
  #   A list of points simulated from the estimated prior and corresponding bandwidth matrix.
  n <- nrow(data)
  p <- ncol(data)
  res <- shiftPoints(data)
  H <- var(data)*res$bandwidth^2  # bandwidth matrix
  pos <- sample(n, num, TRUE, rep(1/n, n))
  sim <- data[pos, ]+mvrnorm(num, rep(0, p), H)
  return(list(prior=sim, bandwidth=H))
}

clabcStep <- function(obs, margin, par, data, h) {
  # Composite likelihood ABC for single step.
  #
  # Args:
  #   obs - observed summary statistic;
  #   margin - 1: x*y; 2: (x,y).
  #   par - unknown parameters for likelihood;
  #   data - simulated data;
  #   h - threshold.
  #
  # Returns:
  #   The abc estimation of the distribution of parameters.
  if (margin==0) {
    target <- obs
    sim <- data
    abcRes <- abc(target, par, sim, h, "rejection")
    return(abcRes$unadj.values)
  } else if (margin==1) {  # x*y
    target <- obs[1]*obs[2]
    sim <- data[, 1]*data[, 2]
    abcRes <- abc(target, par, sim, h, "rejection")
    return(abcRes$unadj.values)
  } else if (margin==2) {  # (x,y) or x only
    target <- obs
    sim <- data
    abcRes <- abc(target, par, sim, h, "rejection")
    return(abcRes$unadj.values)
  }
}

clabc <- function(obs, prior, sim, n, h, method, type) {
  # Composite Likelihood ABC.
  #
  # Args:
  #   obs - observed summary statistic;
  #   prior - prior of the parameters;
  #   sim - simulation for the component of the composite likelihood;
  #   n - number of simulation;
  #   h - threshold;
  #   method - approach to estimate the prior;
  #   type - 1: x*y; 2: (x,y).
  #
  # Returns:
  #   A list of the abc estimation of parameters par, simulated prior of the 
  #   estimated distribution from the par and the corresponding bandwidth matrix.
  if (method=="kernel") {
    simPrior <- simPriorKernel
  } else if (method=="shrinkage"){
    simPrior <- simPriorShrinkage
  }
  res.par <- clabcStep(obs, type, prior, sim, h)  # abc
  res.sim <- simPrior(res.par, n)  # estimate the prior
  return(list(par=res.par, prior=res.sim$prior, 
              bandwidth=res.sim$bandwidth, type=method))
}

adjMargin <- function(join, margin) {
  adj <- join
  for (ind in 1:ncol(join)) {
    adj[order(join[, ind]) ,ind] <- sort(margin[, ind])
  }
  return(adj)
}