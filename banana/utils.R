# Untils for "banana" shaped distribution example.
library(abc)
library(MASS)

clabc.step <- function(num, p, obs, h, type, b) {
  # ABC for 1 obs.
  #
  # Args:
  #   num - number of samples form the prior of the parameters;
  #   p - dimension of the parameter;
  #   obs - observed summary statistic;
  #   h - threshold;
  #   type - full & pair;
  #   b - bananacity;
  #
  # Return:
  #   The sample points.
  
  pmin <- -100
  pmax <- 100
  
  obs.val <- as.vector(obs)
  d <- length(obs.val)  # dimension for summary statistic
  n <- num*h  # number of parameter points
  ret <- list()
  ret$prior <- matrix(runif(num*p, pmin, pmax), nrow=num)
  rotate.mat <- matrix(c(cos(pi/4), -sin(pi/4), cos(pi/4), sin(pi/4)), nrow=2)
  
  op <- options(warn=(-1))  # suppress warnings
#   ptm.final <- proc.time()  # time record
  if (type=="full") {
    # 1.simulation of likelihood function.
    sim <- matrix(0, nrow=num, ncol=d)  # simulated data
    sim[, 1] <- rnorm(num, sd=10)
    sim[, 2] <- -b*sim[, 1]^2+100*b+rnorm(num)
    sim[, 1:2] <- sim[, 1:2]%*%rotate.mat+ret$prior[, 1:2]
    if (d>2) {
      sim[, -c(1, 2)] <- mvrnorm(num, rep(0, d-2), diag(d-2))
      if (p>2) {
        sim[, 3:p]  <- ret$prior[, 3:p] + sim[, 3:p]
      }
    }
    # 2.abc.
    ret$par <- abc(obs.val, ret$prior, sim, h, "rejection")$unadj.values
    # 3.simulation of the prior distribution.
    ret$prior <- ret$par[sample(n, num, TRUE), ]+mvrnorm(num, rep(0, p), var(ret$par)*(4/((p+2)*n))^(2/(p+4)))
    gc()
  } else if (type=="pair") {
    order <- combn(d, 2)  # order of the composite likelihood
    sim <- matrix(0, nrow=num, ncol=d)  # simulated data
    for (ind in 1:(d*(d-1)/2)) {
      # 1.simulation of likelihood function.
      sim[, 1] <- rnorm(num, sd=10)
      sim[, 2] <- -b*sim[, 1]^2+100*b+rnorm(num)
      sim[, 1:2] <- sim[, 1:2]%*%rotate.mat+ret$prior[, 1:2]
      if (d>2) {
        sim[, -c(1, 2)] <- mvrnorm(num, rep(0, d-2), diag(d-2))
        if (p>2) {
          sim[, 3:p]  <- ret$prior[, 3:p] + sim[, 3:p]
        }
      }
      # 2.abc.
      ret$par <- abc(obs.val[order[, ind]], ret$prior, sim[, order[, ind]], h, "rejection")$unadj.values
      # 3.simulation of the prior distribution.
      ret$prior <- ret$par[sample(n, num, TRUE), ]+mvrnorm(num, rep(0, p), var(ret$par)*(4/((p+2)*n))^(2/(p+4)))
      gc()
    }
  }
  
  # Finalize the running.
#   cost.final <- proc.time()-ptm.final
#   print(cost.final["elapsed"])
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

dbanana <- function(y, b=B) {
  # The true joint posterior.
  x <- matrix(c(cos(pi/4), -sin(pi/4), cos(pi/4), sin(pi/4)), nrow=2) %*% y
  dnorm(x[1], sd=10) * dnorm(x[2]- b * x[1]^2 + 100 * b, sd=1)
}