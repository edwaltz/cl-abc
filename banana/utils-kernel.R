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

  options(op)
  return(ret)
}

get.corr <- function(b, p, num) {
  # Marginal adjustment for composite likelihood ABC.
  #
  # Args:
  #   b - bananacity;
  #   p - dimension of the summary statistics;
  #   num - number of samples form the prior of the parameters.
  #
  # Returns:
  #   The adjusted joint distribution.
  tol <- .0001
  n <- num * tol
  
  pmin <- -100
  pmax <- 100
  lim.x <- c(pmin, pmax)
  lim.y <- c(pmin, pmax)
  
  # run the algorithm.
  # d.par & d.summ should have the same length.
  d.par <- 2
  d.summ <- p
  
  obs <- matrix(0, nrow=1, ncol=d.summ, byrow=TRUE)
  margin <- clabc.step(num, 2, obs[1:2], tol, "full", b)$par  # get margin
  join <- clabc.step(num, d.par, obs, tol, "pair", b)$par[, 1:2]
  adj <- join
  # margin adjustment
  for (ind in 1:2) {
    adj[order(join[, ind]) ,ind] <- sort(margin[, ind])
  }
  ret.corr <- c(cor(join)[1, 2], cor(adj)[1, 2])
  return(list(cor=ret.corr, unadj=join, adjust=adj))
}

run.corr <- function(total, b, p, num) {
  # Marginal adjustment for composite likelihood ABC.
  #
  # Args:
  #   total - loop times;
  #   b - bananacity;
  #   p - dimension of the summary statistics;
  #   num - number of samples form the prior of the parameters.
  #
  # Returns:
  #   corrrlation for unadj. & adj.
  
  corr <- matrix(0, nrow=total, ncol=2)
  for (ind in 1:total) {
    corr[ind, ] <- get.corr(b, p, num)$cor
  }
  return(corr)
}

dbanana <- function(y, b=B) {
  # The true joint posterior.
  x <- matrix(c(cos(pi / 4), -sin(pi / 4), cos(pi / 4), sin(pi / 4)), nrow=2) %*% y
  dnorm(x[1], sd=10) * dnorm(x[2]- b * x[1]^2 + 100 * b, sd=1)
}