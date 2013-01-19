# Untils for "banana" shaped distribution example.
library(MASS)
library(abc)
source("cl_abc.R")

B <- .01  # bananacity
# 1 obs - x:(-30,30) & y:(-15,15);
# 2 obs - x:(-40,40) & y:(-15,25);
# 5 obs - x:(-50,50) & y:(-15,35);
# 10 obs - x:(-75,75) & y:(-20,75).

x.lim <- c(-20, 60)
y.lim <- c(-20, 60)

# funtions
simLik <- function(par, n, scale=1, b=B) {
  # Simulate from likelihood function
  #
  # Args:
  #   par - unknown parameters for likelihood
  #   n - number of simulation
  #   scale - the scale of the marginal distribution
  #   b - bananacity
  #
  # Returns:
  #   Simulation for the banana likelihood.
  p <- ncol(par)
  data <- matrix(0, nrow=n, ncol=p)  # simulated data
  temp1 <- rnorm(n, sd=10*sqrt(scale))
  data[, 1] <- par[, 1]+temp1
  # standard high-dimensinal banana distribution
#   data[, 2] <- par[, 2]-b*temp1^2+100*b+temp2
#   data[, -c(1, 2)] <- par[, -c(1, 2)]+matrix(rnorm(n*(p-2), sd=1), nrow=n, ncol=p-2)
  # all the rest variables are correlated with the first variable 
  # while they are independent with each other
  for (ind in 2:p) {
    temp2 <- rnorm(n, sd=sqrt(scale))
    data[, ind] <- par[, ind]-b*temp1^2+100*b+temp2
  }
  return(data)
}

clabc.times <- function(prior, obs, num, n, h, method, type, scale) {
  # ABC for many obs.
  #
  # Args:
  #   prior - prior of the parameters;
  #   obs - observed summary statistic;
  #   num - number of simulations form likelihood;
  #   n - number of obs;
  #   h - threshold;
  #   method - method for kde;
  #   type - 0: full; 1: x*y; 2: (x,y);
  #   scale - scale of the likelihood;
  #
  # Return:
  #   The sample points.
  op <- options(warn=(-1))  # suppress warnings
  ptm.final <- proc.time()  # time record
  order <- combn(length(obs), 2)  # order of the composite likelihood
  temp <- prior
  for (times in 1:n) {
    if (type==0) {
      sim <- simLik(temp, num, scale)
      res <- clabc(obs, temp, sim, num, h, method, type)
      temp <- res$prior
    } else {
      for (ind in 1:(p*(p-1)/2)) {
        sim <- simLik(temp, num, scale)
        res <- clabc(obs[order[, ind]], temp, sim[,order[, ind]], num, h, method, type)
        temp <- res$prior
      }
    }
  }
  
  # Finalize the running.
  cost.final <- proc.time()-ptm.final
  print(cost.final)
  options(op)
  
  return(res$par)  
}

# Margin ABC
clabc.times.margin <- function(prior, obs, num, n, h, method, scale, margin) {
  # ABC for many obs.
  #
  # Args:
  #   prior - prior of the parameters;
  #   obs - observed summary statistic;
  #   num - number of simulations form likelihood;
  #   n - number of obs;
  #   h - threshold;
  #   method - method for kde;
  #   scale - scale of the likelihood;
  #   margin - the specific variable
  # Return:
  #   The sample points.
  op <- options(warn=(-1))  # suppress warnings
  ptm.final <- proc.time()  # time record
  temp <- prior
  for (times in 1:n) {
    sim <- simLik(temp, num, scale)
    res <- clabc(obs[margin], temp, sim[, margin], num, h, method, 0)
    temp <- res$prior
  }
  
  # Finalize the running.
  cost.final <- proc.time()-ptm.final
  print(cost.final)
  options(op)
  
  return(res$par)
}
  
getCorr <- function(data) {
  data.l <- data[data[, 1]<=20, ]
  data.r <- data[data[, 1]>20, ]
  cc <- (cor(data.r[, 1], data.r[, 2])-cor(data.l[, 1], data.l[, 2]))/2
  return(cc)
}

# plotResult <- function(data, bandwidth, type="kernel") {
#   # 
#   if (type=="shrinkage") {
#     shifted <- shiftPoints(data)$shifted    
#   } else if (type=="kernel") {
#     shifted <- data
#   }
#   H.shifted <- bandwidth
#   density.x <- density(shifted[, 1], H.shifted[1, 1])
#   density.y <- density(shifted[, 2], H.shifted[2, 2])
#   density.z <- density(shifted[, 3], H.shifted[3, 3])
# 
#   old <- par(mfrow=c(2, 3))
#   # Joint distribution.
#   contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
#           ylim=y.lim, ylab="theta2", main="banana")
#   points(data[, 1], data[, 2], pch=".")  # sample
#   contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
#           ylim=y.lim, ylab="theta3", main="banana")
#   points(data[, 1], data[, 3], pch=".")  # sample
#   plot(data[, 2], data[, 3], pch=".", xlim=x.lim, xlab="theta2",
#           ylim=y.lim, ylab="theta3", main="banana")
# 
#   # Marginal distribution of theta1.
#   curve(dMarginalPosterior, ylim=c(0, .1), ylab="density", 
#         xlim=x.lim, xlab="theta1", main="theta1")
#   plot(density.x, add=TRUE, lty="dashed", col="blue")
#   # Marginal distribution of theta1.
#   plot(density.y, lty="dashed", col="blue", ylim=c(0, .5), ylab="density", 
#        xlim=y.lim, xlab="theta2", main="theta2")
#   # Marginal distribution of theta3.
#   plot(density.z, lty="dashed", col="blue", ylim=c(0, .5), ylab="density", 
#        xlim=y.lim, xlab="theta3", main="theta3")
# 
#   par(old)
# }

plotJointResult <- function(data) {
  # Joint distribution.
  contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
          ylim=y.lim, ylab="theta2", main="banana")
  points(data[, 1], data[, 2], pch=".")  # sample
}

plotMarginalResult <- function(data, bandwidth, type="kernel") {
  # 
  if (type=="shrinkage") {
    shifted <- shiftPoints(data)$shifted    
  } else if (type=="kernel") {
    shifted <- data
  }
  H.shifted <- bandwidth
  density.x <- density(shifted[, 1], H.shifted[1, 1])  # shrinkage
  density.y <- density(shifted[, 2], H.shifted[2, 2])  # estimator
  old <- par(mfrow=c(1, 2))
  # Marginal distribution of theta1.
  curve(dMarginalPosterior, ylim=c(0, .1), ylab="density", 
        xlim=x.lim, xlab="theta1", main="theta1")
  plot(density.x, add=TRUE, lty="dashed", col="blue")
  # Marginal distribution of theta1.
  plot(density.y, lty="dashed", col="blue", ylim=c(0, .5), ylab="density", 
       xlim=y.lim, xlab="theta2", main="theta2")
  par(old)
}

# Plotting tools
dJointPosterior <- function(x, b=B, n=1) {
  # The true joint posterior.
  dnorm(x[1]-20, sd=10*sqrt(n))*dnorm(x[2]-20-b*(x[1]-20)^2+100*b, sd=1*sqrt(n))
}

dMarginalPosterior <- function(x, n=1) {
  # The true marginal poserior
  dnorm(x-20, sd=10*sqrt(n))
}

# Joint distribution.
grid.x <- seq(x.lim[1], x.lim[2], .5)
grid.y <- seq(y.lim[1], y.lim[2], .5)
grid.z <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
for (ind in 1:length(grid.x)) {
  grid.z[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dJointPosterior)
}