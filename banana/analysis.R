# Analysis
# initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
B <- .01  # bananacity
num <- 10000000  # number of simulated parameters
tol <- .0005  # accuracy of the ABC
p <- 2  # dim of parameters
n <- num*tol

# run the algorithm.
# standard range
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)

# standard (full)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- matrix(runif(num*p, -100, 100), nrow=num)
full.std <- clabc.step(prior, obs, tol, rbanana, "full", b=B)
plot(full.std$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 1])), 
     main="theta 1")
plot(density(full.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 2])), 
     main="theta 2")

# standard (pair)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- matrix(runif(num*p, -100, 100), nrow=num)
pair.std <- clabc.step(prior, obs, tol, rbanana, "pair", b=B)
plot(pair.std$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 1])), 
     main="theta 1")
plot(density(pair.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 2])), 
     main="theta 2")


# rotated range
B <- .05
lim.xr <- c(-100, 100)
lim.yr <- c(-100, 100)
# rotated (full)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- cbind(runif(num, -100, 100), runif(num, -100, 100), 
               matrix(runif(num*(p-2), -100, 100), nrow=num, ncol=p-2))
full.rot <- clabc.step(prior, obs, tol, rbanana.rot, "full", b=B)
plot(full.rot$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.xr, ylim=lim.yr)
plot(density(full.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 1])), 
     main="theta 1")
plot(density(full.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 2])), 
     main="theta 2")

# rotated (pair)
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
prior <- cbind(runif(num, -100, 100), runif(num, -100, 100), 
               matrix(runif(num*(p-2), -100, 100), nrow=num, ncol=p-2))
pair.rot <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)
plot(pair.rot$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.xr, ylim=lim.yr)
plot(density(pair.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 1])), 
     main="theta 1")
plot(density(pair.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 2])), 
     main="theta 2")