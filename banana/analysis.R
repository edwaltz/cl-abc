# Analysis
# initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
B <- .075  # bananacity
num <- 5000000  # number of simulated parameters
tol <- .001  # accuracy of the ABC
n <- num*tol


# run the algorithm.
# standard range
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)
# standard (full)
p <- 2
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, -100, 100), nrow=num, ncol=p)
full.std <- clabc.step(prior, obs, tol, rbanana, "full")
plot(full.std$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 1])), 
     main="theta 1")
plot(density(full.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 2])), 
     main="theta 2")

# standard (pair)
p <- 3
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, -100, 100), nrow=num, ncol=p)
pair.std <- clabc.step(prior, obs, tol, rbanana, "pair")
plot(pair.std$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 1])), 
     main="theta 1")
plot(density(pair.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 2])), 
     main="theta 2")

# rotated range
lim.xr <- c(-100, 100)
lim.yr <- c(-50, 150)
# rotated (full)
p <- 2
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- cbind(runif(num, -100, 100), runif(num, -50, 150), 
               matrix(runif(num*(p-2), -100, 100), nrow=num, ncol=p-2))
full.rot <- clabc.step(prior, obs, tol, rbanana.rot, "full")
plot(full.rot$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.xr, ylim=lim.yr)
plot(density(full.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 1])), 
     main="theta 1")
plot(density(full.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 2])), 
     main="theta 2")

# rotated (pair)
p <- 7
num <- 10000000
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- cbind(runif(num, -100, 100), runif(num, -50, 150), 
               matrix(runif(num*(p-2), -100, 100), nrow=num, ncol=p-2))
pair.rot <- clabc.step(prior, obs, tol, rbanana.rot, "pair")
gc()
plot(pair.rot$par[, c(1, 2)], pch=".", main="7-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.xr, ylim=lim.yr)
plot(density(pair.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 1])), 
     main="theta 1")
plot(density(pair.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 2])), 
     main="theta 2")