# Analysis
# initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
B <- .01  # bananacity
num <- 10000000  # number of simulated parameters
tol <- .0005  # accuracy of the ABC
n <- num*tol

# run the algorithm.
# standard range
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)

# standard (full)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
full.std <- clabc.step(num, 2, obs, tol, rbanana, "full", b=B)
plot(full.std$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 1])), 
     main="theta 1")
plot(density(full.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.std$par[, 2])), 
     main="theta 2")

# standard (pair)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
pair.std <- clabc.step(num, 2, obs, tol, rbanana, "pair", b=B)
plot(pair.std$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair.std$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 1])), 
     main="theta 1")
plot(density(pair.std$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.std$par[, 2])), 
     main="theta 2")


# rotated range
B <- .05

# rotated (full)
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
full.rot <- clabc.step(num, 2, obs, tol, rbanana.rot, "full", b=B)
plot(full.rot$par, pch=".", main="2-dim standard likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 1])), 
     main="theta 1")
plot(density(full.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(full.rot$par[, 2])), 
     main="theta 2")

# rotated (pair)
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
pair.rot <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)
plot(pair.rot$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair.rot$par[, 1], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 1])), 
     main="theta 1")
plot(density(pair.rot$par[, 2], bw=(4/((p+2)*n))^(1/(p+4))*sd(pair.rot$par[, 2])), 
     main="theta 2")