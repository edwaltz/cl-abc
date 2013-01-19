# Toy example of "banana".
# Initial settings.
source("utils_banana.R")
num <- 5000000  # number of simulated parameters
threshold <- .001  # accuracy of the ABC
n <- num*threshold

# 2 dim with and without adjustment
p <- 2
obs <- matrix(20, nrow=1, ncol=p, byrow=TRUE)  # observations
prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
pairwise.2dim <- clabc.times(prior, obs, num, 1, threshold, "kernel", 2, 1)
theta.margin <- matrix(0, nrow=n, ncol=p)
for (ind in 1:p)
{
  prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
  margin <- clabc.times.margin(prior, obs, num, 1, threshold, "kernel", 1, ind)
  theta.margin[, ind] <- margin[, ind]
}
adj.2dim <- adjMargin(pairwise.2dim, theta.margin)

# 5 dim with and without adjustment
p <- 5
obs <- matrix(20, nrow=1, ncol=p, byrow=TRUE)  # observations
prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
pairwise.5dim <- clabc.times(prior, obs, num, 1, threshold, "kernel", 2, 1)
theta.margin <- matrix(0, nrow=n, ncol=p)
for (ind in 1:p)
{
  prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
  margin <- clabc.times.margin(prior, obs, num, 1, threshold, "kernel", 1, ind)
  theta.margin[, ind] <- margin[, ind]
}
adj.5dim <- adjMargin(pairwise.5dim, theta.margin)

# 10 dim with and without adjustment
p <- 10
obs <- matrix(20, nrow=1, ncol=p, byrow=TRUE)  # observations
prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
pairwise.10dim <- clabc.times(prior, obs, num, 1, threshold, "kernel", 2, 1)
theta.margin <- matrix(0, nrow=n, ncol=p)
for (ind in 1:p)
{
  prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
  margin <- clabc.times.margin(prior, obs, num, 1, threshold, "kernel", 1, ind)
  theta.margin[, ind] <- margin[, ind]
}
adj.10dim <- adjMargin(pairwise.10dim, theta.margin)

# 20 dim with and without adjustment
p <- 20
obs <- matrix(20, nrow=1, ncol=p, byrow=TRUE)  # observations
prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
pairwise.20dim <- clabc.times(prior, obs, num, 1, threshold, "kernel", 2, 1)
theta.margin <- matrix(0, nrow=n, ncol=p)
for (ind in 1:p)
{
  prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
  margin <- clabc.times.margin(prior, obs, num, 1, threshold, "kernel", 1, ind)
  theta.margin[, ind] <- margin[, ind]
}
adj.20dim <- adjMargin(pairwise.20dim, theta.margin)

# Plot
old <- par(mfrow=c(2, 2))
# without adjustment
contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
        ylim=y.lim, ylab="theta2", main="2 dim before adj. cor=.6598")
points(pairwise.2dim[, 1], pairwise.2dim[, 2], pch=".")
contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
        ylim=y.lim, ylab="theta2", main="5 dim before adj. cor=.0209")
points(pairwise.5dim[, 1], pairwise.5dim[, 2], pch=".")
# with adjustment
contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
        ylim=y.lim, ylab="theta2", main="2 dim with adj. cor=.6750")
points(adj.2dim[, 1], adj.2dim[, 2], pch=".")
contour(grid.x, grid.y, grid.z, xlim=x.lim, xlab="theta1",
        ylim=y.lim, ylab="theta2", main="5 dim with adj. cor=.0240")
points(adj.5dim[, 1], adj.5dim[, 2], pch=".")
par(old)