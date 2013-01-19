# Initial trial.
# Initialization
p <- 5
obs <- matrix(20, nrow=1, ncol=p, byrow=TRUE)  # observations
num <- 5000000  # number of simulated parameters
threshold <- .001  # accuracy of the ABC
n <- num*threshold

# Run the algorithm.
prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
full <- clabc.times(prior, obs, num, 1, threshold, "kernel", 0, 1)

# prior <- cbind(runif(num, -50, 50), runif(num, -50, 50), runif(num, -50, 50))
# pairtimes <- clabc.times(prior, obs, num, 1, threshold, "kernel", 1, 1)

prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
pairwise <- clabc.times(prior, obs, num, 1, threshold, "kernel", 2, 1)

# Margin adjustment
theta.margin <- matrix(0, nrow=n, ncol=p)
for (ind in 1:p)
{
  prior <- matrix(runif(num*p, -50, 50), nrow=num, ncol=p)
  margin <- clabc.times.margin(prior, obs, num, 1, threshold, "kernel", 1, ind)
  theta.margin[, ind] <- margin$par[, ind]
}
theta.adj <- adjMargin(pairwise$par, theta.margin)

# Analysis
plotJointResult(full$par[, c(1, 2)])
getCorr(full$par[, c(1, 2)])
plotJointResult(pairwise$par[, c(1, 2)])
getCorr(pairwise$par[, c(1, 2)])
plotJointResult(theta.adj[, c(1, 2)])
getCorr(theta.adj[, c(1, 2)])