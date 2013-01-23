# Toy example of "banana".
# Initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
num <- 5000000
tol <- .001
n <- num*tol
pmin <- -100
pmax <- 100


# run the algorithm.
# 1.
B <- 0  # bananacity is 0.
res1 <- list()

# full
p <- 2
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par

# pair
p <- 3
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 5
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 10
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

gc()


# 2.
B <- .01  # bananacity is 0.01.
res2 <- list()

# full
p <- 2
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par

# pair
p <- 3
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 5
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 10
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

gc()


# 3.
B <- .075  # bananacity is 0.075.
res3 <- list()

# full
p <- 2
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par

# pair
p <- 3
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 5
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

p <- 10
obs <- matrix(0, nrow=1, ncol=p, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair")$par[, 1:2]

gc()


# draw picture
par(mfrow=c(3, 4))
