# Toy example of "banana".
# Initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
num <- 10000000
tol <- .0005
p <- 2
pmin <- -100
pmax <- 100
n <- num*tol


# run the algorithm.
# 1.
B <- 0  # bananacity is 0.

res1 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=10, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res1$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()


# 2.
B <- .01  # bananacity is 0.01.

res2 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=10, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res2$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()


# 3.
B <- .05  # bananacity is 0.075.

res3 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim2 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim3 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim5 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=10, byrow=TRUE)
prior <- matrix(runif(num*p, pmin, pmax), nrow=num, ncol=p)
res3$dim10 <- clabc.step(prior, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()


# draw picture
# par(mfrow=c(3, 4))