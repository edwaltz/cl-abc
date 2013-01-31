# Toy example of "banana".
# Initial settings.
source("base/cl-abc.R")
source("banana/utils.R")

# constant
num <- 10000000
tol <- .0005
n <- num*tol


# run the algorithm.
# 1.
B <- 0  # bananacity is 0.

res1 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
res1$dim2 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res1$dim3 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res1$dim5 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res1$dim7 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res1$dim9 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()


# 2.
B <- .01  # bananacity is 0.01.

res2 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
res2$dim2 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res2$dim3 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res2$dim5 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res2$dim7 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res2$dim9 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()

# 3.
B <- .05  # bananacity is 0.075.

res3 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
res3$dim2 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res3$dim3 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res3$dim5 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res3$dim7 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res3$dim9 <- clabc.step(num, 2, obs, tol, rbanana.rot, "pair", b=B)$par[, 1:2]

gc()


# draw picture
# par(mfrow=c(3, 4))