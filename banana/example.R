# Toy example of "banana".
# Initial settings.
source("./banana/utils.R")

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
res1$dim2 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res1$dim3 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res1$dim5 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res1$dim7 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res1$dim9 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

gc()


# 2.
B <- .01  # bananacity is 0.01.

res2 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
res2$dim2 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res2$dim3 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res2$dim5 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res2$dim7 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res2$dim9 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

gc()

# 3.
B <- .05  # bananacity is 0.075.

res3 <- list()

# full
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
res3$dim2 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par

# pair
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
res3$dim3 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
res3$dim5 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
res3$dim7 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
res3$dim9 <- clabc.step(num, 2, obs, tol, "banana.rot", "pair", b=B)$par[, 1:2]

gc()


# draw picture
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)
par(mfrow=c(3, 4))

plot(res1[[1]], pch=".", main="2 dim, B=0, cor=0.9498", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res1[[2]], pch=".", main="3 dim, B=0, cor=0.8633", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res1[[3]], pch=".", main="5 dim, B=0, cor=0.7003", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res1[[4]], pch=".", main="10 dim, B=0, cor=0.3065", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)

plot(res2[[1]], pch=".", main="2 dim, B=0.01, cor=0.9152", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res2[[2]], pch=".", main="3 dim, B=0.01, cor=0.8328", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res2[[3]], pch=".", main="5 dim, B=0.01, cor=0.7114", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res2[[4]], pch=".", main="10 dim, B=0.01, cor=0.4456", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)

plot(res3[[1]], pch=".", main="2 dim, B=0.05, cor=0.4456", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res3[[2]], pch=".", main="3 dim, B=0.05, cor=0.3334", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res3[[3]], pch=".", main="5 dim, B=0.05, cor=0.1312", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(res3[[4]], pch=".", main="10 dim, B=0.05, cor=-0.0282", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
