setwd("/home/edwaltz/Documents/R/cl-abc")
source("./banana/utils.R")

# constant
num <- 5000000
tol <- .001
n <- num*tol

pmin <- -50
pmax <- 50
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)

B <- .01  # bananacity

obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
full <- clabc.step(num, 2, obs, tol, "full", b=B)
plot(full$par, pch=".", main="2 dim", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y, asp=1)

par(mfrow=c(1, 4))

obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
full <- clabc.step(num, 3, obs, tol, "full", b=B)
plot(full$par, pch=".", main="3 dim", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
full <- clabc.step(num, 5, obs, tol, "full", b=B)
plot(full$par, pch=".", main="5 dim", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
full <- clabc.step(num, 7, obs, tol, "full", b=B)
plot(full$par, pch=".", main="7 dim", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
full <- clabc.step(num, 9, obs, tol, "full", b=B)
plot(full$par, pch=".", main="9 dim", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)