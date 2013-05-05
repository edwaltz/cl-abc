# Unit test.
# Initial settings.
source("./banana/utils-gaussian.R")

# constant
B <- .01  # bananacity
num <- 10000000  # number of simulated parameters
tol <- .0001  # accuracy of the ABC
n <- num*tol
# standard range
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)

# full - 2 dim
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
full <- clabc.step(num, 2, obs, tol, "full", b=B)
plot(full$par, pch=".", main="2-dim standard likelihood", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full$par[, 1], bw=(4/((ncol(full$par)+2)*n))^(1/(ncol(full$par)+4))*sd(full$par[, 1])), main="theta 1")
plot(density(full$par[, 2], bw=(4/((ncol(full$par)+2)*n))^(1/(ncol(full$par)+4))*sd(full$par[, 1])), main="theta 2")
colMeans(full$par)
cor(full$par)

# full - 3 dim
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
full <- clabc.step(num, 3, obs, tol, "full", b=B)
plot(full$par, pch=".", main="3-dim standard likelihood", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(full$par[, 1], bw=(4/((ncol(full$par)+2)*n))^(1/(ncol(full$par)+4))*sd(full$par[, 1])), main="theta 1")
plot(density(full$par[, 2], bw=(4/((ncol(full$par)+2)*n))^(1/(ncol(full$par)+4))*sd(full$par[, 1])), main="theta 2")
plot(density(full$par[, 3], bw=(4/((ncol(full$par)+2)*n))^(1/(ncol(full$par)+4))*sd(full$par[, 1])), main="theta 3")
colMeans(full$par)
cor(full$par)

# pair - 3 dim
obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
pair <- clabc.step(num, 2, obs, tol, "pair", b=B)
plot(pair$par[, c(1, 2)], pch=".", main="3-dim pairwise likelihood", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair$par[, 1], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 1])), main="theta 1")
plot(density(pair$par[, 2], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 2])), main="theta 2")
plot(density(pair$par[, 3], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 3])), main="theta 3")
colMeans(pair$par)
cor(pair$par)

# pair - 5 dim
obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
pair <- clabc.step(num, 5, obs, tol, "pair", b=B)
plot(pair$par[, c(1, 2)], pch=".", main="5-dim pairwise likelihood", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair$par[, 1], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 1])), main="theta 1")
plot(density(pair$par[, 2], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 2])), main="theta 2")
plot(density(pair$par[, 3], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 3])), main="theta 3")
colMeans(pair$par)
cor(pair$par)

# pair - 7 dim
obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
pair <- clabc.step(num, 7, obs, tol, "pair", b=B)
plot(pair$par[, c(1, 2)], pch=".", main="7-dim pairwise likelihood", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(density(pair$par[, 1], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 1])), main="theta 1")
plot(density(pair$par[, 2], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 2])), main="theta 2")
plot(density(pair$par[, 3], bw=(4/((ncol(pair$par)+2)*n))^(1/(ncol(pair$par)+4))*sd(pair$par[, 3])), main="theta 3")
colMeans(pair$par)
cor(pair$par)

# margin adjustment - 3 dim
ret <- get.corr(B, 3, num)
plot(ret$unadj, pch=".", main="3-dim pairwise likelihood, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret$adjust, pch=".", main="3-dim pairwise likelihood, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
ret$cor

ret2 <- run.corr(10, B, 3, num)