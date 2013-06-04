# A simple example.
source("./banana/utils-gaussian.R")

# constant
pmin <- -40
pmax <- 40
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)
B <- .01  # bananacity

grid.x <- seq(lim.x[1], lim.x[2], .5)
grid.y <- seq(lim.y[1], lim.y[2], .5)
grid.z <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
for (ind in 1:length(grid.x)) {
  grid.z[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana)
}

num <- 10000000
ret <- list()

tol <- .0001
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
ret[[1]] <- clabc.step(num, 2, obs, tol, "full", b=B)$par

tol <- .001
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
ret[[2]] <- clabc.step(num, 2, obs, tol, "full", b=B)$par

tol <- .01
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
ret[[3]] <- clabc.step(num, 2, obs, tol, "full", b=B)$par

tol <- .1
obs <- matrix(0, nrow=1, ncol=2, byrow=TRUE)
ret[[4]] <- clabc.step(num, 2, obs, tol, "full", b=B)$par

old  <- par(mfrow=c(2, 2))

contour(grid.x, grid.y, grid.z, main=expression(paste(k[N]/N == 10, "%")), 
        xlab=expression(theta[1]), ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[4]][sample(1000000, size=1000, replace=TRUE), ], pch=".", cex=1.5)

contour(grid.x, grid.y, grid.z, main=expression(paste(k[N]/N == 1, "%")), 
        xlab=expression(theta[1]), ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[3]][sample(100000, size=1000, replace=TRUE), ], pch=".", cex=1.5)

contour(grid.x, grid.y, grid.z, main=expression(paste(k[N]/N == 0.1, "%")), 
        xlab=expression(theta[1]), ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[2]][sample(10000, size=1000, replace=TRUE), ], pch=".", cex=1.5)

contour(grid.x, grid.y, grid.z, main=expression(paste(k[N]/N == .01, "%")), 
        xlab=expression(theta[1]), ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[1]], pch=".", cex=1.5)

par(old)