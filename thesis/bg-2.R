# The simple example revisited in high dimensions.
setwd("/home/edwaltz/Documents/R/cl-abc")
source("./banana/utils.R")

# constant
B <- .01  # bananacity
pmin <- -40
pmax <- 40
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)

grid.x <- seq(lim.x[1], lim.x[2], .5)
grid.y <- seq(lim.y[1], lim.y[2], .5)
grid.z <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
for (ind in 1:length(grid.x)) {
  grid.z[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana)
}

num <- 10000000
tol <- .0001
ret <- list()

obs <- matrix(0, nrow=1, ncol=3, byrow=TRUE)
ret[[1]] <- clabc.step(num, 3, obs, tol, "full", b=B)$par

obs <- matrix(0, nrow=1, ncol=5, byrow=TRUE)
ret[[2]] <- clabc.step(num, 5, obs, tol, "full", b=B)$par

obs <- matrix(0, nrow=1, ncol=7, byrow=TRUE)
ret[[3]] <- clabc.step(num, 7, obs, tol, "full", b=B)$par

obs <- matrix(0, nrow=1, ncol=9, byrow=TRUE)
ret[[4]] <- clabc.step(num, 9, obs, tol, "full", b=B)$par

par(mfrow=c(2, 2))
contour(grid.x, grid.y, grid.z, main="3 dimensions", xlab=expression(theta[1]), 
        ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[1]], pch=".", cex=1.5)
contour(grid.x, grid.y, grid.z, main="5 dimensions", xlab=expression(theta[1]), 
        ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[2]], pch=".", cex=1.5)
contour(grid.x, grid.y, grid.z, main="7 dimensions", xlab=expression(theta[1]), 
        ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[3]], pch=".", cex=1.5)
contour(grid.x, grid.y, grid.z, main="9 dimensions", xlab=expression(theta[1]), 
        ylab=expression(theta[2]), xlim=lim.x, ylim=lim.y, asp=1)
points(ret[[4]], pch=".", cex=1.5)