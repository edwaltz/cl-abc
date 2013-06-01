# figures for the likelihood of banana-shaped distribution.
# constant

dbanana.r <- function(y, b) {
  # The true joint posterior.
  x <- matrix(c(cos(pi / 4), -sin(pi / 4), cos(pi / 4), sin(pi / 4)), nrow=2) %*% y
  dnorm(x[1], sd=10) * dnorm(x[2] + b * x[1]^2 - 100 * b, sd=1)
}

dbanana.s <- function(y, b) {
  # The true joint posterior.
  x <- y
  dnorm(x[1], sd=10) * dnorm(x[2] + b * x[1]^2 - 100 * b, sd=1)
}

pmin <- -25
pmax <- 25
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)

grid.x <- seq(lim.x[1], lim.x[2], .5)
grid.y <- seq(lim.y[1], lim.y[2], .5)
grid.z1 <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
grid.z2 <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))

old <- par(mfrow=c(3, 2))

# standard B=0
for (ind in 1:length(grid.x)) {
  grid.z1[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.s, 0)
}
contour(grid.x, grid.y, grid.z1, main="Standard likelihood, B=0", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)
# rotated B=0
for (ind in 1:length(grid.x)) {
  grid.z2[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.r, 0)
}
contour(grid.x, grid.y, grid.z2, main="Modified likelihood, B=0", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)
# standard B=0.01
for (ind in 1:length(grid.x)) {
  grid.z1[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.s, 0.01)
}
contour(grid.x, grid.y, grid.z1, main="Standard likelihood, B=0.01", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)
# rotated B=0.01
for (ind in 1:length(grid.x)) {
  grid.z2[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.r, 0.01)
}
contour(grid.x, grid.y, grid.z2, main="Modified likelihood, B=0.01", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)
# standard B=0.05
for (ind in 1:length(grid.x)) {
  grid.z1[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.s, 0.05)
}
contour(grid.x, grid.y, grid.z1, main="Standard likelihood, B=0.05", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)
# rotated B=0.05
for (ind in 1:length(grid.x)) {
  grid.z2[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana.r, 0.05)
}
contour(grid.x, grid.y, grid.z2, main="Modified likelihood, B=0.05", xlab=expression(theta[(1)]), 
        ylab=expression(theta[(2)]), xlim=lim.x, ylim=lim.y, asp=1)

par(old)