# shape analysis before & after adjustment.
dbanana <- function(y, b=B) {
  # The true joint posterior.
  x <- matrix(c(cos(pi / 4), -sin(pi / 4), cos(pi / 4), sin(pi / 4)), nrow=2) %*% y
  dnorm(x[1], sd=10) * dnorm(x[2]- b * x[1]^2 + 100 * b, sd=1)
}

pmin <- -25
pmax <- 25
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)
grid.x <- seq(lim.x[1], lim.x[2], .5)
grid.y <- seq(lim.y[1], lim.y[2], .5)

grid.z1 <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
grid.z2 <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))
grid.z3 <- matrix(0, nrow=length(grid.x), ncol=length(grid.y))

for (ind in 1:length(grid.x)) {
  grid.z1[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana, 0)
  grid.z2[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana, 0.01)
  grid.z3[ind, ] <- apply(cbind(grid.x[ind], grid.y),1, dbanana, 0.05)
}

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 1: B=0, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 2: B=0, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 3: B=0, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 4: B=0, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 5: B=0.01, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 6: B=0.01, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 7: B=0.01, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 8: B=0.01, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 9: B=0.05, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 10: B=0.05, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 11: B=0.05, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 12: B=0.05, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 1: B=0, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 2: B=0, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 3: B=0, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 4: B=0, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 5: B=0.01, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 6: B=0.01, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 7: B=0.01, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 8: B=0.01, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 9: B=0.05, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 10: B=0.05, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 11: B=0.05, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 12: B=0.05, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

par(old)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=12")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-3dim.rda")
plot(ret$unadj[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-6dim.rda")
plot(ret$unadj[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-9dim.rda")
plot(ret$unadj[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-12dim.rda")
plot(ret$unadj[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=12")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-3dim.rda")
plot(ret$unadj[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-6dim.rda")
plot(ret$unadj[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-9dim.rda")
plot(ret$unadj[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-12dim.rda")
plot(ret$unadj[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=12")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==1, ",000, p=12")),
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-3dim.rda")
plot(ret$adjust[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-6dim.rda")
plot(ret$adjust[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-9dim.rda")
plot(ret$adjust[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/5000/xin-clabc-banana-num-test-12dim.rda")
plot(ret$adjust[sample(5000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==5, ",000, p=12")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-3dim.rda")
plot(ret$adjust[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=3")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-6dim.rda")
plot(ret$adjust[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=6")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-9dim.rda")
plot(ret$adjust[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=9")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/10000/xin-clabc-banana-num-test2-12dim.rda")
plot(ret$adjust[sample(10000, 500), ], asp=1, pch=".", main=expression(paste(k[N]==10, ",000, p=12")), 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

par(old)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="B=0, p=3", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="B=0, p=6", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="B=0, p=9", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="B=0, p=12", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="B=0.01, p=3", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="B=0.01, p=6", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="B=0.01, p=9", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="B=0.01, p=12", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="B=0.05, p=3", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="B=0.05, p=6", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="B=0.05, p=9", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="B=0.05, p=12", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 1: B=0, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 2: B=0, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 3: B=0, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 4: B=0, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 5: B=0.01, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 6: B=0.01, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 7: B=0.01, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 8: B=0.01, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 9: B=0.05, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 10: B=0.05, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 11: B=0.05, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$unadj[sample(1000, 500), ], asp=1, pch=".", main="Model 12: B=0.05, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 1: B=0, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 2: B=0, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 3: B=0, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 4: B=0, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z1, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 5: B=0.01, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 6: B=0.01, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 7: B=0.01, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 8: B=0.01, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z2, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 9: B=0.05, p=3", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 10: B=0.05, p=6", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 11: B=0.05, p=9", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$adjust[sample(1000, 500), ], asp=1, pch=".", main="Model 12: B=0.05, p=12", 
     xlim=lim.x, ylim=lim.y, xlab=expression(theta[1]), ylab=expression(theta[2]))
contour(grid.x, grid.y, grid.z3, add=TRUE)

par(old)