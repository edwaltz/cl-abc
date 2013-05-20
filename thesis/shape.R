# shape analysis before & after adjustment.
lim.x <- c(-50, 50)
lim.y <- c(-50, 50)

dbanana <- function(y, b) {
  # The true joint posterior.
  x <- matrix(c(cos(pi / 4), -sin(pi / 4), cos(pi / 4), sin(pi / 4)), nrow=2) %*% y
  dnorm(x[1], sd=10) * dnorm(x[2]- b * x[1]^2 + 100 * b, sd=1)
}

posterior1 <- matrix(0, nrow=1000, ncol=1000)
posterior2 <- matrix(0, nrow=1000, ncol=1000)
posterior3 <- matrix(0, nrow=1000, ncol=1000)
pos.x <- seq(-50, 50, length.out=1000)
pos.y <- seq(-50, 50, length.out=1000)
pos <- matrix(0,nrow=2, ncol=1)
for (i in 1:100) {
  for (j in 1:1000) {
    pos[1, 1] = pos.x[i]
    pos[2, 1] = pos.y[j]
    posterior1[i, j]  <- dbanana(pos, 0)
    posterior2[i, j]  <- dbanana(pos, 0.01)
    posterior3[i, j]  <- dbanana(pos, 0.05)
  }
}
contour(pos.x, pos.y, posterior2)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="3-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="6-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="9-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="12-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="3-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="6-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="9-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="12-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="3-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="6-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="9-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="12-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="3-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="6-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="9-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="12-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="3-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="6-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="9-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="12-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="3-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="6-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="9-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="12-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="3-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="6-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="9-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="12-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/5000/xin-clabc-banana-num-test-3dim.rda")
plot(ret$unadj, asp=1, pch=".", main="3-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-6dim.rda")
plot(ret$unadj, asp=1, pch=".", main="6-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-9dim.rda")
plot(ret$unadj, asp=1, pch=".", main="9-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-12dim.rda")
plot(ret$unadj, asp=1, pch=".", main="12-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/10000/xin-clabc-banana-num-test2-3dim.rda")
plot(ret$unadj, asp=1, pch=".", main="3-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-6dim.rda")
plot(ret$unadj, asp=1, pch=".", main="6-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-9dim.rda")
plot(ret$unadj, asp=1, pch=".", main="9-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-12dim.rda")
plot(ret$unadj, asp=1, pch=".", main="12-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="3-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="6-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="9-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/1000/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="12-dim, 1,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/5000/xin-clabc-banana-num-test-3dim.rda")
plot(ret$adjust, asp=1, pch=".", main="3-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-6dim.rda")
plot(ret$adjust, asp=1, pch=".", main="6-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-9dim.rda")
plot(ret$adjust, asp=1, pch=".", main="9-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/5000/xin-clabc-banana-num-test-12dim.rda")
plot(ret$adjust, asp=1, pch=".", main="12-dim, 5,000 particles", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/10000/xin-clabc-banana-num-test2-3dim.rda")
plot(ret$adjust, asp=1, pch=".", main="3-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-6dim.rda")
plot(ret$adjust, asp=1, pch=".", main="6-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-9dim.rda")
plot(ret$adjust, asp=1, pch=".", main="9-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/10000/xin-clabc-banana-num-test2-12dim.rda")
plot(ret$adjust, asp=1, pch=".", main="12-dim, 10,000", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# before adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="3-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="6-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="9-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$unadj, asp=1, pch=".", main="12-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="3-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="6-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="9-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$unadj, asp=1, pch=".", main="12-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="3-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="6-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="9-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$unadj, asp=1, pch=".", main="12-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)

# after adjustment
old  <-  par(mfrow=c(3, 4))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="3-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="6-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="9-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[1]]$adjust, asp=1, pch=".", main="12-dim, b=0", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="3-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="6-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="9-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[2]]$adjust, asp=1, pch=".", main="12-dim, b=0.01", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

load("./data/banana/gaussian/xin-clabc-banana-corr-test-3dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="3-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-6dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="6-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="9-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))
load("./data/banana/gaussian/xin-clabc-banana-corr-test-12dim.rda")
plot(ret[[3]]$adjust, asp=1, pch=".", main="12-dim, b=0.05", xlim=lim.x, ylim=lim.y,
     xlab=expression(theta[1]), ylab=expression(theta[2]))

par(old)