# shape analysis before & after adjustment.
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)

load("./data/banana/1000/xin-clabc-banana-corr-test-3dim.rda")
old <- par(mfrow=c(1, 2))
plot(ret[[1]]$unadj, pch=".", main="3-dim pairwise likelihood, b=0, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[1]]$adjust, pch=".", main="3-dim pairwise likelihood, b=0, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[2]]$unadj, pch=".", main="3-dim pairwise likelihood, b=0.01, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[2]]$adjust, pch=".", main="3-dim pairwise likelihood, b=0.01, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[3]]$unadj, pch=".", main="3-dim pairwise likelihood, b=0.05, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[3]]$adjust, pch=".", main="3-dim pairwise likelihood, b=0.05, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)

load("./data/banana/1000/xin-clabc-banana-corr-test-6dim.rda")
old <- par(mfrow=c(1, 2))
plot(ret[[1]]$unadj, pch=".", main="6-dim pairwise likelihood, b=0, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[1]]$adjust, pch=".", main="6-dim pairwise likelihood, b=0, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[2]]$unadj, pch=".", main="6-dim pairwise likelihood, b=0.01, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[2]]$adjust, pch=".", main="6-dim pairwise likelihood, b=0.01, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[3]]$unadj, pch=".", main="6-dim pairwise likelihood, b=0.05, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[3]]$adjust, pch=".", main="6-dim pairwise likelihood, b=0.05, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)

load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
old <- par(mfrow=c(1, 2))
plot(ret[[1]]$unadj, pch=".", main="9-dim pairwise likelihood, b=0, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[1]]$adjust, pch=".", main="9-dim pairwise likelihood, b=0, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[2]]$unadj, pch=".", main="9-dim pairwise likelihood, b=0.01, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[2]]$adjust, pch=".", main="9-dim pairwise likelihood, b=0.01, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)
old <- par(mfrow=c(1, 2))
plot(ret[[3]]$unadj, pch=".", main="9-dim pairwise likelihood, b=0.05, unadjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[3]]$adjust, pch=".", main="9-dim pairwise likelihood, b=0.05, adjustment", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)