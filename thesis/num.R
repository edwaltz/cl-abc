# number of particles analysis
lim.x <- c(-100, 100)
lim.y <- c(-100, 100)

old <- par(mfrow=c(2, 4))
load("./data/banana/1000/xin-clabc-banana-corr-test-9dim.rda")
plot(ret[[2]]$unadj, pch=".", main="9-dim, n=1,000, b=0.01, unadjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[2]]$adjust, pch=".", main="9-dim, n=1,000, b=0.01, adjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
load("./data/banana/5000/xin-clabc-banana-num-test-9dim.rda")
plot(ret$unadj, pch=".", main="9-dim, n=5,000, b=0.01, unadjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret$adjust, pch=".", main="9-dim, n=5,000, b=0.01, adjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
load("./data/banana/10000/xin-clabc-banana-num-test2-9dim.rda")
plot(ret$unadj, pch=".", main="9-dim, n=10,000, b=0.01, unadjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret$adjust, pch=".", main="9-dim, n=10,000, b=0.01, adjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
load("./data/banana/10000/xin-clabc-banana-num-test3-9dim.rda")
plot(ret$unadj, pch=".", main="9-dim, n=20,000, b=0.01, unadjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret$adjust, pch=".", main="9-dim, n=20,000, b=0.01, adjustment", 
     xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)