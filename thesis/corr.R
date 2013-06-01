# correlation analysis

# 1,000 particles for b = 0, 0.01, 0.05.
# load data and data clean
load("./data/banana/clabc-banana-corr.rda")

# mean and sd analysis
unadj.mu <- apply(unadj.corr, c(1, 2), mean)
unadj.sd <- apply(unadj.corr, c(1, 2), sd)
adj.mu <- apply(adj.corr, c(1, 2), mean)
adj.sd <- apply(adj.corr, c(1, 2), sd)

rho1 <- 0.9780
rho2 <- 0.9442
rho3 <- 0.3492

# plot
# unadjustment
old <- par(mfrow=c(2, 2))

matplot(seq(3, 12, by=3), t(unadj.mu), type="l", lty=1:3, col=1, ylim=c(0,1),
        main="Before adjustment", xlab="p", ylab=expression(bar(rho)[m]))
matpoints(seq(3, 12, by=3), t(unadj.mu), lty=1, col=1, pch=1)
abline(h=c(rho1, rho2, rho3), lty=1:3)

matplot(seq(3, 12, by=3), t(unadj.sd), type="l", lty=1:3, col=1, 
        main="Before adjustment", xlab="p", ylab=expression(hat(sd)(rho[m])))
matpoints(seq(3, 12, by=3), t(unadj.sd), lty=1, col=1, pch=1)

# adjustment
matplot(seq(3, 12, by=3), t(adj.mu), type="l", lty=1:3, col=1, ylim=c(0,1),
        main="After adjustment", xlab="p", ylab=expression(bar(rho)[m]^a))
matpoints(seq(3, 12, by=3), t(adj.mu), lty=1, col=1, pch=1)
abline(h=c(rho1, rho2, rho3), lty=1:3)
matplot(seq(3, 12, by=3), t(adj.sd), type="l", lty=1:3, col=1, 
        main="After adjustment", xlab="p", ylab=expression(hat(sd)(rho[m]^a)))
matpoints(seq(3, 12, by=3), t(adj.sd), lty=1, col=1, pch=1)

par(old)

# 1,000, 5,000 and 10,000 particles for b =0.01.
# load data and data clean
load("./data/banana/clabc-banana-num.rda")

# mean and sd analysis
unadj.mu <- apply(unadj.num, c(1, 2), mean)
unadj.sd <- apply(unadj.num, c(1, 2), sd)
adj.mu <- apply(adj.num, c(1, 2), mean)
adj.sd <- apply(adj.num, c(1, 2), sd)

# plot
# unadjustment
old <- par(mfrow=c(2, 2))

matplot(seq(3, 12, by=3), t(unadj.mu), type="l", lty=1:3, col=1, ylim=c(0,1), 
        main="Before adjustment", xlab="p", ylab=expression(bar(rho)[m]))
matpoints(seq(3, 12, by=3), t(unadj.mu), lty=1, col=1, pch=1)
abline(h=rho2)
matplot(seq(3, 12, by=3), t(unadj.sd), type="l", lty=1:3, col=1, 
        main="Before adjustment", xlab="p", ylab=expression(hat(sd)(rho[m])))
matpoints(seq(3, 12, by=3), t(unadj.sd), lty=1, col=1, pch=1)
# adjustment
matplot(seq(3, 12, by=3), t(adj.mu), type="l", lty=1:3, col=1, ylim=c(0,1), 
        main="After adjustment", xlab="p", ylab=expression(bar(rho)[m]^a))
matpoints(seq(3, 12, by=3), t(adj.mu), lty=1, col=1, pch=1)
abline(h=rho2)
matplot(seq(3, 12, by=3), t(adj.sd), type="l", lty=1:3, col=1, 
        main="After adjustment", xlab="p", ylab=expression(hat(sd)(rho[m]^a)))
matpoints(seq(3, 12, by=3), t(adj.sd), lty=1, col=1, pch=1)

par(old)