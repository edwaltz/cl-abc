# correlation analysis
# True correlation


# 1,000 particles for b = 0, 0.01, 0.05.
# load data and data clean
load("./data/banana/clabc-banana-corr.rda")

# mean and sd analysis
unadj.mu <- apply(unadj.corr, c(1, 2), mean)
unadj.sd <- apply(unadj.corr, c(1, 2), sd)
adj.mu <- apply(adj.corr, c(1, 2), mean)
adj.sd <- apply(adj.corr, c(1, 2), sd)

# plot
# unadjustment
old <- par(mfrow=c(2, 2))
matplot(seq(3, 12, by=3), t(unadj.mu), type="l", lty=1:3, col=1, 
        main="Mean before adjustment", xlab="dimension", ylab="mean")
matpoints(seq(3, 12, by=3), t(unadj.mu), lty=1, col=1, pch=1)
legend("topright", legend=c("b=0", "b=0.01", "b=0.05"), lty=1:3, cex=0.4)
matplot(seq(3, 12, by=3), t(unadj.sd), type="l", lty=1:3, col=1, 
        main="Standard deviation before adjustment", xlab="dimension", ylab="standard deviation")
matpoints(seq(3, 12, by=3), t(unadj.sd), lty=1, col=1, pch=1)
legend("topleft", legend=c("b=0", "b=0.01", "b=0.05"), lty=1:3, cex=0.4)
# adjustment
matplot(seq(3, 12, by=3), t(adj.mu), type="l", lty=1:3, col=1, 
        main="Mean after adjustment", xlab="dimension", ylab="mean")
matpoints(seq(3, 12, by=3), t(adj.mu), lty=1, col=1, pch=1)
legend("topright", legend=c("b=0", "b=0.01", "b=0.05"), lty=1:3, cex=0.4)
matplot(seq(3, 12, by=3), t(adj.sd), type="l", lty=1:3, col=1, 
        main="Standard deviation after adjustment", xlab="dimension", ylab="standard deviation")
matpoints(seq(3, 12, by=3), t(adj.sd), lty=1, col=1, pch=1)
legend("topleft", legend=c("b=0", "b=0.01", "b=0.05"), lty=1:3, cex=0.4)
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
matplot(seq(3, 12, by=3), t(unadj.mu), type="l", lty=1:3, col=1, 
        main="Mean before adjustment", xlab="dimension", ylab="mean")
matpoints(seq(3, 12, by=3), t(unadj.mu), lty=1, col=1, pch=1)
legend("topright", legend=c("1,000 paricles", "5,000 paricles", "10,000 paricles"), lty=1:3, cex=0.4)
matplot(seq(3, 12, by=3), t(unadj.sd), type="l", lty=1:3, col=1, 
        main="Standard deviation before adjustment", xlab="dimension", ylab="standard deviation")
matpoints(seq(3, 12, by=3), t(unadj.sd), lty=1, col=1, pch=1)
legend("topleft", legend=c("1,000 paricles", "5,000 paricles", "10,000 paricles"), lty=1:3, cex=0.4)
# adjustment
matplot(seq(3, 12, by=3), t(adj.mu), type="l", lty=1:3, col=1, 
        main="Mean after adjustment", xlab="dimension", ylab="mean")
matpoints(seq(3, 12, by=3), t(adj.mu), lty=1, col=1, pch=1)
legend("topright", legend=c("1,000 paricles", "5,000 paricles", "10,000 paricles"), lty=1:3, cex=0.4)
matplot(seq(3, 12, by=3), t(adj.sd), type="l", lty=1:3, col=1, 
        main="Standard deviation after adjustment", xlab="dimension", ylab="standard deviation")
matpoints(seq(3, 12, by=3), t(adj.sd), lty=1, col=1, pch=1)
legend("topleft", legend=c("1,000 paricles", "5,000 paricles", "10,000 paricles"), lty=1:3, cex=0.4)
par(old)