# correlation analysis
# data clean
load("./data/xin-clabc-banana-corr-raw-1.rda")
ret.corr <- ret
load("./data/xin-clabc-banana-corr-raw-2.rda")
ret.corr <- c(ret.corr, ret)
rm(ret)
arr.corr <- array(0, dim=c(40, 3, 10))
for (ind in 1:20) {
  arr.corr[(ind * 2 - 1):(ind * 2), , ] <- ret.corr[[ind]]
}
rm(ret.corr)
gc()

# mean analysis
mu.corr <- apply(arr.corr, c(2, 3), mean)
sd.corr <- apply(arr.corr, c(2, 3), sd)

old <- par(mfrow=c(1, 2))
matplot(3:12, t(mu.corr[1:2, ]), type="l", main="mean", xlab="dimension", ylab="mu")
matplot(3:12, t(sd.corr[1:2, ]), type="l", main="standard deviation", 
        xlab="dimension", ylab="sd")
par(old)