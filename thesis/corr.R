# correlation analysis
# 1000 particles
# load data and data clean
unadj.corr <- array(0, dim=c(3, 4, 100))
adj.corr <- array(0, dim=c(3, 4, 100))

# 3 dim
load("./data/banana/1000/xin-clabc-banana-corr-3dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 1, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 1, ]  <-  ret[[ind]][, 2]
}
# 6 dim
load("./data/banana/1000/xin-clabc-banana-corr-6dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 2, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 2, ]  <-  ret[[ind]][, 2]
}
# 9 dim
load("./data/banana/1000/xin-clabc-banana-corr-9dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 3, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 3, ]  <-  ret[[ind]][, 2]
}
# 12 dim
load("./data/banana/1000/xin-clabc-banana-corr-12dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 4, ]  <- c(ret[[2 * ind - 1]][, 1], ret[[2 * ind]][, 1])
  adj.corr[ind, 4, ]  <-  c(ret[[2 * ind - 1]][, 2], ret[[2 * ind]][, 2])
}
rm(ret)

# mean and sd analysis
unadj.mu <- apply(unadj.corr, c(1, 2), mean)
unadj.sd <- apply(unadj.corr, c(1, 2), sd)
adj.mu <- apply(adj.corr, c(1, 2), mean)
adj.sd <- apply(adj.corr, c(1, 2), sd)

# plot
# unadjustment
old <- par(mfrow=c(1, 2))
matplot(seq(3, 12, by=3), t(unadj.mu), type="l", main="mean", xlab="dimension", ylab="mu")
matplot(seq(3, 12, by=3), t(unadj.sd), type="l", main="standard deviation", xlab="dimension", ylab="sd")
par(old)
# adjustment
old <- par(mfrow=c(1, 2))
matplot(seq(3, 12, by=3), t(adj.mu), type="l", main="mean", xlab="dimension", ylab="mu")
matplot(seq(3, 12, by=3), t(adj.sd), type="l", main="standard deviation", xlab="dimension", ylab="sd")
par(old)

# 5000 particles
# load data and clean data
unadj2.corr <- matrix(0, nrow=3, ncol=100)
adj2.corr <- matrix(0, nrow=3, ncol=100)
# 3 dim
load("./data/banana/5000/xin-clabc-banana-num-3dim-raw.rda")
for (ind in 1:10) {
  unadj2.corr[1, ((ind - 1) * 10 + 1):(ind * 10)]  <- ret[[ind]][, 1]
  adj2.corr[1, ((ind - 1) * 10 + 1):(ind * 10)]  <- ret[[ind]][, 2]
}
# 6 dim
load("./data/banana/5000/xin-clabc-banana-num-6dim-raw.rda")
for (ind in 1:5) {
  unadj2.corr[2, ((ind - 1) * 20 + 1):(ind * 20)]  <- ret[[ind]][, 1]
  adj2.corr[2, ((ind - 1) * 20 + 1):(ind * 20)]  <- ret[[ind]][, 2]
} 
# 9 dim
load("./data/banana/5000/xin-clabc-banana-num-9dim-raw.rda")
for (ind in 1:4) {
  unadj2.corr[3, ((ind - 1) * 25 + 1):(ind * 25)]  <- ret[[ind]][, 1]
  adj2.corr[3, ((ind - 1) * 25 + 1):(ind * 25)]  <- ret[[ind]][, 2]
} 

# mean and sd analysis
unadj2.mu <- apply(unadj2.corr, 1, mean)
unadj2.sd <- apply(unadj2.corr, 1, sd)
adj2.mu <- apply(adj2.corr, 1, mean)
adj2.sd <- apply(adj2.corr, 1, sd)

# plot
# unadjustment
old <- par(mfrow=c(1, 2))
plot(c(3, 6, 9), unadj.mu[2, 1:3], type="l", main="mean", xlab="dimension", ylab="mu")
lines(c(3, 6, 9), unadj2.mu, lty=2)
plot(c(3, 6, 9), unadj.sd[2, 1:3], type="l", main="sd", xlab="dimension", ylab="mu")
lines(c(3, 6, 9), unadj2.sd, lty=2)
par(old)
# adjustment
old <- par(mfrow=c(1, 2))
plot(c(3, 6, 9), adj.mu[2, 1:3], type="l", main="mean", xlab="dimension", ylab="mu")
lines(c(3, 6, 9), adj2.mu, lty=2)
plot(c(3, 6, 9), adj.sd[2, 1:3], type="l", main="sd", xlab="dimension", ylab="mu")
lines(c(3, 6, 9), adj2.sd, lty=2)
par(old)