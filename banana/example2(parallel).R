# Toy example of "banana" - parallel.
# Initial settings.
source("./banana/utils.R")
library(parallel)

lim.x <- c(-100, 100)
lim.y <- c(-100, 100)
num <- 10000000
p <- 3
b <- c(0, .01, .05)  # bananacity

ptm.final <- proc.time()  # time record
jobs <- lapply(b, function(x) mcparallel(get.corr(x, p, num)))
ret <- mccollect(jobs)
gc()
cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])

old <- par(mfrow=c(1, 2))
plot(ret[[1]]$unadj, pch=".", main="b=0, 3-dim, unadj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[1]]$adjust, pch=".", main="b=0, 3-dim, adj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)

old <- par(mfrow=c(1, 2))
plot(ret[[2]]$unadj, pch=".", main="b=0.01, 3-dim, unadj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[2]]$adjust, pch=".", main="b=0.01, 3-dim, adj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)

old <- par(mfrow=c(1, 2))
plot(ret[[3]]$unadj, pch=".", main="b=0.01, 3-dim, unadj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
plot(ret[[3]]$adjust, pch=".", main="b=0.01, 3-dim, adj.", xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
par(old)