# Toy example of "banana".
# Initial settings.
setwd("/home/edwaltz/Documents/R/cl-abc")
source("./banana/utils.R")

# constant
num <- 5000000
tol <- .001
n <- num*tol
total <- 50  # total looping times

pmin <- -100
pmax <- 100
lim.x <- c(pmin, pmax)
lim.y <- c(pmin, pmax)

# run the algorithm.
b <- c(0, .01, .05)  # bananacity
# d.par & d.summ should have the same length.
d.par <- c(2, 2, 2, 2)
d.summ <- c(3, 5, 7, 9)

ret.cor <- array(0, dim=c(total, length(b), length(d.summ)))  # correlation matrix

for (times in 1:total) {
  res <- list()
  pdf(paper="a4")
  old <- par(mfrow=c(length(b), length(d.summ)))
  for (ind in 1:length(b)) {
    for (ind2 in 1:length(d.summ)) {
      obs <- matrix(0, nrow=1, ncol=d.summ[ind2], byrow=TRUE)
      res[[(ind-1)*5+ind2]] <- clabc.step(num, d.par[ind2], obs, tol, "pair", b=b[ind])$par[, 1:2]
      ret.cor[times, ind, ind2] <- cor(res[[(ind-1)*5+ind2]])[1, 2]
      # draw picture
      title <- paste0(d.summ[ind2], " dim, B=", b[ind], ", cor=", sprintf("%.4f.", ret.cor[times, ind, ind2]))
      plot(res[[(ind-1)*5+ind2]], pch=".", main=title, xlab="theta1", ylab="theta2", xlim=lim.x, ylim=lim.y)
      gc()
    }
  }
  dev.off()
  rm(res)
  gc()
}