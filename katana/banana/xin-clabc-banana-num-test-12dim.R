# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 50000000
p <- 12
b <- .01  # bananacity

ptm.final <- proc.time()  # time record

ret <- get.corr(b, p, num)
save(ret, file="xin-clabc-banana-num-test-12dim.rda")

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])