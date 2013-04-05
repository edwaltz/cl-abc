# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 200000000
p <- 9
b <- .01  # bananacity

ptm.final <- proc.time()  # time record

ret <- get.corr(b, p, num)
save(ret, file="xin-clabc-banana-num-test3-9dim.rda")

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])