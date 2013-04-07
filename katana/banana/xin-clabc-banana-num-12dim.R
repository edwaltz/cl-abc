# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 50000000
p <- 12
b <- c(.01, .01, .01)  # bananacity

ptm.final <- proc.time()  # time record

jobs <- lapply(b, function(x) mcparallel(run.corr(20, x, p, num)))
ret <- mccollect(jobs)
save(ret, file="xin-clabc-banana-num-12dim-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])