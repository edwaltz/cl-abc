# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 10000000
p <- 12
b <- c(0, 0, .01, .01, .05, .05)  # bananacity

ptm.final <- proc.time()  # time record

jobs <- lapply(b, function(x) mcparallel(run.corr(50, x, p, num)))
ret <- mccollect(jobs)
save(ret, file="xin-clabc-banana-corr-12dim-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])