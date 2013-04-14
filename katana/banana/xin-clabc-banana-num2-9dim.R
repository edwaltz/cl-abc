# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 100000000
p <- 9
b <- c(.01, .01)  # bananacity

ptm.final <- proc.time()  # time record

jobs <- lapply(b, function(x) mcparallel(run.corr(50, x, p, num)))
ret <- mccollect(jobs)
save(ret, file="xin-clabc-banana-num-9dim2-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])