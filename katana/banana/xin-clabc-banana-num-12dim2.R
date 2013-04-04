# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 20000000
p <- 12
b <- c(0, .01, .05)  # bananacity

ptm.final <- proc.time()  # time record

jobs <- lapply(b, function(x) mcparallel(run.corr(10, x, p, num)))
ret <- mccollect(jobs)
save(ret, file="xin-clabc-banana-num-12dim2-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])