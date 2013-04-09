# Time test for correlation analysis.
source("xin-clabc-banana-library.R")
library(parallel)

num <- 100000000
p <- 12
b <- c(.01, .01, .01, .01, .01)  # bananacity

ptm.final <- proc.time()  # time record
cl <- makeCluster(5)
ret <- parLapply(b, run.corr, total=20, p=p, num=num)
stopCluster(cl)

save(ret, file="xin-clabc-banana-num2-12dim-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])