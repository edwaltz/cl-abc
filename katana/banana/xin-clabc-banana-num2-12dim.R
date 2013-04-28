# Time test for correlation analysis.
source("xin-clabc-banana-library.R")

num <- 100000000
p <- 12
b <- .01  # bananacity

ptm.final <- proc.time()  # time record
ret <- run.corr(total=20, b=b, p=p, num=num)

save(ret, file="xin-clabc-banana-num2-12dim-raw.rda")
rm(ret)

gc()

cost.final <- proc.time() - ptm.final
print(cost.final["elapsed"])