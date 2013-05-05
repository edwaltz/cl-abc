# clean data

# 1000 particles for b = 0, 0.03 and 0.05.
unadj.corr <- array(0, dim=c(3, 4, 100))
adj.corr <- array(0, dim=c(3, 4, 100))

# 3 dim
load("./data/banana/1000/xin-clabc-banana-corr-3dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 1, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 1, ]  <-  ret[[ind]][, 2]
}
# 6 dim
load("./data/banana/1000/xin-clabc-banana-corr-6dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 2, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 2, ]  <-  ret[[ind]][, 2]
}
# 9 dim
load("./data/banana/1000/xin-clabc-banana-corr-9dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 3, ]  <- ret[[ind]][, 1]
  adj.corr[ind, 3, ]  <-  ret[[ind]][, 2]
}
# 12 dim
load("./data/banana/1000/xin-clabc-banana-corr-12dim-raw.rda")
for (ind in 1:3) {
  unadj.corr[ind, 4, ]  <- c(ret[[2 * ind - 1]][, 1], ret[[2 * ind]][, 1])
  adj.corr[ind, 4, ]  <-  c(ret[[2 * ind - 1]][, 2], ret[[2 * ind]][, 2])
}
rm(ret)

save(unadj.corr, adj.corr, file="./data/banana/clabc-banana-corr.rda")


# 1000, 5000 and 10,000 particles for b = 0.03.
load("./data/banana/clabc-banana-corr.rda")
unadj.num <- array(0, dim=c(3, 4, 100))
adj.num <- array(0, dim=c(3, 4, 100))

# 1000 particles.
unadj.num[1, , ] = unadj.corr[2, , ]
adj.num[1, , ] = adj.corr[2, , ]

# 5000 particles.
load("./data/banana/5000/xin-clabc-banana-num-3dim-raw.rda")
for (ind in 1:10) {
  unadj.num[2, 1, (1 + 10 * (ind - 1)):(10 * ind)] = ret[[ind]][, 1]
  adj.num[2, 1, (1 + 10 * (ind - 1)):(10 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/5000/xin-clabc-banana-num-6dim-raw.rda")
for (ind in 1:5) {
  unadj.num[2, 2, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 1]
  adj.num[2, 2, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/5000/xin-clabc-banana-num-9dim-raw.rda")
for (ind in 1:4) {
  unadj.num[2, 3, (1 + 25 * (ind - 1)):(25 * ind)] = ret[[ind]][, 1]
  adj.num[2, 3, (1 + 25 * (ind - 1)):(25 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/5000/xin-clabc-banana-num-12dim-raw.rda")
for (ind in 1:3) {
  unadj.num[2, 4, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 1]
  adj.num[2, 4, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/5000/xin-clabc-banana-num-12dim2-raw.rda")
for (ind in 1:2) {
  unadj.num[2, 4, 60 + (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 1]
  adj.num[2, 4, 60 + (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 2]
}

# 10,000 particles.
load("./data/banana/10000/xin-clabc-banana-num2-3dim-raw.rda")
for (ind in 1:5) {
  unadj.num[3, 1, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 1]
  adj.num[3, 1, (1 + 20 * (ind - 1)):(20 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/10000/xin-clabc-banana-num2-6dim-raw.rda")
for (ind in 1:2) {
  unadj.num[3, 2, (1 + 50 * (ind - 1)):(50 * ind)] = ret[[ind]][, 1]
  adj.num[3, 2, (1 + 50 * (ind - 1)):(50 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/10000/xin-clabc-banana-num2-9dim-raw.rda")
for (ind in 1:2) {
  unadj.num[3, 3, (1 + 50 * (ind - 1)):(50 * ind)] = ret[[ind]][, 1]
  adj.num[3, 3, (1 + 50 * (ind - 1)):(50 * ind)] = ret[[ind]][, 2]
}

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw.rda")
unadj.num[3, 4, 1:20] = ret[, 1]
adj.num[3, 4, 1:20] = ret[, 2]

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw2.rda")
unadj.num[3, 4, 21:40] = ret[, 1]
adj.num[3, 4, 21:40] = ret[, 2]

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw31.rda")
unadj.num[3, 4, 41:50] = ret[, 1]
adj.num[3, 4, 41:50] = ret[, 2]

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw32.rda")
unadj.num[3, 4, 51:60] = ret[, 1]
adj.num[3, 4, 51:60] = ret[, 2]

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw4.rda")
unadj.num[3, 4, 61:80] = ret[, 1]
adj.num[3, 4, 61:80] = ret[, 2]

load("./data/banana/10000/xin-clabc-banana-num2-12dim-raw5.rda")
unadj.num[3, 4, 81:100] = ret[, 1]
adj.num[3, 4, 81:100] = ret[, 2]

save(unadj.num, adj.num, file="./data/banana/clabc-banana-num.rda")