n <- 41.1
pv <- 0.22
m1 <- 0.089
m2 <- 0.03
m3 <- 0.108
mu2 <- 0.017
mu3 <- 0.053
mu4 <- 0.017

L <- matrix(c(-m1, 0, 0, n*pv,
              m1, -m2 - mu2, 0, 0,
              0, m2, -m3 - mu3, 0,
              0, 0, m3, -mu4), nrow = 4, ncol = 4, byrow = T)


eigen(L)$values
