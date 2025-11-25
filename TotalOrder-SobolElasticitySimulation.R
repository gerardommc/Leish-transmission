library(foreach)
library(deSolve)
library(ODEsensitivity)

epsilon <- c(0.13 , 0.01 , 0.03 , 0.01 , 0.01 , 0.04)

params <- list(
  r1 =0.22, r2 =0.22,
  beta.1 = 0.174, beta.2 = 0.174,
  a1=0.14, a2 = 0.5,
  b1 = 14, b2 = 2.5,
  r.r = 0.035, r.h = 0.0002,
  beta.r = 0.048, 
  n1 = 4, epsilon = median(epsilon),
  n2 = 0.83,
  p1 = 0.01, p2 = 0.2,
  Kr = 1500, 
  omega = 1/21, sigma = 1/20, 
  mu = 1/15, mu.r = 1 / 180,
  mu.h = 1/(70 * 365)
)

eta.1 <- 60 * params$b1 + 1500 * params$a1
eta.2 <- 60 * params$b2 + 1500 * params$a2

initState <- c(V1s = eta.1, V1i = 0, 
               V2s = eta.2, V2i = 0, 
               Rs = 1499, Ri = 1,
               Hs = 60, He = 0, Hi = 0)
t = seq(0.01, 150.01, by = 1)

par.inf <- sapply(params, function(x){x*0.80})
par.sup <- sapply(params, function(x){x*1.2})
par.names <- names(params)

set.seed(1234)

source("ModelFunction.R")

sensit <- ODEsobol(mod = leish,
                   pars = par.names,
                   state_init = initState,
                   times = t, 
                   n = 500,
                   rfuncs = "runif",
                   rargs = c(paste0("min = ", par.inf,
                                    ", max = ", par.sup)),
                   sobol_method = "Martinez",
                   ode_method = "lsoda",
                   parallel_eval = TRUE,
                   parallel_eval_ncores = 4)

saveRDS(sensit, "Results/SobolLeish.rds")


