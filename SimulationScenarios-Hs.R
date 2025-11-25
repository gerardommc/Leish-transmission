library(deSolve)
library(foreach)

epsilon <- seq(0, 0.1, len = 10)

params <- list(
  r1 =0.15, r2 =0.15,
  beta.1 = 0.174, beta.2 = 0.174,
  a1=0.14, a2 = 0.5,
  b1 = 14, b2 = 2.5,
  r.r = 0.03, r.h = 0.0002,
  beta.r = 0.048, 
  beta.h1 = NA, beta.h2 = NA,
  epsilon = NA
  n1 = 4,
  n2 = 0.83,
  p1 = 0.01, p2 = 0.2,
  Kr = 1500, 
  omega = 1/21, sigma = 1/20, 
  mu = 1/15, mu.r = 1 / 180,
  mu.h = 1/(70 * 365)
)

Hs <- c(60, 330, 600)

eta.1 <- Hs * params$b1 + 1500 * params$a1
eta.2 <- Hs * params$b2 + 1500 * params$a2

y0 <- c(V1s = NA, V1i = 0, 
        V2s = NA, V2i = 0, 
        Rs = 1500, Ri = 1,
        Hs = NA, He = 0, Hi = 0)

t = 0:150

source("ModelFunction.R")

out <- foreach(i = seq_along(Hs), .combine = rbind) %do% {
  foreach(ii = seq_along(epsilon), .combine = rbind) %do% {
    beta.h1 <- - 4 * log(1 - mean(epsilon))
    beta.h2 <- - 0.83 * log(1 - mean(epsilon))

    y0["Hs"] <- Hs[i]
    y0["V1s"] <- eta.1[i]
    y0["V2s"] <- eta.2[i]
  
    beta.h1 <- - 4 * log(1 - epsilon[ii])
    beta.h2 <- - 0.83 * log(1 - epsilon[ii])

    params$beta.h1 <- beta.h1
    params$beta.h2 <- beta.h2
    params$epsilon <- epsilon[ii]
    
    df <- lsoda(y = y0, times = t, parms = params, func = leish) |> as.data.frame()

    df$Hs.init <- Hs[i]
    df$epsilon <- epsilon[ii]

    return(df)
  }
}

out.df <- data.frame(out)
names(out.df) <- c("t", "V1s", "V1i", "V2s", "V2i", "Rs", "Ri", "Hs", "He", "Hi", "Hs.init", "epsilon")

out.df$Prevalence <- with(out.df, Hi/(Hi + Hs + He))

out.10 <- subset(out.df, t %in% seq(0, 150, by = 15))

library(RColorBrewer)
colr <- colorRampPalette(Spectral.colors, space = "rgb")

library(lattice)

wireframe(Prevalence ~ t + epsilon | Hs.init, out.10, shade = F,  
  default.scales = list(distance = c(5, 5, 5), arrows = F), 
  col.regions = colr(100), drape = T,
  scales = list(y = list(at = round(seq(0.01, 0.13, len = 4), 3),
  labels = round(seq(0.01, 0.13, len = 4), 3))),
  xlab = "Time", ylab = "E", zlab = "")

pdf("Results/Hs-init.pdf", width = 5.5, height = 5)
  wireframe(Prevalence ~ t + epsilon | Hs.init, out.10, shade = F,  
      default.scales = list(distance = c(5, 5, 5), arrows = F), 
      col.regions = colr(100), drape = T,
      scales = list(y = list(at = round(seq(0.01, 0.13, len = 4), 3),
      labels = round(seq(0.01, 0.13, len = 4), 3))),
      xlab = "Time", ylab = "E", zlab = "")
dev.off()
