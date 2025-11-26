library(deSolve)
library(foreach)

epsilon <- c(0.13 , 0.01 , 0.03 , 0.01 , 0.01 , 0.04)

params <- list(
  r1 =0.22, r2 =0.22, mu = 1/15,
  beta.1 = 0.174, beta.2 = 0.174,
  a1=0.14, a2 = 0.5,
  b1 = 14, b2 = 2.5,
  r.r = 0.035, r.h = 0.0002, mu.r = 1 / 180,
  beta.r = -mean(c(4, 0.83))*log(1-median(epsilon)), 
  n1 = 4, epsilon = max(epsilon),
  n2 = 0.83,
  p1 = 0.01, p2 = 0.2,
  Kr = 15000, 
  omega = 1/21, sigma = 1/20, 
  mu.h = 1/(70 * 365)
)

Hs <- 330

eta.1 <- c(0, Hs * params$b1 + 1500 * params$a1)
eta.2 <- c(0, Hs * params$b2 + 1500 * params$a2)

etas <- expand.grid(eta.1 = eta.1,
                    eta.2 = eta.2)
etas$Scenario <- c("Both absent",
                   "Only Lu. cruciata",
                   "Only B. olmeca",
                   "Both present")
etas <- subset(etas, Scenario != "Both absent")

y0 <- c(V1s = NA, V1i = 0, 
        V2s = NA, V2i = 0, 
        Rs = 1500, Ri = 1,
        Hs = Hs, He = 0, Hi = 0)

t = 0:2500

source("ModelFunction.R")

out <- foreach(i = 1:nrow(etas), .combine = rbind) %do% {
  y0["V1s"] <- etas$eta.1[i]
  y0["V2s"] <- etas$eta.2[i]
  
  df <- lsoda(y = y0, times = t, parms = params, func = leish) |> as.data.frame()
  df$Scenario <- as.factor(etas$Scenario[i])
  return(df)
}

library(ggplot2)

out.df <- data.frame(out) |> na.omit()
names(out.df) <- c("t", "V1s", "V1i", "V2s", "V2i", "Rs", "Ri", "Hs", "He", "Hi", "Scenario")

out.df$Humans <- with(out.df, Hi/(Hi + Hs + He))
out.df$Rodents <- with(out.df, Ri/(Ri + Rs))
out.df$`Lu. cruciata` <- with(out.df, V1i/(V1i + V1s))
out.df$`B. olmeca` <- with(out.df, V2i/(V2i + V2s))

out.prev <- subset(out.df, select = c("t","Humans",
                                      "Rodents",
                                      "Lu. cruciata",
                                      "B. olmeca",
                                      "Scenario"))

out.prev.l <- reshape2::melt(out.prev, id.vars = c("t", "Scenario")) |> na.omit()

library(RColorBrewer)

p <- ggplot(out.prev.l) + geom_line(aes(x = t, y = (value), linetype = variable)) +
  scale_colour_brewer(palette = "Set2") + 
  facet_wrap(~Scenario) + 
  labs(x = "Time", y = "Prevalence", linetype = "") +
  theme_light() +
  theme(legend.position = "bottom")

p

pdf("Results/Sand-fly-scenarios-SupplMat.pdf", width = 12, height = 5)
p
dev.off()

