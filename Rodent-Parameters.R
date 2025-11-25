df <- read.csv("Data/Rodent-populations-parameters.csv")

library(ggplot2)

ggplot(df, aes(x = Tiempo, y = Densidad, colour = Spp)) + geom_point() + 
  geom_path( ) +
  geom_smooth(method = "loess")

hg <- subset(df, Spp == "Heteromys gaumeri")
oph <- subset(df, Spp == "Ototylomys phyllotis")
py <- subset(df, Spp == "Peromyscus yucatanicus")

hg.e <- subset(hg, Densidad < max(Densidad)/2)
hg.l <- subset(hg, Densidad > max(Densidad)/2)

oph.e <- subset(oph, Densidad < max(Densidad)/2)
oph.l <- subset(oph, Densidad > max(Densidad)/2)

py.e <- subset(py, Densidad < max(Densidad)/2)
py.l <- subset(py, Densidad > max(Densidad)/2)

#capacidad de carga promedio

mean(hg.l$Densidad)
mean(oph.l$Densidad)
mean(py.l$Densidad)

#Tasa de crecimiento

hg.e.dn <- (hg.e$Densidad[-nrow(hg.e)] - hg.e$Densidad[-1])/(hg.e$Tiempo[-nrow(hg.e)]*30 - hg.e$Tiempo[-1]*30)
oph.e.dn <- (oph.e$Densidad[-nrow(oph.e)] - oph.e$Densidad[-1])/(oph.e$Tiempo[-nrow(oph.e)]*30 - oph.e$Tiempo[-1]*30)
py.e.dn <- (py.e$Densidad[-nrow(py.e)] - py.e$Densidad[-1])/(py.e$Tiempo[-nrow(py.e)]*30 - py.e$Tiempo[-1]*30)

mean(hg.e.dn)
mean(oph.e.dn)
mean(py.e.dn)

param <- data.frame(Spp = c("Heteromys gaumeri", "Ototylomys phyllotis", "Peromyscus yucatanicus"),
                    r = c(mean(hg.e.dn),
                          mean(oph.e.dn),
                          mean(py.e.dn)),
                    K = c(mean(hg.l$Densidad),
                          mean(oph.l$Densidad),
                          mean(py.l$Densidad)))
write.csv(param, "Results/Rodent-parameters.csv", row.names = F)
