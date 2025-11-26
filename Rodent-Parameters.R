df <- read.csv("Data/Rodent-populations-parameters.csv")

library(ggplot2)

ggplot(df, aes(x = Time, y = Density, colour = Spp)) + geom_point() + 
  geom_path( ) +
  geom_smooth(method = "loess")

pdf("Results/Rodent-raw-data.pdf", width = 6, height = 4)
ggplot(df, aes(x = Time, y = Density, colour = Spp)) + geom_point() + 
  geom_path( ) +
  geom_smooth(method = "loess")
dev.off()

hg <- subset(df, Spp == "Heteromys gaumeri")
oph <- subset(df, Spp == "Ototylomys phyllotis")
py <- subset(df, Spp == "Peromyscus yucatanicus")

hg.e <- subset(hg, Density < max(Density)/2)
hg.l <- subset(hg, Density > max(Density)/2)

oph.e <- subset(oph, Density < max(Density)/2)
oph.l <- subset(oph, Density > max(Density)/2)

py.e <- subset(py, Density < max(Density)/2)
py.l <- subset(py, Density > max(Density)/2)

#capacidad de carga promedio

mean(hg.l$Density)
mean(oph.l$Density)
mean(py.l$Density)

#Tasa de crecimiento

hg.e.dn <- (hg.e$Density[-nrow(hg.e)] - hg.e$Density[-1])/(hg.e$Time[-nrow(hg.e)]*30 - hg.e$Time[-1]*30)
oph.e.dn <- (oph.e$Density[-nrow(oph.e)] - oph.e$Density[-1])/(oph.e$Time[-nrow(oph.e)]*30 - oph.e$Time[-1]*30)
py.e.dn <- (py.e$Density[-nrow(py.e)] - py.e$Density[-1])/(py.e$Time[-nrow(py.e)]*30 - py.e$Time[-1]*30)

mean(hg.e.dn)
mean(oph.e.dn)
mean(py.e.dn)

param <- data.frame(Spp = c("Heteromys gaumeri", "Ototylomys phyllotis", "Peromyscus yucatanicus"),
                    r = c(mean(hg.e.dn),
                          mean(oph.e.dn),
                          mean(py.e.dn)),
                    K = c(mean(hg.l$Density),
                          mean(oph.l$Density),
                          mean(py.l$Density)))
write.csv(param, "Results/Rodent-parameters.csv", row.names = F)
