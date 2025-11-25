library(ODEsensitivity)

sens <- readRDS("Results/SobolLeish.rds")

parnames <- c(sens$V1s$S |> rownames())[-1]
varnames <- names(sens)

library(foreach)

FirstOrd <- lapply(sens, function(x){
  d <- x$S |>t() |> data.frame()
  d$StateVar <- NA
  return(d)
})

Total <- lapply(sens, function(x){
  d <- x$T |>t() |> data.frame()
  d$StateVar <- NA
  return(d)
})

for(i in seq_along(FirstOrd)){
  FirstOrd[[i]]$StateVar <- varnames[i]
  Total[[i]]$StateVar <- varnames[i]
}

FirstOrd.l <- lapply(FirstOrd, function(x){
  reshape2::melt(x, id.vars = c("time", "StateVar"))
})

Total.l <- lapply(Total, function(x){
  reshape2::melt(x, id.vars = c("time", "StateVar"))
})

library(data.table)

FirstOrd.df <- rbindlist(FirstOrd.l)
FirstOrd.df$Sens <- "First Order"
Total.df <- rbindlist(Total.l)
Total.df$Sens <- "Total" 

Total.df$StateVar <- factor(Total.df$StateVar)
Total.df$StateVar <- ordered(Total.df$StateVar,
                             levels = c("V1s", "V1i",
                                        "V2s", "V2i",
                                        "Rs", "Ri",
                                        "Hs", "He", "Hi"))
Total.df$variable <- factor(Total.df$variable)
Total.df$variable <- ordered(Total.df$variable, 
                             levels = c("a1", "a2", "b1", "b2",
                                        "beta.1", "beta.2", "beta.r",
                                        "epsilon", "Kr",
                                        "mu", "mu.h", "mu.r", "n1", "n2",
                                        "omega", "p1", "p2", "r.h", "r.r",
                                        "r1", "r2", "sigma"))

FirstOrd.df$variable <- factor(FirstOrd.df$variable)
FirstOrd.df$variable <- ordered(FirstOrd.df$variable, 
                             levels = c("a1", "a2", "b1", "b2",
                                        "beta.1", "beta.2", "beta.r",
                                        "epsilon", "Kr",
                                        "mu", "mu.h", "mu.r", "n1", "n2",
                                        "omega", "p1", "p2", "r.h", "r.r",
                                        "r1", "r2", "sigma"))
FirstOrd.df$StateVar <- factor(FirstOrd.df$StateVar)
FirstOrd.df$StateVar <- ordered(FirstOrd.df$StateVar,
                             levels = c("V1s", "V1i",
                                        "V2s", "V2i",
                                        "Rs", "Ri",
                                        "Hs", "He", "Hi"))

SensData.df <- rbind(FirstOrd.df, Total.df)

Total.df.max <- subset(Total.df, time == 150.01)
SensData.df.max <- subset(SensData.df, time == 150.01)

library(ggplot2)
library(ggpattern)
library(MetBrewer)

pdf("Results/Sobol-Sens.pdf", width = 11, height = 3)
ggplot(Total.df) + geom_line(aes(x = time, y = value, colour = StateVar)) + 
  facet_grid("~variable") + 
  labs(colour = "", x = "Time", 
       y = "Total sensitivity") +
  scale_colour_met_d("Hiroshige") +
  theme_light() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, vjust = 0.7, size = 5),
        strip.text.x = element_blank())
dev.off()

pdf("Results/FirstTotal-Sobol.pdf", width = 12, height = 6)
ggplot(SensData.df) + geom_line(aes(x = time, y = value, colour = StateVar)) + 
  facet_grid("Sens~variable") + 
  labs(colour = "", x = "Time", y = "Sensitivity") +
  scale_colour_met_d("Hiroshige") +
  theme_light() +
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 60, vjust = 0.7, size = 5))
dev.off()

pdf("Results/Sobol-Sens-FinalT.pdf", width = 12, height = 6)
ggplot(SensData.df.max) + geom_bar(aes(x = StateVar, y = value, fill = StateVar, colour = StateVar), 
                                        stat = "identity", position = position_dodge()) + 
  facet_grid("Sens~variable") + 
  labs(colour = "", fill = "", y = "Sensitivity") +
  scale_fill_met_d("Hiroshige") + scale_colour_met_d("Hiroshige") +
  theme_light() +
  theme(legend.position = "right",
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank())
dev.off()
