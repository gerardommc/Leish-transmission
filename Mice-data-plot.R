library(readxl)
library(ggplot2)
library(lubridate)

mice.data <- read_xlsx("Data/Rodent-populations.xlsx")
mice.data$Date <- dmy(paste0('0',1, ifelse(mice.data$Month < 10, '0', ''), mice.data$Month,'-', mice.data$Year))

p <-ggplot(mice.data) + geom_line(aes(x = Date, y = Density, linetype = as.factor(Paper))) +
  facet_wrap(~ Spp) +
  labs(x = "year", y = "Individuals/ha") +
  theme_light() +
  theme(strip.text.x = element_text(face = "italic"),
        axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = "none")


pdf("Results/Mice-density.pdf", width = 9, height = 4)
p
dev.off()
