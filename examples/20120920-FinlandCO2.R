

library(ggplot2)
library(WDI)

k <- 27
indi <- WDIsearch()[k,1]
titl <- WDIsearch()[k,2]

DF <- WDI(country="all", indicator=indi, start=1990, end=2008)

library(ggplot2)
p <- ggplot(data = subset(DF, country == "Finland")) + aes(x = year, y = EN.ATM.CO2E.KD.GD) + theme_set(theme_bw(20)) + geom_line() + opts(title = titl)

png("FinlandCO2.png")
print(p)
dev.off()
