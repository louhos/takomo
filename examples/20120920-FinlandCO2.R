# Load libraries

if (!try(require(ggplot2))) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!try(require(WDI))) {
  install.packages("WDI")
  library(WDI)
}

# --------------------------

# Select investigated variable
k <- 27
indi <- WDIsearch()[k,1]
titl <- WDIsearch()[k,2]

# Get the data
DF <- WDI(country="all", indicator=indi, start=1990, end=2008)

# Visualize
theme_set(theme_bw(20)) 
p <- ggplot(data = subset(DF, country == "Finland")) + aes(x = year, y = EN.ATM.CO2E.KD.GD) + geom_line() + ggtitle(titl) + xlab("Year") + ylab("CO2 emissions")

# Store into file by uncommenting #
#png("FinlandCO2.png")
print(p)
#dev.off()


