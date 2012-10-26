library(reshape)

some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")

csome <- cast(some, Puolue~Media)
csome$TW[which(is.na(csome$TW))]  <- 0

p <- ggplot(csome, aes(x=FB, y=TW, color=Puolue)) + geom_point()
p