some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")

library(ggplot2)
p <- ggplot(some, aes(x = Puolue, y = dat, group = Media)) + geom_bar()

