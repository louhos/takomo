some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")


# Barplot
library(ggplot2)

theme_set(theme_bw(10))
p <- ggplot(some, aes(x = Puolue, y = dat, group = Media)) + geom_bar(stat = "identity") + facet_grid(. ~ Media) + coord_flip()

# Facebook update activity crossplot
some <- some[rev(order(some$dat)),]
#some <- subset(some, Media == "FB")
p <- ggplot(some, aes(y = log10(dat), x = 1:nrow(some), label = Puolue)) + geom_text(size = 3, aes(color = Media)) + ylab("Päivitysten määrä (log10)") + xlab("Puolueen järjestysluku") + ggtitle("Kunnallisvaaliehdokkaiden aktiivisuus sosiaalisessa mediassa")
p