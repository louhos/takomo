# This script is posted to the Louhos-blog
# http://louhos.wordpress.com
# Copyright (C) 2008-2011 Juuso Parkkinen <juuso.parkkinen@gmail.com>. All rights reserved.

# Tested with soRvi version 0.1.42
# Asennusohjeet: http://sorvi.r-forge.r-project.org/asennus.html

# This program is open source software; you can redistribute it and/or modify
# it under the terms of the FreeBSD License (keep this notice):
# http://en.wikipedia.org/wiki/BSD_licenses

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

# Use the XML library to load the name data from the Rantamarton result pages
# Main page: http://beta.marathon.se/racetimer?v=%252Fsv%252Frace%252Fshow%252F672%253Flayout%253Dmarathon
library(XML)

# Links to men's and women's 10km results
urls <- list(m10a = "http://www.racetimer.se/fi/race/resultlist/672?layout=marathon&page=1&rc_id=3733")
urls$m10b <- "http://www.racetimer.se/fi/race/resultlist/672?layout=marathon&page=2&rc_id=3733"
urls$w10 <- "http://www.racetimer.se/fi/race/resultlist/672?layout=marathon&page=1&rc_id=3749"

# Load and combine data
men <- rbind(readHTMLTable(urls$m10a)[["top3-list"]], readHTMLTable(urls$m10b)[["top3-list"]])
women <- readHTMLTable(urls$w10)[["top3-list"]]
results <- rbind(men, women)
results$Gender <- factor(c(rep("Miehet", nrow(men)), rep("Naiset", nrow(women))), levels=c("Naiset", "Miehet"))

# Fix result running time format
times.raw <- strsplit(as.vector(results$Netto), split=":")
fix.time <- function(time.raw) {
time.raw <- as.numeric(time.raw)
if (length(time.raw)==2)
return(time.raw[1] + time.raw[2]/60)
else
return(60 + time.raw[2] + time.raw[3]/60)
}
results$Time <- sapply(times.raw, fix.time)

# Plot histogram of running times
library(ggplot2)
p <- ggplot(results, aes(x=Time, colour=Gender))
p <- p + geom_histogram(binwidth=1)
p <- p + facet_grid(Gender ~ .)
p <- p + xlab("Juoksuaika (min)") + ylab("Määrä")
p <- p + scale_x_continuous(limits=c(min(results$Time), 80), breaks=seq(35,80,5))
p <- p + scale_colour_discrete(legend=FALSE)
#ggsave("Aaltokymppi2011_ajat_20110925.png", plot=p)
png("Aaltokymppi2011_ajat_20110925.png")
print(p)
dev.off()


