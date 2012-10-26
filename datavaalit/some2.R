library(reshape)

some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")

isot <- c('VAS', 'SDP', 'KOK','VIHR', 'KESK','PS','KD','SKP','RKP', 'PIR','M11')

some <- subset(some, Puolue %in% isot)

csome <- cast(some, Puolue~Media)
csome$TW[which(is.na(csome$TW))]  <- 0
csome$FB[which(is.na(csome$FB))]  <- 0

p <- ggplot(csome, aes(x=log10(FB), y=log10(TW), label=Puolue)) 
p + geom_text(size = 5) + xlab("Facebook-päivitysten määrä (log10)") + ylab("Twitter-päivitysten määrä (log10)")