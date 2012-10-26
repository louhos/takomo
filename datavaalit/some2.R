library(reshape)
library(plyr)

some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")

isot <- c('VAS', 'SDP', 'KOK','VIHR', 'KESK','PS','KD','SKP','RKP', 'PIR','M11')

some <- subset(some, Puolue %in% isot)


csome <- cast(some, Puolue~Media)
csome$TW[which(is.na(csome$TW))]  <- 0
csome$FB[which(is.na(csome$FB))]  <- 0

diag <- data.frame(TW=1:max(csome$TW, rm.na=TRUE), FB=1:max(csome$TW, rm.na=TRUE))

p <- ggplot(csome, aes(x=FB, y=TW, label=Puolue)) 
p + geom_text(size = 5) + xlab("Facebook-päivitysten määrä") + 
    ylab("Twitter-päivitysten määrä") + geom_smooth(method = lm) 


# Time series -------------------------------------------------------------

tsome <- read.csv("datavaalit/data/some-updates.csv", sep=";", header=FALSE)
names(tsome) <- c("Nimi", "Puolue", "Kunta", "Sukupuoli", "Media", "Aika", "X")

ttsome <- tsome[1:100000,]
tsome$Aika <- as.factor(as.Date(tsome$Aika)) 

tsome <- subset(tsome, Puolue %in% isot)
tsome$Puolue <- droplevels(tsome$Puolue)

# All social media updates

p <- ggplot(tsome, aes(x = Aika)) 
p + geom_histogram(binaxis = "y", binwidth=0.1) + ylab("lkm") +
    theme(axis.title.x = element_text(size=18),
          axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
          axis.title.y = element_text(size=18),
          axis.text.y  = element_text(size=12),
          strip.text.y = element_text(size=12, face="bold")) +
    facet_grid(Puolue ~ .) + ggtitle("Kaikki some-päivitykset")

# Facebook media updates
tsome.fb <- subset(tsome, Media == 'FB')
p <- ggplot(tsome.fb, aes(x = Aika)) 
p + geom_histogram(binaxis = "y", binwidth=0.1, fill="#3B5998") + ylab("lkm") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size=12, face="bold")) +
  facet_grid(Puolue ~ .) + ggtitle("Kaikki some-päivitykset")

# Twitter media updates
tsome.tw <- subset(tsome, Media == 'TW')
p <- ggplot(tsome.tw, aes(x = Aika)) 
p + geom_histogram(binaxis = "y", binwidth=0.1, fill="#00ACED") + ylab("lkm") +
  theme(axis.title.x = element_text(size=18),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(size=18),
        axis.text.y  = element_text(size=12),
        strip.text.y = element_text(size=12, face="bold")) +
  facet_grid(Puolue ~ .) + ggtitle("Kaikki some-päivitykset")



party_stats  <- ddply(tsome, c(.(Aika), .(Puolue)), summarise,
                      lkm=length(Nimi))

p2 <- ggplot(party_stats, aes(x = Aika, y = lkm, group = Puolue)) 
p2 + geom_line(aes(colour = Puolue))