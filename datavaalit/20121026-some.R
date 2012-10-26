library(reshape)
library(plyr)
library(ggplot2)

some <- read.csv("http://www.datavaalit.fi/storage/some-updates-stats-2012-10-26.csv", sep = ",")
names(some) <- c("Puolue", "Media", "dat")

isot <- list('KD'=1870, 'KESK'=8401,'KOK'=6874, 'PIR'=121, 'PS'=4394, 
             'RKP'=1350, 'SDP'=6987, 'SKP'=304, 'VAS'=3506,'VIHR'=2298)

some <- subset(some, Puolue %in% names(isot))

csome <- cast(some, Puolue~Media)
csome$TW[which(is.na(csome$TW))]  <- 0
csome$FB[which(is.na(csome$FB))]  <- 0
csome$Puolue <- droplevels(csome$Puolue)

# NOTE: order of isot matters!!!
csome$TWfr <- csome$TW / unlist(isot[csome$Puolue])
csome$FBfr <- csome$FB / unlist(isot[csome$Puolue])

# Customize ggplot2 theme
theme_custom <- ggplot2::theme_bw(20)
theme_custom$axis.title.x <- element_text(size=18)
theme_custom$axis.text.x <- element_text(angle=90, vjust=0.5, size=12)
theme_custom$axis.title.y <- element_text(angle=90, size=18)
theme_custom$axis.text.y <- element_text(angle=90, size=12)
theme_custom$strip.text.y <- element_text(size=12, face="bold")
theme_set(theme_custom)

p <- ggplot(csome, aes(x=FB, y=TW, label=Puolue)) 
p <- p + geom_text(size = 5) + xlab("Facebook-päivitysten määrä") + 
    ylab("Twitter-päivitysten määrä") + geom_smooth(method = lm)
print(p)

p <- ggplot(csome, aes(x=log10(FBfr), y=log10(TWfr), label=Puolue)) 
p <- p + geom_text(size = 5) + xlab("log10(Facebook-päivitysten määrä)") + 
    xlim(c(-1, 2)) + ylim(c(-1, 2)) +
    ylab("log10(Twitter-päivitysten määrä)") +
    ggtitle("Normalisoidut some-päivitysten määrät")
print(p)


# Time series -------------------------------------------------------------

# theme_set(theme_bw(20))

#tsome <- read.csv("datavaalit/data/some-updates.csv", sep=";", header=FALSE)
temp <- tempfile()
download.file("http://www.datavaalit.fi/storage/some-updates-2012-10-26.zip", temp)
tsome <- read.csv(unz(temp, "some-updates.csv"), sep=";", header=FALSE)
unlink(temp)
names(tsome) <- c("Nimi", "Puolue", "Kunta", "Sukupuoli", "Media", "Aika", "X")

tsome$Aika <- as.factor(as.Date(tsome$Aika)) 

tsome <- subset(tsome, Puolue %in% names(isot))
tsome$Puolue <- droplevels(tsome$Puolue)

tsome.fb <- subset(tsome, Media == 'FB')
tsome.tw <- subset(tsome, Media == 'TW')

# All social media updates
p <- ggplot(tsome, aes(x = Aika)) 
p <- p + geom_histogram(binaxis = "y", binwidth=0.1) + ylab("lkm") +
    facet_grid(Puolue ~ .) + ggtitle("Kaikki some-päivitykset")
print(p)

# Facebook updates

p <- ggplot(tsome.fb, aes(x = Aika)) 
p <- p + geom_histogram(binaxis = "y", binwidth=0.1, fill="#3B5998") + ylab("lkm") +
  facet_grid(Puolue ~ .) + ggtitle("Facebook päivitykset")
print(p)

# Twitter updates
p <- ggplot(tsome.tw, aes(x = Aika)) 
p <- p + geom_histogram(binaxis = "y", binwidth=0.1, fill="#00ACED") + ylab("lkm") +
  facet_grid(Puolue ~ .) + ggtitle("Twitter päivitykset")
print(p)

party_stats  <- ddply(tsome, c(.(Aika), .(Puolue)), summarise,
                      lkm=length(Nimi),
                      ehd=unlist(isot[Puolue][1]))
party_stats$lkmfr <- party_stats$lkm / party_stats$ehd

p2 <- ggplot(party_stats, aes(x = Aika, y = lkmfr, group = Puolue)) 
p2 <- p2 + geom_line(aes(colour = Puolue))
print(p2)


# All social media updates
p <- ggplot(party_stats, aes(x = Aika, y=lkmfr)) 
p <- p + geom_bar() + ylab("lkm") + ggtitle("Kaikki some-päivitykset")
p <- p + facet_grid(Puolue ~ .) 
#p + facet_grid(Puolue ~ ., scales="free_y")
print(p)

# Facebook updates 
party_stats_fb  <- ddply(tsome.fb, c(.(Aika), .(Puolue)), summarise,
                         lkm=length(Nimi),
                         ehd=unlist(isot[Puolue][1]))
party_stats_fb$lkmfr <- party_stats_fb$lkm / party_stats_fb$ehd

p <- ggplot(party_stats_fb, aes(x = Aika, y=lkmfr)) 
p <- p + geom_bar(fill="#3B5998") + ylab("lkm") + ggtitle("Facebook päivitykset")
p <- p + facet_grid(Puolue ~ .) 
#p + facet_grid(Puolue ~ ., scales="free_y")
print(p)

# Twitter updates 
party_stats_tw  <- ddply(tsome.tw, c(.(Aika), .(Puolue)), summarise,
                         lkm=length(Nimi),
                         ehd=unlist(isot[Puolue][1]))

party_stats_tw$lkmfr <- party_stats_tw$lkm / party_stats_tw$ehd

p <- ggplot(party_stats_tw, aes(x = Aika, y=lkmfr)) 
p <- p + geom_bar(fill="#00ACED") + ylab("lkm") + facet_grid(Puolue ~ .) + ggtitle("Twitter päivitykset")
print(p)

