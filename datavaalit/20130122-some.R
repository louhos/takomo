# Started to code examples with the latest SOME aggregate CSV dump.

# Download and unpack data
download.file("http://www.datavaalit.fi/storage/some-updates-2013-01-22.csv.bz2", destfile = "some-updates-2013-01-22.csv.bz2")
system("bunzip2 some-updates-2013-01-22.csv.bz2")

# Read table and name columns
tab <- read.csv("some-updates-2013-01-22.csv", sep = ";", header = FALSE)
names(tab) <- c("Nimi", "Puolue", "Paikkakunta", "Sukupuoli", "Media", "Aika")


# Pick 2012 updates
tab <- tab[grep("2012", tab$Aika),]


# Most active updaters in social media

media <- "TW"

library(ggplot2)
theme_set(theme_bw(20))

df <- data.frame(list(count = table(subset(tab, Media == media)$Nimi)))
df <- df[order(df$count.Freq),]
df$ind <- 1:nrow(df)
dfs <- df
#dfs <- subset(df, count.Freq > 200)
p <- ggplot(data = dfs, aes(x = ind, y = count.Freq)) + geom_text(label = dfs$count.Var1) + ylab("Updates") + xlim(limit = c(1, max(dfs$ind) + 500))
p


# Updates of the given party in time
#> names(sort(table(tab$Puolue)))
# [1] "SIT"    "E1065"  "YSI"    "Edist." "Saarir" "YL T&T" "SKS"    "STP"   
# [9] "TASI"   "E673"   "YL JS"  "FS GL"  "YLPV"   "ITSP"   "YL MP"  "YL SIT"
#[17] "Ves. s" "YLEP"   "YLSS"   "YL JP"  "KemPar" "YL Myö" "YVV"    "YL1"   
#[25] "Tupu"   "Maa YL" "M11"    "SKP"    "KD"     "PIR"    "PS"     "RKP"   
#[33] "KESK"   "SDP"    "VAS"    "KOK"    "VIHR"  
df <- data.frame(list(count = table(subset(tab, Media == media)$Puolue)))
df <- df[order(df$count.Freq),]
top <- "KOK"
u <- table(as.character(subset(tab, Puolue == top & Media == media)$Aika))
pvm <- sapply(strsplit(names(u), " "), function (x) {x[[1]]})
plot(table(pvm), main = top)

# -------------------

# FB and TW activities correlate

tw <- table(subset(tab, Media == "TW")$Nimi)
fb <- table(subset(tab, Media == "FB")$Nimi)
coms <- intersect(names(tw), names(fb))
tw <- tw[coms]
fb <- fb[coms]
plot(log10(tw), log10(fb))

# -------------------

# Puolue vs. aktiivisuus? (huom: Datawikissä on jotain tästä)
# ks myös yllä on puolueen twiitit aikajanalla
# Kunnallisvaalipiikki puoluettain?

# Sukupuoli vs. aktiivisuus
# Miehet twiittaa, naiset facebookkaa'
# (aktiivisimmat yksilöt voi vääristää kuvaa)
fisher.test(table(tab$Sukupuoli, tab$Media)[-1,])
a <- table(tab$Sukupuoli, tab$Media)[-1,]
(a/sum(a)) / outer(rowSums(a)/sum(a), colSums(a)/sum(a))

