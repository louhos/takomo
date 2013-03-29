# R examples for Sotkanet API
# (C) Opasnet & Louhos 2013 
# Contact: <louhos@googlegroups.com>

# Sotkanetin kuntaindikaattoreiden visualisointi Suomen kartalla

# -------------------------------------------------------------

# SOTKAnet REST API on tarkoitettu tietojen noutamiseen erissä
# käytettäväksi eri sovelluksissa. Rajapintaa ei ole tarkoitettu
# suoraan käyttöön.

# SOTKAnetin rajapintaa saa käyttää vapaasti muiden järjestelmien
# tietopohjana. Alueiden ja indikaattorien kuvailutieto
# julkaistaanCreative Commons Attribution 3.0
# -lisenssin<http://creativecommons.org/licenses/by/3.0/> mukaisesti.
# THL tuottamat tilastotiedot ja indikaattorit julkaistaan Creative
# Commons Attribution 3.0 –lisenssin mukaisesti. Muiden kuin THL:n
# tuottamia tilastotietoja ja indikaattoreita saa käyttää vain ja
# ainoastaan erillisen sopimuksen mukaisesti. Erillisessä sopimuksessa
# osapuolina ovat rajapinnan käyttäjä, THL ja aineiston tuottaja.

# Creative Commons Attribution 3.0
# -lisenssin<http://creativecommons.org/licenses/by/3.0/> mukaisesti
# rajapinnan kautta saatua aineistoa käytettäessä on mainittava
# lähteenä SOTKAnet ja tarjottava linkki osoitteeseen
# http://www.sotkanet.fi<http://www.sotkanet.fi/>. Kunkin aineiston
# osalta on mainittava erikseen tilaston tai indikaattorin
# tuottajataho.

# THL ei vastaa rajapintaa käyttävien sovellusten toiminnasta. THL
# tuottaa rajapinnan sellaisenaan ilman takuita. Rajapintaa käytetään
# omalla vastuulla.

# -------------------------------------------

# Install and load libraries
if (try(library(devtools)) == "try-error") {install.packages("devtools")}
library(devtools)
install_github("sorvi", "louhos", ref = "master")
library(sorvi) # http://louhos.github.com/sorvi

# Get Complete Sotkanet Data for municipalities
#source("sotkanet.download.R")
load("sotkanet.Mon-Mar-25-14:24:01-2013.RData") #datlist

# Get province info
sotkanet.regions <- SotkanetRegions("table")
maakunnat <- subset(sotkanet.regions, region.category == "MAAKUNTA")

# Match municipalities and provinces
kunnat <- SotkanetRegions("raw") 
kunta.maakunta <- lapply(kunnat, function (x) {x$memberOf})
names(kunta.maakunta) <- sapply(kunnat, function (x) {x$title[["fi"]]})

# List all indicators in Sotkanet database
sotkanet.indicators <- SotkanetIndicators(type = "table")

# Pick THL indicators
thl.indicators <- as.character(unique(subset(sotkanet.indicators, indicator.organization.title.fi == "Terveyden ja hyvinvoinnin laitos (THL)")$indicator))

# Select indicators that concern municipalities; ignore gender
municipal.data <- lapply(datlist, function (tab) {subset(tab, region.category == "KUNTA" & gender == "total")})

# Select indicators with the longest time series
long.indicators <- names(which(sapply(municipal.data, function (tab) {length(unique(tab$year))}) == 22))

# Combine all indicators
sotkanet <- do.call("rbind", municipal.data[long.indicators])

# Pick some indicators for closer inspection
selected.indicators <- c("Väestö, keskiväkiluku", 
			 "Yksityisten lääkäripalvelujen kustannukset, 1 000 euroa",
			 "16-24 -vuotiaat, % väestöstä",
 			 "Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä",      
			 "Alkoholijuomien myynti asukasta kohti 100 %:n alkoholina, litraa")

# Construct data table
df <- subset(sotkanet, indicator.title.fi %in% selected.indicators)
df <- df[, c("region.title.fi", "indicator.title.fi", "year", "primary.value", "absolute.value")]
colnames(df) <- c("Kunta", "Muuttuja", "Vuosi", "primary.value", "absolute.value")
df$Kunta <- as.character(df$Kunta)
df$Muuttuja <- as.character(df$Muuttuja)

# Ensure all variables have the same time span (shared years)
coms <- unique(subset(df, Muuttuja == df$Muuttuja[[1]])$Vuosi); 
df <- subset(df, Vuosi %in% coms)

# Construct motion chart 
# Data set must be given as data.frame which can contain NUMERIC and 
# CHARACTER fields (NO FACTORS, NOR LOGICAL variables!). 
# The FIRST FOUR FIELDS must be in the following order: 
# idvar, timevar, two numeric fields,
# then any number of numeric and character fields 

if (try(library(reshape)) == "try-error") {install.packages("reshape")}
library(reshape)
dfm <- melt(df, id = c("Kunta", "Vuosi", value = "primary.value"))
dfm$variable <- as.character(dfm$variable)
dfm$value <- as.character(dfm$value)
dfm$variable <- NULL
dfm <- dfm[!is.na(dfm$value), ]
dfr <- reshape(dfm, timevar = "value", idvar = c("Kunta", "Vuosi"), direction = "wide")
colnames(dfr) <- gsub("primary\\.value\\.", "", colnames(dfr))

# Add province information
dfr$Maakunta <- as.character(sapply(as.character(dfr$Kunta), function (kunta) {maakunnat[na.omit(match(kunta.maakunta[[kunta]], maakunnat$region)), "region.title.fi"]}))

# Plot Motion Chart with googleVis
if (try(library(googleVis)) == "try-error") { install.packages("googleVis") }
library(googleVis)
mchart <- gvisMotionChart(dfr, idvar="Kunta", timevar="Vuosi", colorvar = "Maakunta", options=list(height=600, width=700))

# Plot immediately (opens in browser)
plot(mchart)

# Save as html (needs javascript to open!)
print(mchart, file="Sotkanet.html")

