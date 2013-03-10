# R examples for Sotkanet API
# (C) Louhos 2013 http://louhos.github.com

# SOTKAnet REST API on tarkoitettu tietojen noutamiseen erissä
# käytettäväksi eri sovelluksissa. Rajapintaa ei ole tarkoitettu
# suoraan käyttöön.

#SOTKAnetin rajapintaa saa käyttää vapaasti muiden järjestelmien
#tietopohjana. Alueiden ja indikaattorien kuvailutieto
#julkaistaanCreative Commons Attribution 3.0
#-lisenssin<http://creativecommons.org/licenses/by/3.0/> mukaisesti.
#THL tuottamat tilastotiedot ja indikaattorit julkaistaan Creative
#Commons Attribution 3.0 –lisenssin mukaisesti. Muiden kuin THL:n
#tuottamia tilastotietoja ja indikaattoreita saa käyttää vain ja
#ainoastaan erillisen sopimuksen mukaisesti. Erillisessä sopimuksessa
#osapuolina ovat rajapinnan käyttäjä, THL ja aineiston tuottaja.

#Creative Commons Attribution 3.0
#-lisenssin<http://creativecommons.org/licenses/by/3.0/> mukaisesti
#rajapinnan kautta saatua aineistoa käytettäessä on mainittava
#lähteenä SOTKAnet ja tarjottava linkki osoitteeseen
#http://www.sotkanet.fi<http://www.sotkanet.fi/>. Kunkin aineiston
#osalta on mainittava erikseen tilaston tai indikaattorin
#tuottajataho.

#THL ei vastaa rajapintaa käyttävien sovellusten toiminnasta. THL
#tuottaa rajapinnan sellaisenaan ilman takuita. Rajapintaa käytetään
#omalla vastuulla.

# Install and load libraries
if (try(library(devtools)) == "try-error") {install.packages("devtools")}
library(devtools)

install_github("sorvi", "louhos", ref = "develop")
library(sorvi) # http://louhos.github.com/sorvi
#source("~/Louhos/sorvi/R/sotkanet.R")

if (try(library(xtable)) == "try-error") {install.packages("xtable")}
library(xtable)

# Specify indicators to retrieve
sotkanet.indicators <- SotkanetIndicators()
idx <- grep("opiskelijoista", sotkanet.indicators[,2])
inds <- c(2616, sotkanet.indicators[idx, "indicator"]) # Lisää keskiväkiluku

# Get Sotkanet data table for selected indicators
sotkanet.df <- GetDataSotkanet(indicators = inds, years = 1990:2013, genders = c("total"), region.category = "KUNTA")

# Recommended to avoid queries by retrieving the data only once 
save(sotkanet.df, file = "sotkanet.rda", compress = "xz")
#load("sotkanet.rda")

# Get province info
sotkanet.regions <- SotkanetRegions(type = "table")
sotkanet.kunnat <- subset(sotkanet.regions, region.category == "KUNTA")
maakunnat <- subset(sotkanet.regions, region.category == "MAAKUNTA")
sotkanet.regions.raw <- SotkanetRegions("raw")
kunnat <- sotkanet.regions.raw[sapply(sotkanet.regions.raw, function (x) {x$category == "KUNTA"})]
kunta.maakunta <- lapply(kunnat, function (x) {x$memberOf})
names(kunta.maakunta) <- sapply(kunnat, function (x) {x$title[["fi"]]})

# Plot Motion Chart using googleVis -package
if (try(library(googleVis)) == "try-error") { install.packages("googleVis") }
library(googleVis)

df <- sotkanet.df[, c("region.title.fi", "indicator.title.fi", "year", "primary.value")]
colnames(df) <- c("Kunta", "Muuttuja", "Vuosi", "arvo")
df$Kunta <- as.character(df$Kunta)
df$Muuttuja <- as.character(df$Muuttuja)

# Keep only those variables that have time series of 10 years or more
keep <- names(which(sapply(split(df, df$Muuttuja), function (x) {length(unique(x$Vuosi))}) >= 10))
df <- subset(df, Muuttuja %in% keep)

# Pick data such that all variables are taken across the same time span (shared years)
coms <- unique(subset(df, Muuttuja == df$Muuttuja[[1]])$Vuosi); 
sapply(split(df, df$Muuttuja), function (x) {coms <<- intersect(coms, unique(x$Vuosi))})
df <- subset(df, Vuosi %in% coms)

# Form a motion chart from example data NOTE: the data set must be given
# as data.frame which can contain NUMERIC and CHARACTER fields (NO
# FACTORS, NOR LOGICAL variables!).  The FIRST FOUR FIELDS must be
# provided in the following order: idvar, timevar, two numeric fields,
# then any number of numeric and character fields
# 
# NOTE: the plot shows only the first time point.  Replace the time field
# with a constant to plot all in one figure using df$time <- rep(1,
# nrow(df))

if (try(library(reshape)) == "try-error") {install.packages("reshape")}
library(reshape)
dfm <- melt(df, id = c("Kunta", "Vuosi", value = "arvo"))
dfm$variable <- as.character(dfm$variable)
dfm$value <- as.character(dfm$value)
dfm$variable <- NULL
dfr <- reshape(dfm, timevar = "value", idvar = c("Kunta", "Vuosi"), direction = "wide")
colnames(dfr) <- gsub("arvo\\.", "", colnames(dfr))

# Add province information
dfr$Maakunta <- as.character(sapply(as.character(dfr$Kunta), function (kunta) {maakunnat[na.omit(match(kunta.maakunta[[kunta]], maakunnat$region)), "region.title.fi"]}))

# Plot a Motion Chart using googleVis -package
mchart <- gvisMotionChart(dfr, idvar="Kunta", timevar="Vuosi", colorvar = "Maakunta", sizevar = "Väestö, keskiväkiluku", options=list(height=600, width=700))

# Plot immediately (opens in browser)
plot(mchart)

# Save as html (needs javascript to open!)
print(mchart, file="Sotkanet.html")


