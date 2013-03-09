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

if (try(library(xtable)) == "try-error") {install.packages("xtable")}
library(xtable)

# List all indicators in Sotkanet database
sotkanet.indicators.table <- SotkanetCollect(SotkanetIndicators(), "indicator")

# Get Sotkanet data table for randomly selected indicators
set.seed(3556)
inds <- sample(sotkanet.indicators.table$indicator, 3)
sotkanet.df <- GetDataSotkanet(indicators = inds, years = 1990:2013, genders = c("total"))


# Plot Motion Chart using googleVis -package
if (try(library(googleVis)) == "try-error") {install.packages("googleVis")}
library(googleVis)

df <- sotkanet.df[, c("regionResult", "indicatorResult", "year", "primary.value")]
colnames(df) <- c("Alue", "Muuttuja", "Vuosi", "arvo")
df$Alue <- as.character(df$Alue)
df$Muuttuja <- as.character(df$Muuttuja)


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
dfm <- melt(df, id = c("Alue", "Vuosi", value = "arvo"))
dfm$variable <- as.character(dfm$variable)
dfm$value <- as.character(dfm$value)
dfm$variable <- NULL
dfr <- reshape(dfm, timevar = "value", idvar = c("Alue", "Vuosi"), direction = "wide")
colnames(dfr) <- gsub("arvo\\.", "", colnames(dfr))

# Plot a Motion Chart using googleVis -package
mchart <- gvisMotionChart(dfr, idvar="Alue", timevar="Vuosi", options=list(height=600, width=700))

# Plot immediately (opens in browser)
plot(mchart)

# Save as html (needs javascript to open!)
print(mchart, file="Sotkanet.html")




