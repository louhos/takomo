# R examples for Sotkanet API
# (C) Einari Happonen, Opasnet & Louhos 2013 <louhos@googlegroups.com>
# Forked and modified from http://en.opasnet.org/w/Sandbox#Sotkanet
# by Leo Lahti 8.3.2013

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

#install_github("sorvi", "louhos", ref = "master")
install_github("sorvi", "louhos", ref = "develop")
library(sorvi) # http://louhos.github.com/sorvi
#source("~/Louhos/sorvi/R/sotkanet.R")

if (try(library(xtable)) == "try-error") {install.packages("xtable")}
library(xtable)

# List all indicators in Sotkanet database
sotkanet.indicators <- SotkanetIndicators(type = "table")

# Sotkanet regions
sotkanet.regions <- SotkanetRegions(type = "table")

# Get data for a given indicator
indicator.index <- 10013
dat <- GetDataSotkanet(indicators = indicator.index, years = 1990:2012, genders = c('female', 'male'), region.category = "EUROOPPA", region = "Suomi")

# Pick subset of the data and indicator name
indicator.name <- as.character(unique(dat$indicator.title.fi))

# Visualize
if (try(library(ggplot2)) == "try-error") {install.packages("ggplot2")}
library(ggplot2); 
theme_set(theme_bw(15)); 
p <- ggplot(dat, aes(x = year, y = primary.value, group = gender, color = gender)) 
p <- p + geom_line() 
p <- p + ggtitle(paste(indicator.name, sep = " / "))

# Investigate results
print(head(dat))
print(p)


