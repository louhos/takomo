# R examples for Sotkanet API
# (C) Opasnet & Louhos 2013 
# Contact: <louhos@googlegroups.com>

# Valitun indikaattorin aikasarjavisualisointi valitulle kunnalle

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

sotkanet.indicators <- SotkanetIndicators(type = "table")
sotkanet.regions <- SotkanetRegions(type = "table")
municipalities <- as.character(unique(subset(sotkanet.regions, region.category == "KUNTA")$region.title.fi))

region <- "Helsinki" # municipalities[[34]]
indicator <- 7 # sotkanet.indicators$indicator[[4]]

# Get Data from Sotkanet
dat <- GetDataSotkanet(indicators = indicator, years = 1990:2012, genders = c('total'))
dat <- dat[dat$region.title.fi == region, ]

# Pick the indicator name and source
indicator.name <- as.character(unique(dat$indicator.title.fi))
indicator.source <- as.character(unique(dat$indicator.organization.title.fi))

# Visualize time series
library(ggplot2)
theme_set(theme_bw(20))
p <- ggplot(dat, aes(x = year, y = primary.value)) 
p <- p + geom_line()
p <- p + ggtitle(paste(indicator.name, indicator.source, sep = "/"))
p <- p + xlab("Vuosi") + ylab("Indikaattorin arvo")
print(p)

