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

# Select data to visualize
year <- 2011
indicator <- 7

# Get Data from Sotkanet
dat <- GetDataSotkanet(indicators = indicator, years = year, genders = c('total'))

# Pick the indicator name and source
indicator.name <- as.character(unique(dat$indicator.title.fi))
indicator.source <- as.character(unique(dat$indicator.organization.title.fi))

# Load MML municipality map data
sp <- LoadMML(data.id = "kunta4_p", resolution = "4_5_milj_shape_etrs-tm35fin")

# Match municipality names between shape (map) object and indicator data
# and add indicator data to the shape object
sp[["indicator"]] <- dat[match(sp@data$Kunta.FI, dat$region.title.fi), "primary.value"]

# Replace NAs by 0
sp[["indicator"]][is.na(sp[["indicator"]])] <- 0

# Visualize the indicator on Finnish map
q <- PlotShape(sp, "indicator", main = paste(indicator.name, indicator.source, sep = "/"))
print(q)

