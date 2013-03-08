# R examples for Sotkanet API
# Forked and modified from http://en.opasnet.org/w/Sandbox#Sotkane
# antagomir 8.3.2013

# SOTKAnet REST API on tarkoitettu tietojen noutamiseen erissä
# käytettäväksi eri sovelluksissa. Rajapintaa ei ole tarkoitettu suoraan
# käyttöön.

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
#rajapinnan kautta saatua aineistoa käytettäessä on mainittava lähteenä
#SOTKAnet ja tarjottava linkki osoitteeseen
#http://www.sotkanet.fi<http://www.sotkanet.fi/>. Kunkin aineiston
#osalta on mainittava erikseen tilaston tai indikaattorin tuottajataho.

#THL ei vastaa rajapintaa käyttävien sovellusten toiminnasta. THL
#tuottaa rajapinnan sellaisenaan ilman takuita. Rajapintaa käytetään
#omalla vastuulla.

library(sorvi) # http://louhos.github.com/sorvi
library(xtable)

# List all indicators in Sotkanet database
sotkanet.indicators <- SotkanetIndicators()
sotkanet.indicators.table <- SotkanetCollect(sotkanet.indicators, "indicator")
# Print as HTML: print(xtable(indicator.table), type = 'html')

# Sotkanet regions
sotkanet.regions <- SotkanetRegions()
sotkanet.regions.table <- SotkanetCollect(sotkanet.regions, "region")

# Get Sotkanet data table
# 1990-2012 currently available
dat <- GetDataSotkanet(indicator = 127, years = 2011, genders = 'female')
#print(xtable(dat),type='html')


