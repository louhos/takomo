

Kuntatiedon visualisointi Suomen kartalla R-kielellä nousi kuluneella viikolla keskusteluun Finnish Open Data Ecosystem-foorumin [Facebook-ketjussa](https://www.facebook.com/groups/fi.okfn/permalink/10153486329250628/).

Tähän on monta tapaa, ja valmiiden välineiden käyttö helpottaa tehtävää. Suomalaisen paikkatiedon visualisointiin suunnatussa [gisfin](https://github.com/rOpenGov/gisfin)-paketissa on valmiit wrapperit kuntatiedon visualisointiin. [Päivitä paketin kehitysversio Githubista](https://github.com/rOpenGov/gisfin/blob/master/vignettes/gisfin_tutorial.md) saadaksesi kaikki toiminnot käyttöön.

Kokosimme tähän blogipostaukseen tiiviit esimerkit. Lisää suomalaisen paikkatiedon R-työkaluista löytyy gisfin-paketin [tutoriaalisivulta](https://github.com/rOpenGov/gisfin/blob/master/vignettes/gisfin_tutorial.md), jossa kuntien lisäksi saatavilla on mm. maakuntarajat, postinumeroalueet, äänestysalueet, aluejakokartat, Helsingin aluekarttoja, väestöruututietoja, spatiaalisia tilastoja ym. 




### Esimerkkidata: THL:n sairastavuusindeksi

Käytämme esimerkkinä kuntatason aineistojen visualisoinnissa
sairastavuusindeksiä, jonka voi ladata [THL:n
sotkanet-palvelusta](http://www.sotkanet.fi/sotkanet/fi/index) R:ään
[sotkanet](https://github.com/rOpenGov/sotkanet)-paketilla.


```r
# List all available indicators from THL Sotkanet
library(sotkanet) 
sotkanet.indicators <- SotkanetIndicators(type = "table")

# Check specific indicators for 'sairastavuusindeksi'
# sotkanet.indicators[grep("sairastavuusindeksi", sotkanet.indicators$indicator.title.fi),]

# This shows that the index 244 is THL sairastavuusindeksi.
# Retrieve THL sairastavuusindeksi 2010 for municipalities
# (newer information is not available???)
healthindex <- GetDataSotkanet(indicators = 244, year = 2010, region.category = "KUNTA")

# Let us rename the value field for clarity
healthindex$Sairastavuusindeksi <- healthindex$primary.value
```


### Kuntarajojen lataus Maanmittauslaitokselta

Lataa kuntarajat R:ään seuraavalla komennolla. Tässä esimerkissä käytämme [valmiiksi esikäsiteltyjä Maanmittauslaitoksen kuntarajoja 2013](https://github.com/avoindata/mml) (Yleiskartta-1000 Hallintoalueet). Muiden MML-aineistojen lataamiseen perehdy funktioon get_mml.


```r
library(gisfin)
map <- get_municipality_map(data.source = "MML")
```

Toinen vaihtoehto olisi ladata kuntarajat [GADM-palvelusta](http://gadm.org/country). GADMin kuntakartta sisältää tällä hetkellä vanhentunutta tietoa, joten sivuutamme sen tässä esimerkissä. GADMin kartoista voi kuitenkin olla apua muiden alueitten visualisoinnissa.


### Tietojen liittäminen kuntakarttaan


Liitä THL:n sairastavuusindeksin tiedot MML:n kuntakarttaan
kuntakoodin nojalla (MML-datassa "kuntakoodi" ja healthindex-datassa
"region.code"):


```r
sp <- sp::merge(map, healthindex, by.x = "kuntakoodi", by.y="region.code")
```


### Visualisointi Maanmittauslaitoksen kuntarajoilla

Nyt datan voi visualisoida Suomen kuntakartalla sp-paketilla:


```r
# Define the 20% quantiles for plotting
quantiles <- quantile(sp$Sairastavuusindeksi, probs = seq(0, 1, .2))

# Discretize the data. This is easier to comprehend than continuous scale.
# And I could not find how to change color scale in spplot for continuous scale -
# suggestions are welcome.
sp$Sairastavuusindeksi2 <- cut(sp$Sairastavuusindeksi, quantiles)

# Define blue-white-red palette 
library(leaflet)
palette <- colorNumeric(c("blue", "white", "red"), domain = range(quantiles), na.color = "black")

# Visualize:
spplot(sp, zcol = "Sairastavuusindeksi2", colorkey = TRUE,
	   main = "THL ikävakioimaton sairastavuusindeksi 2010 (Sotkanet-indeksi 244)",
	   col.regions = palette(quantiles))
```

![plot of chunk 20150606-mmlplot](figure/20150606-mmlplot-1.png) 


### Nopea visualisointi

Voit vaihtoehtoisesti käyttää aluedatan nopeaan visualisointiin räätälöityä wrapperiä. Tämä tuottaa ggplot2-objektin, jonka visualisointiparametreja voi halutessasi säätää lisää tarpeen mukaan:


```r
p <- region_plot(sp, color = "Sairastavuusindeksi", region = "kuntakoodi")
print(p)
```

![plot of chunk 20150606-mmlplot2b](figure/20150606-mmlplot2b-1.png) 

### Vuorovaikutteinen kuntakartta

Karttojen vuorovaikutteiseen visualisointiin soveltuvat ainakin [rMaps](http://rmaps.github.io/) ja [leaflet](https://rstudio.github.io/leaflet/)-paketit. Seuraava esimerkki tulostaa leaflet-paketin avulla selaimeen vuorovaikutteisen kuntakartan [Dmitry Poletaevin koodia mukaillen](https://github.com/finKeva/DataKuntakartalle/blob/master/dataKuntakartalle.r). Lopputulos muistuttaa [Dmitryn RPubs-esimerkkiä](http://rpubs.com/welxo88/kela_tth_IvsIIsuhde):


```r
# Define the region and color variables and title:
region <- "kuntanimi"
color <- "Sairastavuusindeksi"
main <- "THL:n Sairastavuusindeksi"

# Define color palette
palette <- colorNumeric(c("blue", "white", "red"), NULL)

# Define the text for popup box
state_popup <- paste0("<strong>Kunta: </strong>", sp[[region]], 
                        "<br><strong>", main, ": </strong>", 
                        round(sp@data[,c(color)], digits=2))

# Generate interactive visualization
p <- leaflet(data = sp) %>%
       addTiles() %>% 
       addPolygons(fillColor = ~palette(get(color)), 
                fillOpacity = 0.7, 
                color = "#000000", 
                weight = 1,
                popup = state_popup)

# Open in browser
print(p)
```


rOpenGov-pakettien kehitys tapahtuu vapaaehtoispohjalta. Pakettien toiminnallisuuteen, dokumentaatioon, tutoriaaliin tai liittyvät [kontribuutiot ovat tervetulleita](https://github.com/rOpenGov/gisfin). Julkaisemme mieluusti täydennyksiä tähän postaukseen sekä muita R-kielen käyttöä käsitteleviä blogikirjoituksia.

