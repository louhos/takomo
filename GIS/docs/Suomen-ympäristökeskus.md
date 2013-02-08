[Suomen ympäristökeskuksen](http://www.ymparisto.fi/syke) [OIVA]( www.ymparisto.fi/oiva)-palvelu sisältää lukuisia avoimia aineistoja. Aineistot ovat saatavilla sekä latauspalvelun että WMS-rajapinnan kautta. OIVA vaati ilmaisen käyttäjätunnuksen luomista, WMS-palvelu ei edellytä tunnusta. 

SYKE-aineistojen käytön edistämiseksi suunnitteilla on lisäksi [[WMS/WFS-rajapinta|wmswfs]] R-kielelle.

[Esimerkkejä](http://louhos.wordpress.com/2011/12/28/paikkatieto-rajapinnat-ja-sorvi-osa-1/) näiden aineistojen käytöstä löytyy Louhos-blogista. Seuraava esimerkkikoodi hakee rasterimuotoisia kartta-aineistoja Suomen ympäristökeskuksen (SYKE) OIVA-palvelusta (WMS). Aloitetaan lataamalla tarvittavat ainestot:
 


```r
library(sorvi)  # http://louhos.github.com/sorvi/asennus.html
library(rgdal)  # See instructions http://louhos.github.com/sorvi/asennus.html
library(raster)

# MML:n kuntadata ladataan soRvista
sp <- LoadMML(data.id = "kunta1_p", resolution = "1_milj_Shape_etrs_shape")
```

```
## Error: cannot open the connection
```

```r

# Listaa kaikki soRvi:ssa olevat WMS urlit
wms.urls <- ListWMSurls()

# Jokaiselle WMS URLille (ts. palvelulle) on luotava oma WMS-olio.  Corine
# 2006 maankäyttöluokat
corine.wms <- PreprocessWMS(url = LoadWMSurl(provider = "OIVA", service = "Corine"))
```

```
## Error: Could not get capabilities for WMS Error: 1: failed to load HTTP
## resource
```

```r

# Natura2000-suojelualueet
suojelu.wms <- PreprocessWMS(url = LoadWMSurl(provider = "OIVA", service = "Suojelu"))
```

```
## Error: Could not get capabilities for WMS Error: 1: failed to load HTTP
## resource
```

```r

# Pohjavesialueet
pohjavesi.wms <- PreprocessWMS(url = LoadWMSurl(provider = "OIVA", service = "Pohjavesi"))
```

```
## Error: Could not get capabilities for WMS Error: 1: failed to load HTTP
## resource
```

```r

# Ortoilmakuvat
ortoilma.wms <- PreprocessWMS(url = LoadWMSurl(provider = "OIVA", service = "Image"))
```

```
## Error: Could not get capabilities for WMS Error: 1: failed to load HTTP
## resource
```

```r

# Erotellaan kunta-aineistosta Lahden polygoni
sp.lahti <- sp[which(sp@data$Kunta_ni1 == "Lahti"), ]
```

```
## Error: missing value where TRUE/FALSE needed
```

```r

# Haetaan Corine rasteri Lahden alueelta HUOM: jos aineistoa haetaan
# suuren tai useamman kunnan alueelta, on syytä käyttää pienempää
# resoluutiota kuin 25x25m (esim 100x100m) HUOM: toistaiseksi WMS-palvelun
# karttakerroksia (layer) ei voi kysellä soRvin kautta, vaan kerroksen
# nimi pitää tietää (ominaisuus tulossa)
corine.lahti <- GetWMSraster(WMS = corine.wms, layer = "CorineLandCover2006_25m", 
    extent = sp.lahti, resolution = 25)
```

```
## Error: object 'corine.wms' not found
```

```r

# Liitetään rasterin 3 kaistaa yhdeksi (RGB)
brick.corine.lahti <- brick(corine.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'brick': Error: object 'corine.lahti' not found
```


### Rasteriaineistojen visualisointi

Käytä raster-paketin plotRGB:tä visualisointiin:


```r
plotRGB(brick.corine.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plotRGB': Error: object 'brick.corine.lahti' not found
```

```r
# Lisätään kuntaraja
plot(sp.lahti, add = TRUE, lwd = 2)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plot': Error: object 'sp.lahti' not found
```


### Ortoilmakuvien visualisointi


```r
image.lahti <- GetWMSraster(WMS = ortoilma.wms, layer = "Image2006mosaiikki", 
    extent = sp.lahti, resolution = 25)
```

```
## Error: object 'ortoilma.wms' not found
```

```r

brick.image.lahti <- brick(image.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'brick': Error: object 'image.lahti' not found
```

```r
plotRGB(brick.image.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plotRGB': Error: object 'brick.image.lahti' not found
```

```r
plot(sp.lahti, add = TRUE, lwd = 2)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plot': Error: object 'sp.lahti' not found
```



### Natura2000-alueiden visualisointi


```r
natura.lahti <- GetWMSraster(WMS = suojelu.wms, layer = "ProtectedSites.Natura2000Polygons", 
    extent = sp.lahti, resolution = 25)
```

```
## Error: object 'suojelu.wms' not found
```

```r

brick.natura.lahti <- brick(natura.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'brick': Error: object 'natura.lahti' not found
```

```r
plotRGB(brick.natura.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plotRGB': Error: object 'brick.natura.lahti' not found
```

```r
plot(sp.lahti, add = TRUE, lwd = 2)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plot': Error: object 'sp.lahti' not found
```


### Pohjavesialueiden visualisointi


```r
pohjavesi.lahti <- GetWMSraster(WMS = pohjavesi.wms, layer = "Pohjavesialue", 
    extent = sp.lahti, resolution = 25)
```

```
## Error: object 'pohjavesi.wms' not found
```

```r

brick.pohjavesi.lahti <- brick(pohjavesi.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'brick': Error: object 'pohjavesi.lahti' not found
```

```r
plotRGB(brick.pohjavesi.lahti)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plotRGB': Error: object 'brick.pohjavesi.lahti' not found
```

```r
plot(sp.lahti, add = TRUE, lwd = 2)
```

```
## Error: error in evaluating the argument 'x' in selecting a method for
## function 'plot': Error: object 'sp.lahti' not found
```




