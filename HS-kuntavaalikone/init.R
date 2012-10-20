# Install dependencies

tmp <- try(require(psych))
if (tmp == "try-error") {install.packages("psych"); require(psych)}

tmp <- try(require(GPArotation))
if (tmp == "try-error") {install.packages("GPArotation"); require(GPArotation)}

tmp <- try(require(ks))
if (tmp == "try-error") {install.packages("ks"); require(ks)}

# ------------------------------------------------------------------

# Load visualization functions
source('visualization.R') 

# ------------------------------------------------------------------

# Luetaan siivottu HS-data, missä profiilit sarakkeissa 1-8
# vastaukset kysymyksiin sarakkeissa 9-18 pisteillä 1-5 (HS:n numerointi)
# csv-tiedoston merkistökoodaus utf-8
ehdokas <- read.csv2('ehdokasvastaukset.csv')

# Haetaan ensimmäisen 8 kysymyksen arvot erilliseen matriisiin
# Kysymykset 9-10 mittaavat "vihreyttä", mikä ei kiinnosta tällä kertaa
# Voit kokeilla myös niiden kanssa; 
# vihreys korreloi suomessa liberaaliuteen, joten akselit menevät sekaisin
val <- ehdokas[,9:16]

# Ehdokkaiden nimet
nimet <- paste(ehdokas[,4], ehdokas[,3])

# -------------------------------------------------------------------

# Sailyta vain ne rivit joissa on vastattu kaikkiin kysymyksiin
keep <- rowSums(is.na(val)) == 0
val <- val[keep,]
ehdokas <- ehdokas[keep,]
nimet <- nimet[keep]

# Lisaa pieni maara kohinaa, jottei pisteet mene paallekkain
val <- val + array(rnorm(prod(dim(val)), sd = 0.05), dim = dim(val))

# -------------------------------------------------------------------

# Värjätään kiinnostavat puolueet
p.lev <- levels(ehdokas[,2])
p.col <- rep('#FFFFFF00', length(p.lev)) # oletus: läpinäkyvä
p.col[p.lev=='KOK'] <- '#0000FF'
p.col[p.lev=='SDP'] <- '#FF0000'
p.col[p.lev=='KESK'] <- '#00FF00'
p.col[p.lev=='PS'] <- '#000000'
p.col[p.lev=='VIHR'] <- '#00AAAA' # punavihersokeiden pelastukseksi!
p.col[p.lev=='VAS'] <- '#9F1E24'
p.col[p.lev=='RKP'] <- '#FBD700'
p.col[p.lev=='KD'] <- '#E58E00'
e.col <- p.col[ehdokas[,2]]

