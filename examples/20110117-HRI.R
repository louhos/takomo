library(sorvi)

setwd("/home/jlehtoma/Data/Seutukartta/pk_seudun_aanestysalueet/pk_seudun_aanestysalueet/shapefiles/")
kkj2 <- "+init=epsg:2392"
files <- list.files(pattern="*[.shp]$")

sp.kunnat <- ReadShape(files=files, proj4string=kkj2)

# TODO 1: read in the additional information form the csv's
# TODO 2: use spChFIDS on the distrcit codes to change the fids

sp.helsinki <- sp.kunnat$Helsinki_aanestysalueet
sp.espoo <- sp.kunnat$Espoo_aanestysalueet
sp.espoo$KUNTA <- "092"

sp.pks <- spRbind(sp.helsinki, sp.espoo)

spplot(sp.helsinki, "KUNTA")
spplot(sp.espoo, "KUNTA")