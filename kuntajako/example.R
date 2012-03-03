library(rgdal)
library(RColorBrewer)

# sp
download.file("http://roihu.info/temp/sorvi/data/kunnat.RData", destfile = "kunnat.RData")
load("kunnat.RData")

# Color new municipalities with 6 colors but keep old borders visible

# Assign one of the ncol colors on each factor level
ncol <- 12
nlevels <- length(levels(sp$uusi.kunta))
col.regions <- rep(brewer.pal(ncol, "Paired"), ceiling(nlevels/ncol))[1:nlevels]

pic <- spplot(sp, "uusi.kunta", col.regions = col.regions, colorkey = FALSE)

print(pic)

