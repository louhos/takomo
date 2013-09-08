# Script for processing SotkaNet data for Demos

# Juuso Parkkinen, 2013/09

## PREPARE ############

# Install and load sorvi package
# library(devtools)
# install_github("sorvi", "louhos", ref = "develop")
library(sorvi) # http://louhos.github.com/sorvi
library(RCurl)
library(XML)
library(ggplot2)

# Folders for data and output
data.folder <- "/Users/juusoparkkinen/Documents/workspace/Rdrafts/demos/sotkanet/"

## PROCESS REGION INFORMATION ############

# Get province and municipality info
sotkanet.regions <- SotkanetRegions("table")
kunnat <- droplevels(subset(sotkanet.regions, region.category == "KUNTA"))
maakunnat <- droplevels(subset(sotkanet.regions, region.category == "MAAKUNTA"))
seutukunnat <- droplevels(subset(sotkanet.regions, region.category == "SEUTUKUNTA"))

# Match municipalities and provinces
regions.raw <- SotkanetRegions("raw")
regions.id <- sapply(regions.raw, function(x) x$id)
kunnat$maakunta.region <- sapply(regions.raw[match(kunnat$region, regions.id)], function(x) x$memberOf[x$memberOf %in% maakunnat$region])
kunnat$seutukunta.region <- sapply(regions.raw[match(kunnat$region, regions.id)], function(x) x$memberOf[x$memberOf %in% seutukunnat$region])

kunnat$maakunta.title.fi <- maakunnat$region.title.fi[match(kunnat$maakunta.region, maakunnat$region)]
kunnat$seutukunta.title.fi <- seutukunnat$region.title.fi[match(kunnat$seutukunta.region, seutukunnat$region)]

# Get Finnish region id's for gvisGeoChart
temp <- readHTMLTable("http://en.wikipedia.org/wiki/ISO_3166-2:FI")
fincodes <- temp[[1]][,1:2]
names(fincodes)[2] <- "Maakunta"
levels(fincodes$Maakunta) <- iconv(levels(fincodes$Maakunta), from="UTF-8", to="ISO-8859-1")
levels(fincodes$Maakunta)[1] <- "Ahvenanmaa"
kunnat <- merge(kunnat, fincodes, by.x="maakunta.title.fi", by.y="Maakunta")

# Get province map data from MML

mk.sp <- LoadMML(data.id = "maaku4_p", resolution = "4_5_milj_shape_etrs-tm35fin")
mk.df <- fortify(mk.sp, region="Maaku_ni1")
mk.df$Maakunta <- factor(mk.df$id)
levels(mk.df$Maakunta)[1] <- c("Ahvenanmaa")
ggplot(mk.df, aes(x=long, y=lat, fill=Maakunta)) + geom_polygon()


# Save
save(kunnat, mk.df, file=file.path(data.folder, "Sotkanet_MunicipalityData_20130908.RData"))

## GET DATA FOR CHOSEN INDICATORS ################

# Read list of chosen indicators from Github
chosen.indicators <- read.csv(text=getURL("https://raw.github.com/louhos/takomo/master/Demos_Helsinki/Sotkanet/Sotkanet_valitut_indikaattorit_20130817.csv"))

# Process (includes now indicator.category)
chosen.indicators <- chosen.indicators[1:3]
chosen.indicators <- droplevels(subset(chosen.indicators, indicator.category!=""))

# Get Sotkanet data for the chosen indicators from the web with sorvi package
# dat.raw <- GetDataSotkanet(indicators=chosen.indicators$indicator, genders="total")
# save(dat.raw, file=file.path(data.folder, "Sotkanet_chosen_raw_20130817.RData"))
load(file.path(data.folder, "Sotkanet_chosen_raw_20130817.RData"))

# Load region info
load(file.path(data.folder, "Sotkanet_MunicipalityData_20130908.RData"))

# Take subset of important variables
sotkanet.df <- dat.raw[, c("indicator", "indicator.title.fi","region.title.fi", "region.category", "year", "primary.value")]

# Add indicator category, also tag to indicator
sotkanet.df$Category <- chosen.indicators$indicator.category[match(sotkanet.df$indicator, chosen.indicators$indicator)]
sotkanet.df$Category.Indicator <- factor(paste0("[",sotkanet.df$Category,"] ", sotkanet.df$indicator.title.fi))

# Construct data for different region categories
head(sort(table(sotkanet.df$region.category), decreasing=T))
# KUNTA          SEUTUKUNTA   SAIRAANHOITOPIIRI            MAAKUNTA ALUEHALLINTOVIRASTO 
# 216183               46112               14340               12976                4607 
# ERVA 
# 2665 

## Kunta
kunta.df <- droplevels(subset(sotkanet.df, region.category=="KUNTA"))
kunta.df <- merge(kunta.df, kunnat[c("region.title.fi", "maakunta.title.fi")])#
kunta.df <- kunta.df[c(8,1,5,6,9)]
names(kunta.df) <- c("Muuttuja", "Alue", "Vuosi", "Arvo", "Maakunta")

## Maakunta
maakunta.df <- droplevels(subset(sotkanet.df, region.category=="MAAKUNTA"))
maakunta.df <- maakunta.df[c(8,3,5,6,3)]
names(maakunta.df) <- c("Muuttuja", "Alue", "Vuosi", "Arvo", "Maakunta")

## Seutukunta
seutukunta.df <- droplevels(subset(sotkanet.df, region.category=="SEUTUKUNTA"))
seutukunta.df <- merge(seutukunta.df, unique(kunnat[c("seutukunta.title.fi", "maakunta.title.fi")]), by.x="region.title.fi", by.y="seutukunta.title.fi")
seutukunta.df <- seutukunta.df[c(8,1,5,6,9)]
names(seutukunta.df) <- c("Muuttuja", "Alue", "Vuosi", "Arvo", "Maakunta")

# Join as a list
sotkanet.list <- list(kunta=kunta.df, maakunta=maakunta.df, seutukunta=seutukunta.df)

# Save
save(sotkanet.list, file=file.path(data.folder, "sotkanet_data_20130908.RData"))

# # Write down subset with KOETTU HYVINVOINTI
# kh.df <- subset(sotkanet.df, Category=="KOETTU HYVINVOINTI")
# write.csv(kh.df, "/Users/juusoparkkinen/Dropbox/Public/Demos/Koettu_hyvinvointi_data_20130819.csv")

# Write down names for indicators with province-level data (for shiny maps)
# write.table(levels(maakunta.df$Muuttuja), quote=TRUE, eol=",\n",row.names=FALSE, col.names=FALSE, file=file.path(data.folder, "Maakunta_muuttujat_20130908.txt"))