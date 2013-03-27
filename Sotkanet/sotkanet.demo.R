library(sorvi)

# Get Sotkanet Data
#source("sotkanet.download.R")
load("sotkanet.Mon-Mar-25-14:24:01-2013.RData") #datlist

# Get province info
sotkanet.regions <- SotkanetRegions(type = "table")
sotkanet.kunnat <- subset(sotkanet.regions, region.category == "KUNTA")
maakunnat <- subset(sotkanet.regions, region.category == "MAAKUNTA")
sotkanet.regions.raw <- SotkanetRegions("raw")
kunnat <- sotkanet.regions.raw[sapply(sotkanet.regions.raw, function (x) {x$category == "KUNTA"})]
kunta.maakunta <- lapply(kunnat, function (x) {x$memberOf})
names(kunta.maakunta) <- sapply(kunnat, function (x) {x$title[["fi"]]})

# List all indicators in Sotkanet database
sotkanet.indicators <- SotkanetIndicators(type = "table")

#List all indicators from THL
thl.indicators <- as.character(unique(subset(sotkanet.indicators, indicator.organization.title.fi == "Terveyden ja hyvinvoinnin laitos (THL)")$indicator))

# Select indicators that concern municipalities; ignore gender
municipal.data <- lapply(datlist, function (tab) {subset(tab, region.category == "KUNTA" & gender == "total")})

# Select indicators with the longest time series
long.indicators <- names(which(sapply(municipal.data, function (tab) {length(unique(tab$year))}) == 22))
dats <- municipal.data[long.indicators]
sotkanet <- do.call("rbind", dats)

# For each indicator,
# Correlate indicators with time in each municipality
corlist <- list()
for (i in names(dats)) {
  dat <- dats[[i]]; 
  dat <- dat[order(dat$year), ]; 
  dat <- dat[!duplicated(dat),]; 
  spl <- split(1:nrow(dat), dat$region.title.fi); 
  cors <- sapply(spl, function(inds) {cor(dat$year[inds], dat$primary.value[inds])})
  corlist[[i]] <- cors
}


# Sort indicators by median time correlation
s <- names(sort(sapply(corlist, function (x) {median(na.omit(x))})))
#> unname(sapply(s[1:10], function (nam) {as.character(unique(dats[[nam]]$indicator.title.fi))}))


# Pick some indicators for closer inspection
selected.indicators <- c("Väestö, keskiväkiluku", 
			 "Yksityisten lääkäripalvelujen kustannukset, 1 000 euroa")
		         #"Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä", 
			 #"16-24 -vuotiaat, % väestöstä",
 			 #"Korkea-asteen koulutuksen saaneet, % 15 vuotta täyttäneistä",      
 			 #"Muu kuin suomi, ruotsi tai saame äidinkielenä / 1000 asukasta")
			 #"Alkoholijuomien myynti asukasta kohti 100 %:n alkoholina, litraa")

# Construct data table
df <- subset(sotkanet, indicator.title.fi %in% selected.indicators)
df <- df[, c("region.title.fi", "indicator.title.fi", "year", "primary.value", "absolute.value")]
colnames(df) <- c("Kunta", "Muuttuja", "Vuosi", "arvo", "arvo.abs")
df$Kunta <- as.character(df$Kunta)
df$Muuttuja <- as.character(df$Muuttuja)

# Pick data such that all variables are taken across the same time span (shared years)
coms <- unique(subset(df, Muuttuja == df$Muuttuja[[1]])$Vuosi); 
#sapply(split(df, df$Muuttuja), function (x) {coms <<- intersect(coms, unique(x$Vuosi))})
df <- subset(df, Vuosi %in% coms)

# Form a motion chart from example data NOTE: the data set must be given
# as data.frame which can contain NUMERIC and CHARACTER fields (NO
# FACTORS, NOR LOGICAL variables!).  The FIRST FOUR FIELDS must be
# provided in the following order: idvar, timevar, two numeric fields,
# then any number of numeric and character fields 

if (try(library(reshape)) == "try-error") {install.packages("reshape")}
library(reshape)
dfm <- melt(df, id = c("Kunta", "Vuosi", value = "arvo"))
dfm$variable <- as.character(dfm$variable)
dfm$value <- as.character(dfm$value)
dfm$variable <- NULL
dfr <- reshape(dfm, timevar = "value", idvar = c("Kunta", "Vuosi"), direction = "wide")
colnames(dfr) <- gsub("arvo\\.", "", colnames(dfr))

# Add province information
dfr$Maakunta <- as.character(sapply(as.character(dfr$Kunta), function (kunta) {maakunnat[na.omit(match(kunta.maakunta[[kunta]], maakunnat$region)), "region.title.fi"]}))

# Plot a Motion Chart using googleVis -package
# Plot Motion Chart using googleVis -package
if (try(library(googleVis)) == "try-error") { install.packages("googleVis") }
library(googleVis)
mchart <- gvisMotionChart(dfr, idvar="Kunta", timevar="Vuosi", colorvar = "Maakunta", options=list(height=600, width=700))

# Plot immediately (opens in browser)
plot(mchart)

# Save as html (needs javascript to open!)
print(mchart, file="Sotkanet.html")

# --------------------------------------------------------

library(ggplot2)
region <- unique(sotkanet$region.title.fi)[[34]]
indicator <- 7
indicator.name <- as.character(sotkanet.indicators[sotkanet.indicators$indicator == indicator, "indicator.title.fi"])
dat <- sotkanet[sotkanet$region.title.fi == "Helsinki" & sotkanet$indicator == 7, ]

theme_set(theme_bw(20))
p <- ggplot(dat, aes(x = year, y = primary.value)) 
p <- p + geom_line()
p <- p + ggtitle(indicator.name)
p <- p + xlab("Vuosi") + ylab("Indikaattorin arvo")
print(p)

# ---------------------------------------------------------

# Load MML data
#sp <- LoadMML(data.id = "kunta4_p", resolution = "4_5_milj_shape_etrs-tm35fin")
sp <- LoadData("kuntarajat.maa.shp")

# Select data to visualize
year <- 2011
indicator <- 7
indicator.name <- as.character(sotkanet.indicators[sotkanet.indicators$indicator == indicator, "indicator.title.fi"])
dat <- sotkanet[sotkanet$year == year & sotkanet$indicator == indicator, ]
# Match municipality names between shape (map) object and indicator data
# and add indicator data to the shape object
varname <- "indicator"
sp[[varname]] <- dat[match(sp@data$Kunta.FI, dat$region.title.fi), "primary.value"]
# Compare the value to the mean over all municipalities to highlight differences
sp[[varname]] <- sp[[varname]] - mean(na.omit(sp[[varname]]))
# Replace NAs by 0
sp[[varname]][is.na(sp[[varname]])] <- 0

# Visualize indicators on Finnish map
int <- max(na.omit(abs(sp[[varname]])))
q <- PlotShape(sp, varname, type = "twoway", main = indicator.name, at = seq(0 - int, 0 + int, length = 11))
#print(q)
pdf("~/test.pdf"); print(q); dev.off(); 

# --------------------------------------------------------------

