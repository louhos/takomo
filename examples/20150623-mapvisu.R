
# Get the latest package versions
library(devtools)
install_github("ropengov/sotkanet")
install_github("ropengov/gisfin")

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

# -------------------------

# Write the data to file
write.csv(healthindex, file = "mydata.csv", row.names = FALSE, quote = FALSE)

print(head(healthindex))

# Read the same data from file
# (instead you can just use the original mydata that was written in this file)
# or alternatively you can replace your own data in the file mydata.csv:
mydata <- read.csv(file = "mydata.csv")

print(head(mydata))

# -------------------------

# Get municipality map from Land Survey Finland 
library(gisfin)
map <- get_municipality_map(data.source = "MML")

# Check the variable names
names(map)

# -------------------------------

# All municipality IDs on the map file are available in our data set
all(map$kuntakoodi %in% mydata$Kuntakoodi)

# The mydata contains some municipality IDs that
# are not in our map file ut that does not matter since these
# are not municipalities but some other entities (provinces, whole country etc)
# that are not used in municipality visualization anyway
setdiff(mydata$Kuntakoodi, map$kuntakoodi)

# We have now checked that all municipalities in "map" have
# values available in the data.
# Then merge the map and the data based on the specified
# identifier fields. The non-matching fields are ignored in merging
map2 <- sp::merge(map, mydata, by.x = "kuntakoodi", by.y="Kuntakoodi")

# Check the variable names
names(map2)

# ------------------------------------

# Plot the map
# You can also use map2 to try the other visualizations shown in
# http://louhos.github.io/news/2015/06/06/kuntakartat/
# https://github.com/rOpenGov/gisfin/blob/master/vignettes/gisfin_tutorial.md
p <- region_plot(map2, color = "Asukasluku", region = "kuntakoodi")
print(p)


