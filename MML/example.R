# Get the latest sorvi development version
library(devtools)
install_github(repo = "sorvi", username = "louhos", ref = "develop")

# Load sorvi
library(sorvi, quietly = TRUE)

# There are also 2012 datasets in a separate
# directory that is (currently) not shown in the listing.
# Get a specific 2012 map (see list_mml_datasets() for 2013 options)
# Browse http://www.datavaalit.fi/storage/avoindata/mml/rdata/2012/
# for full list of data sets

# Municipality borders
map.id <- "2012/Yleiskartta-4500"
data.id <- "kunta4_p"
sp <- LoadMML(map.id, data.id) 

# Map without sea regions
map.id <- "2012/Maa-alueet"
data.id <- "kuntarajat.maa.shp"
sp <- LoadMML(map.id, data.id) 

# Investigate available variables in this map
varnames <- names(sp)
print(varnames)
head(as.data.frame(sp))

# Visualize the shape file
p <- PlotShape(sp, "Kunta", plot = FALSE)
p

