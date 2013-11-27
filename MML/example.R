library(devtools)
install_github(repo = "sorvi", username = "louhos", ref = "develop")

library(sorvi, quietly = TRUE)
list_mml_datasets()

# Get a specific map (see list_mml_datasets() for options)
map.id <- "Yleiskartta1000"
sp <- LoadMML(map.id, data.id) 

# Investigate available variables in this map
varnames <- names(sp)
print(varnames)
head(as.data.frame(sp))

# Visualize the shape file
p <- PlotShape(sp, "Maakunta", plot = FALSE)
p


