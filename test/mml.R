# Update and load sorvi development version
library(devtools)
install_github(repo = "sorvi", username = "louhos", ref = "develop")
library(sorvi)

# Check Hammarland
kartta <- LoadMML(map.id = "Yleiskartta-1000", data.id = "HallintoAlue")
print(kartta@data[kartta@data$Kunta.FI=="Hammarland",])

# See other MML files available in RData format
print(list_mml_datasets())


