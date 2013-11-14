# Convert certain key data files from MML, obtained through Kapsi, into RData format.
# From http://kartat.kapsi.fi/
# List the data sets relevant to our present R tools
library(sorvi)
source("url.list.R")
source("funcs.R")

# Get MML Yleiskartta Shape file list from Kapsi server
MML <- GetMML("yleiskartta", url.list, remove.temporary.dir = FALSE)

# Convert to RData and store to rdata/ subdir
output.dir <- ConvertMMLToRData(MML, output.data.dir = "rdata/")

# Send RData to datavaalit site (will require password)
# system(paste("scp -r", output.dir, "username@server.xxx:../datavaalit/storage/avoindata/mml/"))

