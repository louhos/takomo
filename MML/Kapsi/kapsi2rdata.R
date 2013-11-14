# Convert certain key data files from MML, obtained through Kapsi, into RData format.
# From http://kartat.kapsi.fi/
# List the data sets relevant to our present R tools
library(sorvi)
source("url.list.R")
source("funcs.R")

# Get MML Yleiskartta Shape files from Kapsi server (Shape files)

# Yleiskartta 1000; 45M; 
url.1000 <- "http://kartat.kapsi.fi/files/yleiskartta_1000k/kaikki/etrs89/shape/1_milj_Shape_etrs_shape.zip"

# Yleiskartta 4500; 2.8M; 
url.4500 <- "http://kartat.kapsi.fi/files/yleiskartta_4500k/kaikki/etrs89/shape/4_5_milj_shape_etrs-tm35fin.zip"

# Get the data
MML <- list()
MML[["1000"]] <- GetMML(url.1000, "tmp.yleiskartta.1000")
MML[["4500"]] <- GetMML(url.4500, "tmp.yleiskartta.4500")

# Convert to RData and store to rdata/ subdir
# and save the original zip files
for (id in names(MML)) {
  output.dir <- ConvertMMLToRData(MML[[id]]$shape.list, output.dir = paste("rdata/", id, "/", sep = ""))
  system(paste("cp ", MML[[id]]$zipfile, output.dir))
}

# Save batch information into a README file
fnam <- paste("rdata/", "README", sep = "")
write(paste("Last conversion:", date()), file = fnam)
write("For documentation, see https://github.com/louhos/takomo/blob/master/MML/Kapsi/README", file = fnam, append = TRUE)

# Remove the temporary dirs
system(paste("rm -rf tmp.yleiskartta.1000"))
system(paste("rm -rf tmp.yleiskartta.4500"))

# Send RData to datavaalit site (will require password)
# system(paste("scp -r", output.dir, "username@server.xxx:../datavaalit/storage/avoindata/mml/"))

