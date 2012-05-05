# Script for extracting all PC-Axis tables from Statistics of Finland
# Main wep page: http://pxweb2.stat.fi/database/StatFin/databasetree_fi.asp

library(XML)
# Use page source to dig out the file with the sub page urls
url <- "http://pxweb2.stat.fi/database/StatFin/databasetreeNodes_fi.js"

# Write the contents of databasetreeNodes_fi.js directly into a text file
temp.filename <- "TEMP_databasetreeNodes.txt"
sink(temp.filename)
print(htmlParse(url))
sink()

# Read the data and delete the file
databasetreeNodes <- readLines(temp.filename)
file.remove(temp.filename)

# Extract urls for the subfolders
# NOTE! This skips the parent folders, e.g. Asuminen or Elinolot
# Those could be found with grep("insFld", ...)
# If we want to retain the whole hierarchy we would need to get those as well
insDocs <- grep("insDoc", databasetreeNodes)
folder.urls <- as.vector(sapply(databasetreeNodes[insDocs], function(x) unlist(strsplit(x, split="\""))[6]))
folder.urls <- gsub("\\.\\.\\/", "http://pxweb2.stat.fi/database/", folder.urls)

# For each subfolder, extract urls for the .px files
# px.tree has now two levels of hierarchy
px.tree <- vector("list", length(folder.urls))
for (i in seq(px.tree)) {
  if (i %% 10 == 0)
    message(i, ", ", appendLF=FALSE)
  # Write the contents of the statfi file to a temporary file
  temp.filename2 <- "TEMP_statfi.txt"
  sink(temp.filename2)
  print(htmlParse(folder.urls[i]))
  sink()
  # Read and delete the file
  statfi <- readLines(temp.filename2)
  file.remove(temp.filename2)
  # Extract .px file names
  px.inds <- grep("\\.px", statfi)
  px.files <- lapply(statfi[px.inds], function(x) unlist(strsplit(x, split="\"")))
  px.files <- as.vector(sapply(px.files, function(x) x[grep(".px", x)]))
  # Complete urls
  url.temp <- unlist(strsplit(folder.urls[i], split="\\/"))
  url.base <- paste(url.temp[1:(length(url.temp)-1)], collapse="/")
  px.tree[[i]] <- paste(url.base, px.files, sep="/")
  # Extract title
  names(px.tree)[i] <- gsub("</title>", "", gsub("<title>", "", statfi[4]))
}
save(px.tree, file="StatFi_px_urls_20120505.RData")

# CSV conversion
# Create file/folder structure similar to the original
message("Changing folder to StatFin where the data will be saved!")
setwd("statfi/StatFin")
for (i in seq(px.tree)) {
  # Loop over the files
  for (j in seq(px.tree[[i]])) {
    path <- unlist(strsplit(px.tree[[i]][[j]], split="\\/"))
    l1.folder <- path[6]
    l2.folder <- paste(l1.folder, path[7], sep="/")
    filename <- gsub("\\.px", "\\.txt", path[8])
    # Check whether level 1 folder exists - if not, create it
    if (!file.exists(l1.folder))
      dir.create(l1.folder)
    # Check whether level 2 folder exists - if not, create it
    if (!file.exists(l2.folder))
      dir.create(l2.folder)
    # Write temporary files 
    write("TEMPTEMPTEMP", file=paste(l2.folder, filename, sep="/"))
  }
}


