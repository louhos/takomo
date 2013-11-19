# Script for extracting all PC-Axis tables from Statistics of Finland
# Main wep page: http://pxweb2.stat.fi/database/StatFin/databasetree_fi.asp

library(sorvi)

# Read open data URL listing for StatFi
df <- list_statfi_files("px")

# CSV conversion
dims <- list()
for (f in df$File) {

  print(f)
  df <- get_statfi(f)
  dims[[f]] <- dim(df)

}

