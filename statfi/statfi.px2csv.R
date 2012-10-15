# CSV conversion
# Create file/folder structure similar to the original

library(sorvi)
#library(pxR)
#source("read.pxr.fix.R") # Fix read.px function

#source("statfi_20120922.R")
setwd("/home/lei/Rpackages/louhos/takomo/statfi")
load("StatFi_px_urls.RData") # px.tree
#load("StatFi_px_urls_20120505.RData") # px.tree

#non.extant.files <- filesalreadyhandled <- NULL; i <- 0; save(non.extant.files, filesalreadyhandled, i, file = "tmp.RData")
load("tmp.RData")

# Manual test
#good.url <- "http://pxweb2.stat.fi/database/StatFin/asu/asas/010_asas_tau_101.px"
#bad.url <- "http://pxweb2.stat.fi/database/StatFin/hin/pthi/004_pthi_tau_004_fi.px"
#bad <- sorvi::read.px(bad.url, na.strings = c('"."', '".."', '"..."', '"...."', '"-"'))
#good <- sorvi::read.px(good.url)

message("Changing folder to StatFin")
setwd("StatFin")

pxf.errors <- c()
bad.px.files <- c("http://pxweb2.stat.fi/database/StatFin/oik/koikrs/020_koikrs_tau_102_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/koikrs/010_koikrs_tau_101_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/pkei/025_pkei_tau_106_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/pkei/030_pkei_tau_105_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/pkei/040_pkei_tau_103_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/pkei/050_pkei_tau_104_fi.px", "http://pxweb2.stat.fi/database/StatFin/oik/polrik/020_polrik_tau_102.px", "http://pxweb2.stat.fi/database/StatFin/oik/syyttr/040_syyttr_tau_102_fi.px")

for (i in 1:length(px.tree)) {

  gc()
  save(i, filesalreadyhandled, non.extant.files, file = "../tmp.RData")

  for (j in seq(px.tree[[i]])) {
    path <- unlist(strsplit(px.tree[[i]][[j]], split="\\/"))
    l1.folder <- path[6]
    l2.folder <- paste(l1.folder, path[7], sep="/")
    filename <- gsub("\\.px", "\\.csv", path[8])

    if (!filename %in% filesalreadyhandled) {

     # Check whether level 1 folder exists - if not, create it
      if (!file.exists(l1.folder))
        dir.create(l1.folder)
      # Check whether level 2 folder exists - if not, create it
      if (!file.exists(l2.folder))
        dir.create(l2.folder)

      # Write temporary files 
      pxf <- px.tree[[i]][[j]]

      if (!pxf %in% bad.px.files) {

        print(c(i, pxf))

        openingtest <- NULL
	px <- NULL
        openingtest <- try(px <- sorvi::read.px(pxf))
	if (length(openingtest)==1 && grep("cannot open the connection", openingtest) == 1) {

	  print(paste("not found", pxf))
	  non.extant.files <- c(non.extant.files, pxf)

 	} else {

          df <- try(as.data.frame(px))

          if (is.data.frame(df)) {
            fn <- unlist(strsplit(pxf, "/")); 
            fn <- strsplit(fn[[length(fn)]], "\\.")[[1]][[1]] 
            #fnam <- paste("csv/", fn, ".csv", sep = "")
            fnam <- paste(l2.folder, filename, sep = "/")

	    # Write the table as CSV file
            # write.table(df, file = fnam, quote = FALSE, sep = "\t", row.names = FALSE)

          } else {
            pxf.errors <- c(pxf.errors, pxf)
          }

          filesalreadyhandled <-  unique(c(filesalreadyhandled, non.extant.files, filename))
	  save(i, filesalreadyhandled, non.extant.files, file = "../tmp.RData")

        }
      }
    }
  }

}
save(pxf.errors, file = "../pxf.errors.RData")

#[2] "http://pxweb2.stat.fi/database/StatFin/oik/koikrs/010_koikrs_tau_101_fi.px"
#Error in data.frame(do.call(expand.grid, values[names.vals]), x$DATA$value) : 
#  arguments imply differing number of rows: 65007360, 55245105
