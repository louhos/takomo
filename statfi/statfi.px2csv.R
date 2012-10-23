# CSV conversion
# Create file/folder structure similar to the original

library(sorvi)
#library(pxR)
#source("read.pxr.fix.R") # Fix read.px function

# Get PX file urls
source("statfi_files.R") # px.urls

# i <- 0; save(i, file = "tmp.RData")
load("tmp.RData")

if (!exists("filesalreadyhandled")) { filesalreadyhandled <- c()}
if (!exists("non.extant.files")) { non.extant.files <- c()}
if (!exists("pxf.errors")) { pxf.errors <- c()}
if (!exists("pxf.ok")) { pxf.ok <- c()}

k <- max(i, 1) # Start from the latest point
for (i in k:length(px.urls)) {

  pxf <- px.urls[[i]]

  if (!pxf %in% filesalreadyhandled) {

    print(c(i, pxf))

    openingtest <- NULL
    px <- NULL
    openingtest <- try(px <- sorvi::read.px(pxf))

    if (length(openingtest)==1 && grep("cannot open the connection", openingtest) == 1) {

      print(paste("not found", pxf))
      non.extant.files <- c(non.extant.files, pxf)

    } else {

      df <- try(as.data.frame(px))

      if (!is.data.frame(df)) {
        pxf.errors <- c(pxf.errors, pxf)
      } else {
        pxf.ok <- c(pxf.ok, pxf)
      }
      
    }

    filesalreadyhandled <-  c(filesalreadyhandled, pxf)

    save(i, filesalreadyhandled, non.extant.files, pxf.errors, pxf.ok, file = "tmp.RData")
    gc()
  }

}

#[2] "http://pxweb2.stat.fi/database/StatFin/oik/koikrs/010_koikrs_tau_101_fi.px"
#Error in data.frame(do.call(expand.grid, values[names.vals]), x$DATA$value) : 
#  arguments imply differing number of rows: 65007360, 55245105
