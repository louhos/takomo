library(sorvi)
#library(pxR)
#source("read.pxr.fix.R") # Fix read.px function
# i <- 0; save(i, file = "tmp.RData")

# --------------------------------------

# Get PX file urls

# For more information, see:
# http://www.stat.fi/org/lainsaadanto/avoin_data.html

urls <- list(statfi = "http://pxweb2.stat.fi/database/StatFin/StatFin_rap.csv",
     	     eurostat = "http://pxweb2.stat.fi/database/Eurostat/Eurostat_rap.csv")

# StatFi
#tab <- read.csv(urls$statfi, sep = ";")

# Eurostat
tab <- read.csv(urls$eurostat, sep = ";")

# Combine the tables
#tab <- rbind(tab1, tab2)

# --------------------------------------

px.urls <- unique(as.character(tab$File))

load("tmp.RData")

if (!exists("filesalreadyhandled")) { filesalreadyhandled <- c()}
if (!exists("non.extant.files")) { non.extant.files <- c()}
if (!exists("pxf.errors")) { pxf.errors <- c()}
if (!exists("pxf.ok")) { pxf.ok <- c()}

k <- max(i+1, 1) # Start from the latest point

# --------------------------------------

for (i in k:length(px.urls)) {

  pxf <- px.urls[[i]]

  if (!pxf %in% filesalreadyhandled) {

    print(c(i, pxf, length(px.urls)))

    openingtest <- NULL
    px <- NULL
    openingtest <- try(px <- sorvi::read.px(pxf))

    if (length(openingtest)==1 && (grep("cannot open the connection", openingtest) == 1 || substr(openingtest, 1, 5) == "Error") ) {

      print(paste("errors: ", pxf))
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

# ------------------------------------

# mv tmp.RData statfi.screen.RData
# mv tmp.RData eurostat.screen.RData

# Statfi
# c(length(pxf.ok), length(px.urls), length(pxf.ok)/length(px.urls))
# 1989.0000000 2614.0000000    0.7609028

# Eurostat
# c(length(pxf.ok), length(px.urls), length(pxf.ok)/length(px.urls))
# 1158.0000000 1159.0000000    0.9991372

#[2] "http://pxweb2.stat.fi/database/StatFin/oik/koikrs/010_koikrs_tau_101_fi.px"
#Error in data.frame(do.call(expand.grid, values[names.vals]), x$DATA$value) : 
#  arguments imply differing number of rows: 65007360, 55245105
