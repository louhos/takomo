library(sorvi)
source("read.pxr.fix.R") # Fix read.px function

# --------------------------------------

# Get PX file urls

# For more information, see:
# http://www.stat.fi/org/lainsaadanto/avoin_data.html
urls <- list(statfi = list_statfi_files(),
     	     eurostat = list_eurostat_files())

# --------------------------------------

# STATFI

px.urls <- unique(urls$statfi$File)

if (!exists("non.extant.files")) { non.extant.files <- c()}
if (!exists("pxf.errors")) { pxf.errors <- c()}
if (!exists("pxf.ok")) { pxf.ok <- c()}

for (i in 1:length(px.urls)) {

  pxf <- px.urls[[i]]

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

}

# ------------------------------------

system("mv tmp.RData statfi.screen.RData")
# mv tmp.RData eurostat.screen.RData

# Statfi
print(as.matrix(c(OK = length(pxf.ok), OKpercentage = length(pxf.ok)/length(px.urls), N = length(px.urls))))
#OK           2193.0000000
#OKpercentage    0.7837741
#N            2798.0000000


# Eurostat
# c(length(pxf.ok), length(px.urls), length(pxf.ok)/length(px.urls))
# 1158.0000000 1159.0000000    0.9991372

#[2] "http://pxweb2.stat.fi/database/StatFin/oik/koikrs/010_koikrs_tau_101_fi.px"
#Error in data.frame(do.call(expand.grid, values[names.vals]), x$DATA$value) : 
#  arguments imply differing number of rows: 65007360, 55245105
