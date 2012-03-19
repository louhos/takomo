# Code for analysing and visualising traffic data
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2012 Juuso Parkkinen, juuso.parkkinen@gmail.com. All rights reserved.

# TODO
# Add coordinates for the data points
library(reshape)

# Raw data is currently (13.3.2012) found in Antti Poikola's dropbox
# http://dl.dropbox.com/u/2949803/pyoralaskennat.zip
# Initialize
data.folder <- "/Users/juusoparkkinen/Documents/workspace/data/pyoralaskennat/"
loc.names <- read.table(paste(data.folder, "pyoralaskennat_readme.txt", sep=""), skip=4, sep="=", fileEncoding="ISO-8859-1")
month.folders <- dir(data.folder, pattern="kuu")
final.df <- c()
for (mi in 1:length(month.folders)) {
  month.files <- dir(paste(data.folder, month.folders[mi],sep=""))
  for (fi in 1:length(month.files)) {
    # Extract site number and name
    filename <- paste(data.folder, month.folders[mi], "/", month.files[fi],sep="")
    site.number <- as.numeric(unlist(strsplit(unlist(strsplit(filename, split="\\."))[1], split="/"))[9])
    site.init <- as.numeric(substr(as.character(site.number), 1, 3))
    if (site.init==117)
      site.init <- as.numeric(substr(as.character(site.number), 1, 4))
    site.name <- as.vector(loc.names$V2[match(site.init, loc.names$V1)])
    dat.raw <- scan(filename, what=character(), sep="\n", strip.white=TRUE)
    
    # Separate locations
    loc.rows <- grep("LOCATION", dat.raw)
    loc.list <- list()
    for (li in 1:(length(loc.rows)-1))
      loc.list[[li]] <- dat.raw[loc.rows[li]:(loc.rows[li+1]-1)]  
    loc.list[[li+1]] <- dat.raw[loc.rows[li+1]:length(dat.raw)]
    
    # Extract data from each location
    loc.mat <- c()
    for (li in 1:length(loc.list)) {
      date.row <- grep("DATE", loc.list[[li]])
      # Check whether tabs or spaces were used as separator
      if (length(grep("\t", loc.list[[li]][date.row]))>0) {
        # Fix first date row
        loc.list[[li]][date.row] <- gsub("\t/", "/", loc.list[[li]][date.row])
        # Change then all "\t" to " "
        loc.list[[li]][date.row:(date.row+25)] <- gsub("\t", " ", loc.list[[li]][date.row:(date.row+25)])
      }
      date.temp <- unlist(strsplit(loc.list[[li]][date.row], split=" "))
      date.temp <- date.temp[-which(date.temp=="")]
      dates <- gsub("0011", "2011", as.character(as.Date(date.temp[2])+0:6))
      dat.mat <- matrix(NA, nrow=7, ncol=24, dimnames=list(dates, 0:23))
      for (hi in 1:24) {
        hrow <- date.row + hi + 1
        h.temp <- unlist(strsplit(loc.list[[li]][hrow], split=" "))
        if (!is.na(h.temp)[1]) { # Needed as e.g. 1171001.211 in January is not complete
          if (any(h.temp=="")) # Needed for '\t' -files 
            h.temp <- h.temp[-which(h.temp=="")]
        }
        dat.mat[,hi] <- suppressWarnings(as.numeric(h.temp[2:8]))
      }
      loc.mat <- rbind(loc.mat, dat.mat)
    }
    loc.df <- data.frame(LocationID1=site.number, LocationID2=site.init, melt(loc.mat))
    names(loc.df)[3:5] <- c("Date", "Hour", "Value")
    final.df <- rbind(final.df, loc.df)
  }
}
# Reorder based on 1) Location, 2) Date, 3) Hour
final.df <- final.df[order(final.df$Location, final.df$Date, final.df$Hour),]
# Add weekday
final.df$WeekDay <- factor(weekdays(as.Date(final.df$Date)))
final.df <- final.df[c(1:3, 6, 4:5)]
save(final.df, file=paste(data.folder, "pyoralaksennat_table_20120317.RData"))
write.csv(final.df, file=paste(data.folder, "pyoralaksennat_table_20120317.csv"))
# Data can be found in my Dropbox:
# http://dl.dropbox.com/u/792906/misc/pyoralaskennat_table_20120317.csv