library(plyr)

source("HSL-Navigator/utilities.R")

# data.folder is set in the local .Rprofile

# Read in the data --------------------------------------------------------s

# Read data
dat <- read.csv(file.path(data.folder, "hastusgps20130114-20130120.rdm"), 
                sep=";", header=FALSE)

# Read and set header
temp <- scan(file.path(data.folder, "hastus_kuvaus.txt"), what="character", 
             sep=";", n=19)
temp <- gsub(" ", "_", temp)
names(dat) <- temp

# Clean the data ----------------------------------------------------------

dat$Route <- trim.str(dat$Route)

# Fix times

# Scheduled.time and Measured.arrival.time need to include date as well
dat$time <- padd.time(dat$Scheduled_time)
dat$hour <- substr(dat$time, 1, 2)

dat$scheduled_time <- paste(dat$Measured_date, 
                                 paste0(dat$time, "00"))
dat$measured_arrival_time <- paste(dat$Measured_date, 
                                        padd.time(dat$Measured_arrival_time))

# Calculate the time diff between scheduled time and the measure arrival time
dat$timediff <- diffschedule(dat$scheduled_time, 
                                  dat$measured_arrival_time,
                                  units="mins")

# Remove all the data for which timediff is not available
dat <- dat[!is.na(dat$timediff),]