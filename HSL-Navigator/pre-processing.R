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

# Fix route ids --------------------------------------

# Read given route information
routes <- read.csv(file.path(data.folder, "routes.csv"))

# Separate route id's from the Trip variable
# Split Trip ids to get route ids
# Real route ids are 4-6 characters long
temp <- sapply(strsplit(as.vector(dat$Trip), split=" "), "[", 1)
route.ids <- substr(temp, 1, 6)
unique.ids <- unique(route.ids)

# Only these 5 do not match
unique.ids[!(unique.ids %in% routes$route_id)]
# [1] "2018Z"  "7974"   "1094A7" "1094N3" "1094A5"

# These can be fixed manually
route.ids[grep("1094A", route.ids)] <- "1094A"
route.ids[grep("1094N", route.ids)] <- "1094N"

# Two route ids remain without match, keep them as such
unique.ids2 <- unique(route.ids)
unique.ids2[!(unique.ids2 %in% routes$route_id)]
# [1] "2018Z" "7974"

# Results:
length(unique(route.ids)) # 430 unique route ids
length(which(unique(route.ids) %in% routes$route_id)) # 428 of which match to given routes data

# Set route ids
dat$route_id <- route.ids

# Confirm matching manually
dat[-which(duplicated(dat$Route)), c("Route", "route_id")]


# Remove "outliers". These are not necessary outliers...

