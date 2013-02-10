############################
## OLD STUFF OLD STUFF ######

# ggplot(temp$bus1, aes(x=Rank, y=timediff)) + geom_smooth() + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1)

# Take subset
bus.106 <- droplevels(subset(dat, Route=="39"))
bus.106.1 <- subset(bus.106, Direction==1)
bus.106.2 <- subset(bus.106, Direction==2)

# This is important!
hist(bus.106$Rank, breaks=100)
hist(table(bus.106$Trip), breaks=100)

library(ggplot2)
ggplot(bus.106, aes(x=Rank, y=timediff, group=Trip)) + geom_path() + ylim(-15, 15) + facet_wrap(~Direction, nrow=2)
bus.106$Alpha <- 1/abs(bus.106$timediff)
bus.106$Alpha[bus.106$Alpha==Inf] <- 1
ggplot(bus.106, aes(x=Rank, y=timediff, group=Trip, colour=as.numeric(hour))) + geom_path(aes(alpha=Alpha), size=1.2) + ylim(-15, 15) + facet_wrap(~Direction, nrow=2)

# Rank vs. Location (=bus stop)
rank2loc1 <- table(bus.106.1$Rank, bus.106.1$Location)
rank2loc2 <- table(bus.106.2$Rank, bus.106.2$Location)

library(reshape2)
r2l.df <- rbind(cbind(Direction=1, melt(rank2loc1)), cbind(Direction=2, melt(rank2loc2)))
ggplot(r2l.df, aes(x=factor(Var1), y=factor(Var2), fill=value)) + geom_tile() + facet_wrap(~ Direction, scales="free")

## CLEAN DATA

# # TRY 1: Get only those trips which occur Nstops times (max +1)
# trip.freq1 <- table(bus.106.1$Trip)
# trip.freq2 <- table(bus.106.2$Trip)
# 
# bus.106.clean1 <- droplevels(subset(bus.106.1, Trip %in% names(which(trip.freq1==max(bus.106.1$Rank) +1))))
# bus.106.clean2 <- droplevels(subset(bus.106.2, Trip %in% names(which(trip.freq2==max(bus.106.2$Rank) +1))))
# 
# ggplot(bus.106.clean1, aes(x=Rank, y=timediff, group=Trip)) + geom_path() + ylim(-15, 15)
# ggplot(bus.106.clean2, aes(x=Rank, y=timediff, group=Trip)) + geom_path() + ylim(-15, 15)
# 
# hist(bus.106.clean1$Rank, breaks=100)
# hist(bus.106.clean2$Rank, breaks=100)
# message("Cleaning is not perfect yet!!!")

# TRY 2: 
library(plyr)
ur1 <- droplevels(ddply(bus.106.1, "Trip", summarise, NuniqueRanks=length(unique(Rank)), Nranks=length(Rank)))
ur2 <- droplevels(ddply(bus.106.2, "Trip", summarise, NuniqueRanks=length(unique(Rank)), Nranks=length(Rank)))
ok.trips1 <- as.vector(ur1$Trip[ur1$NuniqueRanks==max(ur1$NuniqueRanks) & ur1$NuniqueRanks==ur1$Nranks])
ok.trips2 <- as.vector(ur2$Trip[ur2$NuniqueRanks==max(ur2$NuniqueRanks) & ur2$NuniqueRanks==ur2$Nranks])

bus.106.clean1 <- droplevels(subset(bus.106.1, Trip %in% ok.trips1))
bus.106.clean2 <- droplevels(subset(bus.106.2, Trip %in% ok.trips2))

ggplot(bus.106.clean1, aes(x=Rank, y=timediff, group=Trip)) + geom_path() + ylim(-15, 15) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1)
ggplot(bus.106.clean2, aes(x=Rank, y=timediff, group=Trip)) + geom_path() + ylim(-15, 15) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1)

hist(bus.106.clean1$Rank, breaks=100)
hist(bus.106.clean1$Trip, breaks=100)

hist(bus.106.clean2$Rank, breaks=100)

# TODO
# Compute ratio of discarded Trips
# Check time distribution the discarded Trips 

# Plot on map
stops <- read.csv(file.path(data.folder, "stops.txt"))
library(ggmap)
Hel.center <- geocode("Helsinki")
Hel.center$lat <- Hel.center$lat + 0.1
Hel.googlemap <- get_map(location=c(lon=Hel.center$lon, lat=Hel.center$lat), zoom=10, source="google")

# Take subset
bus <- droplevels(subset(dat, Route=="68"))
dat2 <- merge(bus, stops, by.x="Location", by.y="stop_id")
# Locate Helsinki

hmap1 <- ggmap(Hel.googlemap) + geom_point(data=dat2, aes(x=stop_lon, y=stop_lat))
hmap1
