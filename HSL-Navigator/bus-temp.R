# Visualizations from bus statistics

CleanRoute <- function(dat, route.id) {
  
  # Check if route.id is valid!
  stopifnot(route.id %in% dat$route_id)
  
  library(plyr)
  # Separate directions for the given route
  bus <- droplevels(subset(dat, route_id==route.id))
  bus.1 <- subset(bus, Direction==1)
  bus.2 <- subset(bus, Direction==2)
  
  # Compute number of unique and total ranks per trip
  ur1 <- droplevels(ddply(bus.1, "Trip", summarise, NuniqueRanks=length(unique(Rank)), Nranks=length(Rank)))
  ur2 <- droplevels(ddply(bus.2, "Trip", summarise, NuniqueRanks=length(unique(Rank)), Nranks=length(Rank)))
  # Accept only those trips for which there is exactly one observation per rank
  ok.trips1 <- as.vector(ur1$Trip[ur1$NuniqueRanks==max(ur1$NuniqueRanks) & ur1$NuniqueRanks==ur1$Nranks])
  ok.trips2 <- as.vector(ur2$Trip[ur2$NuniqueRanks==max(ur2$NuniqueRanks) & ur2$NuniqueRanks==ur2$Nranks])
  # Filter and report
  bus.clean1 <- droplevels(subset(bus.1, Trip %in% ok.trips1))
  bus.clean2 <- droplevels(subset(bus.2, Trip %in% ok.trips2))
  message("Valid runs, direction 1: ", nrow(bus.clean1), " / ", nrow(bus.1))
  message("Valid runs, direction 2: ", nrow(bus.clean2), " / ", nrow(bus.2))
  
  # Combine data
  res <- rbind(bus.clean1, bus.clean2)
#  res$DirInfo <- paste("Direction", res$Direction)
  
  # Set timediff for Rank=0 to be at least 0
  res$timediff[res$Rank==0 & res$timediff < 0] <- 0
  
  # Fix missing stop names
  res$StopName <- "NA"
  res$StopName[!is.na(res$stop_name)] <- as.vector(res$stop_name[!is.na(res$stop_name)])
  
  return(res)
}

# Object dat obtained using pre-processing.R
library(ggplot2)
library(gridExtra)

# Plot without stop names
route.id <- "2106"
temp <- CleanRoute(dat, route.id)
p1 <- ggplot(temp, aes(x=Rank, y=timediff, group=Trip)) + geom_path(alpha=0.5) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1) + xlab("BusStop") + ylab("Observed - Schedule (min)") + ggtitle(paste("Route: ", temp$Route[1], " (route id: ", route.id,")",sep="")) + facet_wrap(~Direction, nrow=2) + ylim(-10, 20)
# ggsave(plot=p, width=10, height=8, file=paste("HSL-Navigator/Route_",route.id,".png",sep=""))

# For stop names, read the information and combine data

stops <- read.csv(file.path(data.folder, "stops.txt"))
dat2 <- merge(dat, stops, by.x="Location", by.y="stop_id", all.x=TRUE)
# dat2$stop_name[is.na(dat2$stop_name)] <- "NotAvailable"

# Plot with stop names
route.id <- "2106"
temp2 <- CleanRoute(dat2, route.id)
plots <- list()
for (di in 1:2) {
  dir.df <- droplevels(subset(temp2, Direction==di))
  dir.df$StopName <- reorder(dir.df$StopName, dir.df$Rank)
  p <- ggplot(dir.df, aes(x=StopName, y=timediff, group=Trip))
  p <- p + geom_line(alpha=0.5) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1)
  p <- p + ggtitle(paste("Direction", di)) + xlab(NULL) + ylab(NULL)
  p <- p + theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=12)) #+ ylim(-5, 15)
  p <- p + theme(axis.text.y=element_text(size=12))
  p <- p + ylim(max(-10, min(temp2$timediff)), min(20, max(temp2$timediff)))
  
  plots[[di]] <- p
#   plots[[di]] <- ggplot(dir.df, aes(x=stop_name, y=timediff, group=Trip)) + geom_line(alpha=0.5) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1)
#   + xlab("BusStop") + ylab("Observed - Schedule (min)") + ggtitle(paste("Direction", di)) + ylim(-10, 20) + theme(axis.text.x=element_text(angle=90))
}
p <- arrangeGrob(plots[[1]], plots[[2]], nrow=2, main=paste("Route: ", temp2$Route[1], " (route id: ", route.id,")",sep=""), left="Observed arrival time - scheduled time (min)")
p
ggsave(plot=p, width=10, height=8, file=paste("HSL-Navigator/Route_",route.id,".png",sep=""))

# temp2$stop_name <- reorder(temp2$stop_name, temp2$Rank)
# p2 <- ggplot(temp2, aes(x=stop_name, y=timediff, group=Trip)) + geom_line(alpha=0.5) + geom_hline(y=c(-1, 3), linetype="dashed", colour="red", size=1) + xlab("BusStop") + ylab("Observed - Schedule (min)") + ggtitle(paste("Route: ", temp2$Route[1], " (route id: ", route.id,")",sep="")) + facet_wrap(~Direction, nrow=2) + ylim(-10, 20) + theme(axis.text.x=element_text(angle=90))
# ggsave(plot=p2, width=10, height=8, file=paste("HSL-Navigator/Route_",route.id,".png",sep=""))
# # 
# Plot for all trams
routes <- read.csv(file.path(data.folder, "routes.csv"))
tram.route.ids <- as.vector(routes$route_id[routes$route_type==0])
any(tram.route.ids %in% dat$route_id)
# [1] FALSE
