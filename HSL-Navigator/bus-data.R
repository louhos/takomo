library(plyr)

source("HSL-Navigator/utilities.R")

bus.data <- read.csv("HSL-Navigator/data/HSL.data.csv2", header=TRUE, sep=";")
bus.data$Route <- trim.str(bus.data$Route)

bus.data$time <- padd.time(bus.data$Scheduled.time)
bus.data$hour <- substr(bus.data$time, 1, 2)

bus.data$ontime <- diffschedule(bus.data$Scheduled.time, 
                                bus.data$Measured.arrival.time,
                                units="mins")

bus.data <- bus.data[!is.na(bus.data$ontime),]

daily.bus <- ddply(bus.data, c("Measured.date", "hour"), summarise,
                      sd=sd(ontime, na.rm=T),
                      mean=mean(ontime),
                      median=median(ontime),
                      lower=quantile(ontime, probs=c(0.025)),
                      upper=quantile(ontime, probs=c(0.975)))

bus.14 <- subset(bus.data, bus.data$Route == "14")

bus.14$ontime <- diffschedule(bus.14$Scheduled.time, 
                              bus.14$Measured.arrival.time,
                              units="mins")

bus.14 <- bus.14[!is.na(bus.14$ontime),]
boxplot(log(abs(bus.14$ontime)), breaks=100)


bus.68 <- subset(bus.data, bus.data$Route == "68")

bus.68$ontime <- diffschedule(bus.68$Scheduled.time, 
                              bus.68$Measured.arrival.time,
                              units="mins")

# Remove NAs no need to keep them
bus.68 <- bus.68[!is.na(bus.68$ontime),]
hist(log(abs(bus.68$ontime)), breaks=100)

bus.68$time <- padd.time(bus.68$Scheduled.time)
bus.68$hour <- substr(bus.68$time, 1, 2)

sd.68 <- sd(bus.68$ontime, na.rm=TRUE)
bus.68.norm <- subset(bus.68, ontime < (4 * sd.68) & ontime > (4 * -sd.68))

daily.bus.68 <- ddply(bus.68, c("Measured.date", "hour"), summarise,
                      sd=sd(ontime, na.rm=T),
                      mean=mean(ontime),
                      median=median(ontime))

