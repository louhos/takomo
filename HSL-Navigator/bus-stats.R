source(".Rprofile")
source("HSL-Navigator/pre-processing.R")

daily.bus <- ddply(dat, c("Measured_date", "hour"), summarise,
                      sd=sd(timediff, na.rm=T),
                      mean=mean(timediff),
                      median=median(timediff),
                      lower=quantile(timediff, probs=c(0.025)),
                      upper=quantile(timediff, probs=c(0.975)))

hist(dat[which(dat$timediff >50 | dat$timediff < -50), ]$timediff)
temp <- dat[which(dat$timediff > 10 & dat$timediff < -10), ]

bus.14 <- subset(dat, dat$Route == "14")
hist(table(bus.14$Trip), breaks=100)

bus.14$timediff <- diffschedule(bus.14$Scheduled.time, 
                              bus.14$Measured.arrival.time,
                              units="mins")

bus.14 <- bus.14[!is.na(bus.14$timediff),]
boxplot(log(abs(bus.14$timediff)), breaks=100)

bus.68 <- subset(dat, dat$Route == "68")

# Remove NAs no need to keep them

hist(abs(bus.68$timediff), breaks=100, xlim=10)

bus.68$time <- padd.time(bus.68$Scheduled.time)
bus.68$hour <- substr(bus.68$time, 1, 2)

sd.68 <- sd(bus.68$timediff, na.rm=TRUE)
bus.68.norm <- subset(bus.68, timediff < (4 * sd.68) & timediff > (4 * -sd.68))

daily.bus.68 <- ddply(bus.68, c("Measured.date", "hour"), summarise,
                      sd=sd(timediff, na.rm=T),
                      mean=mean(timediff),
                      median=median(timediff))

# Testing -----------------------------------------------------------------

temp <- subset(dat, Route == "94N" & measured_arrival_time > "13/01/2013 240000")

temp2 <- subset(dat, timediff > 100 & timediff < 800)

bus.73 <- subset(dat, Route == "73" & Trip == "1073  2071815401")
