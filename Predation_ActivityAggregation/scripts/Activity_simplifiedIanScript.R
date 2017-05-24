#New activity scripts


DAM1 <- read.table("../data/Activity_Drosophila_EvolvedPops_May2015/PredationActivityDAM1_May2015_RD.txt")
DAM2 <- read.table("../data/Activity_Drosophila_EvolvedPops_May2015/PredationActivityDAM2_May2015_RD.txt")
#monitor
DAM2$v45 <- 2
DAM1$v45 <- 1
DAM_data <- rbind(DAM1, DAM2)


# Sample information is in a seperate csv
sample_info <- read.csv("../data/Activity_Drosophila_EvolvedPops_May2015/Predation_ActivityMetaData_May2015_RD.csv")

colnames(DAM_data) <- c("bin", "day", "month", "year", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4",
                        "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8',
                        'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17',
                        'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26',
                        'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

DAM_data2 <- DAM_data[, -c(7:11, 37:44)]


#make variables the right class
str(DAM_data2)

#DAM_data2$day <- as.factor(DAM_data2$day)

# Get time set up.
DAM_data2$datetime <- as.POSIXct( strptime( paste( DAM_data2$day,DAM_data2$month,DAM_data2$year, DAM_data2$time), 
                                            "%d %B %y %H:%M:%S") )
DAM_data2$monitor <- as.factor(DAM_data2$monitor)
head(DAM_data2) 


# munge the sample_info to have consistent naming conventions with DAM.

sample_info$vial <- paste("vial", sample_info$Location, sep="")
colnames(sample_info)[3] <- "day"
sample_info$day.vial <- interaction(sample_info$day, sample_info$vial) # day is the start day.

# reshape

head(DAM_data2)
DAM_long <- reshape(DAM_data2, varying=list(8:31), v.names="activity_counts", direction="long")
head(DAM_long)

# Add the identifier (14400 is the number of time points per day times the number of days)
DAM_long$vial <- rep(colnames(DAM_data2)[8:31], each=14400)

start_day <- c(19,21,23,26,28) # The starting days of the experiment

# Make the start days match up for the experiments
DAM_long$start_day <- ifelse( (DAM_long$day %in% start_day), DAM_long$day, (DAM_long$day -1 ))

DAM_long$day.vial <- interaction(DAM_long$start_day, DAM_long$vial)

DAM_long3 <- merge(DAM_long, sample_info, by="day.vial")

head(DAM_long2)

# To get minute
DAM_long2$minute <- as.numeric(strftime(DAM_long2$datetime, format ="%M"))
# to get hour
DAM_long2$hour <- as.numeric(strftime(DAM_long2$datetime, format ="%H"))


# To create a variable for each individual
DAM_long2$individual <- with(DAM_long2, interaction(day.vial, monitor, drop=FALSE))
nlevels(DAM_long2$individual)



### Getting hourly average
dat.hourly <- with(DAM_long2, 
                   aggregate(activity_counts, FUN=sum, by=list(vial.x, monitor, start_day, hour, Trt, Population)))
colnames(dat.hourly) <- c("vial.x", "monitor", "start_day", "hour", "Trt", "Population", "Hourly_activity")     
dat.hourly$individual <- with(dat.hourly, interaction(start_day, vial.x, monitor, drop=FALSE))
head(dat.hourly)


## ggplot try from time 0

head(dat.hourly)
gg5 <- ggplot(dat.hourly, aes(x=hour, y= Hourly_activity, colour=Trt)) + xlim(0,24) + ylim(0,600)
gg6 <- gg5 + geom_jitter(size=0.5) + geom_smooth(method = "loess")
#gg6 + geom_rect(aes(xmin=10, xmax=22, ymin=0, ymax=600), fill="yellow", alpha=0.5)
gg6 + annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 600) +
  geom_vline(xintercept = 8)


