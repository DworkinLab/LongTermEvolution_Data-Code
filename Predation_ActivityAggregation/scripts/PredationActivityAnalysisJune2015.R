
### This is the analysis for the Drosophila activity data for the 12 continuous predation populations. Data was collected by Reuven Dukas May 24-29th 2015. It was collected on two activity monitors acting in parallel. UNfortunately it does not look the temperature/light/humidity metre was activitated so we do not have those covariates. 

#Some notes
# I think that flies were done in replicate on each activity monitor. So whoever was in position 1 on DAM1 on day 1, was also in position 1 on DAM2 on day 1.

# Only 24 of the 32 monitors per DAM were used each day.

# Each individual was run for only 24 hours in the activity monitors.

#libraries
library(lme4)
#Data input
setwd("../data")
#setwd("/Users/ian/Projects_current/Predation_Social/data")
dir() 

# the actual data are as tab delimited txt files.

DAM1 <- read.table("PredationActivityDAM1_May2015_RD.txt")
# Create a new variable for the DAM monitor ID
DAM1$v45 <- 1

DAM2 <- read.table("PredationActivityDAM2_May2015_RD.txt")
DAM2$v45 <- 2
DAM_data <- rbind(DAM1, DAM2)


# Sample information is in a seperate csv
sample_info <- read.csv("Predation_ActivityMetaData_May2015_RD.csv")

colnames(DAM_data) <- c("bin", "day", "month", "year", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4",
                        "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8',
                        'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17',
                        'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26',
                        'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

# I used
# crap <- paste("'", "vial", 1:32, "'", sep="", collapse=', ') to get all the vials

# Parse data, and remove unnecessary columns

DAM_data2 <- DAM_data[, -c(7:11, 37:44)]


#make variables the right class
str(DAM_data2)

#DAM_data2$day <- as.factor(DAM_data2$day)

# Get time set up.
DAM_data2$datetime <- as.POSIXct( strptime( paste( DAM_data2$day,DAM_data2$month,DAM_data2$year, DAM_data2$time), 
                                 "%d %B %y %H:%M:%S") )
DAM_data2$monitor <- as.factor(DAM_data2$monitor)
head(DAM_data2) 
# Check to see if the two monitors show similar patterns
# Add more EDA to check...


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

DAM_long2 <- merge(DAM_long, sample_info, by="day.vial")

head(DAM_long2)

# To get minute
DAM_long2$minute <- as.numeric(strftime(DAM_long2$datetime, format ="%M"))
# to get hour
DAM_long2$hour <- as.numeric(strftime(DAM_long2$datetime, format ="%H"))


# To create a variable for each individual
DAM_long2$individual <- with(DAM_long2, interaction(day.vial, monitor, drop=FALSE))
nlevels(DAM_long2$individual)

### quick analysis: This is with an average activity level per individual computed across the whole day. So it ignores daily rhythms, but does get an aggregate measure.

# compute mean activity (per minute) across the whole day
crap3 <- with(DAM_long2, aggregate(activity_counts, FUN=mean, by=list(individual, Trt, Population, monitor, start_day)))
colnames(crap3) <- c("individual", "Trt", "Population",  "monitor", "start_day","mean_activity" )
lattice::bwplot(mean_activity ~ as.factor(Population):Trt, data=crap3)
lattice::bwplot(mean_activity ~ as.factor(monitor):as.factor(start_day), data=crap3)

crap3$Population <- as.factor(crap3$Population)
crap3$monitor <- as.factor(crap3$monitor)
crap3$start_day <- as.factor(crap3$start_day)

# The model
crap3.lmer <- lmer(mean_activity ~ Trt + monitor + (1|Trt:Population)  + (1|start_day), data=crap3)
car::Anova(crap3.lmer)
summary(crap3.lmer)
confint(crap3.lmer)



### Getting hourly average
dat.hourly <- with(DAM_long2, 
     aggregate(activity_counts, FUN=sum, by=list(vial.x, monitor, start_day, hour, Trt, Population)))
colnames(dat.hourly) <- c("vial.x", "monitor", "start_day", "hour", "Trt", "Population", "Hourly_activity")     
dat.hourly$individual <- with(dat.hourly, interaction(start_day, vial.x, monitor, drop=FALSE))

# compare this....
test <- dat.hourly[dat.hourly$individual=="23.vial1.1",]

# test - TO THIS
craps <- DAM_long2[DAM_long2$individual=="23.vial1.1",]
craps.hour <- aggregate(craps$activity_counts, FUN=sum, by=list(craps$hour))
dim(craps.hour)
# test 
cbind(test$Hourly_activity,test$hour, craps.hour)

## Ok, everything seems to work. I can work with dat.hourly.
with(dat.hourly, boxplot(Hourly_activity ~ hour))

#need to make 8 the zero hour (start of experiment). Since the "hour goes to zero at midnight, 24 hours needs to added to the calculation
dat.hourly$hour.shifted <- ifelse(dat.hourly$hour >= 8, (dat.hourly$hour - 8), (dat.hourly$hour - 8 +24 ))
with(dat.hourly, boxplot(Hourly_activity ~ hour.shifted))

dat.hourly$Population <- as.factor(dat.hourly$Population)
dat.hourly$start_day <- as.factor(dat.hourly$start_day)

# lights on at 10AM and off at 10PM
dat.hourly$light <- with(dat.hourly, ifelse(hour >= 10 & hour < 22, "light", "dark"))
library(nlme)

hour.mod <- lm(Hourly_activity ~ Trt + Trt:Population + hour.shifted + monitor + start_day,data=dat.hourly)
summary(hour.mod)
pacf(resid(hour.mod))

# Accounting for the auto-correlation
gls.mod <- gls(Hourly_activity ~ Trt + Trt:Population + hour.shifted + monitor + start_day,
    correlation = corAR1(form =~hour.shifted|individual),
    data=dat.hourly)
anova(gls.mod)
summary(gls.mod)
acf(resid(gls.mod))

gls.mod.2 <- gls(Hourly_activity ~ Trt + Trt:Population + light + light:Trt +  hour.shifted + monitor + start_day,
    correlation = corAR1(form =~hour.shifted|individual),
    data=dat.hourly)
summary(gls.mod.2)
confint(gls.mod.2)


with(dat.hourly, plot(Hourly_activity ~ jitter(hour.shifted), pch=20, cex=0.1))
lines(smooth.spline(y=dat.hourly$Hourly_activity, x = dat.hourly$hour.shifted))
lines(lowess(dat.hourly$Hourly_activity ~ dat.hourly$hour.shifted, f=0.1), col="red")


with(dat.hourly[dat.hourly$Trt=="C",], 
    plot(Hourly_activity ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2,
         xlab ="hours after initiation", ylab = "hourly activity",
         main = "temporal patterns of activity ",
         ylim=c(0,800)))
         
with(dat.hourly[dat.hourly$Trt=="C",], lines(smooth.spline(y=Hourly_activity, x = hour.shifted),lwd=2))

with(dat.hourly[dat.hourly$Trt=="P",], points(Hourly_activity ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2, col="red"))
with(dat.hourly[dat.hourly$Trt=="P",], lines(smooth.spline(y=Hourly_activity, x = hour.shifted), col="red", lwd=2))

with(dat.hourly[dat.hourly$Trt=="S",], points(Hourly_activity ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2, col="blue"))
with(dat.hourly[dat.hourly$Trt=="S",], lines(smooth.spline(y=Hourly_activity, x = hour.shifted), col="blue", lwd=2))

legend(x=21, y=820, legend=c("LTC", "LTP", "LTS"), pch=20, col=c(1, "red", "blue"))
rect(xleft=2, xright=14, ybottom = 0, ytop = 830, col="#ffff0032", border=NA)




# should we try this with the per minute measures?



# other time series approaches?
#try with one individual (60*24 time points)
ts_trial1 <- with(DAM_long2[1:1440,], ts(activity_counts, start=1, frequency=60)) # hours (1440/24)
ts_trial2 <- with(DAM_long2[1:1440,], ts(activity_counts, start=1, frequency=288)) # 5minute intervals 1440/5)

plot(stl(ts_trial1, s.window=2))

plot(stl(ts_trial2, s.window=2))



### Sleep analysis ####

# we will (based on recommendation from RD) use the following definition of "sleep" for flies.
# intervals of 5 minutes with no recorded activity. We will then do sliding window assessment of sleep non sleep.
# So for each individual (at each time point) we will record sleeping as binary, and treat these binary assessment as the response variable in the time series models.


# Generate data frame for "sleep"

# First think about the strategy for recoding the data. Currently we have minute by minute counts.
head(DAM_long2)
table(DAM_long2$activity_counts)
hist(DAM_long2$activity_counts)

# Use a for loop to iterate within an individual, and then by or tapply to do it over each individual?

# Make a variable for each individual
DAM_long2$individual <- with(DAM_long2, interaction(start_day, vial.x, monitor, drop=TRUE))
nlevels(DAM_long2$individual)
table(DAM_long2$individual) # Each cell has 1440 observations as expected (24*60)


# Test case with one individual
levels(DAM_long2$individual)[1] # First individual
test_dat <- DAM_long2[DAM_long2$individual == "19.vial1.1",]

dim(test_dat)

# First test computing sleep in 5 minute intervals
sleep_test <- rep(NA,length = (nrow(test_dat) - 4))
time_test <- rep(NA, length = (nrow(test_dat)- 4)) # we only need time for one point in time interval
counts <- rep(NA,length = (nrow(test_dat) - 4))
for (i in 1:(nrow(test_dat) -4)) {  # the minus 4 so we do not go beyond the time intervals..
	
	# calculate sum of activity counts in 5 minute interval
	counts[i] <- sum(test_dat$activity_counts[i:(i+4)]) 
}

sleep_test2 <- ifelse(counts  == 0, 0, 1)
sleep_test3 <- ifelse(counts  <= 1, 0, 1)
mean_counts <- mean(counts[counts > 0]) # mean activity when they are not "sleeping"
sleep_test4 <- ifelse(counts  < 0.05*mean_counts, 0, 1)
par(mfrow=c(4,1))
plot(counts, col="red", main = "counts in 5 minute intervals")
plot(sleep_test2, col="blue", main = "assume no noise")
plot(sleep_test3, col="blue", main = " assume a bit of noise")
plot(sleep_test4, col="blue", main = " less than 5% of mean activity")

# These all seem to give pretty similar results. This is because the mean counts across the day is generally small enough that 5% means a value less 1, so it will generally be the same. I guess go with the standard?


### Generate it over the whole data set first, and then just remove the rows where the summing would cross individuals.

test_dat <- DAM_long2
counts <- rep(NA,length = (nrow(DAM_long2) - 4))
for (i in 1:(nrow(DAM_long2) -4)) {  # the minus 4 so we do not go beyond the time intervals..
	
	# calculate sum of activity counts in 5 minute interval
	counts[i] <- sum(DAM_long2$activity_counts[i:(i+4)]) 
}
sleep_test2 <- ifelse(counts  == 0, 0, 1) # 4 cells shorter than the full data set
DAM_long2$sleep_score <- c(sleep_test2, NA, NA, NA, NA)

head(DAM_long2)
tail(DAM_long2)

# Now for each individual we have 1440 rows (60*24). However, the last 4 rows for each individual (1437:1440, 2*(1437:1440), 240*(1437:1440)) are incorrect using the above method and need to be removed.

index_for_removal <- c((1440*(0:239)) + 1437, (1440*(0:239)) + 1438,(1440*(0:239)) + 1439 ,(1440*(0:239)) + 1440)
length(index_for_removal) # 960 

DAM_long3 <- DAM_long2[-index_for_removal,]

(nrow(DAM_long3) + length(index_for_removal)) == nrow(DAM_long2) # Correct size of data frame

# Visual inspection
DAM_long3[(2870:2878),]

## Ok this seems to be working. Now for the model.

# compute mean "sleep" across the whole day
crap4 <- with(DAM_long3, aggregate(sleep_score, FUN=mean, by=list(individual, Trt, Population, monitor, start_day)))
colnames(crap4) <- c("individual", "Trt", "Population",  "monitor", "start_day","mean_sleep" )
lattice::bwplot(mean_sleep~ Trt, data=crap4)
lattice::bwplot(mean_sleep~ as.factor(Population):Trt, data=crap4)
lattice::bwplot(mean_sleep ~ as.factor(monitor):as.factor(start_day), data=crap4)

# Hourly sleep average
sleep.hourly <- with(DAM_long3, 
     aggregate(sleep_score, FUN=mean, by=list(vial.x, monitor, start_day, hour, Trt, Population)))
colnames(sleep.hourly) <- c("vial.x", "monitor", "start_day", "hour", "Trt", "Population", "Hourly_sleep")     
sleep.hourly$individual <- with(sleep.hourly, interaction(start_day, vial.x, monitor, drop=FALSE))

sleep.hourly$hour.shifted <- ifelse(sleep.hourly$hour >= 8, (sleep.hourly$hour - 8), (sleep.hourly$hour - 8 +24 ))
with(sleep.hourly, boxplot(Hourly_sleep ~ hour.shifted))

sleep.hourly$Population <- as.factor(sleep.hourly$Population)
sleep.hourly$start_day <- as.factor(sleep.hourly$start_day)

# lights on at 10AM and off at 10PM
sleep.hourly$light <- with(sleep.hourly, ifelse(hour >= 10 & hour < 22, "light", "dark"))

gls.mod.2.sleep <- gls(Hourly_sleep ~ Trt + Trt:Population + light + light:Trt + hour.shifted + monitor + start_day,
    correlation = corAR1(form =~hour.shifted|individual),
    data=sleep.hourly)
summary(gls.mod.2.sleep)
confint(gls.mod.2.sleep)

lmm.sleep <- lmer(Hourly_sleep ~ Trt + (1|Trt:Population) + light + light:Trt +  hour.shifted + monitor + (1|start_day),
    data = sleep.hourly)
summary(lmm.sleep)  

lmm.sleep.2 <- lmer(Hourly_sleep ~ Trt + (1|Trt:Population) + light + light:Trt +  hour.shifted + monitor 
    + (1|start_day) + (hour.shifted|individual),
    data = sleep.hourly)
summary(lmm.sleep.2) 

car::Anova(lmm.sleep.2)  


with(sleep.hourly, plot(Hourly_sleep ~ jitter(hour.shifted), pch=20, cex=0.1))
lines(smooth.spline(y=sleep.hourly$Hourly_sleep, x = sleep.hourly$hour.shifted))
lines(lowess(sleep.hourly$Hourly_sleep ~ sleep.hourly$hour.shifted, f=0.1), col="red")


with(sleep.hourly[sleep.hourly$Trt=="C",], 
    plot(Hourly_sleep ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2,
         xlab ="hours after initiation", ylab = "hourly sleep (mean)",
         main = "temporal patterns of sleep ",
         ylim=c(0,1)))
         
with(sleep.hourly[sleep.hourly$Trt=="C",], lines(smooth.spline(y=Hourly_sleep, x = hour.shifted),lwd=2))

with(sleep.hourly[sleep.hourly$Trt=="P",], points(Hourly_sleep ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2, col="red"))
with(sleep.hourly[sleep.hourly$Trt=="P",], lines(smooth.spline(y=Hourly_sleep, x = hour.shifted), col="red", lwd=2))

with(sleep.hourly[sleep.hourly$Trt=="S",], points(Hourly_sleep ~ jitter(hour.shifted, factor=1.3), pch=20, cex=0.2, col="blue"))
with(sleep.hourly[sleep.hourly$Trt=="S",], lines(smooth.spline(y=Hourly_sleep, x = hour.shifted), col="blue", lwd=2))

legend(x=21, y=0.9, legend=c("LTC", "LTP", "LTS"), pch=20, col=c(1, "red", "blue"))
rect(xleft=2, xright=14, ybottom = 0, ytop = 1, col="#ffff0032", border=NA)


   