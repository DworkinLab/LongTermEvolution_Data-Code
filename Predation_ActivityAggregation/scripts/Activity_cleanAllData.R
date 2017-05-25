###Clean activity data script (to use as a source file)

#######################################

source("Packages_source_file.R")

#Naming:
# LT's == DAM / dat
# ComplexCues2 == Exp2
# ComplexCues3 == Exp3
# Mantid Cues == Mantid
# SpiderCues == No prefix

########## Long Term Evolved Populations:


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

DAM_long2 <- merge(DAM_long, sample_info, by="day.vial")

head(DAM_long2)

# To get minute
DAM_long2$minute <- as.numeric(strftime(DAM_long2$datetime, format ="%M"))
# to get hour
DAM_long2$hour <- as.numeric(strftime(DAM_long2$datetime, format ="%H"))


# To create a variable for each individual
DAM_long2$individual <- with(DAM_long2, interaction(day.vial, monitor, drop=FALSE))
nlevels(DAM_long2$individual)


dat.hourly <- with(DAM_long2, 
                   aggregate(activity_counts, FUN=sum, by=list(vial.x, monitor, start_day, hour, Trt, Population)))
colnames(dat.hourly) <- c("vial.x", "monitor", "start_day", "hour", "Trt", "Population", "Hourly_activity")     
dat.hourly$individual <- with(dat.hourly, interaction(start_day, vial.x, monitor, drop=FALSE))

dat.hourly$light <- with(dat.hourly, ifelse(hour >= 10 & hour < 22, "light", "dark"))




########## Complex Cues Exp_2:


ComExp2_Mon1 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M1.txt")
ComExp2_Mon2 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M2.txt")

#Variable for monitor:
ComExp2_Mon1$monitor <- 1
ComExp2_Mon2$monitor <- 2

#Comlumn Names:
ex2_colnames <- c("date", "Remove", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'monitor')

colnames(ComExp2_Mon1) <- ex2_colnames
colnames(ComExp2_Mon2) <- ex2_colnames

#Remove unneeded columns (unknowns and remove):
ComExp2_Mon1 <- ComExp2_Mon1[,-c(5:9)]
ComExp2_Mon2 <- ComExp2_Mon2[,-c(5:9)]


ComExp2_Mon1 <- ComExp2_Mon1[,-c(2)]
ComExp2_Mon2 <- ComExp2_Mon2[,-c(2)]

#Reform Date:
ComExp2_Mon1$datetime <- as.POSIXct(paste(ComExp2_Mon1$date, ComExp2_Mon1$time), format="%Y-%m-%d %H:%M:%S")
ComExp2_Mon2$datetime <- as.POSIXct(paste(ComExp2_Mon2$date, ComExp2_Mon2$time), format="%Y-%m-%d %H:%M:%S")


#Reshape to long

ComExp2_Mon1_long <- gather(ComExp2_Mon1, Vial, Activity_counts, vial1:vial16, factor_key = FALSE)
ComExp2_Mon2_long <- gather(ComExp2_Mon2, Vial, Activity_counts, vial1:vial16, factor_key = FALSE)


# To get minute
ComExp2_Mon1_long$minute <- as.numeric(strftime(ComExp2_Mon1_long$datetime, format ="%M"))
ComExp2_Mon2_long$minute <- as.numeric(strftime(ComExp2_Mon2_long$datetime, format ="%M"))
# to get hour
ComExp2_Mon1_long$hour <- as.numeric(strftime(ComExp2_Mon1_long$datetime, format ="%H"))
ComExp2_Mon2_long$hour <- as.numeric(strftime(ComExp2_Mon2_long$datetime, format ="%H"))
# to get day
ComExp2_Mon1_long$day <- as.numeric(strftime(ComExp2_Mon1_long$datetime, format = "%d"))
ComExp2_Mon2_long$day <- as.numeric(strftime(ComExp2_Mon2_long$datetime, format = "%d"))


head(ComExp2_Mon1_long)
head(ComExp2_Mon2_long)

#Variable for treatment:

#Instead of two if else statments; could have changed column names...
#Mon1 == Spider 1-8, Cricket 9-16
ComExp2_Mon1_long$Treatment <- ifelse(ComExp2_Mon1_long$Vial == "vial1", "Spider", ifelse (ComExp2_Mon1_long$Vial == "vial2", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial3", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial4", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial5", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial6", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial7", "Spider", ifelse(ComExp2_Mon1_long$Vial == "vial8", "Spider","Cricket"))))))))

ComExp2_Mon2_long$Treatment <- ifelse(ComExp2_Mon2_long$Vial == "vial9", "Spider", ifelse (ComExp2_Mon2_long$Vial == "vial10", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial11", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial12", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial13", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial14", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial15", "Spider", ifelse(ComExp2_Mon2_long$Vial == "vial16", "Spider","Cricket"))))))))

Complex_2_long <- rbind(ComExp2_Mon1_long, ComExp2_Mon2_long)

#Change to factors:
Complex_2_long$monitor <- as.factor(Complex_2_long$monitor)
Complex_2_long$Treatment <- as.factor(Complex_2_long$Treatment)
Complex_2_long$day <- as.factor(Complex_2_long$day)
Complex_2_long$Vial <- as.factor(Complex_2_long$Vial)

Exp2_hour <- Complex_2_long %>%
  group_by(Treatment, Vial, monitor, day, hour, hour) %>%
  summarise(activity_counts=sum(Activity_counts))





########### Complex Cues Exp_3:

Exp3_Mon1 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp3_Monitor_1.txt")
Exp3_Mon2 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp3_Monitor_2.txt")

#Monitor variable

Exp3_Mon1$monitor <- 1
Exp3_Mon2$monitor <- 2

#Remove unneeded columns:

Exp3_Mon1 <- Exp3_Mon1[,-c(5:9)]
Exp3_Mon2 <- Exp3_Mon2[,-c(5:9)]

Exp3_Mon1 <- Exp3_Mon1[,-c(2)]
Exp3_Mon2 <- Exp3_Mon2[,-c(2)]

#DateTime variable
#Exp3_Mon1$datetime <- as.POSIXct(paste(Exp3_Mon1$date, Exp3_Mon1$time), format="%Y-%m-%d %H:%M:%S")
#Exp3_Mon2$datetime <- as.POSIXct(paste(Exp3_Mon2$date, Exp3_Mon2$time), format="%Y-%m-%d %H:%M:%S")

Exp3_Mon1$num <- c(1:1560)
Exp3_Mon2$num <- c(1:1560)
#Reform Date:
Exp3_Mon1$datetime <- as.POSIXct(paste(Exp3_Mon1$X, Exp3_Mon1$X.2), format="%Y-%m-%d %H:%M:%S")
Exp3_Mon2$datetime <- as.POSIXct(paste(Exp3_Mon2$X, Exp3_Mon2$X.2), format="%Y-%m-%d %H:%M:%S")


# To get minute
Exp3_Mon1$minute <- as.numeric(strftime(Exp3_Mon1$datetime, format ="%M"))
Exp3_Mon2$minute <- as.numeric(strftime(Exp3_Mon2$datetime, format ="%M"))
# to get hour
Exp3_Mon1$hour <- as.numeric(strftime(Exp3_Mon1$datetime, format ="%H"))
Exp3_Mon2$hour <- as.numeric(strftime(Exp3_Mon2$datetime, format ="%H"))
# to get day
Exp3_Mon1$day <- as.numeric(strftime(Exp3_Mon1$datetime, format = "%d"))
Exp3_Mon2$day <- as.numeric(strftime(Exp3_Mon2$datetime, format = "%d"))

head(Exp3_Mon1)
head(Exp3_Mon2)

#Column Names:
#Make F or C == Vial1_fly and vial2_cricket. etc.
#Mon1: date, time, signal,Light, SF SF.1 SF.2 SF.3 SF.4 SF.5 SF.6 SF.7 SC SC.1 SC.2 SC.3 SC.4 SC.5 SC.6 SC.7 F,  F.1 F.2 F.3 F.4 F.5 F.6 F.7 C C.1 C.2 C.3 C.4 C.5 monitor            datetime minute hour day
colnames(Exp3_Mon1) <- c("date", "time", "signal", "lightON",'vial1_SF', 'vial2_SF', 'vial3_SF', 'vial4_SF', 'vial5_SF', 'vial6_SF', 'vial7_SF', 'vial8_SF', 'vial9_SC', 'vial10_SC', 'vial11_SC', 'vial12_SC', 'vial13_SC', 'vial14_SC', 'vial15_SC', 'vial16_SC','vial17_F','vial18_F','vial19_F','vial20_F','vial21_F','vial22_F','vial23_F','vial24_F','vial25_C','vial26_C','vial27_C','vial28_C','vial29_C','vial30_C', 'monitor','num', 'datetime', 'minute', 'hour', 'day')

#Mon2:
##  date time signal Light  F F.1 F.2 F.3 F.4 F.5 F.6 C C.1 C.2 C.3 C.4 SF SF.1 SF.2 SF.3 SF.4 SF.5 SF.6,  SC SC.1 SC.2 SC.3 SC.4 SC.5 SC.6 SC.7 monitor datetime minute hour day 

colnames(Exp3_Mon2) <- c("date", "time", "signal", "lightON",'vial1_F', 'vial2_F', 'vial3_F', 'vial4_F', 'vial5_F', 'vial6_F', 'vial7_F', 'vial8_C', 'vial9_C', 'vial10_C', 'vial11_C', 'vial12_C', 'vial13_SF', 'vial14_SF', 'vial15_SF', 'vial16_SF','vial17_SF','vial18_SF','vial19_SF','vial20_SC','vial21_SC','vial22_SC','vial23_SC','vial24_SC','vial25_SC','vial26_SC','vial27_SC', 'monitor','num','datetime', 'minute', 'hour', 'day')

head(Exp3_Mon1)
head(Exp3_Mon2)


##Make long:


Exp3_Mon1_long <- gather(Exp3_Mon1, Vial, activity_counts, vial1_SF:vial30_C, factor_key = FALSE)
Exp3_Mon2_long <- gather(Exp3_Mon2, Vial, activity_counts, vial1_F:vial27_SC, factor_key = FALSE)

## Split vial and treatment (SF, F, SC, and C (or need to change how this is done...))

Exp3_Mon1_long <- Exp3_Mon1_long %>%
  separate(Vial, c("Vial", "Treatment"), "_")
Exp3_Mon2_long <- Exp3_Mon2_long %>%
  separate(Vial, c("Vial", "Treatment"), "_")
head(Exp3_Mon1_long)

#Combine into one data set:
Exp3_long <- rbind(Exp3_Mon1_long, Exp3_Mon2_long)

Exp3_long$monitor <- as.factor(Exp3_long$monitor)
Exp3_long$Treatment <- as.factor(Exp3_long$Treatment)
Exp3_long$day <- as.factor(Exp3_long$day)
Exp3_long$Vial <- as.factor(Exp3_long$Vial)


#By hour:

Exp3_hour <- Exp3_long %>%
  group_by(Treatment, Vial, monitor, day, hour, hour) %>%
  summarise(activity_counts=sum(activity_counts))

Exp3_hour$individual <- with(Exp3_hour, interaction(day, Vial, monitor, drop=FALSE))


Exp3_hour$hour <- as.numeric(Exp3_hour$hour)


################# Mantid Cues

#The Data
MantidMon1 <- read.table("../data/Activity_Drosophila_MantidCues_May2016/Mantid_Cues_M1.txt")
MantidMon2 <- read.table("../data/Activity_Drosophila_MantidCues_May2016/Mantid_Cues_M2.txt")

head(MantidMon1)
head(MantidMon2)
#Monitor Variable
MantidMon1$V43 <- 1
MantidMon2$V43 <- 2

#Light = V10
#Mon1 pred = V11-V18, con = V35-V42
#Mon2 con = V11-V18, pred = V25-V42

man_colnames <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

colnames(MantidMon1) <- man_colnames
colnames(MantidMon2) <- man_colnames
  
#Remove unknowns
MantidMon1 <- MantidMon1[,-c(5:9)]
MantidMon2 <- MantidMon2[,-c(5:9)]

#Remove unneeded vials (i.e vials 9 - 24)
#14 - 29
MantidMon1 <- MantidMon1[,-c(14:29)]
MantidMon2 <- MantidMon2[,-c(14:29)]

#Change date-time
MantidMon1$datetime <- as.POSIXct(paste(MantidMon1$date, MantidMon1$time), format="%m/%d/%y %H:%M:%S")
MantidMon2$datetime <- as.POSIXct(paste(MantidMon2$date, MantidMon2$time), format="%m/%d/%y %H:%M:%S")

#Reshape to long

MantidMon1_long <- gather(MantidMon1, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)
MantidMon2_long <- gather(MantidMon2, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)


# To get minute
MantidMon1_long$minute <- as.numeric(strftime(MantidMon1_long$datetime, format ="%M"))
MantidMon2_long$minute <- as.numeric(strftime(MantidMon2_long$datetime, format ="%M"))
# to get hour
MantidMon1_long$hour <- as.numeric(strftime(MantidMon1_long$datetime, format ="%H"))
MantidMon2_long$hour <- as.numeric(strftime(MantidMon2_long$datetime, format ="%H"))
# to get day
MantidMon1_long$day <- as.numeric(strftime(MantidMon1_long$datetime, format = "%d"))
MantidMon2_long$day <- as.numeric(strftime(MantidMon2_long$datetime, format = "%d"))

#Predator or control

MantidMon1_long$Treatment <- ifelse(MantidMon1_long$Vial == "vial1", "Mantid", ifelse (MantidMon1_long$Vial == "vial2", "Mantid", ifelse(MantidMon1_long$Vial == "vial3", "Mantid", ifelse(MantidMon1_long$Vial == "vial4", "Mantid", ifelse(MantidMon1_long$Vial == "vial5", "Mantid", ifelse(MantidMon1_long$Vial == "vial6", "Mantid", ifelse(MantidMon1_long$Vial == "vial7", "Mantid", ifelse(MantidMon1_long$Vial == "vial8", "Mantid","Control"))))))))

MantidMon2_long$Treatment <- ifelse(MantidMon2_long$Vial == "vial1", "Control", ifelse (MantidMon2_long$Vial == "vial2", "Control", ifelse(MantidMon2_long$Vial == "vial3", "Control", ifelse(MantidMon2_long$Vial == "vial4", "Control", ifelse(MantidMon2_long$Vial == "vial5", "Control", ifelse(MantidMon2_long$Vial == "vial6", "Control", ifelse(MantidMon2_long$Vial == "vial7", "Control", ifelse(MantidMon2_long$Vial == "vial8", "Control","Mantid"))))))))

Mantid_long <- rbind(MantidMon1_long, MantidMon2_long)

head(Mantid_long)

Mantid_long$monitor <- as.factor(Mantid_long$monitor)
Mantid_long$Treatment <- as.factor(Mantid_long$Treatment)
Mantid_long$day <- as.factor(Mantid_long$day)
Mantid_long$Vial <- as.factor(Mantid_long$Vial)

#Hourly activity

Mantid_hour <- Mantid_long %>%
  group_by(Treatment, Vial, monitor, day, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

Mantid_hour$individual <- with(Mantid_hour, interaction(day, Vial, monitor, drop=FALSE))
Mantid_hour$light <- with(Mantid_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))


################# Spider Cues

Mon1 <- read.table("../data/Activity_Drosophila_SpiderCues_May2016/DrosophilaActivity_Spider_cues_Monitor1_May2016.txt")
Mon2 <- read.table("../data/Activity_Drosophila_SpiderCues_May2016/DrosophilaActivity_Spider_cues_Monitor2_May2016.txt")

#Variable for monitor:
Mon1$V43 <- 1
Mon2$V43 <- 2
col.names <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

colnames(Mon1) <- col.names
colnames(Mon2) <- col.names

#Remove unknowns
Mon1 <- Mon1[,-c(5:9)]
Mon2 <- Mon2[,-c(5:9)]

#Remove unneeded vials (i.e vials 9 - 24)
Mon1 <- Mon1[,-c(14:29)]
Mon2 <- Mon2[,-c(14:29)]

#Change Day format
Mon1$datetime <- as.POSIXct(paste(Mon1$date, Mon1$time), format="%m/%d/%y %H:%M:%S")
Mon2$datetime <- as.POSIXct(paste(Mon2$date, Mon2$time), format="%m/%d/%y %H:%M:%S")

#Reshape to long
Mon1_long <- gather(Mon1, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)
Mon2_long <- gather(Mon2, Vial, Activity_counts, vial1:vial32, factor_key = FALSE)

# To get minute
Mon1_long$minute <- as.numeric(strftime(Mon1_long$datetime, format ="%M"))
Mon2_long$minute <- as.numeric(strftime(Mon2_long$datetime, format ="%M"))

# to get hour
Mon1_long$hour <- as.numeric(strftime(Mon1_long$datetime, format ="%H"))
Mon2_long$hour <- as.numeric(strftime(Mon2_long$datetime, format ="%H"))

# to get day
Mon1_long$day <- as.numeric(strftime(Mon1_long$datetime, format = "%d"))
Mon2_long$day <- as.numeric(strftime(Mon2_long$datetime, format = "%d"))

# monitor 1: vials 1-8 == control ----- vials 25-32 == predator
# monitor 2: vials 1-8 = predator ------ vials 25- 32 == Control

Mon1_long$Treatment <- ifelse(Mon1_long$Vial == "vial1", "Control", ifelse (Mon1_long$Vial == "vial2", "Control", ifelse(Mon1_long$Vial == "vial3", "Control", ifelse(Mon1_long$Vial == "vial4", "Control", ifelse(Mon1_long$Vial == "vial5", "Control", ifelse(Mon1_long$Vial == "vial6", "Control", ifelse(Mon1_long$Vial == "vial7", "Control", ifelse(Mon1_long$Vial == "vial8", "Control","Spider"))))))))

Mon2_long$Treatment <- ifelse(Mon2_long$Vial == "vial1", "Spider", ifelse (Mon2_long$Vial == "vial2", "Spider", ifelse(Mon2_long$Vial == "vial3", "Spider", ifelse(Mon2_long$Vial == "vial4", "Spider", ifelse(Mon2_long$Vial == "vial5", "Spider", ifelse(Mon2_long$Vial == "vial6", "Spider", ifelse(Mon2_long$Vial == "vial7", "Spider", ifelse(Mon2_long$Vial == "vial8", "Spider","Control"))))))))

Act_long <- rbind(Mon1_long, Mon2_long)

Act_long$monitor <- as.factor(Act_long$monitor)
Act_long$Treatment <- as.factor(Act_long$Treatment)
Act_long$day <- as.factor(Act_long$day)
Act_long$Vial <- as.factor(Act_long$Vial)

day_act <- Act_long %>%
  group_by(Treatment, monitor, Vial, day) %>%
  summarise(mean_activity=mean(Activity_counts))

act_hour <- Act_long %>%
  group_by(Treatment, Vial, monitor, day, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

act_hour$light <- with(act_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))


#### For all: have Activity in long format and activity by hour
