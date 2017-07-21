#Activity Mantid Cues: Read and Clean


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

Mantid_hour$individual <- with(Mantid_hour, interaction(Vial, monitor, drop=FALSE))

Mantid_hour$light <- with(Mantid_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))