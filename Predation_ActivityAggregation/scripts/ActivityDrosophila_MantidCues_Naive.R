#Analysis of naive flies (Dukas Lab Flies) and activity with mantid cues

#From Dukas ReadMe: "Spring 2016 - naive population with predator cues. Note from RD: my color codes on the Excel fiels are yellow for light, orange for spider cues and green for control" 
#-- Assumption = orange == mantid cues -- confirmed in meeting with RD 

#Convert .xlsx to .txt files (and rename to consistant format) online conversion
#https://www.coolutils.com/online/XLSX-to-TXT

library(lme4)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(effects)

#Data input: working directory 
setwd("~/Bioinformatics/Long_Term_Data/Data_LongTermPopulationExperiments_Git/Predation_ActivityAggregation/data/Activity_Drosophila_MantidCues_May2016")

#The Data
MantidMon1 <- read.table("Mantid_Cues_M1.txt")
MantidMon2 <- read.table("Mantid_Cues_M2.txt")

head(MantidMon1)
head(MantidMon2)
#Monitor Variable
MantidMon1$V43 <- 1
MantidMon2$V43 <- 2

#Light = V10
#Mon1 pred = V11-V18, con = V35-V42
#Mon2 con = V11-V18, pred = V25-V42

colnames(MantidMon1) <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

colnames(MantidMon2) <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

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



## Data now all cleaned up:
#WARNING: same variables mostly from Spider script -- Check everything (hour shifting and all may be wrong;; also opposite monitor layout!)


Mantid_hour <- Mantid_long %>%
  group_by(Treatment, Vial, monitor, day, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

Mantid_hour$individual <- with(Mantid_hour, interaction(day, Vial, monitor, drop=FALSE))

##Possibly need to shift times to start of experiment?
#head(Mon1)
#head(Mon2)
MantidMon2$time
#Start time = first reading == 14:01
#shift times by 1 hours:
Mantid_hour$hour <- as.numeric(Mantid_hour$hour)
Mantid_hour$hour_shift <- ifelse(Mantid_hour$hour >= 14, (Mantid_hour$hour - 14), (Mantid_hour$hour +10))


Mantid_hour$hour_shift <- as.factor(Mantid_hour$hour_shift)
man_hourShift_gg <- ggplot(Mantid_hour, aes(x=hour_shift, y= activity_counts, colour=Treatment))
man_hourShift_gg + geom_boxplot()

#Light or Dark -- light on at ~10am, off at 22:00
#Do before hour shifting? -- don't use hour shift and it works
#Actually off at 22:05 and on at 10:05???????????????????????

Mantid_hour$light <- with(Mantid_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))

#Man_hour.mod <- lm(activity_counts ~ Treatment + hour_shift + monitor + day,data=Mantid_hour)
#summary(Man_hour.mod)
#pacf(resid(Man_hour.mod))

#Auto-correlation:
#act_hour$individual <- as.factor(act_hour$individual)
#act_hour$hour_shift <- as.factor(act_hour$hour_shift)

#??gls
#generalized least squares

man_correl_mod <- gls(activity_counts ~ Treatment + hour_shift + monitor + day, correlation = corAR1(form = ~ 1|hour_shift), data=Mantid_hour)
anova(man_correl_mod)
summary(man_correl_mod)
acf(resid(man_correl_mod))

#With light
#manCor_lightMod <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour_shift + monitor + day, correlation = corAR1(form =~1|hour_shift), control = list(singular.ok = TRUE), data=Mantid_hour)
#summary(manCor_lightMod)
#confint(manCor_lightMod)
#acf(resid(manCor_lightMod))

#Same Plot:::::
Mantid_hour$hour_shift <- as.numeric(Mantid_hour$hour_shift)


with(Mantid_hour, plot(activity_counts ~ jitter(hour_shift), pch=20, cex=0.1))
lines(smooth.spline(y=Mantid_hour$activity_counts, x = Mantid_hour$hour_shift))
lines(lowess(Mantid_hour$activity_counts ~ Mantid_hour$hour_shift, f=0.1), col="red")


with(Mantid_hour[Mantid_hour$Treatment=="Control",], 
     plot(activity_counts ~ jitter(hour_shift, factor=1.3), pch=20, cex=0.2,
          xlab ="hours after initiation", ylab = "hourly activity",
          main = "Activity: Lab Flies ",
          ylim=c(0,100)))

with(Mantid_hour[Mantid_hour$Treatment=="Control",], lines(smooth.spline(y=activity_counts, x = hour_shift),lwd=2))

with(Mantid_hour[Mantid_hour$Treatment=="Mantid",], points(activity_counts ~ jitter(hour_shift, factor=1.3), pch=20, cex=0.2, col="red"))
with(Mantid_hour[Mantid_hour$Treatment=="Mantid",], lines(smooth.spline(y=activity_counts, x = hour_shift), col="red", lwd=2))


legend(x=15, y=200, legend=c("Control", "Mantid"), pch=20, col=c(1, "red"))

#Check that the rectangle is for light! == Yes == Remember -- actually off by ~ few minutes of light vs. Dark (10:05...)

#Change when lights went on.. 10:00 am, start was noon (lights already on..., off at 10 at night -- shift == off at "10" after 0, on at 22)
rect(xleft=0, xright=8, ybottom = 0, ytop = 830, col="#ffff0032", border=NA)
rect(xleft=20, xright=24, ybottom = 0, ytop = 830, col="#ffff0032", border=NA)


#Seems odd: check numbers!

