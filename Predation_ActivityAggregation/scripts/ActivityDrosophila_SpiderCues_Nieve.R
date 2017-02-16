#Analysis of nieve flies (Dukas Lab Flies) and activity with only olfactory predatory cues

#From Dukas ReadMe: "Spring 2016 - naive population with predator cues. Note from RD: my color codes on the Excel fiels are yellow for light, orange for spider cues and green for control" 

#Start by getting .txt data.. online conversion?

#Libraries
library(lme4)
library(tidyr)
library(dplyr)
library(ggplot2)

#Data input: working directory 
setwd("~/Bioinformatics/Long_Term_Data/Data_LongTermPopulationExperiments_Git/Predation_ActivityAggregation/data/Activity_Drosophila_SpiderCues_May2016")

Mon1 <- read.table("DrosophilaActivity_Spider_cues_Monitor1_May2016.txt")
Mon2 <- read.table("DrosophilaActivity_Spider_cues_Monitor2_May2016.txt")

#Variable for monitor:
Mon1$V43 <- 1
Mon2$V43 <- 2

colnames(Mon1) <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

colnames(Mon2) <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

#Remove unknowns
Mon1 <- Mon1[,-c(5:9)]
Mon2 <- Mon2[,-c(5:9)]
#Remove unneeded vials (i.e vials 9 - 24)
#14 - 29
Mon1 <- Mon1[,-c(14:29)]
Mon2 <- Mon2[,-c(14:29)]

#Change Day format
Mon1$datetime <- as.POSIXct(paste(Mon1$date, Mon1$time), format="%m/%d/%y %H:%M:%S")
Mon2$datetime <- as.POSIXct(paste(Mon2$date, Mon2$time), format="%m/%d/%y %H:%M:%S")


#Reshape to long
#Episodic_long <- gather(Data_subset, Population, Allele_Freq , ConR1_115:AncestorR1_0, factor_key=TRUE)
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

head(Mon1_long)
head(Mon2_long)


# monitor 1: 
#vials 1-8 == control ----- vials 25-32 == predator
# monitor 2:
#vials 1-8 = predator ------ vials 25- 32 == Control

Mon1_long$Treatment <- ifelse(Mon1_long$Vial == "vial1", "Control", ifelse (Mon1_long$Vial == "vial2", "Control", ifelse(Mon1_long$Vial == "vial3", "Control", ifelse(Mon1_long$Vial == "vial4", "Control", ifelse(Mon1_long$Vial == "vial5", "Control", ifelse(Mon1_long$Vial == "vial6", "Control", ifelse(Mon1_long$Vial == "vial7", "Control", ifelse(Mon1_long$Vial == "vial8", "Control","Spider"))))))))

Mon2_long$Treatment <- ifelse(Mon2_long$Vial == "vial1", "Spider", ifelse (Mon2_long$Vial == "vial2", "Spider", ifelse(Mon2_long$Vial == "vial3", "Spider", ifelse(Mon2_long$Vial == "vial4", "Spider", ifelse(Mon2_long$Vial == "vial5", "Spider", ifelse(Mon2_long$Vial == "vial6", "Spider", ifelse(Mon2_long$Vial == "vial7", "Spider", ifelse(Mon2_long$Vial == "vial8", "Spider","Control"))))))))

Act_long <- rbind(Mon1_long, Mon2_long)

#Make individual codes for each ??? -- look at old code?


#Note; one column (when dark == purple on excel) has a jump in activity of 1 for that time minute--> both monitors == bin 595

#Forgot this:
Act_long$monitor <- as.factor(Act_long$monitor)

#Quick look at daily activity
day_act <- Act_long %>%
  group_by(Treatment, monitor, Vial, day) %>%
  summarise(mean_activity=mean(Activity_counts))


lattice::bwplot(mean_activity ~ as.factor(Treatment), data=day_act)
lattice::bwplot(mean_activity ~ as.factor(monitor):as.factor(day), data=day_act)

#My own plots (ggplot which I know better)

#p1 <- ggplot(data = day_act, aes(x = monitor, y = mean_activity, colour = Treatment))
#p1+geom_boxplot()

plo2 <- ggplot(data = day_act, aes(x = Treatment, y = mean_activity, colour = monitor))
plo2+geom_boxplot()

plo3 <- ggplot(data = day_act, aes(Treatment, mean_activity))
plo3+geom_boxplot()

day_act$Treatment <- as.factor(day_act$Treatment)
#copied model from Ian Script (June2015)

#changed some: look at old (diff)
day_act.lmer <- lmer(mean_activity ~ Treatment + day + (1|day), data=day_act)
car::Anova(day_act.lmer)
summary(day_act.lmer)
confint(day_act.lmer)
plot(allEffects(day_act.lmer))
