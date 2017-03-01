#Analysis of nieve flies (Dukas Lab Flies) and activity with only olfactory predatory cues

#From Dukas ReadMe: "Spring 2016 - naive population with predator cues. Note from RD: my color codes on the Excel fiels are yellow for light, orange for spider cues and green for control" 

#Start by getting .txt data.. online conversion?

#Libraries
library(lme4)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(effects)

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
head(Act_long)
Act_long$monitor <- as.factor(Act_long$monitor)
Act_long$Treatment <- as.factor(Act_long$Treatment)
Act_long$day <- as.factor(Act_long$day)
Act_long$Vial <- as.factor(Act_long$Vial)
#Quick look at daily activity

day_act <- Act_long %>%
  group_by(Treatment, monitor, Vial, day) %>%
  summarise(mean_activity=mean(Activity_counts))


#lattice::bwplot(mean_activity ~ as.factor(Treatment), data=day_act)
#lattice::bwplot(mean_activity ~ as.factor(monitor):as.factor(day), data=day_act)

#My own plots (ggplot which I know better)

#p1 <- ggplot(data = day_act, aes(x = monitor, y = mean_activity, colour = Treatment))
#p1+geom_boxplot()

plo2 <- ggplot(data = day_act, aes(x = Treatment, y = mean_activity, colour = monitor))
plo2+geom_boxplot()

plo3 <- ggplot(data = day_act, aes(Treatment, mean_activity))
plo3+geom_boxplot()

#Moved above to Act_long
#day_act$Treatment <- as.factor(day_act$Treatment)
#day_act$day <- as.factor(day_act$day)
#day_act$monitor <- as.factor(day_act$monitor)


#copied model from Ian Script (June2015)

#changed some: look at old (diff)
head(day_act)
day_act.lmer <- lmer(mean_activity ~ Treatment + day + Vial + monitor + (1|day), data=day_act)
car::Anova(day_act.lmer)
summary(day_act.lmer)
confint(day_act.lmer)
plot(allEffects(day_act.lmer))

head(Act_long)
#All effects:
#Act_mod <- lmer(Activity_counts ~ 1 + Treatment + date + Vial + monitor + (1|date), data = Act_long)
#plot(allEffects(Act_mod))

#Act_all <- Act_long %>%
#  group_by(Treatment, Vial, monitor, day, lightON, time) %>%
#  summarise(meanActivity = mean(Activity_counts))
#act_all_mod <- lmer(meanActivity ~ 1 + Treatment + day + Vial + monitor + lightON + time + (1|day), data = Act_all)


#Hourly Average:
head(Act_long)
act_hour <- Act_long %>%
  group_by(Treatment, Vial, monitor, day, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

#Ian script calls for individuals (did not need above but needed for auto correlation?)

act_hour$individual <- with(act_hour, interaction(day, Vial, monitor, drop=FALSE))

#Ians Boxplot method
with(act_hour, boxplot(activity_counts ~ hour))

#My Boxplot
act_hour$hour <- as.factor(act_hour$hour)
hour_gg <- ggplot(act_hour, aes(x=hour, y= activity_counts, colour=Treatment))
hour_gg + geom_boxplot()


##Possibly need to shift times to start of experiment?
#head(Mon1)
#head(Mon2)
Mon2$time
#Start time = first reading == 12:00
#shift times by 12 hours:
act_hour$hour <- as.numeric(act_hour$hour)
act_hour$hour_shift <- ifelse(act_hour$hour >= 12, (act_hour$hour - 12), (act_hour$hour +12))

#Ian Plot
with(act_hour, boxplot(activity_counts ~ hour_shift))
#My plot
act_hour$hour_shift
act_hour$hour_shift <- as.factor(act_hour$hour_shift)
hourshift_gg <- ggplot(act_hour, aes(x=hour_shift, y= activity_counts, colour=Treatment))
hourshift_gg + geom_boxplot()


head(act_hour)
#Light or Dark -- light on at ~10am, off at 22:00
#Do before hour shifting? -- don't use hour shift and it works
#Actually off at 22:05 and on at 10:05???????????????????????

act_hour$light <- with(act_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))

hour.mod <- lm(activity_counts ~ Treatment + hour_shift + monitor + day,data=act_hour)
summary(hour.mod)
pacf(resid(hour.mod))

#Auto-correlation:
#act_hour$individual <- as.factor(act_hour$individual)
#act_hour$hour_shift <- as.factor(act_hour$hour_shift)

#??gls
#generalized least squares

correl_mod <- gls(activity_counts ~ Treatment + hour_shift + monitor + day, correlation = corAR1(form = ~ 1|hour_shift), data=act_hour)
anova(correl_mod)
summary(correl_mod)
acf(resid(correl_mod))

#With light
act_cor_light_mod <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour_shift + monitor + day, correlation = corAR1(form =~1|hour_shift), control = list(singular.ok = TRUE), data=act_hour)
summary(act_cor_light_mod)
confint(act_cor_light_mod)
acf(resid(act_cor_light_mod))

#Same Plot:::::
act_hour$hour_shift <- as.numeric(act_hour$hour_shift)


with(act_hour, plot(activity_counts ~ jitter(hour_shift), pch=20, cex=0.1))
lines(smooth.spline(y=act_hour$activity_counts, x = act_hour$hour_shift))
lines(lowess(act_hour$activity_counts ~ act_hour$hour_shift, f=0.1), col="red")


with(act_hour[act_hour$Treatment=="Control",], 
     plot(activity_counts ~ jitter(hour_shift, factor=1.3), pch=20, cex=0.2,
          xlab ="hours after initiation", ylab = "hourly activity",
          main = "Activity: Lab Flies ",
          ylim=c(0,400)))

with(act_hour[act_hour$Treatment=="Control",], lines(smooth.spline(y=activity_counts, x = hour_shift),lwd=2))

with(act_hour[act_hour$Treatment=="Spider",], points(activity_counts ~ jitter(hour_shift, factor=1.3), pch=20, cex=0.2, col="red"))
with(act_hour[act_hour$Treatment=="Spider",], lines(smooth.spline(y=activity_counts, x = hour_shift), col="red", lwd=2))


legend(x=15, y=400, legend=c("Control", "Spider"), pch=20, col=c(1, "red"))

#Check that the rectangle is for light! == Yes == Remember -- actually off by ~ few minutes of light vs. Dark (10:05...)

#Change when lights went on.. 10:00 am, start was noon (lights already on..., off at 10 at night -- shift == off at "10" after 0, on at 22)
rect(xleft=0, xright=10, ybottom = 0, ytop = 830, col="#ffff0032", border=NA)
rect(xleft=22, xright=24, ybottom = 0, ytop = 830, col="#ffff0032", border=NA)


