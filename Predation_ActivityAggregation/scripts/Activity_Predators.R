#Predator Activity:

source("Packages_source_file.R")
#To get evolved populations
source("Activity_cleanAllData.R")



spiders <- read.table("../data/Activity_Spiders_Oct2015/SpiderActivityRaw_RD_Oct142015.txt")

#convert("../data/Activity_Mantids_June2016/Mantids All.xlsx", "../data/Activity_Mantids_June2016/Mantids_All.txt")
mantids <- read.table("../data/Activity_Mantids_June2016/Mantids_All.txt")

head(spiders)
#V12 == light, all 44 have spiders
head(mantids)
#Looks like 32 vials for mantids all full

spiders <- spiders[,-c(7:11)]
mantids <- mantids[,-c(5:9)]
mantids <- mantids[,-c(2)]

spi_colnames <- c("bin", "day","month","year", "time", "signal", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32')
colnames(spiders) <- spi_colnames


man_colnames <- c("date", "time", "signal", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32')
colnames(mantids) <- man_colnames

#Date does not matter: only over one 24 hour period but need it to make minutes etc.
spiders$date <- ifelse(spiders$day == 14, "10/14/15", "10/15/15")
spiders$datetime <- as.POSIXct(paste(spiders$date, spiders$time), format="%m/%d/%y %H:%M:%S")
spiders$minute <- as.numeric(strftime(spiders$datetime, format ="%M"))
spiders$hour <- as.numeric(strftime(spiders$datetime, format ="%H"))


mantids$datetime <- as.POSIXct(paste(mantids$date, mantids$time), format="%Y-%m-%d %H:%M:%S")
mantids$minute <- as.numeric(strftime(mantids$datetime, format ="%M"))
mantids$hour <- as.numeric(strftime(mantids$datetime, format ="%H"))
mantids$day <- as.numeric(strftime(mantids$datetime, format = "%d"))

#Make Long
mantids_long <- gather(mantids, vial, Activity_counts, vial1:vial32, factor_key = FALSE)
spiders_long <- gather(spiders, vial, Activity_counts, vial1:vial32, factor_key = FALSE)


mantids_long$day <- as.factor(mantids_long$day)
mantids_long$vial <- as.factor(mantids_long$vial)

mantids_long$day <- as.factor(mantids_long$day)
mantids_long$vial <- as.factor(mantids_long$vial)


mantid_hour <- mantids_long %>%
  group_by(vial, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

spider_hour <- spiders_long %>%
  group_by(vial, hour) %>%
  summarise(activity_counts = sum(Activity_counts))

with(mantid_hour, boxplot(activity_counts ~ hour))
with(spider_hour, boxplot(activity_counts ~ hour))

mantid_hour$light <- with(mantid_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))
spider_hour$light <- with(spider_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))
mantid_hour$Predator <- "Mantid"
spider_hour$Predator <- "Spider"


head(spider_hour)
head(mantid_hour)
daaaa  <- rbind(spider_hour, mantid_hour)

head(daaaa)

Predator_plot <- ggplot(daaaa, aes(x=hour, y= activity_counts, colour=Predator)) + xlim(0,24) + ylim(0,600)
Predator_plot_2 <- Predator_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(Predator_plot_2)


head(dat.hourly)
dat_playing <- dat.hourly
dat_playing <- within(dat_playing, { 
  Predation = ifelse (Trt == "C", "LTC", ifelse(Trt == "S", "LTS", "LTP"))})
dat_playing <- within(dat_playing, Predator <- paste(Predation, Population, sep=""))
head(dat_playing)

colnames(dat_playing) <- c("vial", "monitor", "start_day", "hour", "Trt", "Population", "activity_counts", "individual", "light", "Predation", "Predator")
head(dat_playing)

dat_play <- subset(dat_playing, select=c("vial", "hour", "Predator", "activity_counts", "light"))
#"Predation", "Population"
head(dat_play)
head(daaaa)
dat_play <- as.data.frame(dat_play)
daaaa <- as.data.frame(daaaa)
pred_allDATA <- rbind(dat_play, daaaa)
head(pred_allDATA)


all_plots <- ggplot(pred_allDATA, aes(x=hour, y= activity_counts, colour=Predator)) #+ xlim(0,24) + ylim(0,500)
all_plots2 <- all_plots + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(all_plots2)

head(pred_allDATA)


spiders_all <- pred_allDATA[!(pred_allDATA$Predator %in% c("LTC1", "LTC2", "LTC3", "LTC4","LTP1", "LTP2", "LTP3", "LTP4", "Mantid")), ]
head(spiders_all)
spider_plots <- ggplot(spiders_all, aes(x=hour, y= activity_counts, colour=Predator)) #+ xlim(0,24) + ylim(0,500)
spider_plots2 <- spider_plots + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(spider_plots2)


mantid_all <- pred_allDATA[!(pred_allDATA$Predator %in% c("LTC1", "LTC2", "LTC3", "LTC4","LTS1", "LTS2", "LTS3", "LTS4", "Spider")), ]
head(mantid_all)
mantid_plots <- ggplot(mantid_all, aes(x=hour, y= activity_counts, colour=Predator)) + xlim(0,24) + ylim(0,500)
mantid_plots2 <- mantid_plots + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(mantid_plots2)



## Correlations:
predators2 <- daaaa
predators2$Population <- 1

dat_playing2 <- dat.hourly
dat_playing2 <- within(dat_playing2, { 
  Predation = ifelse (Trt == "C", "LTC", ifelse(Trt == "S", "LTS", "LTP"))})

colnames(dat_playing2) <- c("vial", "monitor", "start_day", "hour", "Trt", "Population", "activity_counts", "individual", "light", "Predator")

dat_play2 <- subset(dat_playing2, select=c("vial", "hour", "activity_counts", "light", "Predator", "Population"))


pred_cor <- rbind(dat_play2, predators2)
head(pred_cor)

pred_cor2 <- pred_cor %>%
  group_by(Predator, hour) %>%
  summarise(activity_counts = mean(activity_counts))
head(pred_cor2)

#Spiders
par(mfrow=c(1,2))
with(pred_cor2, cor(activity_counts[Predator== "Spider"],activity_counts[Predator == "LTS"]))

line2 <- lm(pred_cor2$activity_counts[pred_cor2$Predator == "Spider"] ~ pred_cor2$activity_counts[pred_cor2$Predator == "LTS"])

with(pred_cor2, plot(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS"],abline(line2), main = "Spider: Correlation of activity at different hours",  xlab= "Spider Activity", ylab="Fly Activity" ) )

library(Hmisc)
with(pred_cor2, rcorr(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS"]))

#Mantids
with(pred_cor2, cor(activity_counts[Predator== "Mantid"],activity_counts[Predator == "LTP"] ))

line3 <- lm(pred_cor2$activity_counts[pred_cor2$Predator == "Mantid"] ~ pred_cor2$activity_counts[pred_cor2$Predator == "LTP"])

with(pred_cor2, plot(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP"], abline(line3), main = "Mantid:LTP Correlation activity at differnt hours", xlab= "Mantid Activity", ylab="Fly Activity" ) )

with(pred_cor2, rcorr(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP"]))

par(mfrow=c(1,1))

pred_cor3 <- pred_cor %>%
  group_by(Predator, hour, Population) %>%
  summarise(activity_counts = mean(activity_counts))
head(pred_cor3)

#Mantids Population Correlations
par(mfrow=c(2,2))

line_LTP1 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Mantid"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTP" & pred_cor3$Population == 1])

with(pred_cor3, plot(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP" & Population == 1],  abline(line_LTP1), main = "Mantid:LTP Rep1", xlab= "Mantid Activity", ylab="Fly Activity" ) )

line_LTP2 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Mantid"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTP" & pred_cor3$Population == 2])

with(pred_cor3, plot(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP" & Population == 2],  abline(line_LTP2), main = "Mantid:LTP Rep2", xlab= "Mantid Activity", ylab="Fly Activity" ) )

line_LTP3 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Mantid"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTP" & pred_cor3$Population == 3])

with(pred_cor3, plot(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP" & Population == 3],  abline(line_LTP3), main = "Mantid:LTP Rep3", xlab= "Mantid Activity", ylab="Fly Activity" ) )

line_LTP4 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Mantid"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTP" & pred_cor3$Population == 4])

with(pred_cor3, plot(x = activity_counts[Predator == "Mantid"],y = activity_counts[Predator == "LTP" & Population == 4],  abline(line_LTP4), main = "Mantid:LTP Rep4", xlab= "Mantid Activity", ylab="Fly Activity" ) )

par(mfrow=c(1,1))

#Spiders
par(mfrow=c(2,2))

line_LTS1 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Spider"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTS" & pred_cor3$Population == 1])

with(pred_cor3, plot(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS" & Population == 1],  abline(line_LTS1), main = "Spider:LTS Rep1", xlab= "Spider Activity", ylab="Fly Activity" ) )

line_LTS2 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Spider"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTS" & pred_cor3$Population == 2])

with(pred_cor3, plot(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS" & Population == 2],  abline(line_LTS2), main = "Spider:LTS Rep2", xlab= "Spider Activity", ylab="Fly Activity" ) )

line_LTS3 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Spider"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTS" & pred_cor3$Population == 3])

with(pred_cor3, plot(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS" & Population == 3],  abline(line_LTS3), main = "Spider:LTS Rep3", xlab= "Spider Activity", ylab="Fly Activity" ) )

line_LTS4 <- lm(pred_cor3$activity_counts[pred_cor3$Predator == "Spider"] ~ pred_cor3$activity_counts[pred_cor3$Predator == "LTS" & pred_cor3$Population == 4])

with(pred_cor3, plot(x = activity_counts[Predator == "Spider"],y = activity_counts[Predator == "LTS" & Population == 4],  abline(line_LTS4), main = "Spider:LTS Rep4", xlab= "Spider Activity", ylab="Fly Activity" ) )

par(mfrow=c(1,1))

