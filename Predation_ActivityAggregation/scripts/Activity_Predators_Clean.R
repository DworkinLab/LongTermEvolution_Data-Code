#Predator Activity Read and Clean

#Spiders:
spiders <- read.table("../data/Activity_Spiders_Oct2015/SpiderActivityRaw_RD_Oct142015.txt")


#Mantids:
mantids <- read.table("../data/Activity_Mantids_June2016/Mantids_All.txt")

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

mantid_hour$light <- with(mantid_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))
spider_hour$light <- with(spider_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))
mantid_hour$Predator <- "Mantid"
spider_hour$Predator <- "Spider"

#with(mantid_hour, boxplot(activity_counts ~ hour))
#with(spider_hour, boxplot(activity_counts ~ hour))


