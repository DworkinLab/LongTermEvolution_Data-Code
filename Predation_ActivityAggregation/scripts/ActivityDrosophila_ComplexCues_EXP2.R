##   From Complex Cues Data Directory README.md: In Exp 2, flies had 2 types of cues marked in the top row: spiders or crickets.
#Experiment two with comparison between spider and cricket cues


#Setwd to scripts folder!

source("Packages_source_file.R")


#Convert to .txt
#need rio for this: Moved to packages source

#convert("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp 2 spider vs cricket cues M1.xlsx", "../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M1.txt")
#convert("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp 2 spider vs cricket cues M2.xlsx", "../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M2.txt")


#Import data: Complext Cues Exp. 2
ComExp2_Mon1 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M1.txt")
ComExp2_Mon2 <- read.table("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp2_spi_Vs_Cri_M2.txt")

#Variable for monitor:
ComExp2_Mon1$monitor <- 1
ComExp2_Mon2$monitor <- 2

#Comlumn Names:
colnames(ComExp2_Mon1) <- c("date", "Remove", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'monitor')

colnames(ComExp2_Mon2) <- c("date", "remove", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'monitor')

#Remove unneeded columns (unknowns and remove):
ComExp2_Mon1 <- ComExp2_Mon1[,-c(5:9)]
ComExp2_Mon2 <- ComExp2_Mon2[,-c(5:9)]


ComExp2_Mon1 <- ComExp2_Mon1[,-c(2)]
ComExp2_Mon2 <- ComExp2_Mon2[,-c(2)]

#head(ComExp2_Mon1)
#head(ComExp2_Mon2)

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


head(Complex_2_long)

#dd <- Complex_2_long %>%
#  group_by(Treatment) %>%
#  summarise(mean_Daily_activity=mean(Activity_counts))


#Analysis:
with(Complex_2_long, plot(x = Treatment, y= Activity_counts))


summary(Complex_2_long)
#Lights off at 22:00 , on again at 10:00
#STart time == 12:00, end = 13:59
#-- hours == hours


with(Complex_2_long[Complex_2_long$Treatment=="Spider",], 
     plot(Activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2,
          xlab ="hour", ylab = "hourly activity",
          main = "Activity: Lab Flies ", 
          ylim=c(0,20)))

with(Complex_2_long[Complex_2_long$Treatment=="Spider",], lines(smooth.spline(y=Activity_counts, x = hour),lwd=2))

with(Complex_2_long[Complex_2_long$Treatment=="Cricket",], points(Activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2, col="red"))
with(Complex_2_long[Complex_2_long$Treatment=="Cricket",], lines(smooth.spline(y=Activity_counts, x = hour), col="red", lwd=2))
legend(x=21.3, y=20.5, legend=c("Spider", "Cricket"), pch=20, col=c(1, "red"))

