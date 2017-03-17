###Exp. 3
##   From Complex Cues Data Directory README.md: In Exp. 3, flies had 4 types of cues marked in the top row: spiders fed, flies, spiders fed crickets, flies, crickets. Note that there are missing vials due to mortality of the cue providers.


#Setwd to scripts folder!

source("Packages_source_file.R")

#Convert to .txt

#convert("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp 3 Monitor 1.xlsx", "../data/Activity_Drosophila_ComplexCues_June17_2016/Exp3_Monitor_1.txt")
#convert("../data/Activity_Drosophila_ComplexCues_June17_2016/Exp 3 Monitor 2.xlsx", "../data/Activity_Drosophila_ComplexCues_June17_2016/Exp3_Monitor_2.txt")


#Import data: Complext Cues Exp. 2
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
colnames(Exp3_Mon1) <- c("date", "time", "signal", "lightON",'vial1_SF', 'vial2_SF', 'vial3_SF', 'vial4_SF', 'vial5_SF', 'vial6_SF', 'vial7_SF', 'vial8_SF', 'vial9_SC', 'vial10_SC', 'vial11_SC', 'vial12_SC', 'vial13_SC', 'vial14_SC', 'vial15_SC', 'vial16_SC','vial17_F','vial18_F','vial19_F','vial20_F','vial21_F','vial22_F','vial23_F','vial24_F','vial25_C','vial26_C','vial27_C','vial28_C','vial29_C','vial30_C', 'monitor','datetime', 'minute', 'hour', 'day')

#Mon2:
##  date time signal Light  F F.1 F.2 F.3 F.4 F.5 F.6 C C.1 C.2 C.3 C.4 SF SF.1 SF.2 SF.3 SF.4 SF.5 SF.6,  SC SC.1 SC.2 SC.3 SC.4 SC.5 SC.6 SC.7 monitor datetime minute hour day 

colnames(Exp3_Mon2) <- c("date", "time", "signal", "lightON",'vial1_F', 'vial2_F', 'vial3_F', 'vial4_F', 'vial5_F', 'vial6_F', 'vial7_F', 'vial8_C', 'vial9_C', 'vial10_C', 'vial11_C', 'vial12_C', 'vial13_SF', 'vial14_SF', 'vial15_SF', 'vial16_SF','vial17_SF','vial18_SF','vial19_SF','vial20_SC','vial21_SC','vial22_SC','vial23_SC','vial24_SC','vial25_SC','vial26_SC','vial27_SC', 'monitor','datetime', 'minute', 'hour', 'day')

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

#Combine into one data set:
Exp3_long <- rbind(Exp3_Mon1_long, Exp3_Mon2_long)




#Change things to factors
#Change to factors:
Exp3_long$monitor <- as.factor(Exp3_long$monitor)
Exp3_long$Treatment <- as.factor(Exp3_long$Treatment)
Exp3_long$day <- as.factor(Exp3_long$day)
Exp3_long$Vial <- as.factor(Exp3_long$Vial)


#Analysis:

#simple plot with no changes at all... no change.....
with(Exp3_long[Exp3_long$Treatment=="SC",], 
     plot(activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2,
          xlab ="hours after initiation", ylab = "hourly activity",
          main = "Activity: Lab Flies ", 
          ylim=c(0,20)))

with(Exp3_long[Exp3_long$Treatment=="SC",], lines(smooth.spline(y=activity_counts, x = hour),lwd=2))

with(Exp3_long[Exp3_long$Treatment=="SF",], points(activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2, col="red"))
with(Exp3_long[Exp3_long$Treatment=="SF",], lines(smooth.spline(y=activity_counts, x = hour), col="red", lwd=2))
with(Exp3_long[Exp3_long$Treatment=="C",], points(activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2, col="blue"))
with(Exp3_long[Exp3_long$Treatment=="C",], lines(smooth.spline(y=activity_counts, x = hour), col="blue", lwd=2))
with(Exp3_long[Exp3_long$Treatment=="F",], points(activity_counts ~ jitter(hour, factor=1.3), pch=20, cex=0.2, col="green"))
with(Exp3_long[Exp3_long$Treatment=="F",], lines(smooth.spline(y=activity_counts, x = hour), col="green", lwd=2))

legend(x=22, y=20, legend=c("SC", "SF","C","F"), pch=20, col=c(1, "red", "blue", "green"))

