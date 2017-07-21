## Complex cues 2 (spider vs. Cricket): Clean and Read 


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

Exp2_hour$individual <- with(Exp2_hour, interaction(Vial, monitor, drop=FALSE))

Exp2_hour$light <- with(Exp2_hour, ifelse(hour >= 10 & hour < 22, "light", "dark"))
