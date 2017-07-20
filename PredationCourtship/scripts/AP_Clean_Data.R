#Clean the Data:


AP_evolved_data <- read.csv("../data/AP_EvolvedPopCourtshipCopulation_2014.csv", h=T)

#Start time of agebin 4 not recorded: so removed
AP_evolved_data <- AP_evolved_data[-which(AP_evolved_data$Bin4 == "" & AP_evolved_data$AgeBin =="4"),]
#Make relative start times:
AP_evolved_data$StartTime <- as.difftime(as.character(AP_evolved_data$StartTime), format = "%H:%M", units = "secs")

AP_evolved_data$CourtshipLatency <- as.difftime(as.character(AP_evolved_data$CourtshipLatency), format = "%H:%M", units = "secs")

AP_evolved_data$Bin2 <- as.difftime(as.character(AP_evolved_data$Bin2), format = "%H:%M", units = "secs")

AP_evolved_data$Bin3 <- as.difftime(as.character(AP_evolved_data$Bin3), format = "%H:%M", units = "secs")

AP_evolved_data$Bin4 <- as.difftime(as.character(AP_evolved_data$Bin4), format = "%H:%M", units = "secs")

AP_evolved_data$CopulationLatency <- as.difftime(as.character(AP_evolved_data$CopulationLatency), format = "%H:%M", units = "secs")

AP_evolved_data$CopulationDuration <- as.difftime(as.character(AP_evolved_data$CopulationDuration), format = "%H:%M", units = "secs")

#Needs to be based on the age bin start time!
AP_evolved_data$CourtshipLatency <- with(AP_evolved_data, CourtshipLatency - StartTime)

AP_evolved_data$CopulationLatency <- with(AP_evolved_data, CopulationLatency - StartTime)

AP_evolved_data$CopulationDuration <- with(AP_evolved_data, CopulationDuration - StartTime)

AP_evolved_data$Bin2 <- with(AP_evolved_data, Bin2 - StartTime)
AP_evolved_data$Bin3 <- with(AP_evolved_data, Bin3 - StartTime)
AP_evolved_data$Bin4 <- with(AP_evolved_data, Bin4 - StartTime)
AP_evolved_data$Bin1 <- with(AP_evolved_data, StartTime - StartTime)

#Relative start times for each thing
AP_evolved_data <- within(AP_evolved_data, { 
  Rel_Court_lat = ifelse (AgeBin == 1, CourtshipLatency-Bin1, ifelse(AgeBin == 2, CourtshipLatency-Bin2, ifelse(AgeBin == 3, CourtshipLatency-Bin3, ifelse(AgeBin == 4, CourtshipLatency-Bin4, 0))))})


AP_evolved_data <- within(AP_evolved_data, { 
  Rel_Cop_lat = ifelse (AgeBin == 1, CopulationLatency-Bin1, ifelse(AgeBin == 2, CopulationLatency-Bin2, ifelse(AgeBin == 3, CopulationLatency-Bin3, ifelse(AgeBin == 4, CopulationLatency-Bin4, 0))))})

AP_evolved_data$Rel_Cop_dur <- AP_evolved_data$CopulationDuration - AP_evolved_data$CopulationLatency 

#New Data frame with only important data:
AP_Data <- subset(AP_evolved_data, select = c(Vial, Treatment, Rep, Rel_Court_lat, Rel_Cop_lat, Rel_Cop_dur, Copulation, Date, AgeBin))


AP_Data$Treatment.Rep <- with(AP_Data, paste0(Treatment, Rep))

AP_Data$Treatment.Rep <- as.factor(AP_Data$Treatment.Rep)
AP_Data$AgeBin <- as.factor(AP_Data$AgeBin)
AP_Data$Rep <- as.numeric(AP_Data$Rep)
AP_Data$Rel_Cop_dur <- as.numeric(AP_Data$Rel_Cop_dur)

