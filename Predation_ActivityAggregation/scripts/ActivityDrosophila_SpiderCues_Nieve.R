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

Act_Data <- rbind(Mon1, Mon2)

colnames(Act_Data) <- c("bin", "date", "time", "signal", "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "lightON",'vial1', 'vial2', 'vial3', 'vial4', 'vial5', 'vial6', 'vial7', 'vial8', 'vial9', 'vial10', 'vial11', 'vial12', 'vial13', 'vial14', 'vial15', 'vial16', 'vial17', 'vial18', 'vial19', 'vial20', 'vial21', 'vial22', 'vial23', 'vial24', 'vial25', 'vial26', 'vial27', 'vial28', 'vial29', 'vial30', 'vial31', 'vial32', 'monitor')

head(Act_Data)

#Remove unknowns
Act_Data2 <- Act_Data[,-c(5:9)]
head(Act_Data2)
str(Act_Data2)

#Change Day format
#Act_Data2 <- Act_Data2 %>% 
#  separate(date, c("Month","Day","Year"), "/")
#head(Act_Data2)
Act_Data2$datetime <- as.POSIXct(paste(Act_Data2$date, Act_Data2$time), format="%m/%d/%y %H:%M:%S")
head(Act_Data2)

