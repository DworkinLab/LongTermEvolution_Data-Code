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

head(Exp3_Mon1)
head(Exp3_Mon2)
