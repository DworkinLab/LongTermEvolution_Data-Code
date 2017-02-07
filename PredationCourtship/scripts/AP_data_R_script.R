# Data from Abhijna of the long term populations and their courtship and copulation data


#Change working directory to the Data folder (if from script folder to start)!
setwd("../data")
AP_evolved_data <- read.csv("AP_EvolvedPopCourtshipCopulation_2014.csv", h=T)
head(AP_evolved_data)
summary(AP_evolved_data)
