## Control Flies Together (wild type base line populations)


source("Packages_source_file.R")
source("Activity_cleanAllData.R")

ManCon <- Mantid_hour
SpiCon <- act_hour

ManCon$Exp <- "Mantids"
SpiCon$Exp <- "Spiders"

ManCon <- as.data.frame(ManCon)
SpiCon <- as.data.frame(SpiCon)

BaseActivity <- rbind(ManCon, SpiCon)
head(BaseActivity)

BaseActivity <- BaseActivity[which(BaseActivity$Treatment=='Control'), ]

Base_plots <- ggplot(BaseActivity, aes(x=hour, y= activity_counts)) #+ xlim(0,24) + ylim(0,500)
Base_plots2 <- Base_plots + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(Base_plots2)

head(BaseActivity)
#Evolved Populations (cleaned in ActivityPredators.R script)
head(dat_play)
BaseActivity2 <- subset(BaseActivity, select=c("Treatment", "Vial", "hour", "activity_counts", "light"))
head(BaseActivity2)
colnames(dat_play) <- c("Vial", "hour", "Treatment", "activity_counts", "light")

DATA_baseEvolved <- rbind(dat_play, BaseActivity2)
head(DATA_baseEvolved)


BaseEvolvedPlot <- ggplot(DATA_baseEvolved, aes(x=hour, y= activity_counts, colour=Treatment)) + xlim(0,24) + ylim(0,600)
BaseEvolvedPlot2 <- BaseEvolvedPlot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 500) +
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(BaseEvolvedPlot2)
