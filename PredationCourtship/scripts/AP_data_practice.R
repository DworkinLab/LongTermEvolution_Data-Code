# Data from Abhijna of the long term populations and their courtship and copulation data

library(ggplot2)
library(dplyr)
library(tidyr)
library(lme4)
library(effects)
library(car)
library(glmmTMB)


#Multiplots: to join plots together
#From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#Change working directory to the Data folder (if from script folder to start)!
#setwd("~/Bioinformatics/Long_Term_Data/Data_LongTermPopulationExperiments_Git/PredationCourtship/data")
AP_evolved_data <- read.csv("../data/AP_EvolvedPopCourtshipCopulation_2014.csv", h=T)
AP_start <- AP_evolved_data

#Start time of agebin 4 not recorded: so removed
AP_evolved_data <- AP_evolved_data[-which(AP_evolved_data$Bin4 == "" & AP_evolved_data$AgeBin =="4"),]
#Make relative start times:
head(AP_evolved_data)
AP_evolved_data$StartTime <- as.difftime(as.character(AP_evolved_data$StartTime), format = "%H:%M", units = "secs")

AP_evolved_data$CourtshipLatency <- as.difftime(as.character(AP_evolved_data$CourtshipLatency), format = "%H:%M", units = "secs")

AP_evolved_data$Bin2 <- as.difftime(as.character(AP_evolved_data$Bin2), format = "%H:%M", units = "secs")

AP_evolved_data$Bin3 <- as.difftime(as.character(AP_evolved_data$Bin3), format = "%H:%M", units = "secs")

AP_evolved_data$Bin4 <- as.difftime(as.character(AP_evolved_data$Bin4), format = "%H:%M", units = "secs")

AP_evolved_data$CopulationLatency <- as.difftime(as.character(AP_evolved_data$CopulationLatency), format = "%H:%M", units = "secs")

AP_evolved_data$CopulationDuration <- as.difftime(as.character(AP_evolved_data$CopulationDuration), format = "%H:%M", units = "secs")

head(AP_evolved_data)

#Needs to be based on the age bin start time!
AP_evolved_data$CourtshipLatency <- with(AP_evolved_data, CourtshipLatency - StartTime)

AP_evolved_data$CopulationLatency <- with(AP_evolved_data, CopulationLatency - StartTime)

#Wrong: should be from time of copulation latency!
AP_evolved_data$CopulationDuration <- with(AP_evolved_data, CopulationDuration - StartTime)

AP_evolved_data$Bin2 <- with(AP_evolved_data, Bin2 - StartTime)
AP_evolved_data$Bin3 <- with(AP_evolved_data, Bin3 - StartTime)
AP_evolved_data$Bin4 <- with(AP_evolved_data, Bin4 - StartTime)
AP_evolved_data$Bin1 <- with(AP_evolved_data, StartTime - StartTime)
head(AP_evolved_data)

#Relative start times for each thing

#AP_evolved_data$Bin1 <- AP_evolved_data$StartTime

AP_evolved_data <- within(AP_evolved_data, { 
  Rel_Court_lat = ifelse (AgeBin == 1, CourtshipLatency-Bin1, ifelse(AgeBin == 2, CourtshipLatency-Bin2, ifelse(AgeBin == 3, CourtshipLatency-Bin3, ifelse(AgeBin == 4, CourtshipLatency-Bin4, 0))))})


AP_evolved_data <- within(AP_evolved_data, { 
  Rel_Cop_lat = ifelse (AgeBin == 1, CopulationLatency-Bin1, ifelse(AgeBin == 2, CopulationLatency-Bin2, ifelse(AgeBin == 3, CopulationLatency-Bin3, ifelse(AgeBin == 4, CopulationLatency-Bin4, 0))))})

#Wrong: should be after copulation latency!
#AP_evolved_data <- within(AP_evolved_data, { 
#  Rel_Cop_dur = ifelse (AgeBin == 1, CopulationDuration-Bin1, ifelse(AgeBin == 2, CopulationDuration-Bin2, ifelse(AgeBin == 3, CopulationDuration-Bin3, ifelse(AgeBin == 4, CopulationDuration-Bin4, 0))))})
AP_evolved_data$Rel_Cop_dur <- AP_evolved_data$CopulationDuration - AP_evolved_data$CopulationLatency 

head(AP_evolved_data)
#New Data frame with only important data:
AP_Data <- subset(AP_evolved_data, select = c(Treatment, Rep, Rel_Court_lat, Rel_Cop_lat, Rel_Cop_dur, Copulation, Temperature, Humidity, Date, AgeBin))

#AP_court_lat <- subset(AP_Data, select = c(Treatment, Rep, Rel_Court_lat))
#AP_cop_dur <- subset(AP_Data, select = c(Treatment, Rep, Rel_Cop_dur))
#AP_cop_lat <- subset(AP_Data, select = c(Treatment, Rep, Rel_Cop_lat))


#AP_evolved_data <- AP_evolved_data[-which(AP_evolved_data$Bin4 == "" & AP_evolved_data$AgeBin =="4"),]

#AP_court_lat <- AP_court_lat[-which(AP_court_lat$Rel_Court_lat == ""),]
#AP_cop_lat <- AP_cop_lat[-which(AP_cop_lat$Rel_Cop_lat ==""),]
#AP_cop_dur <- AP_cop_dur[-which(AP_cop_dur$Rel_Cop_dur ==""),]

AP_groups <-AP_Data %>%
  group_by(Treatment, Rep) %>%
  summarise(copulation_prop=sum(Copulation/(n())), mean_court_lat= mean(Rel_Court_lat, na.rm = TRUE), mean_cop_lat = mean(Rel_Cop_lat, na.rm=TRUE), mean_cop_dur = mean(Rel_Cop_dur, na.rm=TRUE), cop_sum=sum(Copulation))

p1 <- ggplot(AP_groups, aes(x = Treatment, y = copulation_prop))
p2 <- ggplot(AP_groups, aes(x=Treatment, y = mean_court_lat))
p3 <- ggplot(AP_groups, aes(x=Treatment, y = mean_cop_lat))
p4 <- ggplot(AP_groups, aes(x=Treatment, y = mean_cop_dur))
p5 <- ggplot(AP_groups, aes(x=Rep, y=cop_sum, colour=Treatment))

#p1+geom_point()
p1+geom_boxplot()

#p2+geom_point()
p2+geom_boxplot()

#p3+geom_point()
p3+geom_boxplot()

#p4+geom_point()
p4+geom_boxplot()

#p5+geom_point()
p5+geom_boxplot()


#head(AP_Data)

AP_Data$Rep <- as.factor(AP_Data$Rep)

p6 <- ggplot(AP_Data, aes(x = Treatment, y = Rel_Court_lat))
p7 <- ggplot(AP_Data, aes(x=Treatment, y = Rel_Cop_lat))
p8 <- ggplot(AP_Data, aes(x=Treatment, y = Rel_Cop_dur))
p9 <- ggplot(AP_Data, aes(x=Treatment, y = Copulation), ylab("Copulation"))

#Units are millisecond (1sec = 1000 milliseconds ((something not right)))
aaa <- p6 + geom_boxplot() +
  ylab("Courtship Latency (sec)")
bbb <- p7 + geom_boxplot() +
  ylab("Copulation Latency (sec)")
ccc <- p8 + geom_boxplot() +
  ylab("Copulation Duration (sec)")
ddd <- p1 + geom_boxplot() +
  ylab("Copulation Proportion")

multiplot(aaa,ccc,bbb, ddd, cols=2)

AP_Data$Treatment.Rep <- with(AP_Data, paste0(Treatment, Rep))
AP_groups$Treatment.Rep <- with(AP_groups, paste0(Treatment, Rep))
p10 <- ggplot(AP_Data, aes(x = Treatment.Rep, y = Rel_Court_lat, colour = Treatment))
p11 <- ggplot(AP_Data, aes(x=Treatment.Rep, y = Rel_Cop_lat, colour=Treatment))
p12<- ggplot(AP_Data, aes(x=Treatment.Rep, y = Rel_Cop_dur, colour = Treatment))

p10+geom_boxplot()
p11+geom_boxplot()
p12+geom_boxplot()

head(AP_Data)

#Models
AP_Data$Rep <- as.numeric(AP_Data$Rep)
mod_court <- lmer(Rel_Court_lat ~ 1 + Treatment + Rep + Temperature + Humidity + (1|Date) + (1|Treatment:Rep), data = AP_Data)
summary(mod_court)
Anova(mod_court)
plot(allEffects(mod_court))

mod_court_plot <- lmer(Rel_Court_lat ~ 1 + Treatment + (1|Date) + (1|Treatment:Rep), data = AP_Data)
plot(allEffects(mod_court_plot))
mod_copl_plot <- lmer(Rel_Cop_lat ~ 1 + Treatment + (1|Date) + (1|Treatment:Rep), data = AP_Data)
plot(allEffects(mod_copl_plot))
AP_Data$Rel_Cop_dur <- as.numeric(AP_Data$Rel_Cop_dur)
mod_copd_plot <- lmer(Rel_Cop_dur ~ 1 + Treatment + (1|Date) + (1|Treatment:Rep), data = AP_Data)
plot(allEffects(mod_copd_plot))

mod_cop_count <- lmer(Copulation ~ 1+ Treatment + (1|Date) + (1|Treatment:Rep), data=AP_Data)
head(AP_Data)

plot(effect("Treatment", mod_court_plot), main = "Relative Courtship Latency", ylab = "Courtship Latency (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 1,nrow = 2,ncol = 2, more=TRUE)
plot(effect("Treatment", mod_copl_plot), main = "Relative Copulation Latency", ylab = "Copulation Latency (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 2,nrow = 2,ncol = 2, more=TRUE)
plot(effect("Treatment", mod_copd_plot), main = "Relative Copulation Duration", ylab = "Copulation Duration (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 2,col = 1,nrow = 2,ncol = 2, more=TRUE)
plot(effect("Treatment", mod_cop_count), main = "Copulation", ylab = "Copulation ", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 2,col = 2,nrow = 2,ncol = 2)


#Works: add in all effects to correct models!


plot(effect("Treatment", mod_court_plot), main = "Courtship Latency", ylab = "Courtship Latency (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 1,nrow = 1,ncol = 2, more=TRUE)
plot(effect("Treatment", mod_copl_plot), main = "Copulation Latency", ylab = "Copulation Latency (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 2,nrow = 1,ncol = 2)

plot(effect("Treatment", mod_copd_plot), main =  "Copulation Duration", ylab = "Copulation Duration (sec)", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 1,nrow = 1,ncol = 2, more=TRUE)
plot(effect("Treatment", mod_cop_count), main = "Copulation Proportion", ylab = "Copulation ", xlab = "Treatment",style="stacked",rug=F, key.args=list(space="right"), row = 1,col = 2,nrow = 1,ncol = 2)



copprop <- effect("Treatment", mod_cop_count)
copprop <- as.data.frame(copprop)
copprop

copDur <- effect("Treatment", mod_copd_plot)
copDur <- as.data.frame(copDur)
copDur$Behaviour <- "Copulation Duration"

CourLat <- effect("Treatment", mod_court_plot)
CourLat <- as.data.frame(CourLat)
CourLat$Behaviour <- "Courtship Latency"

copLat <- effect("Treatment", mod_copl_plot)
copLat <- as.data.frame(copLat)
copLat$Behaviour <- "Copulation Latency"

Times <- rbind(copDur, CourLat, copLat)
head(Times)

#Times <- within(Times, { 
#  Predation = ifelse (Treatment == "LTC", "Control", ifelse(Treatment == "LTS", "Spider", "Mantids"))})

gg1 <- ggplot(Times, aes(x=Behaviour, y=fit, fill=Treatment))
gg1 + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + 
  ylab("Times (seconds)")  +
  xlab("") +
  theme(text = element_text(size=15), axis.text.x=element_text(size=11)) + 
  scale_fill_manual(values=c("#999999", "#56B4E9", "#E69F00")) #+
  #scale_x_discrete(labels = abbreviate)
  coord_flip()


gg2 <- ggplot(copprop, aes(x=Treatment, y=fit))
gg2 + geom_bar(stat="identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(.9), size = 1.2, width = 0.2) + ylab("Proportion") 





##### Adding age as a factor:
head(AP_Data)

AP_age <-AP_Data %>%
  group_by(Treatment.Rep, AgeBin) %>%
  summarise(copulation_prop=sum(Copulation/(n())), mean_court_lat= mean(Rel_Court_lat, na.rm = TRUE), mean_cop_lat = mean(Rel_Cop_lat, na.rm=TRUE), mean_cop_dur = mean(Rel_Cop_dur, na.rm=TRUE), cop_sum=sum(Copulation))


AP_Data$Treatment.Rep <- as.factor(AP_Data$Treatment.Rep)
AP_Data$AgeBin <- as.factor(AP_Data$AgeBin)
mod_court <- lmer(Rel_Court_lat ~ 1 + Treatment*AgeBin + Temperature + Humidity + (1|Date) + (1|Treatment:Rep), data = AP_Data)
summary(mod_court)
Anova(mod_court)
plot(allEffects(mod_court))

courtLat <- effect("Treatment*AgeBin", mod_court)
courtLat <- as.data.frame(courtLat)
latenCourt <- ggplot(courtLat, aes(y=fit, x=AgeBin, colour=Treatment))
latenCourt2 <- latenCourt + geom_point(stat="identity", position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5)) + 
  labs(y="Intercept", x="Age Bins") +
  ggtitle("Courtship Latency") + 
  scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))
latenCourt2

#mod_court_plot <- lmer(Rel_Court_lat ~ 1 + Treatment*AgeBin + Temperature + Humidity + (1|Date), data = AP_Data)
#plot(allEffects(mod_court_plot))
#summary(mod_court_plot)

mod_copl_plot <- lmer(Rel_Cop_lat ~ 1+ Treatment*AgeBin + Temperature + Humidity + (1|Date) + (1|Treatment:Rep), data = AP_Data)
plot(allEffects(mod_copl_plot))
summary(mod_copl_plot)
Anova(mod_copl_plot)

copLate <- effect("Treatment*AgeBin", mod_copl_plot)
copLate <- as.data.frame(copLate)
LatenCop <- ggplot(copLate, aes(y=fit, x=AgeBin, colour=Treatment))
LatenCop2 <-  LatenCop + geom_point(stat="identity", position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5)) + 
  labs(y="Intercept", x="Age Bins") +
  ggtitle("Copulation Latency") + 
  scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))
LatenCop2

AP_Data$Rel_Cop_dur <- as.numeric(AP_Data$Rel_Cop_dur)


mod_copd_plot <- lmer(Rel_Cop_dur ~ 1+ Treatment*AgeBin + Temperature + Humidity + (1|Date) + (1|Treatment:Rep), data = AP_Data)


plot(allEffects(mod_copd_plot))
summary(mod_copd_plot)
Anova(mod_copd_plot)


copdur_plot <- effect("Treatment*AgeBin", mod_copd_plot)
copdur_plot <- as.data.frame(copdur_plot)

DuratCop <- ggplot(copdur_plot, aes(y=fit, x=AgeBin, colour=Treatment))
DuratCop2 <- DuratCop + geom_point(stat="identity", position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5)) + 
  labs(y="Intercept", x="Age Bins") +
  ggtitle("Copulation Duration") + 
  scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))
DuratCop2

head(AP_Data)

# Paul and Ian need to finish figuring out convergence issues for mixed glm. 
mod_cop_count <- glmer(Copulation ~ 1 + Temperature + Humidity + Treatment*AgeBin + (1|Date) + (1|Treatment:Rep), family = "binomial", data = AP_Data)

plot(allEffects(mod_cop_count))
summary(mod_cop_count)
Anova(mod_cop_count)

# Use regular glm for moment.
mod_cop_count_glm <- glm(Copulation ~ 1 + Temperature + Humidity + Treatment*AgeBin, family = "binomial", data = AP_Data)
summary(mod_cop_count_glm)
Anova(mod_cop_count_glm)

cop_prop_plot <- effect("Treatment*AgeBin", mod_cop_count)
cop_prop_plot <- as.data.frame(cop_prop_plot)
propCop <- ggplot(cop_prop_plot, aes(y=fit, x=AgeBin, colour=Treatment))
propCop2 <- propCop + geom_point(stat="identity", position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5)) + 
  labs(y="Intercept", x="Age Bins") +
  ggtitle("Copulation Proportion") + 
  scale_colour_manual(values=c("#999999", "#56B4E9", "#E69F00"))
propCop2


multiplot(latenCourt2, LatenCop2, DuratCop2, propCop2, cols=2)

