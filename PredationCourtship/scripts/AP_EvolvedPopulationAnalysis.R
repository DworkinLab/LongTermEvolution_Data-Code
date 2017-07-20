#Official Script

source("AP_Packages.R")


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


#Models

#Courtship Latency:

mod_court <- lmer(Rel_Court_lat ~ 1 + Treatment*AgeBin + 
                    (1|Date) + (1|Treatment:Rep), 
                  data = AP_Data)

summary(mod_court)
Anova(mod_court)


courtLat <- effect("Treatment*AgeBin", mod_court)
courtLat <- as.data.frame(courtLat)

latenCourt <- ggplot(courtLat, 
                     aes(y=fit, x=AgeBin, colour=Treatment))

latenCourt2 <- latenCourt + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", x="Age Bins") +
  ggtitle("Courtship Latency") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))

print(latenCourt2)

# Copulation Latency:

mod_copl_plot <- lmer(Rel_Cop_lat ~ 1+ Treatment*AgeBin + 
                        (1|Date) + (1|Treatment:Rep), 
                      data = AP_Data)

summary(mod_copl_plot)
Anova(mod_copl_plot)

copLate <- effect("Treatment*AgeBin", mod_copl_plot)
copLate <- as.data.frame(copLate)

LatenCop <- ggplot(copLate, 
                   aes(y=fit, x=AgeBin, colour=Treatment))

LatenCop2 <-  LatenCop + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Age Bins") +
  ggtitle("Copulation Latency") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))

print(LatenCop2)

# Copulation Duration:

mod_copd_plot <- lmer(Rel_Cop_dur ~ 1+ Treatment*AgeBin + 
                        (1|Date) + (1|Treatment:Rep), 
                      data = AP_Data)


summary(mod_copd_plot)
Anova(mod_copd_plot)


copdur_plot <- effect("Treatment*AgeBin", mod_copd_plot)
copdur_plot <- as.data.frame(copdur_plot)

DuratCop <- ggplot(copdur_plot, 
                   aes(y=fit, x=AgeBin, colour=Treatment))

DuratCop2 <- DuratCop + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Age Bins") +
  ggtitle("Copulation Duration") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))


print(DuratCop2)


# Paul and Ian need to finish figuring out convergence issues for mixed glm. 
mod_cop_count <- glmer(Copulation ~ 1 + Treatment*AgeBin + 
                         (1|Date) + (1|Treatment:Rep), 
                       family = "binomial", 
                       data = AP_Data)

#This is the issue; some age bin/treatments have 0
X <- AP_Data
X$Treatment.Rep.Age <- with(X, paste0(Treatment, Rep, AgeBin))
X$Treatment.Rep.Age <- as.factor(X$Treatment.Rep.Age)
X2 <- X[-which(X$Copulation == "0"),]
X3 <- X[-which(X$Copulation == "1"),]

summary(X2$Treatment.Rep.Age)
summary(X3$Treatment.Rep.Age)



summary(mod_cop_count)
Anova(mod_cop_count)

#cop_prop_plot <- effect("Treatment*AgeBin", mod_cop_count)
#cop_prop_plot <- as.data.frame(cop_prop_plot)

#propCop <- ggplot(cop_prop_plot, aes(y=fit, x=AgeBin, colour=Treatment))

#propCop2 <- propCop + geom_point(stat="identity",position=position_dodge(0.5)) + 
#  geom_linerange(aes(ymin=lower, ymax=upper), position = position_dodge(0.5)) + labs(y="Intercept", x="Age Bins") +
#  ggtitle("Copulation Proportion") + 
#  scale_colour_manual(values= c("#999999", "#56B4E9", "#E69F00"))

# propCop2


#Use regular glm for moment.
mod_cop_count_glm <- glm(Copulation ~ 1 + Treatment*AgeBin, 
                         family = "binomial", 
                         data = AP_Data)

summary(mod_cop_count_glm)
Anova(mod_cop_count_glm)

cop_prop_plot <- effect("Treatment*AgeBin", mod_cop_count_glm)
cop_prop_plot <- as.data.frame(cop_prop_plot)

propCop <- ggplot(cop_prop_plot, 
                  aes(y=fit, x=AgeBin, colour=Treatment))

propCop2 <- propCop + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Age Bins") +
  ggtitle("Copulation Proportion") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))

propCop2


multiplot(latenCourt2, LatenCop2, DuratCop2, propCop2, cols=2)

