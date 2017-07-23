#Analysis of Activity Data:

source('Packages_source_file.R')
source('Activity_Evolved_Clean.R')
source('Activity_Exp2_Clean.R')
source('Activity_Exp3_Clean.R')
source('Activity_Mantids_Clean.R')
source('Activity_Spider_Clean.R')
source('Activity_Predators_Clean.R')

#Baseline activity may be required
ManCon <- as.data.frame(Mantid_hour)
SpiCon <- as.data.frame(act_hour)

ManCon$Exp <- "Mantids"
SpiCon$Exp <- "Spiders"

BaseActivity <- rbind(ManCon, SpiCon)
BaseActivity <- BaseActivity[which(BaseActivity$Treatment=='Control'), ]

# Models:
head(dat.hourly)
dat.hourly$Predation <- as.factor(dat.hourly$Predation)
dat.hourly$light <- as.factor(dat.hourly$light)

mod_trial_1 <- lmer(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  ns(hour, 4) + monitor + start_day + (1 + ns(hour, 4) + light | individual), data=dat.hourly)

summary(mod_trial_1)
pacf(resid(mod_trial_1))
car::Anova(mod_trial_1)
plot(allEffects(mod_trial_1))

Evolve_plot <- effect("Predation*light", mod_trial_1)
Evolve_plot <- as.data.frame(Evolve_plot)
head(Evolve_plot)
Evolve_plot2 <- ggplot(Evolve_plot, 
                  aes(y=fit, x=light, colour=Predation))

Evolve_plot3 <- Evolve_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Evolved Population") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(Evolve_plot3)
