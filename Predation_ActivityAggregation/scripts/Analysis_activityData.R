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

mod_trial_1 <- lmer(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  bs(hour, 4) + monitor + start_day + (1 + bs(hour, 4) + light | individual), data=dat.hourly)

summary(mod_trial_1)
pacf(resid(mod_trial_1))
car::Anova(mod_trial_1)


