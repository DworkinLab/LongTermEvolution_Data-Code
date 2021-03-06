#Analysis of Activity Data:

source('Packages_source_file.R')
source('Activity_Evolved_Clean.R')
source('Activity_Exp2_Clean.R')
source('Activity_Exp3_Clean.R')
source('Activity_Mantids_Clean.R')
source('Activity_Spider_Clean.R')
source('Activity_Predators_Clean.R')

#ggplot(Exp2_hour, aes(hour, activity_counts, colour = Treatment)) + geom_smooth()
#ggplot(Exp3_hour, aes(hour, activity_counts, colour = Treatment)) + geom_smooth()
#ggplot(act_hour, aes(hour, activity_counts, colour = Treatment)) + geom_smooth()
#ggplot(Mantid_hour, aes(hour, activity_counts, colour = Treatment)) + geom_smooth()

dat.hourly$Treatment <- dat.hourly$Predation

#ggplot(dat.hourly, aes(hour, Hourly_activity, colour = Treatment)) + geom_smooth(method = "loess")

#Baseline activity may be required
ManCon <- as.data.frame(Mantid_hour)
SpiCon <- as.data.frame(act_hour)

ManCon$Exp <- "Mantids"
SpiCon$Exp <- "Spiders"

BaseActivity <- rbind(ManCon, SpiCon)
BaseActivity <- BaseActivity[which(BaseActivity$Treatment=='Control'), ]



# Models:
#Need Treatment and light to be factors:


head(dat.hourly)
dat.hourly$Predation <- as.factor(dat.hourly$Predation)
dat.hourly$light <- as.factor(dat.hourly$light)

head(Exp2_hour)
Exp2_hour$Treatment <- as.factor(Exp2_hour$Treatment)
Exp2_hour$light <- as.factor(Exp2_hour$light)

head(Exp3_hour)
Exp3_hour$Treatment <- as.factor(Exp3_hour$Treatment)
Exp3_hour$light <- as.factor(Exp3_hour$light)

head(act_hour)
act_hour$Treatment <- as.factor(act_hour$Treatment)
act_hour$light <- as.factor(act_hour$light)

head(Mantid_hour)
Mantid_hour$Treatment <- as.factor(Mantid_hour$Treatment)
Mantid_hour$light <- as.factor(Mantid_hour$light)

#Also need a column for hour2 (pi*hour/12)

dat.hourly$hour2 <- (pi*dat.hourly$hour/12)
Exp2_hour$hour2 <- (pi*Exp2_hour$hour/12)
Exp3_hour$hour2 <- (pi*Exp3_hour$hour/12)
act_hour$hour2 <- (pi*act_hour$hour/12)
Mantid_hour$hour2 <- (pi*Mantid_hour$hour/12)




##################
head(dat.hourly)
dat.hourly$Treatment <- dat.hourly$Predation
mod_trial_1 <- lmer(Hourly_activity ~ Treatment + Treatment:Population + light + light:Treatment + ns(hour, 5) + monitor + start_day + (1 + ns(hour, 5) + light | individual), data=dat.hourly)

summary(mod_trial_1)
pacf(resid(mod_trial_1))
car::Anova(mod_trial_1)
fixef(mod_trial_1)
plot(allEffects(mod_trial_1))


mod_trial_2 <- lmer(Hourly_activity ~ sin(hour2) + cos(hour2) + Treatment + Treatment:Population + light + light:Treatment + start_day + monitor
                        + (1 + light | individual), data=dat.hourly)

summary(mod_trial_2)
pacf(resid(mod_trial_2))
car::Anova(mod_trial_2)

Evolve_plot <- effect("Treatment*light", mod_trial_2)
Evolve_plot <- as.data.frame(Evolve_plot)
head(Evolve_plot)
Evolve_plot2 <- ggplot(Evolve_plot, 
                  aes(y=fit, x=light, colour=Treatment))

Evolve_plot3 <- Evolve_plot2 +
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Count", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(Evolve_plot3)

#############

Exp3_mod_spli <- lmer(activity_counts ~ ns(hour, 5) + monitor + Treatment*light +  (1 + ns(hour, 5)) + (1 + light | individual), 
                      data=Exp3_hour)


summary(Exp3_mod_spli)
pacf(resid(Exp3_mod_spli))
car::Anova(Exp3_mod_spli)

Exp3_mod_spli_2 <- lmer(activity_counts ~ sin(hour2) + cos(hour2) + Treatment*light + monitor
                        + (1 + light | individual), data=Exp3_hour)

summary(Exp3_mod_spli_2)
pacf(resid(Exp3_mod_spli_2))
car::Anova(Exp3_mod_spli_2)
anova(Exp3_mod_spli_2)

Exp3_hour$hour3 <- (pi*Exp3_hour$hour/6)
Exp3_hourl <- subset(Exp3_hour, light=="light")
Exp3_hourlmod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3) +monitor +(1|individual), data=Exp3_hourl)
summary(Exp3_hourlmod)
Anova(Exp3_hourlmod)


Exp3_hourd <- subset(Exp3_hour, light=="dark")
Exp3_hourdmod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3)  +(1|individual), data=Exp3_hourd)
summary(Exp3_hourdmod)
Anova(Exp3_hourdmod)


Exp3_plot <- effect("Treatment*light", Exp3_mod_spli_2)
Exp3_plot
Exp3_plot <- as.data.frame(Exp3_plot)
head(Exp3_plot)
head(Exp3_plot)






Exp3_plot2 <- ggplot(Exp3_plot, 
                       aes(y=fit, x=light, colour=Treatment))

Exp3_plot3 <- Exp3_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Count", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("grey20",  "thistle4", "darkorange3", "#E69F00"))
print(Exp3_plot3)





######################

Exp2_mod_spli <- lmer(activity_counts ~ ns(hour, 5) + monitor + Treatment*light +  (1 + ns(hour, 5)) + (1 + light | individual), 
                      data=Exp2_hour)


summary(Exp2_mod_spli)
pacf(resid(Exp2_mod_spli))
car::Anova(Exp2_mod_spli)

Exp2_mod_spli_2 <- lmer(activity_counts ~ sin(hour2) + cos(hour2) + Treatment*light + monitor
                        + (1 + light | individual), data=Exp2_hour)

summary(Exp2_mod_spli_2)
pacf(resid(Exp2_mod_spli_2))
car::Anova(Exp2_mod_spli_2)

Exp2_hour$hour3 <- (pi*Exp2_hour$hour/6)
Exp2_hourl <- subset(Exp2_hour, light=="light")
Exp2_hourlmod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3)  +(1|individual), data=Exp2_hourl)
summary(Exp2_hourlmod)
Anova(Exp2_hourlmod)

Exp2_hourd <- subset(Exp2_hour, light=="dark")
Exp2_hourdmod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3)  +(1|individual), data=Exp2_hourd)
summary(Exp2_hourdmod)
Anova(Exp2_hourdmod)

Exp2_plot <- effect("Treatment*light", Exp2_mod_spli_2)
Exp2_plot <- as.data.frame(Exp2_plot)
head(Exp2_plot)
Exp2_plot2 <- ggplot(Exp2_plot, 
                     aes(y=fit, x=light, colour=Treatment))

Exp2_plot3 <- Exp2_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Count", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15),  axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("grey20", "#E69F00"))
print(Exp2_plot3)


######################

spider_mod_spli <- lmer(activity_counts ~ ns(hour, 5) + monitor + Treatment*light +  (1 + ns(hour, 5)) + (1 + light | individual), 
                      data=act_hour)


summary(spider_mod_spli)
llikAIC(spider_mod_spli)
pacf(resid(spider_mod_spli))
car::Anova(spider_mod_spli)

#Significant:
act_hour$hour3 <- (pi*act_hour$hour/6)
act_hourlight <- subset(act_hour, light=="light")
spider_lightMod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3)  +(1|individual), data=act_hourlight)
summary(spider_lightMod)
Anova(spider_lightMod)

act_hourdark <- subset(act_hour, light=="dark")
spider_darkMod <- lmer(activity_counts ~ Treatment + sin(hour3) + cos(hour3)  +(1|individual), data=act_hourdark)
summary(spider_darkMod)
Anova(spider_darkMod)



spider_mod_spli_2 <- lmer(activity_counts ~ Treatment*light + sin(hour2) + cos(hour2)  + monitor
                        + (1 + light | individual), data=act_hour)

summary(spider_mod_spli_2)
pacf(resid(spider_mod_spli_2))
car::Anova(spider_mod_spli_2)


spider_plot <- effect("Treatment*light", spider_mod_spli_2)
spider_plot <- as.data.frame(spider_plot)
head(spider_plot)
spider_plot2 <- ggplot(spider_plot, 
                     aes(y=fit, x=light, colour=Treatment))

spider_plot3 <- spider_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Count", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999", "#E69F00"))
print(spider_plot3)


####################

mantid_mod_spli <- lmer(activity_counts ~ ns(hour, 5) + monitor + Treatment*light +  (1 + ns(hour, 5)) + (1 + light | individual), data=Mantid_hour)

summary(mantid_mod_spli)
pacf(resid(mantid_mod_spli))
car::Anova(mantid_mod_spli)

#Mantid_hour_Light <- subset(Mantid_hour, light=="light")
#mantid_modLight <- lmer(activity_counts ~ Treatment + sin(hour2) + cos(hour2)  +(1|individual), data=Mantid_hour_Light)
#summary(mantid_modLight)
#Anova(mantid_modLight)

#Mantid_hour_dark <- subset(Mantid_hour, light=="dark")
#mantid_moddark <- lmer(activity_counts ~ Treatment + sin(hour2) + cos(hour2)  +(1|individual), data=Mantid_hour_dark)
#summary(mantid_moddark)
#Anova(mantid_moddark)

mantid_mod_spli_2 <- lmer(activity_counts ~ Treatment*light + sin(hour2) + cos(hour2)  + monitor
                          + (1 + light | individual), data=Mantid_hour)

summary(mantid_mod_spli_2)
pacf(resid(mantid_mod_spli_2))
car::Anova(mantid_mod_spli_2)

mantid_plot <- effect("Treatment*light", mantid_mod_spli_2)
mantid_plot <- as.data.frame(mantid_plot)
head(mantid_plot)
mantid_plot2 <- ggplot(mantid_plot, 
                       aes(y=fit, x=light, colour=Treatment))

mantid_plot3 <- mantid_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Hourly Count", 
       x="Phase") +
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999",  "#56B4E9"))
print(mantid_plot3)
