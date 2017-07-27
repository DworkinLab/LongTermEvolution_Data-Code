#Models:
#### old models here at top; new models at bottom (with ns/bs)

source('Packages_source_file.R')
source('Activity_cleanAllData.R')
source("Activity_allData_Plots.R")


## Evolved Popualtion Models
hour.mod <- lm(Hourly_activity ~ Predation + Predation:Population + hour + monitor + start_day,data=dat.hourly_2)
summary(hour.mod)
pacf(resid(hour.mod))

# Accounting for the auto-correlation
gls.mod <- gls(Hourly_activity ~ Predation + Predation:Population + hour + monitor + start_day,
               correlation = corAR1(form =~hour|individual),
               data=dat.hourly_2)
anova(gls.mod)
summary(gls.mod)
#acf(resid(gls.mod))

gls.mod.2 <- gls(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  hour + monitor + start_day,
                 correlation = corAR1(form =~hour|individual),
                 data=dat.hourly_2)  

summary(gls.mod.2)
#confint(gls.mod.2)

### New model by Ian:
require(stats)
require(graphics)
require(splines)
mod_trial_1 <- lmer(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  bs(hour, 5) + monitor + start_day + (1 + bs(hour, 5) + light | individual), data=dat.hourly_2)

summary(mod_trial_1)
anova(mod_trial_1)
pacf(resid(mod_trial_1))
car::Anova(mod_trial_1)



################
#Model made for a test:
# Models:

head(dat.hourly)
dat.hourly$Predation <- as.factor(dat.hourly$Predation)
dat.hourly$light <- as.factor(dat.hourly$light)


mod_trial_1 <- lmer(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  Predation:ns(hour, 5) + monitor + start_day + (1 + ns(hour, 5) + light | individual), data=dat.hourly)

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


hour_plot <- effect("Predation:ns(hour,5)", mod_trial_1)
hour_plot <- as.data.frame(hour_plot)
head(hour_plot)
hour_plot2 <- ggplot(hour_plot, 
                     aes(y=fit, x=hour, colour=Predation))

hour_plot3 <- hour_plot2 + 
  geom_smooth(stat="identity", 
              position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  ggtitle("Evolved Population") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(hour_plot3)


################




#### Mantid Cues

man.mod <- lm(activity_counts ~ Treatment + hour + monitor,data=Mantid_hour)

summary(man.mod)
#pacf(resid(man.mod))



man_mod_2 <- gls(activity_counts ~ Treatment + hour + monitor, correlation = corAR1(form = ~ 1|hour), data=Mantid_hour)
anova(man_mod_2)
summary(man_mod_2)
#acf(resid(man_mod_2))

man_mod_3 <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour + monitor, correlation = corAR1(form =~1|hour), control = list(singular.ok = TRUE), data=Mantid_hour)

summary(man_mod_3)
#confint(man_mod_3)
#acf(resid(man_mod_3))




##### Spider Cues Models:

hour.mod <- lm(activity_counts ~ Treatment + hour + monitor,data=act_hour)
summary(hour.mod)
pacf(resid(hour.mod))

correl_mod <- gls(activity_counts ~ Treatment + hour + monitor, correlation = corAR1(form = ~ 1|hour), data=act_hour)

anova(correl_mod)
summary(correl_mod)
#acf(resid(correl_mod))

#With light
act_cor_light_mod <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour + monitor, correlation = corAR1(form =~1|hour), control = list(singular.ok = TRUE), data=act_hour)
summary(act_cor_light_mod)
#confint(act_cor_light_mod)
#acf(resid(act_cor_light_mod))


### Complex Cues 2

Exp2_mod <- lm(activity_counts ~ Treatment + hour + monitor,data=Exp2_hour)
summary(Exp2_mod)
#pacf(resid(Exp2_mod))

Exp2_mod_2 <- gls(activity_counts ~ Treatment + hour + monitor, correlation = corAR1(form = ~ 1|hour), data=Exp2_hour)
anova(Exp2_mod_2)
summary(Exp2_mod_2)
#acf(resid(Exp2_mod_2))


Exp2_mod_3 <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour + monitor, correlation = corAR1(form =~1|hour), control = list(singular.ok = TRUE), data=Exp2_hour)

summary(Exp2_mod_3)
#confint(Exp2_mod_3)
#acf(resid(Exp2_mod_3))



### Complex Cues 3


#model
exp3.mod <- lm(activity_counts ~ Treatment + hour + monitor,data=Exp3_hour)
summary(exp3.mod)
#pacf(resid(exp3.mod))

exp3.mod_2 <- gls(activity_counts ~ Treatment + hour + monitor, correlation = corAR1(form = ~ 1|hour), data=Exp3_hour)

anova(exp3.mod_2)
summary(exp3.mod_2)
#acf(resid(exp3.mod_2))

exp3.mod_3 <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour + monitor, correlation = corAR1(form =~1|hour), control = list(singular.ok = TRUE), data=Exp3_hour)


summary(exp3.mod_3)
#confint(exp3.mod_3)
#acf(resid(exp3.mod_3))


#################################################################
### Running the new models (ns/bs knots)

#evovled pop

print(LT_plot2)

mod_trial_1 <- lmer(Hourly_activity ~ Predation + Predation:Population + light + light:Predation +  bs(hour, 4) + monitor + start_day + (1 + bs(hour, 4) + light | individual), data=dat.hourly_2)

summary(mod_trial_1)
anova(mod_trial_1)
pacf(resid(mod_trial_1))
car::Anova(mod_trial_1)







### Mantid Cues

print(Man_plot2)

#man_mod_spli <- lmer(activity_counts ~ Treatment + light + light:Treatment +  bs(hour, 4) + monitor + (1 + bs(hour, 4) + light | individual), data=Mantid_hour)

#summary(man_mod_spli)
#pacf(resid(man_mod_spli))
#car::Anova(man_mod_spli)

man_mod_spli <- lmer(activity_counts ~ Treatment + light + light:Treatment +  ns(hour, 4) + monitor + (1 + light | individual), data=Mantid_hour)

summary(man_mod_spli)
pacf(resid(man_mod_spli))
car::Anova(man_mod_spli)


man_mod_spli_2 <- lmer(activity_counts ~ monitor + sin(pi*hour/12) + cos(pi*hour/12) + Treatment*light + (1 + light | individual), data=Mantid_hour)

summary(man_mod_spli_2)
pacf(resid(man_mod_spli_2))
car::Anova(man_mod_spli_2)



### Spider Cues

print(spi_plot2)

spi_mod_spli <- lmer(activity_counts ~ Treatment + light + light:Treatment +  ns(hour, 5) + monitor + (1  + light | individual), data=act_hour)


spi_mod_spli_2 <- lmer(activity_counts ~ Treatment + light + light:Treatment +  sin(pi*hour/12) + cos(pi*hour/12) + monitor + (1 + sin(pi*hour/12) + cos(pi*hour/12) + light | individual), data=act_hour)

summary(spi_mod_spli)
summary(spi_mod_spli_2)
pacf(resid(spi_mod_spli))
car::Anova(spi_mod_spli)
car::Anova(spi_mod_spli_2)


### Complex Cues: Experiment 2 (Crickets vs. Spiders)

print(Exp2_plot2)

Exp2_mod_spli <- lmer(activity_counts ~ Treatment + light + light:Treatment +  ns(hour, 4) + monitor + (1 + light | individual), data=Exp2_hour)

summary(Exp2_mod_spli)
pacf(resid(Exp2_mod_spli))
car::Anova(Exp2_mod_spli)


Exp2_mod_spli_2 <- lmer(activity_counts ~ monitor + sin(pi*hour/12) + cos(pi*hour/12) + Treatment*light + (1 + light | individual), data=Exp2_hour)

summary(Exp2_mod_spli_2)
pacf(resid(Exp2_mod_spli_2))
car::Anova(Exp2_mod_spli_2)


### Complex Cues: Experiment 3: Crickets, Flies, Spiders fed crickets and spiders fed flies

print(Exp3_plot2)

Exp3_mod_spli <- lmer(activity_counts ~ ns(hour, 3) + monitor + Treatment*light + (1 + light | individual), 
                      data=Exp3_hour)

summary(Exp3_mod_spli)
pacf(resid(Exp3_mod_spli))
car::Anova(Exp3_mod_spli)

Exp3_mod_spli_2 <- lmer(activity_counts ~ monitor + sin(pi*hour/12) + cos(pi*hour/12) + Treatment*light 
                        + (1 + light | individual), data=Exp3_hour)

summary(Exp3_mod_spli_2)
pacf(resid(Exp3_mod_spli_2))
car::Anova(Exp3_mod_spli_2)


#install.packages("lsmeans")
#library(lsmeans)
#install.packages("lmerTest")
#library(lmerTest)

lsmeans(Exp3_mod_spli_2)
lsmeans(Exp3_mod_spli_2, specs = c("Treatment", "light"))
lsmeans(Exp3_mod_spli, specs = c("Treatment", "light"))
#install.packages("lmerTest")
library(lmerTest)
lsmeans(Exp3_mod_spli, specs = c("Treatment", "light"))
lsmeansLT(Exp3_mod_spli, specs = c("Treatment", "light"))
lsmeansLT(Exp3_mod_spli, specs = c("Treatment", "light"))






Exp3_hour$hour2 <- (pi*Exp3_hour$hour/12)

Exp3_mod_spli_2 <- lmer(activity_counts ~ Treatment*light  + sin(hour2) + cos(hour2) + monitor 
                        + (1 + light | individual), data=Exp3_hour)

summary(Exp3_mod_spli_2)
pacf(resid(Exp3_mod_spli_2))
car::Anova(Exp3_mod_spli_2)

plot(allEffects(Exp3_mod_spli_2))

Exp3_plot <- effect("Treatment*light", Exp3_mod_spli_2)
Exp3_plot <- as.data.frame(Exp3_plot)
head(Exp3_plot)
Exp3_plot2 <- ggplot(Exp3_plot, 
                     aes(y=fit, x=light, colour=Treatment))

Exp3_plot3 <- Exp3_plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Exp3") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00", "grey"))
print(Exp3_plot3)
