#Models:

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


