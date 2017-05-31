#Activity All Data: Plots and Models

#https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

source("Activity_cleanAllData.R")

### Data Available:
head(dat.hourly) ## LongTermEvolvedFlies

head(Exp2_hour) ## ComplexCues2

head(Exp3_hour) ## ComplexCues3

head(Mantid_hour) ## MantidCues

head(act_hour) ## SpiderCues

### Plots:

## Evolved Populations
dat.hourly_2 <- dat.hourly
head(dat.hourly_2)
dat.hourly_2 <- within(dat.hourly_2, { 
  Predation = ifelse (Trt == "C", "Control", ifelse(Trt == "S", "Spider", "Mantids"))})

LT_plot <- ggplot(dat.hourly_2, aes(x=hour, y= Hourly_activity, colour=Predation)) + xlim(0,24) + ylim(0,600)
LT_plot2 <- LT_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 600) +
  geom_vline(xintercept = 8) +
  ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(LT_plot2)

#models
hour.mod <- lm(Hourly_activity ~ Trt + Trt:Population + hour + monitor + start_day,data=dat.hourly)
summary(hour.mod)
#pacf(resid(hour.mod))

# Accounting for the auto-correlation
gls.mod <- gls(Hourly_activity ~ Trt + Trt:Population + hour + monitor + start_day,
               correlation = corAR1(form =~hour|individual),
               data=dat.hourly)
anova(gls.mod)
summary(gls.mod)
#acf(resid(gls.mod))

gls.mod.2 <- gls(Hourly_activity ~ Trt + Trt:Population + light + light:Trt +  hour + monitor + start_day,
                 correlation = corAR1(form =~hour|individual),
                 data=dat.hourly)
summary(gls.mod.2)
#confint(gls.mod.2)



## Mantid Cues

#Initiation at two times?
Man_plot <- ggplot(Mantid_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + xlim(0,24) + ylim(0,100)
Man_plot2 <- Man_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 100) +
  geom_vline(xintercept = 14) + geom_vline(xintercept = 11.5) +
  ggtitle("Mantid Cues vs Control: hourly activity counts")
print(Man_plot2)



## Spider Cues:

spi_plot <- ggplot(act_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + ylim(0,400) + xlim(0,24)
spi_plot2 <- spi_plot + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 400) + geom_vline(xintercept = 12) +
  ggtitle("Spider Cues vs. Control: hourly activity counts") + 
  ylab("Hourly Activity") +
  xlab("Hour")
print(spi_plot2)


#models:

hour.mod <- lm(activity_counts ~ Treatment + hour + monitor + day,data=act_hour)
summary(hour.mod)
pacf(resid(hour.mod))


correl_mod <- gls(activity_counts ~ Treatment + hour + monitor + day, correlation = corAR1(form = ~ 1|hour), data=act_hour)
anova(correl_mod)
summary(correl_mod)
acf(resid(correl_mod))

#With light
act_cor_light_mod <- gls(activity_counts ~ Treatment + light + light:Treatment +  hour + monitor + day, correlation = corAR1(form =~1|hour), control = list(singular.ok = TRUE), data=act_hour)
summary(act_cor_light_mod)
confint(act_cor_light_mod)
acf(resid(act_cor_light_mod))

#Days seem weird
## Complex Cues 2:

Exp2_plot <- ggplot(Exp2_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + xlim(0,25) + ylim(0,400)
Exp2_plot2 <- Exp2_plot + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 400) +
  geom_vline(xintercept = 12) +
  ggtitle("Complex Cues Experiment 2: hourly activity counts")
print(Exp2_plot2)


## Complex Cues 3:

Exp3_plot <- ggplot(Exp3_hour, aes(x=hour, y= activity_counts, colour=Treatment)) #+ xlim(0,24) + ylim(0,500)
Exp3_plot2 <- Exp3_plot + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 500) +
  geom_vline(xintercept = 12) +
  ggtitle("Complex Cues Experiment 3: hourly activity counts")
print(Exp3_plot2)


#model
exp3.mod <- lm(activity_counts ~ Treatment + hour + monitor + day,data=Exp3_hour)
summary(exp3.mod)
pacf(resid(exp3.mod))


