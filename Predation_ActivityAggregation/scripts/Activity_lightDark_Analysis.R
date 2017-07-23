#light dark analysis 

source('Packages_source_file.R')
source('Activity_Evolved_Clean.R')
source('Activity_Exp2_Clean.R')
source('Activity_Exp3_Clean.R')
source('Activity_Mantids_Clean.R')
source('Activity_Spider_Clean.R')
source('Activity_Predators_Clean.R')


#Exp2
Exp2_light <- Exp2_hour %>%
  group_by(Treatment, Vial, monitor, light) %>%
  summarise(activity_counts=sum(activity_counts))

#Exp2_light2 <- Complex_2_long %>%
#  group_by(Treatment, Vial, monitor, lightON) %>%
#  summarise(activity_counts=sum(activity_counts))

#Exp2_light == Exp2_light2

head(Exp2_light)
#head(Exp2_light2)

#Different: LightON != 10-10????

Exp2_light$light <- as.factor(Exp2_light$light)

head(Exp2_light)

Exp2.light <- lm(activity_counts ~ Treatment:light + monitor, data=Exp2_light)

summary(Exp2.light)
anova(Exp2.light)

plot(allEffects(Exp2.light))


Exp2.plot <- effect("Treatment*light", Exp2.light)
Exp2.plot <- as.data.frame(Exp2.plot)
head(Exp2.plot)

Exp2.plot2 <- ggplot(Exp2.plot, 
                     aes(y=fit, x=light, colour=Treatment))

Exp2.plot3 <- Exp2.plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Exp2") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9"))
print(Exp2.plot3)


#################
#Exp3

Exp3_light <- Exp3_hour %>%
  group_by(Treatment, Vial, monitor, light) %>%
  summarise(activity_counts=sum(activity_counts))

Exp3_light$light <- as.factor(Exp3_light$light)

head(Exp3_light)

Exp3.light <- lm(activity_counts ~ Treatment:light + monitor, data=Exp3_light)

summary(Exp3.light)


plot(allEffects(Exp3.light))


Exp3.plot <- effect("Treatment*light", Exp3.light)
Exp3.plot <- as.data.frame(Exp3.plot)
head(Exp3.plot)

Exp3.plot2 <- ggplot(Exp3.plot, 
                   aes(y=fit, x=light, colour=Treatment))

Exp3.plot3 <- Exp3.plot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Exp3") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00", "grey"))
print(Exp3.plot3)

#################
#Mantids
head(Mantid_hour)

Mantid_light <- Mantid_hour %>%
  group_by(Treatment, Vial, monitor, light) %>%
  summarise(activity_counts=sum(activity_counts))

Mantid_light$light <- as.factor(Mantid_light$light)

head(Mantid_light)

Mantid.light <- lm(activity_counts ~ Treatment:light + monitor, data=Mantid_light)

summary(Mantid.light)


plot(allEffects(Mantid.light))


Manplot <- effect("Treatment*light", Mantid.light)
Manplot <- as.data.frame(Manplot)
head(Manplot)
Manplot2 <- ggplot(Manplot, 
                   aes(y=fit, x=light, colour=Treatment))

Manplot3 <- Manplot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Mantids") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(Manplot3)



#################
#Spiders
head(act_hour)

spider_light <- act_hour %>%
  group_by(Treatment, Vial, monitor, light) %>%
  summarise(activity_counts=sum(activity_counts))

spider_light$light <- as.factor(spider_light$light)

head(spider_light)

spider.light <- lm(activity_counts ~ Treatment:light + monitor, data=spider_light)

summary(spider.light)
car::Anova(spider.light)

plot(allEffects(spider.light))


spiplot <- effect("Treatment*light", spider.light)
spiplot <- as.data.frame(spiplot)
head(spiplot)
spiplot2 <- ggplot(spiplot, 
                  aes(y=fit, x=light, colour=Treatment))

spiplot3 <- spiplot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Spiders") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(spiplot3)




#######################
head(dat.hourly)

evolved_light <- dat.hourly %>%
  group_by(Trt, Population, vial, monitor, start_day, light) %>%
  summarise(activity_counts=sum(Hourly_activity))


head(evolved_light)
evolved_light$light <- as.factor(evolved_light$light)

evolved.light <- lm(activity_counts ~ Population + Trt:light + monitor, data=evolved_light)

summary(evolved.light)

plot(allEffects(evolved.light))


evplot <- effect("Trt*light", evolved.light)
evplot <- as.data.frame(evplot)
head(evplot)
evplot2 <- ggplot(evplot, 
                   aes(y=fit, x=light, colour=Trt))

evplot23 <- evplot2 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5)) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5)) + 
  labs(y="Intercept", 
       x="Light") +
  ggtitle("Evolved Population 1") + 
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))
print(evplot23)

