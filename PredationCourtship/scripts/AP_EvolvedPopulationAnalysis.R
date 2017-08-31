 #Official Script

source("AP_Packages.R")
source("AP_Clean_Data.R")

head(AP_Data)
AP_Data$Treatment2 <- AP_Data$Treatment
AP_Data$Treatment <- ifelse(AP_Data$Treatment=="LTP", 
                               "Mantids", 
                            ifelse(AP_Data$Treatment=="LTC",
                               "Control", "Spiders"))
AP_Data$Treatment <- as.factor(AP_Data$Treatment)
#Models

#Courtship Latency:

mod_court <- lmer(Rel_Court_lat ~ 1 + Treatment*AgeBin + 
                    (1|Date) + (1|Treatment:Rep), 
                  data = AP_Data)

summary(mod_court)
car::Anova(mod_court)


courtLat <- effect("Treatment*AgeBin", mod_court)
courtLat <- as.data.frame(courtLat)

head(courtLat)
latenCourt <- ggplot(courtLat, 
                     aes(y=fit, x=AgeBin, colour=Treatment))

latenCourt2 <- latenCourt + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Time (sec)", 
       x="Age Bins") +
  #ggtitle("Courtship Latency") + 
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
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
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Time (sec)", 
       x="Age Bins") +
  #ggtitle("Copulation Latency") + 
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
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
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Time (sec)", 
       x="Age Bins") +
  #ggtitle("Copulation Duration") + 
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))


print(DuratCop2)


# Paul and Ian need to finish figuring out convergence issues for mixed glm. 
mod_cop_count <- glmer(Copulation ~ 1 + Treatment*AgeBin + 
                         (1|Date) + (1|Treatment:Rep), 
                      family = "binomial", 
                       data = AP_Data)

#TMB_mod_cop_count <- glmmTMB(Copulation ~ 1 + Treatment*AgeBin + 
 #                              (1|Date) + (1|Treatment:Rep), 
  #                           family = "binomial", 
   #                          data = AP_Data)

#summary(TMB_mod_cop_count)
summary(mod_cop_count)

#Anova(TMB_mod_cop_count)
Anova(mod_cop_count)

cop_prop_plot_glmer <- effect("Treatment*AgeBin", mod_cop_count)
cop_prop_plot_glmer <- as.data.frame(cop_prop_plot_glmer)

propCop_glmer <- ggplot(cop_prop_plot_glmer, 
                  aes(y=fit, x=AgeBin, colour=Treatment))

propCop_glmer_2 <- propCop_glmer + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=3) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Proportion", 
       x="Age Bins") +
  #ggtitle("Copulation Proportion") + 
  theme(text = element_text(size=15), 
        axis.text.x= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))

propCop_glmer_2


#This is the issue; some age bin/treatments have 0 (Namely LTSR4 = 100% copulation)
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

cop_prop_plot2 <- effect("Treatment*AgeBin", mod_cop_count_glm)
cop_prop_plot2 <- as.data.frame(cop_prop_plot2)

propCop4 <- ggplot(cop_prop_plot2, 
                  aes(y=fit, x=AgeBin, colour=Treatment))

propCop3 <- propCop4 + 
  geom_point(stat="identity", 
             position=position_dodge(0.5), size=5) + 
  geom_linerange(aes(ymin=lower, ymax=upper), 
                 position = position_dodge(0.5), size=1.5) + 
  labs(y="Proportion", 
       x="Age Bins") +
  #ggtitle("Copulation Proportion") + 
  theme(text = element_text(size=20), 
        axis.text.x= element_text(size=15), axis.text.y= element_text(size=15)) +
  scale_colour_manual(values=
                        c("#999999", "#56B4E9", "#E69F00"))

propCop3


multiplot(latenCourt2, LatenCop2, DuratCop2, propCop_glmer_2, cols=2)

