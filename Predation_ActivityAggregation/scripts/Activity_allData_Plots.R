#Activity All Data: Plots and Models

#https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

#source("Activity_cleanAllData.R")

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
  #geom_vline(xintercept = 8) +
  #ggtitle("Long Term Evolved Populations: hourly activity counts") + 
  labs(y="Hourly Counts", 
       x="Hour") +
  scale_colour_manual(values=c("#999999",  "#56B4E9", "#E69F00"))
print(LT_plot2)

## Mantid Cues

#Initiation at two times?
Man_plot <- ggplot(Mantid_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + xlim(0,24) + ylim(0,100)
Man_plot2 <- Man_plot + geom_jitter(size=0.5) + geom_smooth(method = "loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
           xmin = 10, xmax = 22,
           ymin = 0, ymax = 100) +
  labs(y="Hourly Counts", 
       x="Hour") +
  #geom_vline(xintercept = 14) + geom_vline(xintercept = 11.5) +
  #ggtitle("Mantid Cues vs Control: hourly activity counts") + 
  scale_colour_manual(values=c("#999999",  "#56B4E9"))
print(Man_plot2)



## Spider Cues:

spi_plot <- ggplot(act_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + ylim(0,400) + xlim(0,24)
spi_plot2 <- spi_plot + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 400) +
  #geom_vline(xintercept = 12) +
  #ggtitle("Spider Cues vs. Control: hourly activity counts") + 
  labs(y="Hourly Counts", 
       x="Hour") +
  scale_colour_manual(values=c("#999999", "#E69F00"))
print(spi_plot2)




## Complex Cues 2:

plot_Exp2 <- ggplot(Exp2_hour, aes(x=hour, y= activity_counts, colour=Treatment)) + xlim(0,25) + ylim(0,400)
plot_Exp2_2 <- plot_Exp2 + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 400) +
  labs(y="Hourly Counts", 
       x="Hour") +
  #geom_vline(xintercept = 12) +
  #ggtitle("Complex Cues Experiment 2: hourly activity counts") + 
  scale_colour_manual(values=c("grey20", "#E69F00"))
print(plot_Exp2_2)


## Complex Cues 3:

plot_Exp3 <- ggplot(Exp3_hour, aes(x=hour, y= activity_counts, colour=Treatment)) #+ xlim(0,24) + ylim(0,500)
plot_Exp3_2 <- plot_Exp3 + geom_jitter(size=0.5) + geom_smooth(size=1, method="loess") + 
  annotate("rect", fill = "yellow", alpha = 0.2, 
               xmin = 10, xmax = 22,
               ymin = 0, ymax = 500) +
  labs(y="Hourly Counts", 
       x="Hour") +
  #geom_vline(xintercept = 12) +
  #ggtitle("Complex Cues Experiment 3: hourly activity counts") + 
  scale_colour_manual(values=c("grey20",  "thistle4", "darkorange3", "#E69F00"))
print(plot_Exp3_2)




#ABS
LT_plot2 + scale_colour_manual(values=c("#999999",  "#56B4E9", "#E69F00"))

spi_plot2 + scale_color_manual(values=c("#999999",  "#E69F00"))

