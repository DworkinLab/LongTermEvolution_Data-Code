#Analysis of naive flies (Dukas Lab Flies) and activity with mantid cues

#From Dukas ReadMe: "Spring 2016 - naive population with predator cues. Note from RD: my color codes on the Excel fiels are yellow for light, orange for spider cues and green for control" 

#Convert .xlsx to .txt files (and rename to consistant format) online conversion

library(lme4)
library(nlme)
library(tidyr)
library(dplyr)
library(ggplot2)
library(effects)

#Data input: working directory 
setwd("~/Bioinformatics/Long_Term_Data/Data_LongTermPopulationExperiments_Git/Predation_ActivityAggregation/data/Activity_Drosophila_SpiderCues_May2016")