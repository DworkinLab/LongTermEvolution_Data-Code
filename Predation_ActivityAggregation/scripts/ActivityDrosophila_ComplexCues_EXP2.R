##   From Complex Cues Data Directory README.md: In Exp 2, flies had 2 types of cues marked in the top row: spiders or crickets.


#Try reading xlsx rather than online convert to .txt
#install.packages("xlsx")
library(xlsx)
#res <- read.xlsx(file, 2)

ComExp2M1 <- read.xlsx("~/Bioinformatics/Long_Term_Data/Data_LongTermPopulationExperiments_Git/Predation_ActivityAggregation/data/Activity_Drosophila_ComplexCues_June17_2016/Exp 2 spider vs cricket cues M1.xlsx", 1)

#Works, but date and time are different set up. need to fix???
#Easier is to just copy and convert to .txt
