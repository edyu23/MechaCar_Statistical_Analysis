library(dplyr) # Loading required library
library(tidyverse)
MechaCar_mpg_df <- read.csv(file='MechaCar_mpg.csv',check.names=F, stringsAsFactors = F) # Reading Data File
#Performing Linear Regression
lm_MechaCar_mpg <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=MechaCar_mpg_df)
#Determining R-squared and p-value
summary_lm_MechaCar_mpg <- summary(lm_MechaCar_mpg)
summary_lm_MechaCar_mpg
#Read the data file
Suspension_Coil_df <- read.csv(file='Suspension_Coil.csv',stringsAsFactors = F)
head(Suspension_Coil_df)
#Summary Statistics
total_summary <- summarize(Suspension_Coil_df, Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
total_summary
#Lot Summary DataFrame
lot_summary <- Suspension_Coil_df %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI), Variance=var(PSI), SD=sd(PSI), groups = 'keep')
lot_summary
sample_table <- Suspension_Coil_df %>% sample_n(50)
head(sample_table)
plt <- ggplot(sample_table,aes(x=PSI))
plt + geom_density()
#Forming our Hypothesis
#H0 : There is no statistical difference between the observed sample mean and its presumed population mean 1500 PSI.
#Ha : There is a statistical difference between the observed sample mean and its presumed population mean of 1500 PSI.
t.test(sample_table$PSI,mu=1500)
#t.test on Lot1
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot=="Lot1"),mu=1500)
#t.test on Lot2
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot=="Lot2"),mu=1500)
#t.test on Lot 3
t.test(subset(Suspension_Coil_df$PSI,Suspension_Coil_df$Manufacturing_Lot=="Lot3"),mu=1500)
