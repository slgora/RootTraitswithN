###  MC Data Analyses Rerun ##
###  Gora_Thesis_Results_MCAnalysis.R
###  by Sarah Gora
###  Date created: Dec 5, 2021

# set WD 

setwd("~/Desktop/Thesis_Data/R Code")

# load packages
install.packages("gt")
library(gt)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)


#load Data
library(readr)
MC_Data <- read_csv("~/Desktop/Gora_MC_Datasheet_FINAL.csv")
View(MC_Data )

#Make sure Plant ID is a factor
MC_Data$Sample_ID <- factor(MC_Data$Sample_ID)
#Make sure Treatment2 is a factor
MC_Data$Treatment2 <- factor(MC_Data$Treatment2)

#filter out by Species
DO_Traits_MC <- MC_Data %>%
  filter(Species=="Dichanthelium_oligosanthes") # N=34
AP_Traits_MC <- MC_Data %>%
  filter(Species=="Ambrosia_psilostachya")  # N=34
SM_Traits_MC <- MC_Data %>%
  filter(Species=="Solidago_missouriensis") # N=30
SN_Traits_MC <- MC_Data %>%
  filter(Species=="Sorghastrum_nutans") # N=35
AG_Traits_MC <- MC_Data %>%
  filter(Species=="Andropogon_gerardii") # N=35

#### Filter out Control, 10g N for each species in MC Data
DO_N0N10_Traits_MC <- MC_Data %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits_MC <- MC_Data %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits_MC <- MC_Data %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits_MC <- MC_Data %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits_MC <- MC_Data %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")


#### Filter out Change data for each species for MC Data
DO_Ch_Traits_MC <- MC_Data %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")
AP_Ch_Traits_MC <- MC_Data %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Experiment=="Change")
SM_Ch_Traits_MC <- MC_Data %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Experiment=="Change")
SN_Ch_Traits_MC <- MC_Data %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Experiment=="Change")
AG_Ch_Traits_MC <- MC_Data %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Experiment=="Change")


## Mixed Model ANOVAS, N0N10

# ANOVA for MC for DO
aov_MC_DO <- aov(Perc_MC_Colonized~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_MC)
summary(aov_MC_DO)
model.tables(aov_MC_DO, "means")
## Means
# 0: 47.77
# 10: 46.47
# p=0.870

# ANOVA for MC for AP
aov_MC_AP <- aov(Perc_MC_Colonized~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_MC)
summary(aov_MC_AP)
model.tables(aov_MC_AP, "means")
## Means
# 0: 23.238
# 10: 30.115
# p= 0.433

# ANOVA for MC for SM
aov_MC_SM <- aov(Perc_MC_Colonized~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_MC)
summary(aov_MC_SM)
model.tables(aov_MC_SM, "means")
## Means
# 0: 45.61
# 10: 53.70
# p= 0.409

# ANOVA for MC for AG
aov_MC_AG <- aov(Perc_MC_Colonized~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_MC)
summary(aov_MC_AG)
model.tables(aov_MC_AG, "means")
## Means
# 0: 30.64
# 10: 27.79
# p= 0.662

# ANOVA for MC for SN
aov_MC_SN <- aov(Perc_MC_Colonized~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_MC)
summary(aov_MC_SN)
model.tables(aov_MC_SN, "means")
## Means
# 0: 32.80
# 10: 42.94
# p= 0.137

### Conclusions: ANOVA for MC ###

# DO: F=0.027 p=0.870
# AP: F=0.641 p=0.433
# SM: F=0.720 p=0.409
# AG: F=0.197 p=0.662
# SN: F=2.376 p=0.137







# GLMs for ChANGE data for MC

#DO MC
reg_DO_Ch_MC <- aov(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = DO_Ch_Traits_MC)
summary(reg_DO_Ch_MC)
model.tables(reg_DO_Ch_MC, "means")
TukeyHSD(reg_DO_Ch_MC) #check
# slope:

#Means: 
#  0:  51.79
# 2.5: 57.24
# 10:  61.06
# 20:  67.11

# adj R2=0.174
#     F= 2.471
# adj p=0.095

#AP MC
reg_AP_Ch_MC <- lm(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits_MC))
summary(reg_AP_Ch_MC)
model.tables(reg_AP_Ch_MC, "means")

# slope:

#Means: 
#  0:   14.99
# 2.5:  21.00
# 10:   26.77
# 20:   18.24

# adj R2= -0.091
#     F= 0.389
# adj p= 0.762


#SM MC
reg_SM_Ch_MC <- lm(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits_MC))
summary(reg_SM_Ch_MC)
model.tables(reg_SM_Ch_MC, "means")

# slope:

#Means: 
#  0:   41.92
# 2.5:  57.52
# 10:   53.11
# 20:   56.17

# adj R2= -0.066
#     F= 0.546
# adj p= 0.657


#AG MC
reg_AG_Ch_MC <- lm(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits_MC))
summary(reg_AG_Ch_MC)
model.tables(reg_AG_Ch_MC, "means")

# slope:

#Means: 
#  0:   24.31
# 2.5:  30.36
# 10:   23.13
# 20:   26.82

# adj R2= -0.110
#     F= 0.274
# adj p= 0.843

#AG MC
reg_AG_Ch_MC <- lm(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits_MC))
summary(reg_AG_Ch_MC)
model.tables(reg_AG_Ch_MC, "means")

# slope: 

#Means: 
#  0:   24.31
# 2.5:  30.36
# 10:   23.13
# 20:   26.82

# adj R2= -0.110
#     F= 0.274
# adj p= 0.843



#SN MC
reg_SN_Ch_MC <- lm(Perc_MC_Colonized~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits_MC))
summary(reg_SN_Ch_MC)
model.tables(reg_SN_Ch_MC, "means")

# slope: 

#Means: 
#  0:   35.67 
# 2.5:  46.66
# 10:   43.35
# 20:   45.00

# adj R2= -0.085
#     F= 0.427
# adj p= 0.736
