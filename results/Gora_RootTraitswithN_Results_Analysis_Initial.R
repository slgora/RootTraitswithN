#### RootTraitswithN Results Analysis Initial ####
###  Gora_RootTraitswithN_Results_Analysis.R
###  by Sarah Gora
###  Date created: Dec 5, 2021

# set WD 

setwd("~/Desktop/Thesis_Data/R Code")

# load packages
install.packages("gt")
library(gt)
library(dplyr)
library(tidyverse)

#load Data
library(readr)
All_Traits <- read_csv("All_Traits_Datasheet.csv")
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet.csv")
View(All_Traits)

##### November 2022
# load NEW datasheet with columns of traits divided by Biomass
All_Traits_DivBio <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet_DivBio_RmvNA.csv")
View(All_Traits_DivBio)

## actual!!
All_Traits_DivBio_a <- read_csv("~/Desktop/Thesis_Data/All_Traits_DivBio_Datasheet_RmvNA_ACTUAL.csv")
View(All_Traits_DivBio_a)


#had to rename column 
names(All_Traits)[names(All_Traits) == "Plant_Area_cm^2"] <- "Plant_Area"
colnames(All_Traits)


### removed NA data 
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet_rmvNA.csv")
View(All_Traits)


#Make sure Plant ID is a factor
All_Traits$ID <- factor(All_Traits$ID)

All_Traits_DivBio$ID <- factor(All_Traits_DivBio$ID)


#Make sure Treatment2 is a factor
All_Traits$Treatment2 <- factor(All_Traits$Treatment2)

All_Traits_DivBio$Treatment2 <- factor(All_Traits_DivBio$Treatment2)


#filter out by Species
DO_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes")
AP_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya")
SM_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis")
SN_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans")
AG_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii")



#### Filter out Control, 10g N for each species
DO_N0N10_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")

#### Filter out Control, 10g N for each species in DivBio data 
DO_N0N10_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")



#### Filter out Change for each species Ordination data
DO_Ch_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")
AP_Ch_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Experiment=="Change")
SM_Ch_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Experiment=="Change")
SN_Ch_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Experiment=="Change")
AG_Ch_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Experiment=="Change")


### Testing out mixed-model ANOVAs ### 


#SLA Datasheet
SLA_Datasheet <- read_csv("SLA_Datasheet_Avg.csv")







###  Mixed model ANOVA for SLA, N0N10

# ANOVA for SLA for DO
aov_SLA_DO <- aov(SLA~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_SLA_DO)
model.tables(aov_SLA_DO, "means")

#divbio data
aov_SLA_divbio_DO <- aov(SLA_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio_a)
summary(aov_SLA_divbio_DO)

# ANOVA for SLA for AP
aov_SLA_AP <- aov(SLA~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_SLA_AP)
model.tables(aov_SLA_AP, "means")

#divbio data
aov_SLA_divbio_AP <- aov(SLA_div_AbG_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_SLA_divbio_AP)
model.tables(aov_SLA_divbio_AP, "means")

# ANOVA for SLA for SM
aov_SLA_SM <- aov(SLA~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_SLA_SM)
model.tables(aov_SLA_SM, "means")

#divbio data
aov_SLA_divbio_SM <- aov(SLA_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_SLA_divbio_SM)
model.tables(aov_SLA_divbio_SM, "means")

# ANOVA for SLA for AG
aov_SLA_AG <- aov(SLA~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_SLA_AG)
model.tables(aov_SLA_AG, "means")

#divbio data
aov_SLA_divbio_AG <- aov(SLA_div_AbG_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio_a)
summary(aov_SLA_divbio_AG)
model.tables(aov_SLA_divbio_AG, "means")

# ANOVA for SLA for SN
aov_SLA_SN <- aov(SLA~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_SLA_SN)
model.tables(aov_SLA_SN, "means")

#divbio data
aov_SLA_divbio_SN <- aov(SLA_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio_a)
summary(aov_SLA_divbio_SN)
model.tables(aov_SLA_divbio_SN, "means")

    ### Conclusions: ANOVA for SLA*** ###

# DO: 0.692  0.411
# AP: 0.165  0.687
# SM: 1.927  0.176
# AG: 9.142 0.0053 , * significant
# SN: 0.661  0.422 

### Conclusions: ANOVA for SLA divided by biomass*** ###

# DO: 11.85  0.002, * significant     DIFFERS, i think data is messed up here
# AP: 2.227  0.145
# SM: 1.991  0.169
# AG: 1.92  0.175, NOT significant    DIFFERS,this may be an error with sla data
# SN: 6.521 0.0153, * significant     DIFFERS




# Mixed model ANOVA for Aboveground Dry Weight

# ANOVA: AbG Dry Weight for DO
aov_AbGDryWeight_DO <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_AbGDryWeight_DO)
model.tables(aov_AbGDryWeight_DO, "means")

# ANOVA: AbG Dry Weight for AP
aov_AbGDryWeight_AP <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_AbGDryWeight_AP)
model.tables(aov_AbGDryWeight_AP, "means")

# ANOVA: AbG Dry Weight for SM
aov_AbGDryWeight_SM <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_AbGDryWeight_SM)
model.tables(aov_AbGDryWeight_SM, "means")

# ANOVA: AbG Dry Weight for AG
aov_AbGDryWeight_AG <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_AbGDryWeight_AG)
model.tables(aov_AbGDryWeight_AG, "means")

# ANOVA: AbG Dry Weight for SN
aov_AbGDryWeight_SN <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_AbGDryWeight_SN)
model.tables(aov_AbGDryWeight_SN, "means")


      ### Conclusions: ANOVA for AbG Dry Weight ###
# DO: 14.68 0.0005 *
# AP: 3.34  0.077
# SM: 0.600  0.445
# AG: 1.354  0.253
# SN: 0.569  0.456



# Mixed model ANOVA for Number of Leaves

# ANOVA: #Leaves for DO
aov_NumLeaves_DO <- aov(Leaf_Number~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumLeaves_DO)
model.tables(aov_NumLeaves_DO, "means")

#divbio data
aov_NumLeaves_divbio_DO <- aov(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov_NumLeaves_divbio_DO)
model.tables(aov_NumLeaves_divbio_DO, "means")

# ANOVA: #Leaves for AP
aov_NumLeaves_AP <- aov(Leaf_Number~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_NumLeaves_AP)
model.tables(aov_NumLeaves_AP, "means")

#divbio data
aov_NumLeaves_divbio_AP <- aov(Leaf_Number_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_NumLeaves_divbio_AP)
model.tables(aov_NumLeaves_divbio_AP, "means")

# ANOVA: #Leaves for SM
aov_NumLeaves_SM <- aov(Leaf_Number~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumLeaves_SM)
model.tables(aov_NumLeaves_SM, "means")

#divbio data 
aov_NumLeaves_divbio_SM <- aov(Leaf_Number_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_NumLeaves_divbio_SM)
model.tables(aov_NumLeaves_divbio_SM, "means")

# ANOVA: #Leaves for AG
aov_NumLeaves_AG <- aov(Leaf_Number~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_NumLeaves_AG)
model.tables(aov_NumLeaves_AG, "means")

#divbio data 
aov_NumLeaves_divbio_AG <- aov(Leaf_Number_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov_NumLeaves_divbio_AG)

# ANOVA: #Leaves for SN
aov_NumLeaves_SN <- aov(Leaf_Number~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_NumLeaves_SN)
model.tables(aov_NumLeaves_SN, "means")

#divbio data 
aov_NumLeaves_divbio_SN <- aov(Leaf_Number_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov_NumLeaves_divbio_SN)

### Conclusions: ANOVA for Number of Leaves ###
# DO: 12.75 0.00109 *
# AP: 7.057 0.0121 *
# SM: 0.008  0.929
# AG: 1.503  0.229
# SN: 0.594  0.446

### Conclusions: ANOVA for Number of Leaves divided by biomass ###
# DO: 6.629 0.0146 *
# AP: 1.129  0.296  not significant     DIFFERS
# SM: 2.982 0.0948
# AG: 1.415  0.242
# SN: 5.618 0.0236*  significant  DIFFERS



# Mixed model ANOVA for Number of Senescing Leaves

# ANOVA: Number Senescing Leaves for DO
aov_NumSLeaves_DO <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumSLeaves_DO)
model.tables(aov_NumSLeaves_DO, "means")

#divbio data


# ANOVA: Number Senescing Leaves for AP
aov_NumSLeaves_AP <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_NumSLeaves_AP)
model.tables(aov_NumSLeaves_AP, "means")

#divbio data

# ANOVA: Number Senescing Leaves for SM
aov_NumSLeaves_SM <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumSLeaves_SM)
model.tables(aov_NumSLeaves_SM, "means")

#divbio data


# ANOVA: Number Senescing Leaves for AG
aov_NumSLeaves_AG <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_NumSLeaves_AG)
model.tables(aov_NumSLeaves_AG, "means")

#divbio data


# ANOVA: Number Senescing Leaves for SN
aov_NumSLeaves_SN <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_NumSLeaves_SN)
model.tables(aov_NumSLeaves_SN, "means")


#divbio data

### Conclusions: ANOVA for Number of Senescing Leaves ###
# DO: 
# AP: 
# SM: 
# AG: 
# SN: 






# Mixed model ANOVA for Number of Emerging Leaves

# ANOVA: Number Emerging Leaves for DO
aov_NumELeaves_DO <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumELeaves_DO)
model.tables(aov_NumELeaves_DO, "means")

# ANOVA: Number Emerging Leaves for AP
aov_NumELeaves_AP <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_NumELeaves_AP)
model.tables(aov_NumELeaves_AP, "means")

# ANOVA: Number Emerging Leaves for SM
aov_NumELeaves_SM <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumELeaves_SM)
model.tables(aov_NumELeaves_SM, "means")

# ANOVA: Number Emerging Leaves for AG
aov_NumELeaves_AG <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_NumELeaves_AG)
model.tables(aov_NumELeaves_AG, "means")

# ANOVA: Number Emerging Leaves for SN
aov_NumELeaves_SN <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_NumELeaves_SN)
model.tables(aov_NumELeaves_SN, "means")

### Conclusions: ANOVA for Number of Emerging Leaves ###
# DO: p=0.0030849, significant for Treatment
# AP: p=0.046587, significant for Treatment
# SM: p=4.1977e-06, significant difference in Treatment; p= 0.0015359 significant difference in Experiment
#     p=8.7827e-05, significant difference in Plot
# AG: no significance
# SN: p=0.0151300, significant difference in Treatment; p=0.0022657 significantly different in Plot



# Mixed model ANOVA for Number of Fully Dev Leaves

# ANOVA: Number Fully Dev  Leaves for DO
aov_NumFLeaves_DO <- aov(F_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumFLeaves_DO)
model.tables(aov_NumFLeaves_DO, "means")

#divbio
aov <- aov(F_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: Number Fully Dev  Leaves for AP
aov_NumFLeaves_AP <- aov(F_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_NumFLeaves_AP)
model.tables(aov_NumFLeaves_AP, "means")

#divbio
aov <- aov(F_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: Number Fully Dev  Leaves for SM
aov_NumFLeaves_SM <- aov(F_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumFLeaves_SM)
model.tables(aov_NumFLeaves_SM, "means")

#divbio
aov <- aov(F_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: Number Fully Dev Leaves for AG
aov_NumFLeaves_AG <- aov(F_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_NumFLeaves_AG)
model.tables(aov_NumFLeaves_AG, "means")

#divbio
aov <- aov(F_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: Number Fully Dev Leaves for SN
aov_NumFLeaves_SN <- aov(F_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_NumFLeaves_SN)
model.tables(aov_NumFLeaves_SN, "means")

#divbio
aov <- aov(F_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

### Conclusions: ANOVA for Number of Fully Dev Leaves ###
# DO: 8.526 0.00618 *
# AP: 1.947  0.172
# SM: 0.607  0.442
# AG: 0      1
# SN: 0.549  0.464

## Conclusions Number of Fully Dev Leaves div Biomass 
# DO: 8.369 0.00662 *
# AP: 1.812  0.188  
# SM: 1.111  0.301
# AG:  3.351 0.0759
# SN: 4.518 0.0409 *  DIFFERS





# Mixed model ANOVA for Number of Flowers

# ANOVA: Number of Flowers for DO
aov_NumFlowers_DO <- aov(Flower_Number~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumFlowers_DO)
model.tables(aov_NumFlowers_DO, "means")

# ANOVA: Number of Flowers for SM
aov_NumFlowers_SM <- aov(Flower_Number~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumFlowers_SM)
model.tables(aov_NumFlowers_SM, "means")

# ANOVA: Number of Flowers for SN
aov_NumFlowers_SN <- aov(Flower_Number~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_NumFlowers_SN)
model.tables(aov_NumFlowers_SN, "means")

### Conclusions: ANOVA for Number of Flowers ###
# DO: no significance
# AP: no flowering data
# SM: p=1.9238e-05, significant for Treatment; p=0.00098173 significantly different in Experiment
#     p=0.00064261, significant difference in Plot
# AG: no flowering data
# SN: no flowering data





# Mixed model ANOVA for Number of Emerging Flowers

# ANOVA: Number of Emerging Flowers for DO
aov_NumEFlowers_DO <- aov(E_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumEFlowers_DO)
model.tables(aov_NumEFlowers_DO, "means")

# ANOVA: Number of Emerging Flowers for SM
aov_NumEFlowers_SM <- aov(E_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumEFlowers_SM)
model.tables(aov_NumEFlowers_SM, "means")
# ERROR MESSAGE ??? 

### Conclusions: ANOVA for Number of Emerging Flowers ###
# DO: p=1.9238e-05, significant for Treatment; p=0.00098173 significantly different in Experiment
#     p=0.00064261, significant difference in Plot
# AP: no flowering data
# SM: ** NA error i think
# AG: no flowering data
# SN: no flowering data 

# ** Note:  SOME KINF OF ERROR IN SM





# Mixed model ANOVA for Number of Scenscing Flowers

# ANOVA: Number of Scenscing Flowers for DO
aov_NumSFlowers_DO <- aov(S_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumSFlowers_DO)
model.tables(aov_NumSFlowers_DO, "means")

# ANOVA: Number of Scenscing Flowers for SM
aov_NumSFlowers_SM <- aov(S_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumSFlowers_SM)
model.tables(aov_NumSFlowers_SM, "means")
# ERROR MESSAGE ??? 

### Conclusions: ANOVA for Number of Scenscing Flowers ###
# DO: ** NA error i think 
# AP: no flowering data
# SM: not enough data
# AG: no flowering data
# SN: no flowering data 



# Mixed model ANOVA for Number of Fully Dev Flowers

# ANOVA: Number of Fully Dev Flowers for DO
aov_NumFFlowers_DO <- aov(F_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumFFlowers_DO)
model.tables(aov_NumFFlowers_DO, "means")

# ANOVA: Number of Fully Dev Flowers for SM
aov_NumFFlowers_SM <- aov(F_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumFFlowers_SM)
model.tables(aov_NumSFlowers_SM, "means")
# ERROR MESSAGE ??? 

### Conclusions: ANOVA for Number of Fully Dev Flowers ###
# DO: ** NA error i think
# AP: no flowering data
# SM: ** NA error i think
# AG: no flowering data
# SN: no flowering data 




# Mixed model ANOVA for Number of Fully Dev Flowers

# ANOVA: Number of Fully Dev Flowers for DO
aov_NumFFlowers_DO <- aov(F_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_NumFFlowers_DO)
model.tables(aov_NumFFlowers_DO, "means")

# ANOVA: Number of Fully Dev Flowers for SM
aov_NumFFlowers_SM <- aov(F_Flower_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_NumFFlowers_SM)
model.tables(aov_NumSFlowers_SM, "means")
# ERROR MESSAGE ??? 

### Conclusions: ANOVA for Number of Fully Dev Flowers ###
# DO: ** NA error i think
# AP: no flowering data
# SM: ** NA error i think
# AG: no flowering data
# SN: no flowering data 






# Mixed model ANOVA for Plant Height

# ANOVA: Height for DO
aov_Height_DO <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_Height_DO)
model.tables(aov_Height_DO, "means")

#divbio data
aov_Height_divbio_DO <- aov(Plant_Height_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov_Height_divbio_DO)

# ANOVA: Height for AP
aov_Height_AP <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_Height_AP)
model.tables(aov_Height_AP, "means")

#divbio data
aov_Height_divbio_AP <- aov(Plant_Height_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_Height_divbio_AP)


#ANOVA: Height for SM
aov_Height_SM <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_Height_SM)
model.tables(aov_Height_SM, "means")

#divbio data
aov_Height_divbio_SM <- aov(Plant_Height_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_Height_divbio_SM)

# ANOVA: Height for AG
aov_Height_AG <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_Height_AG)
model.tables(aov_Height_AG, "means")

#divbio data
aov_Height_divbio_AG <- aov(Plant_Height_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov_Height_divbio_AG)

# ANOVA: Height for SN
aov_Height_SN <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_Height_SN)
model.tables(aov_Height_SN, "means")

#divbio data
aov_Height_divbio_SN <- aov(Plant_Height_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov_Height_divbio_SN)

### Conclusions: ANOVA for Plant height ###
# DO: 4.33 0.0451 *
# AP: 4.038 0.0527
# SM: 0.541  0.467
# AG: 0.541  0.467
# SN: 5.851 0.0211 *

### Conclusions: ANOVA for Plant height divided by biomass ###
# DO: 13.04 0.00097 * 
# AP: 2.116  0.155
# SM: 1.126  0.297
# AG: 2.281   0.14
# SN: 6.554 0.0151 *




# Mixed model ANOVA for Plant Area

# ANOVA: Plant Area for DO
aov_Area_DO <- aov(Plant_Area~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_Area_DO)
model.tables(aov_Area_DO, "means")
colnames(DO_N0N10_Traits)

#divbio data 
aov_Area_divbio_DO <- aov(Plant_Area_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov_Area_divbio_DO)

# ANOVA: Plant Area for AP
aov_Area_AP <- aov(Plant_Area~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_Area_AP)
model.tables(aov_Area_AP, "means")

#divbio data 
aov_Area_divbio_AP <- aov(Plant_Area_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_Area_divbio_AP)

# ANOVA: Plant Area for SM
aov_Area_SM <- aov(Plant_Area~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_Area_SM)
model.tables(aov_Area_SM, "means")

#divbio data 
aov_Area_divbio_SM <- aov(Plant_Area_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_Area_divbio_SM)

# ANOVA: Plant Area for AG
aov_Area_AG <- aov(Plant_Area~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_Area_AG)
model.tables(aov_Area_AG, "means")

#divbio data 
aov_Area_divbio_AG <- aov(Plant_Area_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov_Area_divbio_AG)

# ANOVA: Plant Area for SN
aov_Area_SN <- aov(Plant_Area~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_Area_SN)
model.tables(aov_Area_SN, "means")

#divbio data 
aov_Area_divbio_SN <- aov(Plant_Area_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov_Area_divbio_SN)

### Conclusions: ANOVA for Plant Area ###
# DO: 11.39 0.00186 *
# AP: 6.068 0.0191 *
# SM: 0.512   0.48
# AG: 0.292  0.592
# SN: 2.314  0.137


### Conclusions: ANOVA for Plant Area div Biomass ###
# DO: 0.017  0.898, not sig      DIFFERS
# AP: 3.954 0.0551, not quite sig, DIFFERS
# SM: 2.11  0.157
# AG: 1.982  0.168
# SN: 1.24  0.273



# Mixed model ANOVA for Focal Root Weight

# ANOVA: Root Weight for DO
aov_RootWt_DO <- aov(Focal_Root_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_RootWt_DO)
model.tables(aov_RootWt_DO, "means")

# ANOVA: Root Weight for AP
aov_RootWt_AP <- aov(Focal_Root_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_RootWt_AP)
model.tables(aov_RootWt_AP, "means")

# ANOVA: Root Weight for SM
aov_RootWt_SM <- aov(Focal_Root_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_RootWt_SM)
model.tables(aov_RootWt_SM, "means")

# ANOVA: Root Weight for AG
aov_RootWt_AG <- aov(Focal_Root_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_RootWt_AG)
model.tables(aov_RootWt_AG, "means")

# ANOVA: Root Weight for SN
aov_RootWt_SN <- aov(Focal_Root_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_RootWt_SN)
model.tables(aov_RootWt_SN, "means")

### Conclusions: ANOVA for Root Weight ###
# DO: 2.981 0.0933 
# AP: 0.966  0.333
# SM: 0.006  0.937
# AG: 0.583   0.45
# SN: 0.341  0.563



# Mixed model ANOVA for Specific Root Length

# ANOVA: SRL for DO
aov_SRL_DO <- aov(SRL~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov_SRL_DO)
model.tables(aov_SRL_DO, "means")

# divbio data
aov_SRL_divbio_DO <- aov(SRL_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov_SRL_divbio_DO)

# ANOVA: SRL for AP
aov_SRL_AP <- aov(SRL~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov_SRL_AP)
model.tables(aov_SRL_AP, "means")

# divbio data
aov_SRL_divbio_AP <- aov(SRL_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_SRL_divbio_AP)

# ANOVA: SRL for SM
aov_SRL_SM <- aov(SRL~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_SRL_SM)
model.tables(aov_SRL_SM, "means")

# divbio data
aov_SRL_divbio_SM <- aov(SRL_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_SRL_divbio_SM)

# ANOVA: SRL for AG
aov_SRL_AG <- aov(SRL~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov_SRL_AG)
model.tables(aov_SRL_AG, "means")

# divbio data
aov_SRL_divbio_AG <- aov(SRL_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov_SRL_divbio_AG)

# ANOVA: SRL for SN
aov_SRL_SN <- aov(SRL~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov_SRL_SN)
model.tables(aov_SRL_SN, "means")

# divbio data
aov_SRL_divbio_SN <- aov(SRL_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov_SRL_divbio_SN)

### Conclusions: ANOVA for SRL ###
# DO:  0.689  0.412
# AP: 1.426  0.241
# SM: 0.203  0.656
# AG: 0.843  0.365
# SN: 0.280  0.600

### Conclusions: ANOVA for SRL div Root biomass ###
# DO: 0.167  0.686
# AP: 0.04  0.842
# SM: 0.615   0.44
# AG: 0.391  0.536
# SN: 0.062  0.805






# Mixed model ANOVA for Leaf_Thickness_cm
# ANOVA: SRL for DO
aov <- aov(Leaf_Thickness_cm~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

# divbio data
aov_LeafTh_divbio_DO <- aov(Leaf_Thickness_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov_LeafTh_divbio_DO)

# ANOVA: SRL for AP
aov <- aov(Leaf_Thickness_cm~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

# divbio data
aov_LeafTh_divbio_AP <- aov(Leaf_Thickness_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov_LeafTh_divbio_AP)

# ANOVA: SRL for SM
aov <- aov(Leaf_Thickness_cm~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

# divbio data
aov_LeafTh_divbio_SM <- aov(Leaf_Thickness_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov_LeafTh_divbio_SM)


# ANOVA: Leaf_Thickness_cm for AG
aov <- aov(Leaf_Thickness_cm~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov_LeafTh_divbio_AG <- aov(Leaf_Thickness_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov_LeafTh_divbio_AG)

# ANOVA: SRL for SN
aov <- aov(Leaf_Thickness_cm~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov_LeafTh_divbio_SN <- aov(Leaf_Thickness_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov_LeafTh_divbio_SN)

## Conclusions for ANOVAS for Leaf Thickness N0N10 ##
# DO: 0.689  0.412
#AP: 4.048 0.0522
#SM: 3.627 0.0668
#AG: 0.157  0.695
#SN: 4.048 0.0522

## Conclusions for ANOVAS for Leaf Thickness div by Aboveground biomass##

#DO: 10.15 0.003  *,    DIFFERS
#AP: 1.502  0.229
#SM: 1.459  0.237
#AG: 1.023  0.319
#SN: 6.583 0.0149  * DIFFERs





# Mixed model ANOVA for Root_Diam_Max
# ANOVA: 
aov <- aov(Root_Diam_Max~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Max_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Max~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Max_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Max~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Max_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Max~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Max_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Max~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Max_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

# Conclusions Root diam max ANOVAS 
# DO: 1.981  0.169
# AP: 0  0.993
# SM: 0.024  0.878
# AG: 1.97   0.17
# SN: 0.021  0.886

# Conclusions Root diam max div by root biomass
# DO: 0.043  0.836
# AP: 0.602  0.443
# SM: 0.176  0.678
# AG: 0.629  0.434
# SN: 0.323  0.573




# Mixed model ANOVA for Root_Diam_Mean
# ANOVA: 
aov <- aov(Root_Diam_Mean~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Mean_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Mean~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Mean_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Mean~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Mean_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Mean~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Mean_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Root_Diam_Mean~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Root_Diam_Mean_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

# Conclusions Mean Root diameter
# DO: 0.021  0.886
# AP: 1.132  0.295
# SM: 0.154  0.698
# AG: 4.155 0.0496
# SN: 0.502  0.483

# Conclusions Mean Root diameter divided by root biomass
# DO: 0.011  0.919
# AP: 2.089  0.158
# SM: 0.2  0.658
# AG: 1.247  0.272
# SN: 0.757   0.39






# Mixed model ANOVA for Percent_N_AbG
# ANOVA: 
aov <- aov(Percent_N_AbG~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_AbG~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_AbG~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_AbG~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)


# ANOVA: 
aov <- aov(Percent_N_AbG~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions %N Leaf 
# DO: 52.63 2.13e-08 *
# AP: 10.97 0.00225 *
# SM: 3.332 0.0782 
# AG: 1.61  0.213
# SN: 14.87 0.000489 *

## Conclusions %N Leaf div Biomass
# DO: 5.112 0.0303 *
# AP: 1.726  0.198  not significant        DIFFERS
# SM: 3.882 0.0584
# AG: 1.382  0.248
# SN: 5.222 0.0287 *




# Mixed model ANOVA for Percent_C_AbG
# ANOVA: 
aov <- aov(Percent_C_AbG~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_AbG~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_AbG~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_AbG~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_AbG~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_AbG_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions %C Leaf 
# DO: 11.04 0.00214 *
# AP: 2.478  0.125
# SM: 6.836  0.014 *
# AG: 0.576  0.453
# SN: 2.227  0.145

## Conclusions %C Leaf div biomass
# DO: 12.25 0.00132 **
# AP: 2.252  0.143    
# SM: 1.882  0.181  not signficant-  DIFFERS
# AG: 1.99  0.167
# SN: 6.245 0.0174 * significant- DIFFERS




# Mixed model ANOVA for Total BeG Weight
# ANOVA: 
aov <- aov(Total_Belowground_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Total_Belowground_Weight_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Total_Belowground_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Total_Belowground_Weight_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Total_Belowground_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Total_Belowground_Weight_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Total_Belowground_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Total_Belowground_Weight_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Total_Belowground_Weight_g~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Total_Belowground_Weight_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions Total Belowground  
# DO: 1.218  0.277
# AP: 0.37  0.547
# SM: 1.787  0.192
# AG: 0.428  0.518
# SN: 1.28  0.266

## Conclusions Total Belowground div Biomass 
# DO: 2.764  0.106
# AP: 1.239  0.274
# SM: 0.352  0.558
# AG: 1.294  0.264
# SN: 0.004   0.95





# Mixed model ANOVA for Percent_N_BeG
# ANOVA: 
aov <- aov(Percent_N_BeG~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_BeG~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_BeG~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_BeG~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_N_BeG~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_N_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions %N Root 
# DO: 13.4 0.000848 *
# AP: 38.56 5.21e-07 *
# SM: 6.93 0.0136 *
# AG: 25.49 1.49e-05 *
# SN: 39.72 3.5e-07 *

## Conclusions %N Root div Root Biomass
# DO: 1.134  0.294  DIFFERS
# AP: 2.222  0.146  DIFFERS
# SM: 1.24  0.275   DIFFERS
# AG: 3.50 0.0703   DIFFERS
# SN: 8.039 0.00765 **



# Mixed model ANOVA for E_Leaf_Stage
# ANOVA: 
aov <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(E_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(E_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(E_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(E_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(E_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(E_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions Emerging Leaf number 
# DO: 11.14 0.00205 *
# AP: 7.794 0.00865 *
# SM: 0.148  0.703
# AG: 0.445  0.509
# SN: 0.736  0.397

## Conclusions Emerging Leaf number div biomass
# DO: 0.197   0.66    DIFFERS
# AP: 0.054  0.818    DIFFERS
# SM: 4.128 0.0514
# AG: 0.002  0.963
# SN: 3.29 0.0786


# Mixed model ANOVA for S_Leaf_Stage
# ANOVA: 
aov <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(S_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(S_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(S_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(S_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(S_Leaf_Stage~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(S_Leaf_Stage_div_AbG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions Senescing Leaf number 
# DO: 2.314  0.137
# AP: 1.988  0.168
# SM: 1.195  0.283
# AG: 2.316  0.137
# SN: 0.072  0.79

## Conclusions Senescing Leaf number div biomss
# DO: 0.956  0.335
# AP: 0.963  0.334
# SM: 6.39 0.0172 *   DIFFERS
# AG: 0.65  0.426
# SN: 7.099 0.0117 *  DIFFERS




# Mixed model ANOVA for Percent_C_BeG
# ANOVA: 
aov <- aov(Percent_C_BeG~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=DO_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_BeG~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AP_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_BeG~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_BeG~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=AG_N0N10_Traits_DivBio)
summary(aov)

# ANOVA: 
aov <- aov(Percent_C_BeG~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits)
summary(aov)
model.tables(aov, "means")

#divbio
aov <- aov(Percent_C_BeG_div_BeG_Biomass~Treatment+(1/Experiment)+(1/Plot), data=SN_N0N10_Traits_DivBio)
summary(aov)

## Conclusions Root %C 
# DO: 0.049  0.826
# AP: 2.13  0.154
# SM: 0.02  0.889
# AG: 1.773  0.192
# SN: 0.019  0.891

## Conclusions Root %C div root biomss
# DO: 0.003  0.957
# AP: 0.374  0.545
# SM: 0.263  0.612
# AG: 0.001  0.979
# SN: 0.16  0.692





################################ Change ############################################

# ANOVAS and F-values for Change

#SLA
reg2 <- lm(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(SLA~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SLA~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

#ap
reg2 <- lm(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(SLA~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SLA~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- lm(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(SLA~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SLA~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg10 <- aov(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SLA~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(SLA~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg10 <- aov(SLA~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SLA~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# Leaf_Thickness_cm
reg2 <- lm(Leaf_Thickness_cm~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg10 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Leaf_Thickness_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Leaf_Thickness_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# AG
reg2 <- aov(Leaf_Thickness_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

#sn
reg2 <- lm(Leaf_Thickness_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Thickness_cm~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Aboveground_Dry_Weight_g
reg2 <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



reg2 <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



reg2 <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Aboveground_Dry_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# Percent_C_BeG
reg2 <- lm(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg10 <- aov(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Percent_C_BeG~Treatment+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_BeG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# Percent_N_BeG
reg2 <- aov(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- lm(Percent_N_BeG~Treatment+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- lm(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits, Treatment2=="0"|Treatment2=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- lm(Percent_N_BeG~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_BeG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# Leaf_Number
reg2 <- aov(Leaf_Number~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

#ap
reg2 <- aov(Leaf_Number~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Leaf_Number~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- lm(Leaf_Number~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Number~Treatment2+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Leaf_Number~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg10 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Leaf_Number~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# F_Leaf_Stage

reg2 <- aov(F_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
TukeyHSD(reg2)

model.tables(reg2, "means")

reg2 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(F_Leaf_Stage~Treatment2+(1/Plot), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(F_Leaf_Stage~Treatment2+(1/Plot), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(F_Leaf_Stage~Treatment2+(1/Plot), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



reg2 <- aov(F_Leaf_Stage~Treatment2+(1/Plot), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(F_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# E_Leaf_Stage

reg2 <- lm(E_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



reg2 <- aov(E_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(E_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- aov(E_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- lm(E_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(E_Leaf_Stage~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# S_Leaf_Stage

reg2 <- aov(S_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
TukeyHSD(reg2)
summary(reg2)
model.tables(reg2, "means")


reg2 <- lm(S_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- lm(S_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- lm(S_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(S_Leaf_Stage~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")



# Plant_Height_cm
reg2 <- lm(Plant_Height_cm~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



reg2 <- aov(Plant_Height_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Plant_Height_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Plant_Height_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- lm(Plant_Height_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Height_cm~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Plant_Area

reg2 <- aov(Plant_Area_cm~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Plant_Area_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Plant_Area_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Plant_Area_cm~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Plant_Area_cm~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Plant_Area~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Focal_Root_Weight_g
reg2 <- aov(Focal_Root_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Focal_Root_Weight_g~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Focal_Root_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Total_Belowground_Weight_g
reg2 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Total_Belowground_Weight_g~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# SRL
reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(SRL~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SRL~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(SRL~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SRL~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(SRL~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SRL~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(SRL~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SRL~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(SRL~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(SRL~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(SRL~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Root_Diam_Max
reg2 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Max~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Root_Diam_Mean
reg2 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Root_Diam_Mean~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


# Percent_N_AbG
reg2 <- lm(Percent_N_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Percent_N_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg2 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Percent_N_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Percent_N_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Percent_N_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_N_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)



# Percent_C_AbG
reg2 <- lm(Percent_C_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(DO_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")


reg10 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(DO_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Percent_C_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(AP_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AP_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- lm(Percent_C_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(SM_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SM_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)


reg2 <- aov(Percent_C_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(AG_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(AG_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)

reg2 <- aov(Percent_C_AbG~Treatment2+(1/Plot)+(1/Block), data = subset(SN_Ch_Traits))
summary(reg2)
model.tables(reg2, "means")

reg2 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="2.5"))
summary(reg2)
reg10 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg10)
reg20 <- aov(Percent_C_AbG~Treatment+(1/Plot), data = subset(SN_Ch_Traits, Treatment=="Control"|Treatment=="20"))
summary(reg20)






#Make Table

library(gt)
library(tidyverse)
library(glue)
install.packages("gt")

install.packages("tidyverse")
install.packages("glue")
library(gt)
library(tidyverse)
library(glue)
library(dplyr)
library(kableExtra)

install.packages("kableExtra")



# load data for ANOVA Tables
library(readr)
ANOVA_N0N10_TableData <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/ANOVA_TableData_N0N10.xlsx - N0N10.csv")

ANOVA_Change_TableData <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/ANOVA_TableData_CHANGE.xlsx - CHANGE.csv")



ANOVA_N0N10_TableData %>%
  kbl() %>%
  kable_styling()
  
ANOVA_N0N10_TableData %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")



install.packages('pander')
library(pander)
library(Hmisc)
install.packages("base")
library(base)

# compute matrix correlation
df3 <- rcorr(as.matrix(mtcars), type="pearson")

# we get a list of three items, first item is df of r values
# third item in df of p values
df3

# make cells of r values bold if p value is <0.01
emphasize.strong.cells(which(ANOVA_N0N10_TableData < 0.05, arr.ind = TRUE))


pander(ANOVA_N0N10_TableData) 

ANOVA_N0N10_TableData%>%
  kbl() %>%
  kable_styling()

ANOVA_N0N10_TableData %>%
  kbl(caption = "ANOVA Mixed Model Results for Trait Analysis, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#try this function 
format_cells <- function(df, rows ,cols, value = c("italics", "bold", "strikethrough")){
  
  # select the correct markup
  # one * for italics, two ** for bold
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      df[[c]] <- as.character( df[[c]])
      
      # Update formatting
      df[r, c] <- paste0(markup, df[r, c], markup)
    }
  }
  
  return(df)
}



save <- ANOVA_N0N10_TableData %>%
  format_cells(2, 2, "bold") %>%
  format_cells(3, 1:2, "strikethrough") %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = T, html_font = "Cambria")






#REDO Table 2

ANOVA_N0N10_TableData_AbG <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/ANOVA_Table_N0N10_AbG.xlsx - N0N10.csv")
ANOVA_N0N10_TableData_BeG <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/ANOVA_Table_N0N10_BeG.xlsx - N0N10.csv")


ANOVA_Change_TableData <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/ANOVA_TableData_CHANGE.xlsx - CHANGE.csv")




ANOVA_CHANGE_TableData_AbG <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/Change/ANOVA_Table_CHANGE_AbG.xlsx - CHANGE.csv")
ANOVA_CHANGE_TableData_BeG <- read_csv("~/Desktop/Thesis_Data/ANOVA_Tables/Change/ANOVA_Table_CHANGE_BeG.xlsx - CHANGE.csv")



ANOVA_N0N10_TableData_AbG %>%
  kbl() %>%
  kable_styling()

ANOVA_N0N10_TableData_AbG %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")



ANOVA_N0N10_TableData_BeG %>%
  kbl() %>%
  kable_styling()

ANOVA_N0N10_TableData_BeG %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")




###### CHANGE ######
ANOVA_CHANGE_TableData_AbG %>%
  kbl() %>%
  kable_styling()

ANOVA_CHANGE_TableData_AbG %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")


ANOVA_CHANGE_TableData_BeG %>%
  kbl() %>%
  kable_styling()

ANOVA_CHANGE_TableData_BeG %>%
  kbl(caption = "ANOVA Mixed Model Results, F-Statistic (p-value)") %>%
  kable_classic(full_width = F, html_font = "Cambria")

colnames(DO_Traits)



##### MC Means Data Analysis ######## 

# need MC data
# need 

#ANOVA: MC for DO, only N0N10 data
aov_Height_SM <- aov(Plant_Height_cm~Treatment+(1/Experiment)+(1/Plot), data=SM_N0N10_Traits)
summary(aov_Height_SM)
model.tables(aov_Height_SM, "means")



