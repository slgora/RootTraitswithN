# DivBio change
# 



#load Data
library(readr)
All_Traits <- read_csv("All_Traits_Datasheet.csv")
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet.csv")
View(All_Traits)

#had to rename column 
names(All_Traits)[names(All_Traits) == "Plant_Area_cm^2"] <- "Plant_Area"
colnames(All_Traits)


### removed NA data 
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet_rmvNA.csv")
View(All_Traits)

#Make sure Plant ID is a factor
All_Traits$ID <- factor(All_Traits$ID)

#Make sure Treatment2 is a factor
All_Traits$Treatment2 <- factor(All_Traits$Treatment2)

All_Traits_DivBio$Treatment2 <- factor(All_Traits_DivBio$Treatment2)


##### November 2022
# load NEW datasheet with columns of traits divided by Biomass
All_Traits_DivBio <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet_DivBio_RmvNA.csv")
View(All_Traits_DivBio)

#subset by Change 
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

#subset by Change 
#### Filter out Change for each species DivBio data
DO_Ch_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")
AP_Ch_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Experiment=="Change")
SM_Ch_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Experiment=="Change")
SN_Ch_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Experiment=="Change")
AG_Ch_Traits_DivBio <- All_Traits_DivBio %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Experiment=="Change")






# DO 
# run linear model 
reg <- lm(SLA~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg)

reg_divbio <- lm(SLA_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(SLA~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg)
reg_divbio <- lm(SLA_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(SLA~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg)
reg_divbio <- lm(SLA_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(SLA~Treatment2+(1/Block), data = AG_Ch_Traits)
summary(reg)
reg_divbio <- lm(SLA_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(SLA~Treatment2+(1/Block), data = SN_Ch_Traits)
summary(reg)
reg_divbio <- lm(SLA_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)
# p=0.0188 ** DIFFERS


#  Leaf thickness
# run linear model and again using the divbio traits
reg <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Thickness_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Thickness_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Thickness_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = AG_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Thickness_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = SN_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Thickness_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)
# p=0.017 DIFFERs


# Leaf number
# run linear model and again using the divbio traits
reg <- lm(Leaf_Number~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Number~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
# p is not sig, DIFFERS

reg <- lm(Leaf_Number~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)

reg <- lm(Leaf_Number~Treatment2+(1/Block), data = AG_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
# p is not sig, DIFFERS

reg <- lm(Leaf_Number~Treatment2+(1/Block), data = SN_Ch_Traits)
summary(reg)
reg_divbio <- lm(Leaf_Number_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)
# p is sig, DIFFERS


## Percent_N_AbG
reg_divbio <- lm(Percent_N_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Percent_C_AbG_div_AbG_Biomass
reg_divbio <- lm(Percent_C_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_AbG_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## F_Leaf_Stage_div_AbG_Biomass
reg_divbio <- lm(F_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(F_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(F_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(F_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(F_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)


## E_Leaf_Stage_div_AbG_Biomass
reg_divbio <- lm(E_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(E_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(E_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(E_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(E_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## S_Leaf_Stage_div_AbG_Biomass
reg_divbio <- lm(S_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(S_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(S_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(S_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(S_Leaf_Stage_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)


## Plant_Height_div_AbG_Biomass
reg_divbio <- lm(Plant_Height_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Height_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Height_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Height_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Height_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Plant_Area_div_AbG_Biomass
reg_divbio <- lm(Plant_Area_div_AbG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Area_div_AbG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Area_div_AbG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Area_div_AbG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Plant_Area_div_AbG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)


## Root_Diam_Max_div_BeG_Biomass
reg_divbio <- lm(Root_Diam_Max_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Max_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Max_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Max_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Max_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Root_Diam_Mean_div_BeG_Biomass
reg_divbio <- lm(Root_Diam_Mean_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Mean_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Mean_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Mean_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Root_Diam_Mean_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Total_Belowground_Weight_div_BeG_Biomass
reg_divbio <- lm(Total_Belowground_Weight_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Total_Belowground_Weight_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Total_Belowground_Weight_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Total_Belowground_Weight_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Total_Belowground_Weight_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Percent_N_BeG_div_BeG_Biomass
reg_divbio <- lm(Percent_N_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_N_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## Percent_C_BeG_div_BeG_Biomass
reg_divbio <- lm(Percent_C_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(Percent_C_BeG_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)

## SRL_div_BeG_Biomass
reg_divbio <- lm(SRL_div_BeG_Biomass~Treatment2+(1/Block), data = DO_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(SRL_div_BeG_Biomass~Treatment2+(1/Block), data = AP_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(SRL_div_BeG_Biomass~Treatment2+(1/Block), data = SM_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(SRL_div_BeG_Biomass~Treatment2+(1/Block), data = AG_Ch_Traits_DivBio)
summary(reg_divbio)
reg_divbio <- lm(SRL_div_BeG_Biomass~Treatment2+(1/Block), data = SN_Ch_Traits_DivBio)
summary(reg_divbio)


