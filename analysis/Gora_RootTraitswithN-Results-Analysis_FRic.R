#### RootTraitswithN Results Analysis-FRic ####
###  Gora_RootTraitswithN-Results-Analysis_FRic.R
###  by Sarah Gora
###  Date created: March 28, 2021

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

#load packages 
install.packages("FD")
library(FD)
install.packages("fundiversity")
library(fundiversity)
install.packages("tidyr")   
library("tidyr")
library(readxl)



############## FRic- Functional Richness ###############
# computed as volume of the convex hull from all included traits
# builds on a PCA

# Build data.frame with two columns:
# site the names of the sites as the row names of the input sp_com
# FRic the values of functional richness at each site



# Load data
# TRAIT DROP MODEL
All_Traits_O_DroppedTraits <- read_excel("~/Desktop/All_Traits_O_DroppedTraits.xlsx")
View(All_Traits_O_DroppedTraits) 

# Subset out TRAIT DROP model, data by species 
DO_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Dichanthelium_oligosanthes")
AP_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Ambrosia_psilostachya")
SM_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Solidago_missouriensis")
AG_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Andropogon_gerardii")
SN_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Sorghastrum_nutans")


# Subset out TRAIT DROP model, data by species, by N0 treatment
DO_TraitsDrop_N0 <- subset(DO_Traits_O_Drop, Treatment2=="0")
AP_TraitsDrop_N0 <- subset(AP_Traits_O_Drop, Treatment2=="0")
SM_TraitsDrop_N0 <- subset(SM_Traits_O_Drop, Treatment2=="0")
AG_TraitsDrop_N0 <- subset(AG_Traits_O_Drop, Treatment2=="0")
SN_TraitsDrop_N0 <- subset(SN_Traits_O_Drop, Treatment2=="0")

# Subset out TRAIT DROP model, data by species, by N2.5 treatment
DO_TraitsDrop_N2 <- subset(DO_Traits_O_Drop, Treatment2=="2.5")
AP_TraitsDrop_N2 <- subset(AP_Traits_O_Drop, Treatment2=="2.5")
SM_TraitsDrop_N2 <- subset(SM_Traits_O_Drop, Treatment2=="2.5")
AG_TraitsDrop_N2 <- subset(AG_Traits_O_Drop, Treatment2=="2.5")
SN_TraitsDrop_N2 <- subset(SN_Traits_O_Drop, Treatment2=="2.5")

# Subset out TRAIT DROP model, data by species, by N10 treatment
DO_TraitsDrop_N10 <- subset(DO_Traits_O_Drop, Treatment2=="10")
AP_TraitsDrop_N10 <- subset(AP_Traits_O_Drop, Treatment2=="10")
SM_TraitsDrop_N10 <- subset(SM_Traits_O_Drop, Treatment2=="10")
AG_TraitsDrop_N10 <- subset(AG_Traits_O_Drop, Treatment2=="10")
SN_TraitsDrop_N10 <- subset(SN_Traits_O_Drop, Treatment2=="10")

# Subset out TRAIT DROP model, data by species, by N20 treatment
DO_TraitsDrop_N20 <- subset(DO_Traits_O_Drop, Treatment2=="20")
AP_TraitsDrop_N20 <- subset(AP_Traits_O_Drop, Treatment2=="20")
SM_TraitsDrop_N20 <- subset(SM_Traits_O_Drop, Treatment2=="20")
AG_TraitsDrop_N20 <- subset(AG_Traits_O_Drop, Treatment2=="20")
SN_TraitsDrop_N20 <- subset(SN_Traits_O_Drop, Treatment2=="20")



 
   
# Rip out only the TRAIT columns I want from my species, treatment data frames 

# N0
Rip_DO_TraitsDrop_N0 <- DO_TraitsDrop_N0 %>% select(c(10:20))
Rip_AP_TraitsDrop_N0 <- AP_TraitsDrop_N0 %>% select(c(10:20))
Rip_SM_TraitsDrop_N0 <- SM_TraitsDrop_N0 %>% select(c(10:20))
Rip_AG_TraitsDrop_N0 <- AG_TraitsDrop_N0 %>% select(c(10:20))
Rip_SN_TraitsDrop_N0 <- SN_TraitsDrop_N0 %>% select(c(10:20))

# N2.5
Rip_DO_TraitsDrop_N2 <- DO_TraitsDrop_N2 %>% select(c(10:20))
Rip_AP_TraitsDrop_N2 <- AP_TraitsDrop_N2 %>% select(c(10:20))
Rip_SM_TraitsDrop_N2 <- SM_TraitsDrop_N2 %>% select(c(10:20))
Rip_AG_TraitsDrop_N2 <- AG_TraitsDrop_N2 %>% select(c(10:20))
Rip_SN_TraitsDrop_N2 <- SN_TraitsDrop_N2 %>% select(c(10:20))

# N10
Rip_DO_TraitsDrop_N10 <- DO_TraitsDrop_N10 %>% select(c(10:20))
Rip_AP_TraitsDrop_N10 <- AP_TraitsDrop_N10 %>% select(c(10:20))
Rip_SM_TraitsDrop_N10 <- SM_TraitsDrop_N10 %>% select(c(10:20))
Rip_AG_TraitsDrop_N10 <- AG_TraitsDrop_N10 %>% select(c(10:20))
Rip_SN_TraitsDrop_N10 <- SN_TraitsDrop_N10 %>% select(c(10:20))

# N20
Rip_DO_TraitsDrop_N20 <- DO_TraitsDrop_N20 %>% select(c(10:20))
Rip_AP_TraitsDrop_N20 <- AP_TraitsDrop_N20 %>% select(c(10:20))
Rip_SM_TraitsDrop_N20 <- SM_TraitsDrop_N20 %>% select(c(10:20))
Rip_AG_TraitsDrop_N20 <- AG_TraitsDrop_N20 %>% select(c(10:20))
Rip_SN_TraitsDrop_N20 <- SN_TraitsDrop_N20 %>% select(c(10:20))


######################## DO #########################################

#### DO, N0
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N0_scale <- scale(Rip_DO_TraitsDrop_N0)

# put into a matrix 
Rip_DO_TraitsDrop_N0_matrix <- data.matrix(Rip_DO_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1 0.005394534

#### DO, N2
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N2_scale <- scale(Rip_DO_TraitsDrop_N2)

# put into a matrix 
Rip_DO_TraitsDrop_N2_matrix <- data.matrix(Rip_DO_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N2_matrix, stand = TRUE)
# site        FRic
#   s1        NA
# really low??



#### DO, N10
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N10_scale <- scale(Rip_DO_TraitsDrop_N10)

# put into a matrix 
Rip_DO_TraitsDrop_N10_matrix <- data.matrix(Rip_DO_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.03755279


#### DO, N20
# scale traits to put into a matrix

Rip_DO_TraitsDrop_N20 <- Rip_DO_TraitsDrop_N20 %>% drop_na()

Rip_DO_TraitsDrop_N20_scale <- scale(Rip_DO_TraitsDrop_N20)

# put into a matrix 
Rip_DO_TraitsDrop_N20_matrix <- data.matrix(Rip_DO_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N20_matrix, stand = TRUE)
# site        FRic
#   s1        NA





######################## AP #########################################

#### AP, N0
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N0_scale <- scale(Rip_AP_TraitsDrop_N0)

# put into a matrix 
Rip_AP_TraitsDrop_N0_matrix <- data.matrix(Rip_AP_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       0.02658714

#### AP, N2
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N2_scale <- scale(Rip_AP_TraitsDrop_N2)

# put into a matrix 
Rip_AP_TraitsDrop_N2_matrix <- data.matrix(Rip_AP_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### AP, N10
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N10_scale <- scale(Rip_AP_TraitsDrop_N10)

# put into a matrix 
Rip_AP_TraitsDrop_N10_matrix <- data.matrix(Rip_AP_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.01809333


#### AP, N20
# scale traits to put into a matrix

Rip_AP_TraitsDrop_N20 <- Rip_AP_TraitsDrop_N20 %>% drop_na()

Rip_AP_TraitsDrop_N20_scale <- scale(Rip_AP_TraitsDrop_N20)

# put into a matrix 
Rip_AP_TraitsDrop_N20_matrix <- data.matrix(Rip_AP_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA




######################## SM #########################################

#### SM, N0
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N0_scale <- scale(Rip_SM_TraitsDrop_N0)

# put into a matrix 
Rip_SM_TraitsDrop_N0_matrix <- data.matrix(Rip_SM_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       0.003658497

#### SM, N2
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N2_scale <- scale(Rip_SM_TraitsDrop_N2)

# put into a matrix 
Rip_SM_TraitsDrop_N2_matrix <- data.matrix(Rip_SM_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### SM, N10
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N10_scale <- scale(Rip_SM_TraitsDrop_N10)

# put into a matrix 
Rip_SM_TraitsDrop_N10_matrix <- data.matrix(Rip_SM_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.005047735


#### SM, N20
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N20_scale <- scale(Rip_SM_TraitsDrop_N20)

# put into a matrix 
Rip_SM_TraitsDrop_N20_matrix <- data.matrix(Rip_SM_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA


######################## AG #########################################

#### AG, N0
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N0_scale <- scale(Rip_AG_TraitsDrop_N0)

# put into a matrix 
Rip_AG_TraitsDrop_N0_matrix <- data.matrix(Rip_AG_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       0.02682882

#### SM, N2
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N2_scale <- scale(Rip_AG_TraitsDrop_N2)

# put into a matrix 
Rip_AG_TraitsDrop_N2_matrix <- data.matrix(Rip_AG_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### AG, N10
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N10_scale <- scale(Rip_AG_TraitsDrop_N10)

# put into a matrix 
Rip_AG_TraitsDrop_N10_matrix <- data.matrix(Rip_AG_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.006366802


#### AG, N20
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N20_scale <- scale(Rip_AG_TraitsDrop_N20)

# put into a matrix 
Rip_AG_TraitsDrop_N20_matrix <- data.matrix(Rip_AG_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA


######################## SN #########################################

#### SN, N0
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N0_scale <- scale(Rip_SN_TraitsDrop_N0)

# put into a matrix 
Rip_SN_TraitsDrop_N0_matrix <- data.matrix(Rip_SN_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       .01857858

#### SN, N2
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N2_scale <- scale(Rip_SN_TraitsDrop_N2)

# put into a matrix 
Rip_SN_TraitsDrop_N2_matrix <- data.matrix(Rip_SN_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### SN, N10
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N10_scale <- scale(Rip_SN_TraitsDrop_N10)

# put into a matrix 
Rip_SN_TraitsDrop_N10_matrix <- data.matrix(Rip_SN_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.07541702


#### SN, N20
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N20_scale <- scale(Rip_SN_TraitsDrop_N20)

# put into a matrix 
Rip_SN_TraitsDrop_N20_matrix <- data.matrix(Rip_SN_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N20_matrix2, stand = FALSE)
# site        FRic
#   s1        NA






###################### 6 Traits ##########################

############## Can only use 6 traits for a FRic

# Load data
# TRAIT DROP MODEL
All_Traits_O_DroppedTraits <- read_excel("~/Desktop/All_Traits_O_DroppedTraits.xlsx")
View(All_Traits_O_DroppedTraits) 

# Subset out TRAIT DROP model, data by species 
DO_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Dichanthelium_oligosanthes")
AP_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Ambrosia_psilostachya")
SM_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Solidago_missouriensis")
AG_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Andropogon_gerardii")
SN_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Sorghastrum_nutans")


# Subset out TRAIT DROP model, data by species, by N0 treatment
DO_TraitsDrop_N0 <- subset(DO_Traits_O_Drop, Treatment2=="0")
AP_TraitsDrop_N0 <- subset(AP_Traits_O_Drop, Treatment2=="0")
SM_TraitsDrop_N0 <- subset(SM_Traits_O_Drop, Treatment2=="0")
AG_TraitsDrop_N0 <- subset(AG_Traits_O_Drop, Treatment2=="0")
SN_TraitsDrop_N0 <- subset(SN_Traits_O_Drop, Treatment2=="0")

# Subset out TRAIT DROP model, data by species, by N2.5 treatment
DO_TraitsDrop_N2 <- subset(DO_Traits_O_Drop, Treatment2=="2.5")
AP_TraitsDrop_N2 <- subset(AP_Traits_O_Drop, Treatment2=="2.5")
SM_TraitsDrop_N2 <- subset(SM_Traits_O_Drop, Treatment2=="2.5")
AG_TraitsDrop_N2 <- subset(AG_Traits_O_Drop, Treatment2=="2.5")
SN_TraitsDrop_N2 <- subset(SN_Traits_O_Drop, Treatment2=="2.5")

# Subset out TRAIT DROP model, data by species, by N10 treatment
DO_TraitsDrop_N10 <- subset(DO_Traits_O_Drop, Treatment2=="10")
AP_TraitsDrop_N10 <- subset(AP_Traits_O_Drop, Treatment2=="10")
SM_TraitsDrop_N10 <- subset(SM_Traits_O_Drop, Treatment2=="10")
AG_TraitsDrop_N10 <- subset(AG_Traits_O_Drop, Treatment2=="10")
SN_TraitsDrop_N10 <- subset(SN_Traits_O_Drop, Treatment2=="10")

# Subset out TRAIT DROP model, data by species, by N20 treatment
DO_TraitsDrop_N20 <- subset(DO_Traits_O_Drop, Treatment2=="20")
AP_TraitsDrop_N20 <- subset(AP_Traits_O_Drop, Treatment2=="20")
SM_TraitsDrop_N20 <- subset(SM_Traits_O_Drop, Treatment2=="20")
AG_TraitsDrop_N20 <- subset(AG_Traits_O_Drop, Treatment2=="20")
SN_TraitsDrop_N20 <- subset(SN_Traits_O_Drop, Treatment2=="20")





# Rip out only the TRAIT columns I want from my species, treatment data frames 

# N0
Rip_DO_TraitsDrop_N0_2 <- DO_TraitsDrop_N0 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AP_TraitsDrop_N0_2 <- AP_TraitsDrop_N0 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SM_TraitsDrop_N0_2 <- SM_TraitsDrop_N0 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AG_TraitsDrop_N0_2 <- AG_TraitsDrop_N0 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SN_TraitsDrop_N0_2 <- SN_TraitsDrop_N0 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)

# N2.5
Rip_DO_TraitsDrop_N2_2 <- DO_TraitsDrop_N2 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AP_TraitsDrop_N2_2 <- AP_TraitsDrop_N2 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SM_TraitsDrop_N2_2 <- SM_TraitsDrop_N2 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AG_TraitsDrop_N2_2 <- AG_TraitsDrop_N2 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SN_TraitsDrop_N2_2 <- SN_TraitsDrop_N2 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)

# N10
Rip_DO_TraitsDrop_N10_2 <- DO_TraitsDrop_N10 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AP_TraitsDrop_N10_2 <- AP_TraitsDrop_N10 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SM_TraitsDrop_N10_2 <- SM_TraitsDrop_N10 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AG_TraitsDrop_N10_2 <- AG_TraitsDrop_N10 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SN_TraitsDrop_N10_2 <- SN_TraitsDrop_N10 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)

# N20
Rip_DO_TraitsDrop_N20_2 <- DO_TraitsDrop_N20 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AP_TraitsDrop_N20_2 <- AP_TraitsDrop_N20 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SM_TraitsDrop_N20_2 <- SM_TraitsDrop_N20 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_AG_TraitsDrop_N20_2 <- AG_TraitsDrop_N20 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)
Rip_SN_TraitsDrop_N20_2 <- SN_TraitsDrop_N20 %>% select(SLA, Aboveground_Dry_Weight_g, PercentN_AbG, SRL,Focal_Root_Weight_g, PercentN_BeG)


######################## DO #########################################

#### DO, N0
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N0_scale_2 <- scale(Rip_DO_TraitsDrop_N0_2)

# put into a matrix 
Rip_DO_TraitsDrop_N0_matrix_2 <- data.matrix(Rip_DO_TraitsDrop_N0_scale_2)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N0_matrix_2, stand = FALSE)
#             FRic    
# 11traits    0.005394534
# 6 traits    6.488623


#### DO, N2
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N2_scale_2 <- scale(Rip_DO_TraitsDrop_N2_2)

# put into a matrix 
Rip_DO_TraitsDrop_N2_matrix_2 <- data.matrix(Rip_DO_TraitsDrop_N2_scale_2)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N2_matrix_2, stand = FALSE)
#             FRic    
# 11traits    NA
# 6 traits    NA


#### DO, N10
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N10_scale_2 <- scale(Rip_DO_TraitsDrop_N10_2)

# put into a matrix 
Rip_DO_TraitsDrop_N10_matrix_2 <- data.matrix(Rip_DO_TraitsDrop_N10_scale_2)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N10_matrix_2, stand = FALSE)
#             FRic    
# 11traits    0.03755279
# 6 traits    5.445133


#### DO, N20
# scale traits to put into a matrix
Rip_DO_TraitsDrop_N20_scale_2 <- scale(Rip_DO_TraitsDrop_N20_2)

# put into a matrix 
Rip_DO_TraitsDrop_N20_matrix_2 <- data.matrix(Rip_DO_TraitsDrop_N20_scale_2)

# Run FRic
fd_fric(Rip_DO_TraitsDrop_N20_matrix_2, stand = FALSE)
#             FRic    
# 11traits    NA
# 6 traits    NA





######################## AP #########################################

#### AP, N0
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N0_scale_2 <- scale(Rip_AP_TraitsDrop_N0_2)

# put into a matrix 
Rip_AP_TraitsDrop_N0_matrix_2 <- data.matrix(Rip_AP_TraitsDrop_N0_scale_2)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N0_matrix_2, stand = FALSE)
#             FRic
# 11 traits   0.02658714
# 6 traits    5.88579

#### AP, N2
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N2_scale_2 <- scale(Rip_AP_TraitsDrop_N2_2)

# put into a matrix 
Rip_AP_TraitsDrop_N2_matrix_2 <- data.matrix(Rip_AP_TraitsDrop_N2_scale_2)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N2_matrix_2, stand = FALSE)
# site        FRic
# 11 trais    NA
# 6 traits    NA




#### AP, N10
# scale traits to put into a matrix
Rip_AP_TraitsDrop_N10_scale <- scale(Rip_AP_TraitsDrop_N10)

# put into a matrix 
Rip_AP_TraitsDrop_N10_matrix <- data.matrix(Rip_AP_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.01809333


#### AP, N20
# scale traits to put into a matrix

Rip_AP_TraitsDrop_N20 <- Rip_AP_TraitsDrop_N20 %>% drop_na()

Rip_AP_TraitsDrop_N20_scale <- scale(Rip_AP_TraitsDrop_N20)

# put into a matrix 
Rip_AP_TraitsDrop_N20_matrix <- data.matrix(Rip_AP_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_AP_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA




######################## SM #########################################

#### SM, N0
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N0_scale <- scale(Rip_SM_TraitsDrop_N0)

# put into a matrix 
Rip_SM_TraitsDrop_N0_matrix <- data.matrix(Rip_SM_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       0.003658497

#### SM, N2
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N2_scale <- scale(Rip_SM_TraitsDrop_N2)

# put into a matrix 
Rip_SM_TraitsDrop_N2_matrix <- data.matrix(Rip_SM_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### SM, N10
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N10_scale <- scale(Rip_SM_TraitsDrop_N10)

# put into a matrix 
Rip_SM_TraitsDrop_N10_matrix <- data.matrix(Rip_SM_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.005047735


#### SM, N20
# scale traits to put into a matrix
Rip_SM_TraitsDrop_N20_scale <- scale(Rip_SM_TraitsDrop_N20)

# put into a matrix 
Rip_SM_TraitsDrop_N20_matrix <- data.matrix(Rip_SM_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_SM_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA


######################## AG #########################################

#### AG, N0
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N0_scale <- scale(Rip_AG_TraitsDrop_N0)

# put into a matrix 
Rip_AG_TraitsDrop_N0_matrix <- data.matrix(Rip_AG_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       0.02682882

#### SM, N2
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N2_scale <- scale(Rip_AG_TraitsDrop_N2)

# put into a matrix 
Rip_AG_TraitsDrop_N2_matrix <- data.matrix(Rip_AG_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### AG, N10
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N10_scale <- scale(Rip_AG_TraitsDrop_N10)

# put into a matrix 
Rip_AG_TraitsDrop_N10_matrix <- data.matrix(Rip_AG_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.006366802


#### AG, N20
# scale traits to put into a matrix
Rip_AG_TraitsDrop_N20_scale <- scale(Rip_AG_TraitsDrop_N20)

# put into a matrix 
Rip_AG_TraitsDrop_N20_matrix <- data.matrix(Rip_AG_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_AG_TraitsDrop_N20_matrix, stand = FALSE)
# site        FRic
#   s1        NA


######################## SN #########################################

#### SN, N0
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N0_scale <- scale(Rip_SN_TraitsDrop_N0)

# put into a matrix 
Rip_SN_TraitsDrop_N0_matrix <- data.matrix(Rip_SN_TraitsDrop_N0_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N0_matrix, stand = FALSE)
# site        FRic
#   s1       .01857858

#### SN, N2
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N2_scale <- scale(Rip_SN_TraitsDrop_N2)

# put into a matrix 
Rip_SN_TraitsDrop_N2_matrix <- data.matrix(Rip_SN_TraitsDrop_N2_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N2_matrix, stand = FALSE)
# site        FRic
#   s1        NA
# really low??



#### SN, N10
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N10_scale <- scale(Rip_SN_TraitsDrop_N10)

# put into a matrix 
Rip_SN_TraitsDrop_N10_matrix <- data.matrix(Rip_SN_TraitsDrop_N10_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N10_matrix, stand = FALSE)
# site        FRic
#   s1        0.07541702


#### SN, N20
# scale traits to put into a matrix
Rip_SN_TraitsDrop_N20_scale <- scale(Rip_SN_TraitsDrop_N20)

# put into a matrix 
Rip_SN_TraitsDrop_N20_matrix <- data.matrix(Rip_SN_TraitsDrop_N20_scale)

# Run FRic
fd_fric(Rip_SN_TraitsDrop_N20_matrix2, stand = FALSE)
# site        FRic
#   s1        NA













#################################################################################################




# fd_fric(traits, sp_com, stand=FALSE )

# traits matrix 

# data: DO_TraitsDrop_N0

# Site-species matrix for birds
# site is equal to treatment
# species is equal to

# ripped out only the columns for ID-Treatment matrix 



Rip_DO_TraitsDrop <- DO_Traits_O_Drop %>% select(c(10:20))

fd_fric(Rip_DO_TraitsDrop)





### separate try 
do_traits_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_traits_plants.csv")
do_traits_plants <- data.frame(do_traits_plants, row.names = 1)
do_traits_plants_matrix <- as.matrix(do_traits_plants)

do_trt_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_trt_plants.csv")
do_trt_plants <- data.frame(do_trt_plants, row.names = 1)
do_trt_plants_matrix <- as.matrix(do_trt_plants)


fd_fric(do_traits_plants_matrix, do_trt_plants_matrix, stand=TRUE)
#    site          FRic
# 1   N0           1.027307e-05
# 2  N25           NA
# 3  N10           3.671785e-03
# 4  N20           NA

fd_fric(do_traits_plants_matrix, do_trt_plants_matrix, stand=FALSE)
#    site          FRic
# 1   N0           1.160487e-07
# 2  N25           NA
# 3  N10           4.147792e-05
# 4  N20           NA



### FRIC
do_n0_fric <- fd_fric(do_n0_traits_matrix, do_n0_trt_matrix, stand=FALSE)
do_n0_fric
#  site         FRic
#   N0           1.160487e-07
#  N25           NA
#  N10           NA
#  N20           NA




# Create a gower distance matrix:
mydist_do <- gowdis(do_traits_plants[colnames(do_trt_plants), ], asym.bin = NULL)
do_traits_dist_matrix <- as.matrix(mydist_do)
do_traits_dist_matrix2 <- as.dist(do_traits_dist_matrix)

# Measure FD using the functional dispersion metric:
# fdisp(distance, matrix-of-spec-abund , tol = 1e-07)

# run fdis
DO_Traits_fdisp <- fdisp(do_traits_dist_matrix2, do_trt_plants, tol= 1e-07)

# visualize output - this didnt work
DO_Traits_FDis <- DO_Traits_fdisp$FDis


DO_Traits_FDis2 <- as.data.frame(DO_Traits_fdisp)
DO_Traits_FDis2
# FDis
# 47




# Read in n0, n2.5, n10, n20

# Read n0
do_n0_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n0_traits.csv")
do_n0_traits <- data.frame(do_n0_traits, row.names = 1)
do_n0_traits_matrix <- as.matrix(do_n0_traits)

do_n0_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n0_trt.csv")
do_n0_trt <- data.frame(do_n0_trt, row.names = 1)
do_n0_trt_matrix <- as.matrix(do_n0_trt)



# Create a gower distance matrix:
mydist_do_n0 <- gowdis(do_n0_traits[colnames(do_n0_trt), ], asym.bin = NULL)

do_n0_traits_dist_matrix <- as.matrix(mydist_do_n0)
do_n0_traits_dist_matrix2 <- as.dist(do_n0_traits_dist_matrix)

#do_n0_traits_dist_matrix <- as.matrix(mydist_do_n0)
#do_n0_traits_dist_matrix <- as.dist(mydist_do_n0)
#do_n0_traits_dist_matrix2 <- as.matrix(mydist_do_n0)



# Measure FD using the functional dispersion metric:
# fdisp(distance, matrix-of-spec-abund , tol = 1e-07)

# run fdis
do_n0_fdisp <- fdisp(do_n0_traits_dist_matrix2, do_n0_trt_matrix, tol= 1e-07)
do_n0_fdisp
# FDis
# 17



# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n0_fdisp2 <- fd_fdis(do_n0_traits_matrix, do_n0_trt_matrix)
do_n0_fdisp2
#  site   FDis        FRic
#  N0     9.573049    1.160487e-07 
#  N25    0.000000    0.000000
#  N10    0.000000    0.000000
#  N20    0.000000    0.000000

### FRIC
do_n0_fric <- fd_fric(do_n0_traits_matrix, do_n0_trt_matrix, stand=FALSE)
do_n0_fric





# Read in n0, n2.5, n10, n20



# Read in do N10 data
do_n25_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n25_traits.csv")
do_n25_traits <- data.frame(do_n25_traits, row.names = 1)
do_n25_traits_matrix <- as.matrix(do_n25_traits)

do_n25_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n25_trt.csv")
do_n25_trt <- data.frame(do_n25_trt, row.names = 1)
do_n25_trt_matrix <- as.matrix(do_n25_trt)


# Create a gower distance matrix:
mydist_do_n25 <- gowdis(do_n25_traits[colnames(do_n25_trt), ], asym.bin = NULL)

#do_n10_traits_dist_matrix <- as.dist(mydist_do_n10)

do_n25_traits_dist_matrix <- as.matrix(mydist_do_n25)
do_n25_traits_dist_matrix2 <- as.dist(do_n25_traits_dist_matrix)

do_n25_fdisp <- fdisp(do_n25_traits_dist_matrix2, do_n25_trt_matrix, tol= 1e-07)

do_n25_fdisp2 <- fdisp(do_n25_traits_dist_matrix2, tol= 1e-07)
                      
                      
DO_n25_fdisp <- as.data.frame(do_n25_fdisp)
DO_n25_fdisp
# FDis
#    6

# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n25_fdisp2 <- fd_fdis(do_n25_traits_matrix, do_n25_trt_matrix)
do_n25_fdisp2
#  site    FDis
#   N0     0.000000
#  N25     8.875399
#  N10     0.000000
#  N20     0.000000

### FRIC
do_n25_fric <- fd_fric(do_n25_traits_matrix, do_n25_trt_matrix, stand=FALSE)
do_n25_fric
# N25   NA



# do_no different ids in the two dataframes messing things up 



# Read in n0, n2.5, n10, n20



# Read in do N10 data
do_n10_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n10_traits.csv")
do_n10_traits <- data.frame(do_n10_traits, row.names = 1)
do_n10_traits_matrix <- as.matrix(do_n10_traits)

do_n10_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n10_trt.csv")
do_n10_trt <- data.frame(do_n10_trt, row.names = 1)
do_n10_trt_matrix <- as.matrix(do_n10_trt)


# Create a gower distance matrix:
mydist_do_n10 <- gowdis(do_n10_traits[colnames(do_n10_trt), ], asym.bin = NULL)

do_n10_traits_dist_matrix <- as.dist(mydist_do_n10)

do_n10_traits_dist_matrix <- as.matrix(mydist_do_n10)
do_n10_traits_dist_matrix2 <- as.dist(do_n10_traits_dist_matrix)

do_n10_fdisp <- fdisp(do_n10_traits_dist_matrix2, do_n10_trt_matrix, tol= 1e-07)


do_n10_fdisp <- as.data.frame(do_n10_fdisp)
do_n10_fdisp
# FDis
# 18


# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n10_fdisp2 <- fd_fdis(do_n10_traits_matrix, do_n10_trt_matrix)
do_n10_fdisp2
#   site     FDis
#   N0       0.00000
#   N25      0.00000
#   N10      15.40181
#   N20      0.00000

### FRIC
do_n10_fric <- fd_fric(do_n10_traits_matrix, do_n10_trt_matrix, stand=FALSE)
do_n10_fric
# N10   4.147792e-05

do_n10_fric2 <- fd_fric(do_n10_traits, stand=FALSE)
# N10   4.147792e-05





# Read in n0, n2.5, n10, n20



# Read in do N20 data
do_n20_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n20_traits.csv")
do_n20_traits <- data.frame(do_n20_traits, row.names = 1)
do_n20_traits_matrix <- as.matrix(do_n20_traits)

do_n20_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/do_n20_trt.csv")
do_n20_trt <- data.frame(do_n20_trt, row.names = 1)
do_n20_trt_matrix <- as.matrix(do_n20_trt)


# Create a gower distance matrix:
mydist_do_n20 <- gowdis(do_n20_traits[colnames(do_n20_trt), ], asym.bin = NULL)

#do_n10_traits_dist_matrix <- as.dist(mydist_do_n10)

do_n20_traits_dist_matrix <- as.matrix(mydist_do_n20)
do_n20_traits_dist_matrix2 <- as.dist(do_n20_traits_dist_matrix)

do_n20_fdisp <- fdisp(do_n20_traits_dist_matrix2, do_n20_trt_matrix, tol= 1e-07)


do_n20_fdisp <- as.data.frame(do_n20_fdisp)
# FDis
#    6

### FRIC
do_n20_fric <- fd_fric(do_n20_traits, do_n20_trt, stand=FALSE)
do_n20_fric
# N20   NA

do_n20_fric2 <- fd_fric(do_n20_traits, stand=FALSE)
# N20   NA


# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n20_fdisp2 <- fd_fdis(do_n20_traits_matrix, do_n20_trt_matrix)
do_n20_fdisp2
# site     FDis
#   N0     0.000000
#  N25     0.000000
#  N10     0.000000
#  N20     6.989543







################## AP ####################


# FRic for AP
### separate try 
ap_traits_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_traits_plants.csv")
ap_traits_plants <- data.frame(ap_traits_plants, row.names = 1)
ap_traits_plants_matrix <- as.matrix(ap_traits_plants)

ap_trt_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_trt_plants.csv")
ap_trt_plants <- data.frame(ap_trt_plants, row.names = 1)
ap_trt_plants_matrix <- as.matrix(ap_trt_plants)

fd_fric(ap_traits_plants_matrix, ap_trt_plants_matrix, stand=FALSE)
# Stand= FALSe scales FRic between 0 and 1
#    site          FRic
# 1   N0           1.111541e-05
# 2  N25           NA
# 3  N10           4.147792e-05
# 4  N20           NA

#### FDis for AP


# Read in n0, n2.5, n10, n20

# Read in ap N0 data
ap_n0_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n0_traits.csv")
ap_n0_traits <- data.frame(ap_n0_traits, row.names = 1)
ap_n0_traits_matrix <- as.matrix(ap_n0_traits)

ap_n0_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n0_trt.csv")
ap_n0_trt <- data.frame(ap_n0_trt, row.names = 1)
ap_n0_trt_matrix <- as.matrix(ap_n0_trt)


# Create a gower distance matrix:
mydist_ap_n0 <- gowdis(ap_n0_traits[colnames(ap_n0_trt), ], asym.bin = NULL)
ap_n0_traits_dist_matrix <- as.matrix(mydist_ap_n0)
ap_n0_traits_dist_matrix2 <- as.dist(ap_n0_traits_dist_matrix)

# ap_n0_fdisp <- fdisp(ap_n0_traits_dist_matrix2, ap_n0_trt_matrix, tol= 1e-07)
# ap_n20_fdisp <- as.data.frame(ap_n0_fdisp)
# FDis

### FRIC
ap_n0_fric <- fd_fric(ap_n0_traits, ap_n0_trt, stand=FALSE)
ap_n0_fric
#      FRic
# N0   1.111541e-05

ap_n0_fric2 <- fd_fric(ap_n0_traits, stand=TRUE)
#      FRic
# N0   1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n0_fdisp2 <- fd_fdis(ap_n0_traits_matrix, ap_n0_trt_matrix)
ap_n0_fdisp2
# site     FDis
#   N0     18.1401




# Read in ap N25 data
ap_n25_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n25_traits.csv")
ap_n25_traits <- data.frame(ap_n25_traits, row.names = 1)
ap_n25_traits_matrix <- as.matrix(ap_n25_traits)

ap_n25_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n25_trt.csv")
ap_n25_trt <- data.frame(ap_n25_trt, row.names = 1)
ap_n25_trt_matrix <- as.matrix(ap_n25_trt)


# Create a gower distance matrix:
mydist_ap_n25 <- gowdis(ap_n25_traits[colnames(ap_n25_trt), ], asym.bin = NULL)
ap_n25_traits_dist_matrix <- as.matrix(mydist_ap_n25)
ap_n25_traits_dist_matrix2 <- as.dist(ap_n25_traits_dist_matrix)

### FRIC
ap_n25_fric <- fd_fric(ap_n25_traits, ap_n25_trt, stand=FALSE)
ap_n25_fric
#      FRic
# N0   NA

ap_n25_fric2 <- fd_fric(ap_n25_traits, stand=TRUE)
ap_n25_fric2
#      FRic
# N0   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n25_fdisp2 <- fd_fdis(ap_n25_traits_matrix, ap_n25_trt_matrix)
ap_n25_fdisp2
# site     FDis
#   N25    11.75999




# Read in ap N10 data
ap_n10_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n10_traits.csv")
ap_n10_traits <- data.frame(ap_n10_traits, row.names = 1)
ap_n10_traits_matrix <- as.matrix(ap_n10_traits)

ap_n10_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n10_trt.csv")
ap_n10_trt <- data.frame(ap_n10_trt, row.names = 1)
ap_n10_trt_matrix <- as.matrix(ap_n10_trt)


# Create a gower distance matrix:
mydist_ap_n10 <- gowdis(ap_n10_traits[colnames(ap_n10_trt), ], asym.bin = NULL)
ap_n10_traits_dist_matrix <- as.matrix(mydist_ap_n10)
ap_n10_traits_dist_matrix2 <- as.dist(ap_n10_traits_dist_matrix)

### FRIC
ap_n10_fric <- fd_fric(ap_n10_traits, ap_n10_trt, stand=FALSE)
ap_n10_fric
#      FRic
# N10   1.115483e-05

ap_n10_fric2 <- fd_fric(ap_n10_traits, ap_n10_trt,stand=TRUE)
ap_n10_fric2
#      FRic
# N0  1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n10_fdisp2 <- fd_fdis(ap_n10_traits_matrix, ap_n10_trt_matrix)
ap_n10_fdisp2
# site     FDis
#  N10     15.96303  



# Read in ap N20 data
ap_n20_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n20_traits.csv")
ap_n20_traits <- data.frame(ap_n20_traits, row.names = 1)
ap_n20_traits_matrix <- as.matrix(ap_n20_traits)

ap_n20_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n20_trt.csv")
ap_n20_trt <- data.frame(ap_n20_trt, row.names = 1)
ap_n20_trt_matrix <- as.matrix(ap_n20_trt)


# Create a gower distance matrix:
mydist_ap_n20 <- gowdis(ap_n20_traits[colnames(ap_n20_trt), ], asym.bin = NULL)
ap_n20_traits_dist_matrix <- as.matrix(mydist_ap_n20)
ap_n20_traits_dist_matrix2 <- as.dist(ap_n20_traits_dist_matrix)

### FRIC
ap_n20_fric <- fd_fric(ap_n20_traits, ap_n20_trt, stand=FALSE)
ap_n20_fric
#      FRic
# N0   NA  

ap_n20_fric2 <- fd_fric(ap_n20_traits, stand=TRUE)
ap_n20_fric2
#      FRic
# N0   NA 

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n20_fdisp2 <- fd_fdis(ap_n20_traits_matrix, ap_n20_trt_matrix)
ap_n20_fdisp2
# site     FDis
#   N0     6.555699





################## SM ####################


# FRic for SM
### separate try 
sm_traits_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_traits_plants.csv")
sm_traits_plants <- data.frame(sm_traits_plants, row.names = 1)
sm_traits_plants_matrix <- as.matrix(sm_traits_plants)

sm_trt_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_trt_plants.csv")
sm_trt_plants <- data.frame(sm_trt_plants, row.names = 1)
sm_trt_plants_matrix <- as.matrix(sm_trt_plants)

fd_fric(sm_traits_plants_matrix, sm_trt_plants_matrix, stand=FALSE)
# Stand= FALSe scales FRic between 0 and 1
#    site          FRic
# 1   N0           2.561420e-05
# 2  N25           NA
# 3  N10           7.991932e-06
# 4  N20           NA

fd_fric(sm_traits_plants_matrix, sm_trt_plants_matrix, stand=TRUE)


#### FDis for AP


# Read in n0, n2.5, n10, n20

# Read in sm N0 data
sm_n0_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n0_traits.csv")
sm_n0_traits <- data.frame(sm_n0_traits, row.names = 1)
sm_n0_traits_matrix <- as.matrix(sm_n0_traits)

sm_n0_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n0_trt.csv")
sm_n0_trt <- data.frame(sm_n0_trt, row.names = 1)
sm_n0_trt_matrix <- as.matrix(sm_n0_trt)


# Create a gower distance matrix:
mydist_sm_n0 <- gowdis(sm_n0_traits[colnames(sm_n0_trt), ], asym.bin = NULL)
sm_n0_traits_dist_matrix <- as.matrix(mydist_sm_n0)
sm_n0_traits_dist_matrix2 <- as.dist(sm_n0_traits_dist_matrix)

### FRIC
sm_n0_fric <- fd_fric(sm_n0_traits, sm_n0_trt, stand=FALSE)
sm_n0_fric
#      FRic
# N0   2.56142e-05

sm_n0_fric2 <- fd_fric(sm_n0_traits, stand=TRUE)
sm_n0_fric2
#      FRic
# N0   1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n0_fdisp2 <- fd_fdis(sm_n0_traits_matrix, sm_n0_trt_matrix)
sm_n0_fdisp2
# site     FDis
#   N0     6.535526


# Read in sm N25 data
sm_n25_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n25_traits.csv")
sm_n25_traits <- data.frame(sm_n25_traits, row.names = 1)
sm_n25_traits_matrix <- as.matrix(sm_n25_traits)

sm_n25_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n25_trt.csv")
sm_n25_trt <- data.frame(sm_n25_trt, row.names = 1)
sm_n25_trt_matrix <- as.matrix(sm_n25_trt)


# Create a gower distance matrix:
mydist_sm_n25 <- gowdis(sm_n25_traits[colnames(sm_n25_trt), ], asym.bin = NULL)
sm_n25_traits_dist_matrix <- as.matrix(mydist_sm_n25)
sm_n25_traits_dist_matrix2 <- as.dist(sm_n25_traits_dist_matrix)

### FRIC
sm_n25_fric <- fd_fric(sm_n25_traits, sm_n25_trt, stand=FALSE)
sm_n25_fric
#      FRic
# N0   NA

sm_n25_fric2 <- fd_fric(sm_n25_traits, stand=TRUE)
sm_n25_fric2
#      FRic
# N0   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n25_fdisp2 <- fd_fdis(sm_n25_traits_matrix, sm_n25_trt_matrix)
sm_n25_fdisp2
# site     FDis
#   N25    4.439785




# Read in sm N10 data
sm_n10_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n10_traits.csv")
sm_n10_traits <- data.frame(sm_n10_traits, row.names = 1)
sm_n10_traits_matrix <- as.matrix(sm_n10_traits)

sm_n10_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n10_trt.csv")
sm_n10_trt <- data.frame(sm_n10_trt, row.names = 1)
sm_n10_trt_matrix <- as.matrix(sm_n10_trt)


# Create a gower distance matrix:
mydist_sm_n10 <- gowdis(sm_n10_traits[colnames(sm_n10_trt), ], asym.bin = NULL)
sm_n10_traits_dist_matrix <- as.matrix(mydist_sm_n10)
sm_n10_traits_dist_matrix2 <- as.dist(sm_n10_traits_dist_matrix)

### FRIC
sm_n10_fric <- fd_fric(sm_n10_traits, sm_n10_trt, stand=FALSE)
sm_n10_fric
#      FRic
# N10   7.991932e-06

sm_n10_fric2 <- fd_fric(sm_n10_traits, sm_n10_trt,stand=TRUE)
sm_n10_fric2
#      FRic
# N0    1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n10_fdisp2 <- fd_fdis(sm_n10_traits_matrix, sm_n10_trt_matrix)
sm_n10_fdisp2
# site    FDis
#  N10    7.767028



# Read in sm N20 data
sm_n20_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n20_traits.csv")
sm_n20_traits <- data.frame(sm_n20_traits, row.names = 1)
sm_n20_traits_matrix <- as.matrix(sm_n20_traits)

sm_n20_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n20_trt.csv")
sm_n20_trt <- data.frame(sm_n20_trt, row.names = 1)
sm_n20_trt_matrix <- as.matrix(sm_n20_trt)


# Create a gower distance matrix:
mydist_sm_n20 <- gowdis(sm_n20_traits[colnames(sm_n20_trt), ], asym.bin = NULL)
sm_n20_traits_dist_matrix <- as.matrix(mydist_sm_n20)
sm_n20_traits_dist_matrix2 <- as.dist(sm_n20_traits_dist_matrix)

### FRIC
sm_n20_fric <- fd_fric(sm_n20_traits, sm_n20_trt, stand=FALSE)
sm_n20_fric
#      FRic
# N20   NA  

sm_n20_fric2 <- fd_fric(sm_n20_traits, stand=TRUE)
sm_n20_fric2
#      FRic
# N20   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n20_fdisp2 <- fd_fdis(sm_n20_traits_matrix, sm_n20_trt_matrix)
sm_n20_fdisp2
# site     FDis
#  N20     4.273536












################## AG ####################


# FRic for AG
### separate try 
ag_traits_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_traits_plants.csv")
ag_traits_plants <- data.frame(ag_traits_plants, row.names = 1)
ag_traits_plants_matrix <- as.matrix(ag_traits_plants)

ag_trt_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_trt_plants.csv")
ag_trt_plants <- data.frame(ag_trt_plants, row.names = 1)
ag_trt_plants_matrix <- as.matrix(ag_trt_plants)

fd_fric(ag_traits_plants_matrix, ag_trt_plants_matrix, stand=FALSE)
# Stand= FALSe scales FRic between 0 and 1
#    site          FRic
# 1   N0           7.486341e-07
# 2  N25           NA
# 3  N10           3.328658e-05
# 4  N20           NA

fd_fric(ag_traits_plants_matrix, ag_trt_plants_matrix, stand=TRUE)


#### FDis for AG


# Read in n0, n2.5, n10, n20

# Read in ag N0 data
ag_n0_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n0_traits.csv")
ag_n0_traits <- data.frame(ag_n0_traits, row.names = 1)
ag_n0_traits_matrix <- as.matrix(ag_n0_traits)

ag_n0_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n0_trt.csv")
ag_n0_trt <- data.frame(ag_n0_trt, row.names = 1)
ag_n0_trt_matrix <- as.matrix(ag_n0_trt)


# Create a gower distance matrix:
mydist_ag_n0 <- gowdis(ag_n0_traits[colnames(ag_n0_trt), ], asym.bin = NULL)
ag_n0_traits_dist_matrix <- as.matrix(mydist_ag_n0)
ag_n0_traits_dist_matrix2 <- as.dist(ag_n0_traits_dist_matrix)

### FRIC
ag_n0_fric <- fd_fric(ag_n0_traits, ag_n0_trt, stand=FALSE)
ag_n0_fric
#      FRic
# N0   7.486341e-07

ag_n0_fric2 <- fd_fric(ag_n0_traits, stand=TRUE)
ag_n0_fric2
#      FRic
# N0   1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n0_fdisp2 <- fd_fdis(ag_n0_traits_matrix, ag_n0_trt_matrix)
ag_n0_fdisp2
# site     FDis
#   N0     5.45268




# Read in ap N25 data
ag_n25_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n25_traits.csv")
ag_n25_traits <- data.frame(ag_n25_traits, row.names = 1)
ag_n25_traits_matrix <- as.matrix(ag_n25_traits)

ag_n25_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n25_trt.csv")
ag_n25_trt <- data.frame(ag_n25_trt, row.names = 1)
ag_n25_trt_matrix <- as.matrix(ag_n25_trt)


# Create a gower distance matrix:
mydist_ag_n25 <- gowdis(ag_n25_traits[colnames(ag_n25_trt), ], asym.bin = NULL)
ag_n25_traits_dist_matrix <- as.matrix(mydist_ag_n25)
ag_n25_traits_dist_matrix2 <- as.dist(ag_n25_traits_dist_matrix)

### FRIC
ag_n25_fric <- fd_fric(ag_n25_traits, ag_n25_trt, stand=FALSE)
ag_n25_fric
#      FRic
# N2.5   Na

ag_n25_fric2 <- fd_fric(ag_n25_traits, stand=TRUE)
ag_n25_fric2
#      FRic
# N2.5   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n25_fdisp2 <- fd_fdis(ag_n25_traits_matrix, ag_n25_trt_matrix)
ag_n25_fdisp2
# site     FDis
#   N2.5    7.073222




# Read in ag N10 data
ag_n10_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n10_traits.csv")
ag_n10_traits <- data.frame(ag_n10_traits, row.names = 1)
ag_n10_traits_matrix <- as.matrix(ag_n10_traits)

ag_n10_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n10_trt.csv")
ag_n10_trt <- data.frame(ag_n10_trt, row.names = 1)
ag_n10_trt_matrix <- as.matrix(ag_n10_trt)


# Create a gower distance matrix:
mydist_ag_n10 <- gowdis(ag_n10_traits[colnames(ag_n10_trt), ], asym.bin = NULL)
ag_n10_traits_dist_matrix <- as.matrix(mydist_ag_n10)
ag_n10_traits_dist_matrix2 <- as.dist(ag_n10_traits_dist_matrix)

### FRIC
ag_n10_fric <- fd_fric(ag_n10_traits, ag_n10_trt, stand=FALSE)
ag_n10_fric
#      FRic
# N10  3.328658e-05

ag_n10_fric2 <- fd_fric(ag_n10_traits, ag_n10_trt,stand=TRUE)
ag_n10_fric2
#      FRic
# N10   1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n10_fdisp2 <- fd_fdis(ag_n10_traits_matrix, ag_n10_trt_matrix)
ag_n10_fdisp2
# site     FDis
#  N10     22.12759  



# Read in ag N20 data
ag_n20_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n20_traits.csv")
ag_n20_traits <- data.frame(ag_n20_traits, row.names = 1)
ag_n20_traits_matrix <- as.matrix(ag_n20_traits)

ag_n20_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n20_trt.csv")
ag_n20_trt <- data.frame(ag_n20_trt, row.names = 1)
ag_n20_trt_matrix <- as.matrix(ag_n20_trt)


# Create a gower distance matrix:
mydist_ag_n20 <- gowdis(ag_n20_traits[colnames(ag_n20_trt), ], asym.bin = NULL)
ag_n20_traits_dist_matrix <- as.matrix(mydist_ag_n20)
ag_n20_traits_dist_matrix2 <- as.dist(ag_n20_traits_dist_matrix)

### FRIC
ag_n20_fric <- fd_fric(ag_n20_traits, ag_n20_trt, stand=FALSE)
ag_n20_fric
#      FRic
# N20   NA  

ag_n20_fric2 <- fd_fric(ag_n20_traits, stand=TRUE)
ag_n20_fric2
#      FRic
# N20   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n20_fdisp2 <- fd_fdis(ag_n20_traits_matrix, ag_n20_trt_matrix)
ag_n20_fdisp2
# site     FDis
#   N0     5.859143








################## SN ####################


# FRic for SN
sn_traits_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_traits_plants.csv")
sn_traits_plants <- data.frame(sn_traits_plants, row.names = 1)
sn_traits_plants_matrix <- as.matrix(sn_traits_plants)

sn_trt_plants <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_trt_plants.csv")
sn_trt_plants <- data.frame(sn_trt_plants, row.names = 1)
sn_trt_plants_matrix <- as.matrix(sn_trt_plants)

fd_fric(sn_traits_plants_matrix, sn_trt_plants_matrix, stand=FALSE)
# Stand= FALSe scales FRic between 0 and 1
#    site          FRic
# 1   N0           3.119241e-06
# 2  N25           NA
# 3  N10           2.203906e-06
# 4  N20           NA

fd_fric(sn_traits_plants_matrix, sn_trt_plants_matrix, stand=TRUE)


# Read in n0, n2.5, n10, n20

# Read in sn N0 data
sn_n0_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n0_traits.csv")
sn_n0_traits <- data.frame(sn_n0_traits, row.names = 1)
sn_n0_traits_matrix <- as.matrix(sn_n0_traits)

sn_n0_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n0_trt.csv")
sn_n0_trt <- data.frame(sn_n0_trt, row.names = 1)
sn_n0_trt_matrix <- as.matrix(sn_n0_trt)


# Create a gower distance matrix:
mydist_sn_n0 <- gowdis(sn_n0_traits[colnames(sn_n0_trt), ], asym.bin = NULL)
sn_n0_traits_dist_matrix <- as.matrix(mydist_sn_n0)
sn_n0_traits_dist_matrix2 <- as.dist(sn_n0_traits_dist_matrix)

### FRIC
sn_n0_fric <- fd_fric(sn_n0_traits, sn_n0_trt, stand=FALSE)
sn_n0_fric
#      FRic
# N0   3.119241e-06

sn_n0_fric2 <- fd_fric(sn_n0_traits, stand=TRUE)
sn_n0_fric2
#      FRic
# N0   1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n0_fdisp2 <- fd_fdis(sn_n0_traits_matrix, sn_n0_trt_matrix)
sn_n0_fdisp2
# site     FDis
#   N0     7.769252


# Read in sn N25 data
sn_n25_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n25_traits.csv")
sn_n25_traits <- data.frame(sn_n25_traits, row.names = 1)
sn_n25_traits_matrix <- as.matrix(sn_n25_traits)

sn_n25_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n25_trt.csv")
sn_n25_trt <- data.frame(sn_n25_trt, row.names = 1)
sn_n25_trt_matrix <- as.matrix(sn_n25_trt)


# Create a gower distance matrix:
mydist_sn_n25 <- gowdis(sn_n25_traits[colnames(sn_n25_trt), ], asym.bin = NULL)
sn_n25_traits_dist_matrix <- as.matrix(mydist_sn_n25)
sn_n25_traits_dist_matrix2 <- as.dist(sn_n25_traits_dist_matrix)

### FRIC
sn_n25_fric <- fd_fric(sn_n25_traits, sn_n25_trt, stand=FALSE)
sn_n25_fric
#      FRic
# N0   NA

sn_n25_fric2 <- fd_fric(sn_n25_traits, stand=TRUE)
sn_n25_fric2
#      FRic
# N0   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n25_fdisp2 <- fd_fdis(sn_n25_traits_matrix, sn_n25_trt_matrix)
sn_n25_fdisp2
# site     FDis
# N25      5.449649




# Read in sn N10 data
sn_n10_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n10_traits.csv")
sn_n10_traits <- data.frame(sn_n10_traits, row.names = 1)
sn_n10_traits_matrix <- as.matrix(sn_n10_traits)

sn_n10_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n10_trt.csv")
sn_n10_trt <- data.frame(sn_n10_trt, row.names = 1)
sn_n10_trt_matrix <- as.matrix(sn_n10_trt)


# Create a gower distance matrix:
mydist_sn_n10 <- gowdis(sn_n10_traits[colnames(sn_n10_trt), ], asym.bin = NULL)
sn_n10_traits_dist_matrix <- as.matrix(mydist_sn_n10)
sn_n10_traits_dist_matrix2 <- as.dist(sn_n10_traits_dist_matrix)

### FRIC
sn_n10_fric <- fd_fric(sn_n10_traits, sn_n10_trt, stand=FALSE)
sn_n10_fric
#      FRic
# N10   2.203906e-06

sn_n10_fric2 <- fd_fric(sn_n10_traits, sn_n10_trt,stand=TRUE)
sn_n10_fric2
#      FRic
# N10    1


# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n10_fdisp2 <- fd_fdis(sn_n10_traits_matrix, sn_n10_trt_matrix)
sn_n10_fdisp2
# site    FDis
#  N10    6.055771



# Read in sn N20 data
sn_n20_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n20_traits.csv")
sn_n20_traits <- data.frame(sn_n20_traits, row.names = 1)
sn_n20_traits_matrix <- as.matrix(sn_n20_traits)

sn_n20_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n20_trt.csv")
sn_n20_trt <- data.frame(sn_n20_trt, row.names = 1)
sn_n20_trt_matrix <- as.matrix(sn_n20_trt)


# Create a gower distance matrix:
mydist_sn_n20 <- gowdis(sn_n20_traits[colnames(sn_n20_trt), ], asym.bin = NULL)
sn_n20_traits_dist_matrix <- as.matrix(mydist_sn_n20)
sn_n20_traits_dist_matrix2 <- as.dist(sn_n20_traits_dist_matrix)

### FRIC
sn_n20_fric <- fd_fric(sn_n20_traits, sn_n20_trt, stand=FALSE)
sn_n20_fric
#      FRic
# N20   NA  

sn_n20_fric2 <- fd_fric(sn_n20_traits, stand=TRUE)
sn_n20_fric2
#      FRic
# N20   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n20_fdisp2 <- fd_fdis(sn_n20_traits_matrix, sn_n20_trt_matrix)
sn_n20_fdisp2
# site     FDis
#  N20     6.324076





############################# Change ##############################3

# DO Change N0 and Change N10
# Read in do n0 data
do_n0_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/DO/do_n0_ch_traits.csv")
do_n0_ch_traits <- data.frame(do_n0_ch_traits, row.names = 1)
do_n0_ch_traits_matrix <- as.matrix(do_n0_ch_traits)

do_n0_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/DO/do_n0_ch_trt.csv")
do_n0_ch_trt <- data.frame(do_n0_ch_trt, row.names = 1)
do_n0_ch_trt_matrix <- as.matrix(do_n0_ch_trt)


# Create a gower distance matrix:
mydist_do_n0_ch <- gowdis(do_n0_ch_traits[colnames(do_n0_ch_trt), ], asym.bin = NULL)
do_n0_ch_traits_dist_matrix <- as.matrix(mydist_do_n0_ch)
do_n0_ch_traits_dist_matrix2 <- as.dist(do_n0_ch_traits_dist_matrix)

### FRIC
do_n0_ch_fric <- fd_fric(do_n0_ch_traits, do_n0_ch_trt, stand=FALSE)
do_n0_ch_fric
#      FRic
# N0   NA  

do_n0_ch_fric2 <- fd_fric(do_n0_ch_traits, do_n0_ch_trt, stand=FALSE)
do_n0_ch_fric2
#      FRic
# N0   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n0_ch_fdisp2 <- fd_fdis(do_n0_ch_traits_matrix, do_n0_ch_trt_matrix)
do_n0_ch_fdisp2
# site     FDis
#  N0     6.488423


# Read in do n10 data
do_n10_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/DO/do_n10_ch_traits.csv")
do_n10_ch_traits <- data.frame(do_n10_ch_traits, row.names = 1)
do_n10_ch_traits_matrix <- as.matrix(do_n10_ch_traits)

do_n10_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/DO/do_n10_ch_trt.csv")
do_n10_ch_trt <- data.frame(do_n10_ch_trt, row.names = 1)
do_n10_ch_trt_matrix <- as.matrix(do_n10_ch_trt)


# Create a gower distance matrix:
mydist_do_n10_ch <- gowdis(do_n10_ch_traits[colnames(do_n10_ch_trt), ], asym.bin = NULL)
do_n10_ch_traits_dist_matrix <- as.matrix(mydist_do_n10_ch)
do_n10_ch_traits_dist_matrix2 <- as.dist(do_n10_ch_traits_dist_matrix)

### FRIC
do_n10_ch_fric <- fd_fric(do_n10_ch_traits, do_n10_ch_trt, stand=FALSE)
do_n10_ch_fric
#      FRic
# N10   NA  

do_n10_ch_fric2 <- fd_fric(do_n10_ch_traits, do_n10_ch_trt, stand=FALSE)
do_n10_ch_fric2
#      FRic
# N10   NA

# Use fd_fdis(traits, sp_com) from "fundiversity" package
do_n10_ch_fdisp2 <- fd_fdis(do_n10_ch_traits_matrix, do_n10_ch_trt_matrix)
do_n10_ch_fdisp2
# site     FDis
#  N10     12.41321


# AP Change N0 and Change N10
# Read in ap n0 data
ap_n0_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n0_ch_traits.csv")
ap_n0_ch_traits <- data.frame(ap_n0_ch_traits, row.names = 1)
ap_n0_ch_traits_matrix <- as.matrix(ap_n0_ch_traits)

ap_n0_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n0_ch_trt.csv")
ap_n0_ch_trt <- data.frame(ap_n0_ch_trt, row.names = 1)
ap_n0_ch_trt_matrix <- as.matrix(ap_n0_ch_trt)


# Create a gower distance matrix:
mydist_ap_n0_ch <- gowdis(ap_n0_ch_traits[colnames(ap_n0_ch_trt), ], asym.bin = NULL)
ap_n0_ch_traits_dist_matrix <- as.matrix(mydist_ap_n0_ch)
ap_n0_ch_traits_dist_matrix2 <- as.dist(ap_n0_ch_traits_dist_matrix)

### FRIC
ap_n0_ch_fric <- fd_fric(ap_n0_ch_traits, ap_n0_ch_trt, stand=FALSE)
ap_n0_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n0_ch_fdisp2 <- fd_fdis(ap_n0_ch_traits_matrix, ap_n0_ch_trt_matrix)
ap_n0_ch_fdisp2
# site     FDis
#  N0      6.979187



# Read in ap n10 data
ap_n10_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n10_ch_traits.csv")
ap_n10_ch_traits <- data.frame(ap_n10_ch_traits, row.names = 1)
ap_n10_ch_traits_matrix <- as.matrix(ap_n10_ch_traits)

ap_n10_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AP/ap_n10_ch_trt.csv")
ap_n10_ch_trt <- data.frame(ap_n10_ch_trt, row.names = 1)
ap_n10_ch_trt_matrix <- as.matrix(ap_n10_ch_trt)


# Create a gower distance matrix:
mydist_ap_n10_ch <- gowdis(ap_n10_ch_traits[colnames(ap_n10_ch_trt), ], asym.bin = NULL)
ap_n10_ch_traits_dist_matrix <- as.matrix(mydist_ap_n10_ch)
ap_n10_ch_traits_dist_matrix2 <- as.dist(ap_n10_ch_traits_dist_matrix)

### FRIC
ap_n10_ch_fric <- fd_fric(ap_n10_ch_traits, ap_n10_ch_trt, stand=FALSE)
ap_n10_ch_fric
#      FRic
# N10   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ap_n10_ch_fdisp2 <- fd_fdis(ap_n10_ch_traits_matrix, ap_n10_ch_trt_matrix)
ap_n10_ch_fdisp2
# site     FDis
#  N10     21.27784

## SM Change N0 and Change N10
# Read in sm n0 data
sm_n0_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n0_ch_traits.csv")
sm_n0_ch_traits <- data.frame(sm_n0_ch_traits, row.names = 1)
sm_n0_ch_traits_matrix <- as.matrix(sm_n0_ch_traits)

sm_n0_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n0_ch_trt.csv")
sm_n0_ch_trt <- data.frame(sm_n0_ch_trt, row.names = 1)
sm_n0_ch_trt_matrix <- as.matrix(sm_n0_ch_trt)


# Create a gower distance matrix:
mydist_sm_n0_ch <- gowdis(sm_n0_ch_traits[colnames(sm_n0_ch_trt), ], asym.bin = NULL)
sm_n0_ch_traits_dist_matrix <- as.matrix(mydist_sm_n0_ch)
sm_n0_ch_traits_dist_matrix2 <- as.dist(sm_n0_ch_traits_dist_matrix)

### FRIC
sm_n0_ch_fric <- fd_fric(sm_n0_ch_traits, sm_n0_ch_trt, stand=FALSE)
sm_n0_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n0_ch_fdisp2 <- fd_fdis(sm_n0_ch_traits_matrix, sm_n0_ch_trt_matrix)
sm_n0_ch_fdisp2
# site     FDis
#  N0     5.024617


# Read in sm n10 data
sm_n10_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n10_ch_traits.csv")
sm_n10_ch_traits <- data.frame(sm_n10_ch_traits, row.names = 1)
sm_n10_ch_traits_matrix <- as.matrix(sm_n10_ch_traits)

sm_n10_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SM/sm_n10_ch_trt.csv")
sm_n10_ch_trt <- data.frame(sm_n10_ch_trt, row.names = 1)
sm_n10_ch_trt_matrix <- as.matrix(sm_n10_ch_trt)


# Create a gower distance matrix:
mydist_sm_n10_ch <- gowdis(sm_n10_ch_traits[colnames(sm_n10_ch_trt), ], asym.bin = NULL)
sm_n10_ch_traits_dist_matrix <- as.matrix(mydist_sm_n10_ch)
sm_n10_ch_traits_dist_matrix2 <- as.dist(sm_n10_ch_traits_dist_matrix)

### FRIC
sm_n10_ch_fric <- fd_fric(sm_n10_ch_traits, sm_n10_ch_trt, stand=FALSE)
sm_n10_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sm_n10_ch_fdisp2 <- fd_fdis(sm_n10_ch_traits_matrix, sm_n10_ch_trt_matrix)
sm_n10_ch_fdisp2
# site     FDis
#  N0      3.741547


# AG Change N0 and Change N10
# Read in ag n0 data
ag_n0_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n0_ch_traits.csv")
ag_n0_ch_traits <- data.frame(ag_n0_ch_traits, row.names = 1)
ag_n0_ch_traits_matrix <- as.matrix(ag_n0_ch_traits)

ag_n0_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n0_ch_trt.csv")
ag_n0_ch_trt <- data.frame(ag_n0_ch_trt, row.names = 1)
ag_n0_ch_trt_matrix <- as.matrix(ag_n0_ch_trt)


# Create a gower distance matrix:
mydist_ag_n0_ch <- gowdis(ag_n0_ch_traits[colnames(ag_n0_ch_trt), ], asym.bin = NULL)
ag_n0_ch_traits_dist_matrix <- as.matrix(mydist_ag_n0_ch)
ag_n0_ch_traits_dist_matrix2 <- as.dist(ag_n0_ch_traits_dist_matrix)

### FRIC
ag_n0_ch_fric <- fd_fric(ag_n0_ch_traits, ag_n0_ch_trt, stand=FALSE)
ag_n0_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n0_ch_fdisp2 <- fd_fdis(ag_n0_ch_traits_matrix, ag_n0_ch_trt_matrix)
ag_n0_ch_fdisp2
# site     FDis
#  N0      3.299304


# Read in ag n10 data
ag_n10_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n10_ch_traits.csv")
ag_n10_ch_traits <- data.frame(ag_n10_ch_traits, row.names = 1)
ag_n10_ch_traits_matrix <- as.matrix(ag_n10_ch_traits)

ag_n10_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/AG/ag_n10_ch_trt.csv")
ag_n10_ch_trt <- data.frame(ag_n10_ch_trt, row.names = 1)
ag_n10_ch_trt_matrix <- as.matrix(ag_n10_ch_trt)


# Create a gower distance matrix:
mydist_ag_n10_ch <- gowdis(ag_n10_ch_traits[colnames(ag_n10_ch_trt), ], asym.bin = NULL)
ag_n10_ch_traits_dist_matrix <- as.matrix(mydist_ag_n10_ch)
ag_n10_ch_traits_dist_matrix2 <- as.dist(ag_n10_ch_traits_dist_matrix)

### FRIC
ag_n10_ch_fric <- fd_fric(ag_n10_ch_traits, ag_n10_ch_trt, stand=FALSE)
ag_n10_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
ag_n10_ch_fdisp2 <- fd_fdis(ag_n10_ch_traits_matrix, ag_n10_ch_trt_matrix)
ag_n10_ch_fdisp2
# site     FDis
#  N0      3.877926


# SN Change N0 and Change N10
# Read in sn n0 data
sn_n0_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n0_ch_traits.csv")
sn_n0_ch_traits <- data.frame(sn_n0_ch_traits, row.names = 1)
sn_n0_ch_traits_matrix <- as.matrix(sn_n0_ch_traits)

sn_n0_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n0_ch_trt.csv")
sn_n0_ch_trt <- data.frame(sn_n0_ch_trt, row.names = 1)
sn_n0_ch_trt_matrix <- as.matrix(sn_n0_ch_trt)


# Create a gower distance matrix:
mydist_sn_n0_ch <- gowdis(sn_n0_ch_traits[colnames(sn_n0_ch_trt), ], asym.bin = NULL)
sn_n0_ch_traits_dist_matrix <- as.matrix(mydist_sn_n0_ch)
sn_n0_ch_traits_dist_matrix2 <- as.dist(sn_n0_ch_traits_dist_matrix)

### FRIC
sn_n0_ch_fric <- fd_fric(sn_n0_ch_traits, sn_n0_ch_trt, stand=FALSE)
sn_n0_ch_fric
#      FRic
# N0   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n0_ch_fdisp2 <- fd_fdis(sn_n0_ch_traits_matrix, sn_n0_ch_trt_matrix)
sn_n0_ch_fdisp2
# site     FDis
#  N0      10.37514

# Read in sn n0 data
sn_n10_ch_traits <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n10_ch_traits.csv")
sn_n10_ch_traits <- data.frame(sn_n10_ch_traits, row.names = 1)
sn_n10_ch_traits_matrix <- as.matrix(sn_n10_ch_traits)

sn_n10_ch_trt <- read_csv("~/Desktop/Thesis_Data/FRic/Data/SN/sn_n10_ch_trt.csv")
sn_n10_ch_trt <- data.frame(sn_n10_ch_trt, row.names = 1)
sn_n10_ch_trt_matrix <- as.matrix(sn_n10_ch_trt)


# Create a gower distance matrix:
mydist_sn_n10_ch <- gowdis(sn_n10_ch_traits[colnames(sn_n10_ch_trt), ], asym.bin = NULL)
sn_n10_ch_traits_dist_matrix <- as.matrix(mydist_sn_n10_ch)
sn_n10_ch_traits_dist_matrix2 <- as.dist(sn_n10_ch_traits_dist_matrix)

### FRIC
sn_n10_ch_fric <- fd_fric(sn_n10_ch_traits, sn_n10_ch_trt, stand=FALSE)
sn_n10_ch_fric
#      FRic
# N10   NA  

# Use fd_fdis(traits, sp_com) from "fundiversity" package
sn_n10_ch_fdisp2 <- fd_fdis(sn_n10_ch_traits_matrix, sn_n10_ch_trt_matrix)
sn_n10_ch_fdisp2
# site     FDis
#  N10     4.58068 

