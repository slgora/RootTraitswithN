#### Root Traits Paper New Figure for Trait Comparisons -  ####
###  Gora_RootTraitswithN-TraitPairsFigure.R
###  by Sarah Gora
###  Date created: Jan 17, 2024

###set WD 
setwd("~/Desktop/Thesis_Data/R Code")

###install packages
library(readr)
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)

### Load Data
# All traits drop data used for N0N10 correlation matrixes
# missing individuals dropped if didnt have data for all 11 traits
library(readr)
All_Traits_O_Drop <- read_csv("~/Desktop/All_Traits_O_DroppedTraits.csv")

### Subset out N0N10 data by species
# Ordination data with only a certain number of traits
DO_N0N10_Traits <- All_Traits_O_Drop %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits <- All_Traits_O_Drop %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits <- All_Traits_O_Drop %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits <- All_Traits_O_Drop %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits <- All_Traits_O_Drop %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")


# Subset by Treatments for linear models
### Subset out data by Treatment
DO_N0_Traits <- DO_N0N10_Traits %>% filter(Treatment=="Control")
DO_N10_Traits <- DO_N0N10_Traits %>% filter(Treatment=="10")
AP_N0_Traits <- AP_N0N10_Traits %>% filter(Treatment=="Control")
AP_N10_Traits <- AP_N0N10_Traits %>% filter(Treatment=="10")
SM_N0_Traits <- SM_N0N10_Traits %>% filter(Treatment=="Control")
SM_N10_Traits <- SM_N0N10_Traits %>% filter(Treatment=="10")
AG_N0_Traits <- AG_N0N10_Traits %>% filter(Treatment=="Control")
AG_N10_Traits <- AG_N0N10_Traits %>% filter(Treatment=="10")
SN_N0_Traits <- SN_N0N10_Traits %>% filter(Treatment=="Control")
SN_N10_Traits <- SN_N0N10_Traits %>% filter(Treatment=="10")

## Trait Pairs Figure
## 5 species going down
## 3 columns of 3 trait pairs
# Trait Pair 1: SLA vs SRL
# Trait Pair 2: Aboveground Biomass vs Belowground Biomass
# Trait Pair 3: Leaf %N vs Root %N

# Run linear model: Aboveground Trait x Belowground Trait 
# Aboveground Trait = y
# Belowground Trait = x
# 2 lines, colored by treatments = 0, 10g N
# Dashed line = non-significant
# Solid line = significant

# Color by Treatment: 
# Control= "#6E687E"
# N10= "#769370"

############## Dicanthelium oligosanthes ######################

######## DO- Trait Pair 1 ########
## Overall linear model
reg_DO_N0N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = DO_N0N10_Traits)
summary(reg_DO_N0N10_AbGvsBeG) #Overall: p=0.090
## Control linear model
reg_DO_N0_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = DO_N0_Traits)
summary(reg_DO_N0_AbGvsBeG) #Control: p=0.740
## 10N linear model
reg_DO_N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = DO_N10_Traits)
summary(reg_DO_N10_AbGvsBeG) #N10: p=0.138

par(mfrow = c(5,3))
#par(mfrow = c(1,1))

## Plot
#par(cex.main=1.7)
plot(DO_N0N10_Traits$Aboveground_Dry_Weight_g~DO_N0N10_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Belowground Biomass (g)",
     main = substitute(paste(bolditalic('D. oligosanthes'))))
abline(lm(reg_DO_N0N10_AbGvsBeG), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_DO_N0_AbGvsBeG), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_DO_N10_AbGvsBeG), lwd=3, col = "#769370", lty=1) #N10, * Sig
#legend(x="topright", legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white", xpd = TRUE)


######## DO- Trait Pair 2 ########
## Overall linear model
reg_DO_N0N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = DO_N0N10_Traits)
summary(reg_DO_N0N10_SLAvSRL) #Overall: p=0.992
## Control linear model
reg_DO_N0_SLAvSRL <- lm(SLA~SRL+(1/Block), data = DO_N0_Traits)
summary(reg_DO_N0_SLAvSRL) #Control: p=0.625
## 10N linear model
reg_DO_N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = DO_N10_Traits)
summary(reg_DO_N10_SLAvSRL) #Control: p=0.387 

## Plot
plot(DO_N0N10_Traits$SLA~DO_N0N10_Traits$SRL,
     pch=20, 
     ylab="SLA", 
     xlab="SRL",
     main = substitute(paste(bolditalic('D. oligosanthes'))))
abline(lm(reg_DO_N0N10_SLAvSRL), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_DO_N0_SLAvSRL), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_DO_N10_SLAvSRL), lwd=3, col = "#769370", lty=2) #N10, non-sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## DO- Trait Pair 3 ########
## Overall linear model
reg_DO_N0N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = DO_N0N10_Traits)
summary(reg_DO_N0N10_LfNvRtN) #Overall: p=0.116
## Control linear model
reg_DO_N0_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = DO_N0_Traits)
summary(reg_DO_N0_LfNvRtN) #Control: p=0.772
## 10N linear model
reg_DO_N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = DO_N10_Traits)
summary(reg_DO_N10_LfNvRtN) #Control: p=0.526

## Plot
plot(DO_N0N10_Traits$PercentN_AbG~DO_N0N10_Traits$PercentN_BeG,
     pch=20, 
     ylab="Leaf N (%)", 
     xlab="Root N (%)",
     main = substitute(paste(bolditalic('D. oligosanthes'))))
abline(lm(reg_DO_N0N10_LfNvRtN), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_DO_N0_LfNvRtN), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_DO_N10_LfNvRtN), lwd=3, col = "#769370", lty=2) #N10, non-sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")


############## Ambrosia psilostachya ################################

######## AP- Trait Pair 1 ########
## Overall linear model
reg_AP_N0N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AP_N0N10_Traits)
summary(reg_AP_N0N10_AbGvsBeG) #Overall: p=0.099
## Control linear model
reg_AP_N0_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AP_N0_Traits)
summary(reg_AP_N0_AbGvsBeG) #Control: p=0.327
## 10N linear model
reg_AP_N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AP_N10_Traits)
summary(reg_AP_N10_AbGvsBeG) #Control: p=0.326

## Plot
plot(AP_N0N10_Traits$Aboveground_Dry_Weight_g~AP_N0N10_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Belowground Biomass (g)",
     main = substitute(paste(bolditalic('A. psilostachya'))))
abline(lm(reg_AP_N0N10_AbGvsBeG), lwd=3, col = "Black", lty=1) #Overall, * Sig
abline(lm(reg_AP_N0_AbGvsBeG), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_AP_N10_AbGvsBeG), lwd=3, col = "#769370", lty=1) #N10, * Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## AP- Trait Pair 2 ########
## Overall linear model
reg_AP_N0N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AP_N0N10_Traits)
summary(reg_AP_N0N10_SLAvSRL) #Overall: p=0.026
## Control linear model
reg_AP_N0_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AP_N0_Traits)
summary(reg_AP_N0_SLAvSRL) #Control: p=0.71
## 10N linear model
reg_AP_N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AP_N10_Traits)
summary(reg_DO_N10_SLAvSRL) #Control: p=0.387

## Plot
plot(AP_N0N10_Traits$SLA~AP_N0N10_Traits$SRL,
     pch=20, 
     ylab="SLA", 
     xlab="SRL",
     main = substitute(paste(bolditalic('A. psilostachya'))))
abline(lm(reg_AP_N0N10_SLAvSRL), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_AP_N0_SLAvSRL), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_AP_N10_SLAvSRL), lwd=3, col = "#769370", lty=1) #N10, * Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## AP- Trait Pair 3 ########
## Overall linear model
reg_AP_N0N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AP_N0N10_Traits)
summary(reg_AP_N0N10_LfNvRtN) #Overall: p=0.003
## Control linear model
reg_AP_N0_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AP_N0_Traits)
summary(reg_AP_N0_LfNvRtN) #Control: p=0.313
## 10N linear model
reg_AP_N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AP_N10_Traits)
summary(reg_AP_N10_LfNvRtN) #10N: p=0.988

## Plot
plot(AP_N0N10_Traits$PercentN_AbG~AP_N0N10_Traits$PercentN_BeG,
     pch=20, 
     ylab="Leaf N (%)", 
     xlab="Root N (%)",
     main = substitute(paste(bolditalic('A. psilostachya'))))
abline(lm(reg_AP_N0N10_LfNvRtN), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_AP_N0_LfNvRtN), lwd=3, col = "#6E687E", lty=1) #Control, Sig
abline(lm(reg_AP_N10_LfNvRtN), lwd=3, col = "#769370", lty=1) #N10, Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")


############## Solidago missouriensis ################################

######## SM- Trait Pair 1 ########
## Overall linear model
reg_SM_N0N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SM_N0N10_Traits)
summary(reg_SM_N0N10_AbGvsBeG) #Overall: p=0.551
## Control linear model
reg_SM_N0_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SM_N0_Traits)
summary(reg_SM_N0_AbGvsBeG) #Control: p=0.714
## 10N linear model
reg_SM_N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SM_N10_Traits)
summary(reg_SM_N10_AbGvsBeG) #10N: p=0.383

## Plot
plot(SM_N0N10_Traits$Aboveground_Dry_Weight_g~SM_N0N10_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Belowground Biomass (g)",
     main = substitute(paste(bolditalic('S. missouriensis'))))
abline(lm(reg_SM_N0N10_AbGvsBeG), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_SM_N0_AbGvsBeG), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_SM_N10_AbGvsBeG), lwd=3, col = "#769370", lty=1) #N10, Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## SM- Trait Pair 2 ########
## Overall linear model
reg_SM_N0N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SM_N0N10_Traits)
summary(reg_SM_N0N10_SLAvSRL) #Overall: p=0.441
## Control linear model
reg_SM_N0_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SM_N0_Traits)
summary(reg_SM_N0_SLAvSRL) #Control: p=0.091
## 10N linear model
reg_SM_N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SM_N10_Traits)
summary(reg_SM_N10_SLAvSRL) #Control: p=0.660

## Plot
plot(SM_N0N10_Traits$SLA~SM_N0N10_Traits$SRL,
     pch=20, 
     ylab="SLA", 
     xlab="SRL",
     main = substitute(paste(bolditalic('S. missouriensis'))))
abline(lm(reg_SM_N0N10_SLAvSRL), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_SM_N0_SLAvSRL), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_SM_N10_SLAvSRL), lwd=3, col = "#769370", lty=2) #N10, non-sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## SM- Trait Pair 3 ########
## Overall Linear Model
reg_SM_N0N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SM_N0N10_Traits)
summary(reg_SM_N0N10_LfNvRtN) #Control: p=0.964
## Control linear model
reg_SM_N0_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SM_N0_Traits)
summary(reg_SM_N0_LfNvRtN) #Control: p=0.613
## 10N linear model
reg_SM_N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SM_N10_Traits)
summary(reg_SM_N10_LfNvRtN) #10N: p=0.597

## Plot
plot(SM_N0N10_Traits$PercentN_AbG~SM_N0N10_Traits$PercentN_BeG,
     pch=20, 
     ylab="Leaf N (%)", 
     xlab="Root N (%)",
     main = substitute(paste(bolditalic('S. missouriensis'))))
abline(lm(reg_SM_N0N10_LfNvRtN), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_SM_N0_LfNvRtN), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_SM_N10_LfNvRtN), lwd=3, col = "#769370", lty=2) #N10, non-sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")


############## Andropogon gerardii ################################

######## AG- Trait Pair 1 ########
## Overall linear model
reg_AG_N0N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AG_N0N10_Traits)
summary(reg_AG_N0N10_AbGvsBeG) #Control: p=0.065
## Control linear model
reg_AG_N0_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AG_N0_Traits)
summary(reg_AG_N0_AbGvsBeG) #Control: p=0.772
## 10N linear model
reg_AG_N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = AG_N10_Traits)
summary(reg_AG_N10_AbGvsBeG) #Control: p=0.341

## Plot
plot(AG_N0N10_Traits$Aboveground_Dry_Weight_g~AG_N0N10_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Belowground Biomass (g)",
     main = substitute(paste(bolditalic('A. gerardii'))))
abline(lm(reg_AG_N0N10_AbGvsBeG), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_AG_N0_AbGvsBeG), lwd=3, col = "#6E687E", lty=1) #Control, Sig
abline(lm(reg_AG_N10_AbGvsBeG), lwd=3, col = "#769370", lty=1) #N10, Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## AG- Trait Pair 2 ########
## Control linear model
reg_AG_N0N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AG_N0N10_Traits)
summary(reg_AG_N0N10_SLAvSRL) #Control: p=0.252
## Control linear model
reg_AG_N0_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AG_N0_Traits)
summary(reg_AG_N0_SLAvSRL) #Control: p=0.208
## 10N linear model
reg_AG_N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = AG_N10_Traits)
summary(reg_AG_N10_SLAvSRL) #Control: p=0.039

## Plot
plot(AG_N0N10_Traits$SLA~AG_N0N10_Traits$SRL,
     pch=20, 
     ylab="SLA", 
     xlab="SRL",
     main = substitute(paste(bolditalic('A. gerardii'))))
abline(lm(reg_AG_N0N10_SLAvSRL), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_AG_N0_SLAvSRL), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_AG_N10_SLAvSRL), lwd=3, col = "#769370", lty=2) #N10, non-Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## AG- Trait Pair 3 ########
## Overall linear model
reg_AG_N0N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AG_N0N10_Traits)
summary(reg_AG_N0N10_LfNvRtN) #Control: p=0.068
## Control linear model
reg_AG_N0_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AG_N0_Traits)
summary(reg_AG_N0_LfNvRtN) #Control: p=0.747
## 10N linear model
reg_AG_N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = AG_N10_Traits)
summary(reg_AG_N10_LfNvRtN) #10N: p=0.231

## Plot
plot(AG_N0N10_Traits$PercentN_AbG~AG_N0N10_Traits$PercentN_BeG,
     pch=20, 
     ylab="Leaf N (%)", 
     xlab="Root N (%)",
     main = substitute(paste(bolditalic('A. gerardii'))))
abline(lm(reg_AG_N0N10_LfNvRtN), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_AG_N0_LfNvRtN), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_AG_N10_LfNvRtN), lwd=3, col = "#769370", lty=2) #N10, non-Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")



############## Sorghastum nutans ################################

######## SN- Trait Pair 1 ########
## Control linear model
reg_SN_N0N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SN_N0N10_Traits)
summary(reg_SN_N0N10_AbGvsBeG) #Control: p=0.133
## Control linear model
reg_SN_N0_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SN_N0_Traits)
summary(reg_SN_N0_AbGvsBeG) #Control: p=0.001 *
## 10N linear model
reg_SN_N10_AbGvsBeG <- lm(Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Block), data = SN_N10_Traits)
summary(reg_SN_N10_AbGvsBeG) #Control: p=0.664

## Plot
plot(SN_N0N10_Traits$Aboveground_Dry_Weight_g~SN_N0N10_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Belowground Biomass (g)",
     main = substitute(paste(bolditalic('S. nutans'))))
abline(lm(reg_SN_N0N10_AbGvsBeG), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_SN_N0_AbGvsBeG), lwd=3, col = "#6E687E", lty=1) #Control, Sig
abline(lm(reg_SN_N10_AbGvsBeG), lwd=3, col = "#769370", lty=2) #N10, non-Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## SN- Trait Pair 2 ########
## Overall linear model
reg_SN_N0N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SN_N0N10_Traits)
summary(reg_SN_N0N10_SLAvSRL) #Control: p=0.209
## Control linear model
reg_SN_N0_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SN_N0_Traits)
summary(reg_SN_N0_SLAvSRL) #Control: p=0.662
## 10N linear model
reg_SN_N10_SLAvSRL <- lm(SLA~SRL+(1/Block), data = SN_N10_Traits)
summary(reg_SM_N10_SLAvSRL) #Control: p=0.660

## Plot
plot(SN_N0N10_Traits$SLA~SN_N0N10_Traits$SRL,
     pch=20, 
     ylab="SLA", 
     xlab="SRL",
     main = substitute(paste(bolditalic('S. nutans'))))
abline(lm(reg_SN_N0N10_SLAvSRL), lwd=3, col = "Black", lty=2) #Overall, non-sig
abline(lm(reg_SN_N0_SLAvSRL), lwd=3, col = "#6E687E", lty=2) #Control, non-sig
abline(lm(reg_SN_N10_SLAvSRL), lwd=3, col = "#769370", lty=2) #N10, non-sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

######## SN- Trait Pair 3 ########
## Overall linear model
reg_SN_N0N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SN_N0N10_Traits)
summary(reg_SN_N0N10_LfNvRtN) #Control: p=0.001
## Control linear model
reg_SN_N0_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SN_N0_Traits)
summary(reg_SN_N0_LfNvRtN) #Control: p=0.515
## 10N linear model
reg_SN_N10_LfNvRtN <- lm(PercentN_AbG~PercentN_BeG+(1/Block), data = SN_N10_Traits)
summary(reg_SN_N10_LfNvRtN) #10N: p=0.037 

## Plot
plot(SN_N0N10_Traits$PercentN_AbG~SN_N0N10_Traits$PercentN_BeG,
     pch=20, 
     ylab="Leaf N (%)", 
     xlab="Root N (%)",
     main = substitute(paste(bolditalic('S. nutans'))))
abline(lm(reg_SN_N0N10_LfNvRtN), lwd=3, col = "Black", lty=1) #Overall, Sig
abline(lm(reg_SN_N0_LfNvRtN), lwd=3, col = "#6E687E", lty=1) #Control, Sig
abline(lm(reg_SN_N10_LfNvRtN), lwd=3, col = "#769370", lty=1) #N10, Sig
#legend("topright", inset = 0.01, legend = c("Overall", "Cntrl, 0g N", "Trt, 10g N"), fill = c("Black","#6E687E","#769370"), bg = "white")

