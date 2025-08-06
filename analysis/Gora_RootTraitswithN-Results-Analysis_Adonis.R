#### RootTraitswithN Results Analysis- Adonis ####
###  Gora_RootTraitswithN-Results-Analysis_Adonis.R
###  by Sarah Gora
###  Date created: March 24, 2022

# set WD
setwd("~/Desktop/Thesis_Data/R Code")

#load packages
install.packages("readxl")
library(readxl)
install.packages("readr")
library(readr)
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
install.packages("devtools")
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis", force = TRUE)
library(pairwiseAdonis)



# Load data
# use trait drop data 

# TRAIT DROP DATA
All_Traits_O_DroppedTraits <- read_csv("~/Desktop/All_Traits_O_DroppedTraits.csv")
View(All_Traits_O_DroppedTraits)

# Subset out TRAIT DROP data, by species 
DO_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Dichanthelium_oligosanthes")
AP_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Ambrosia_psilostachya")
SM_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Solidago_missouriensis")
AG_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Andropogon_gerardii")
SN_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Sorghastrum_nutans")


#### Subset out N0N10 data for each species 
DO_N0N10_Traits_O_Drop <- subset(DO_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
AP_N0N10_Traits_O_Drop <- subset(AP_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
SM_N0N10_Traits_O_Drop <- subset(SM_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
AG_N0N10_Traits_O_Drop <- subset(AG_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
SN_N0N10_Traits_O_Drop <- subset(SN_Traits_O_Drop,Treatment2 == "0"| Treatment2== "10")

#### Subset out Change for each species
DO_Ch_Traits_O_Drop <- subset(DO_Traits_O_Drop, Experiment=="Change")
AP_Ch_Traits_O_Drop <- subset(AP_Traits_O_Drop, Experiment=="Change")
SM_Ch_Traits_O_Drop <- subset(SM_Traits_O_Drop, Experiment=="Change")
AG_Ch_Traits_O_Drop <- subset(AG_Traits_O_Drop, Experiment=="Change")
SN_Ch_Traits_O_Drop <- subset(SN_Traits_O_Drop, Experiment=="Change")





################# Subset out only the columns of data for Ordination analysis

#### Select the columns of traits you want from the N0N10 Trait Data
Rip_DO_Traits <- subset(DO_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AP_Traits <- subset(AP_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SM_Traits <- subset(SM_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AG_Traits <- subset(AG_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SN_Traits <- subset(SN_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)

#### Select the columns of traits you want from the Change Trait Data
Rip_DO_Ch_Traits <- subset(DO_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AP_Ch_Traits <- subset(AP_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SM_Ch_Traits <- subset(SM_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AG_Ch_Traits <- subset(AG_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SN_Ch_Traits <- subset(SN_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)




# Run adonis 

# POST HOC TEST: Adonis pair-wise test
# only need to use for Change






#### N0N10 ######

################## DO N0N10 Adonis #######################
# run PERMANOVA, N0N10 ordination data with traits dropped for DO
PERMANOVA <-adonis(Rip_DO_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.08991, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_DO_Traits ~ Treatment, 
                                           permutations = 1000, 
                                           data = DO_N0N10_Traits_O_Drop, 
                                           method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.08577622



################## AP N0N10 Adonis #######################
# run PERMANOVA, N0N10 ordination data with traits dropped for AP
PERMANOVA <-adonis(Rip_AP_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.2967, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AP_Traits ~ Treatment, 
                                           permutations = 1000, 
                                           data = AP_N0N10_Traits_O_Drop, 
                                           method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.304, not significant



################## SM N0N10 Adonis #######################
# run PERMANOVA, N0N10 ordination data with traits dropped for SM
PERMANOVA <-adonis(Rip_SM_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.1499, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SM_Traits ~ Treatment, 
                                           permutations = 1000, 
                                           data = SM_N0N10_Traits_O_Drop, 
                                           method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.1525804, not significant




################## AG N0N10 Adonis #######################
# run PERMANOVA, N0N10 ordination data with traits dropped for AG
PERMANOVA <-adonis(Rip_AG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p<0.001, * significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AG_Traits ~ Treatment, 
                                           permutations = 1000, 
                                           data = AG_N0N10_Traits_O_Drop, 
                                           method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.001, * significant



################## SN N0N10 Adonis #######################
# run PERMANOVA, N0N10 ordination data with traits dropped for AG
PERMANOVA <-adonis(Rip_SN_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.2787, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SN_Traits ~ Treatment, 
                                           permutations = 1000, 
                                           data = SN_N0N10_Traits_O_Drop, 
                                           method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.2948981, not significant











#### Change ######

################## DO Change Adonis #######################
#run PERMANOVA, All-traits_O_Trait_Drop (Change) Dataset for DO
PERMANOVA <-adonis(Rip_DO_Ch_Traits~Treatment, data = DO_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.1998

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_DO_Ch_Traits ~ Treatment, permutations = 1000, data = DO_Ch_Traits_O_Drop, method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.1859281


# POST HOC TEST: Adonis Pair-wise test
pair.mod <- pairwise.adonis(Rip_DO_Ch_Traits,factors=DO_Ch_Traits_O_Drop$Treatment2)
pair.mod

# these are adjusted p-values
# 0 v 2.5: p= 1.000
# 0 v 10: p= 1.000
# 0 v 20: p= 1.000



################## AP Change Adonis #######################
#run PERMANOVA, All-traits_O_Trait_Drop (Change) Dataset for AP
PERMANOVA <-adonis(Rip_AP_Ch_Traits~Treatment, data = AP_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.0969, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AP_Ch_Traits ~ Treatment, permutations = 1000, data = AP_Ch_Traits_O_Drop, method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.1074615


# POST HOC TEST: Adonis Pair-wise test
pair.mod <- pairwise.adonis(Rip_AP_Ch_Traits,factors=AP_Ch_Traits_O_Drop$Treatment2)
pair.mod

# these are adjusted p-values
# 0 v 2.5: p= 1.000
# 0 v 10: p= 1.000
# 0 v 20: p= 0.156



################## SM Change Adonis #######################
#run PERMANOVA, All-traits_O_Trait_Drop (Change) Dataset for SM
PERMANOVA <-adonis(Rip_SM_Ch_Traits~Treatment, data = SM_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.7473, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SM_Ch_Traits ~ Treatment, permutations = 1000, data = SM_Ch_Traits_O_Drop, method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.7278591


# POST HOC TEST: Adonis Pair-wise test
pair.mod <- pairwise.adonis(Rip_SM_Ch_Traits,factors=SM_Ch_Traits_O_Drop$Treatment2)
pair.mod

# these are adjusted p-values
# 0 v 2.5: p= 1.000
# 0 v 10: p= 1.000
# 0 v 20: p= 1.000




################## AG Change Adonis #######################
#run PERMANOVA, All-traits_O_Trait_Drop (Change) Dataset for AG
PERMANOVA <-adonis(Rip_AG_Ch_Traits~Treatment, data = AG_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.06194, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AG_Ch_Traits ~ Treatment, permutations = 1000, data = AG_Ch_Traits_O_Drop, method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.05038462


# POST HOC TEST: Adonis Pair-wise test
pair.mod <- pairwise.adonis(Rip_AG_Ch_Traits,factors=AG_Ch_Traits_O_Drop$Treatment2)
pair.mod

# these are adjusted p-values
# 0 v 2.5: p= 0.480
# 0 v 10: p= 0.084
# 0 v 20: p= 1.000



################## SN Change Adonis #######################
#run PERMANOVA, All_Traits_O_Trait_Drop (Change) Dataset for AG
PERMANOVA <-adonis(Rip_SN_Ch_Traits~Treatment, data = SN_Ch_Traits_O_Drop, 
                   permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.2877, not significant

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) PERMANOVA)
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.2877123


# POST HOC TEST: Adonis Pair-wise test
pair.mod <- pairwise.adonis(Rip_SN_Ch_Traits,factors=SN_Ch_Traits_O_Drop$Treatment2)
pair.mod

# Adjusted p-values
# 0 v 2.5: p= 1.000
# 0 v 10: p= 0.774
# 0 v 20: p= 1.000


