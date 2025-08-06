#### Thesis Results Analysis ####
###  Gora_Thesis_Results_Analysis.R
###  by Sarah Gora
###  Date created: Dec 5, 2021

# set WD 

setwd("~/Desktop/Thesis_Data")

#load packages
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)
library(RColorBrewer)
library(dplyr)
install.packages("apaTables")
library(apaTables)
install.packages("ez")
library(ez)


#load data
library(readr)
SLA_Datasheet <- read_csv("SLA_Datasheet.csv")
View(SLA_Datasheet)
SLA_Datasheet <- read_csv("SLA_Datasheet_Avg.csv")
View(SLA_Datasheet)

# Check to make sure data is in long format



# Run histograms to check out outliers

install.packages("rstatix")
library(rstatix)

outliers<- identify_outliers(SLA_Datasheet,SLA)
?identify_outliers()
View(outliers)

# Mixed model ANOVA
# nutrient treatment = fixed effect
# experiment = random effect
# plot = random effect

# Separate model for each species
# Separate model for each trait

# adjusted p-values for multiple comparisons: Benjamini-Hochberg





### ANOVA: DO, SLA ###
# Is there variance in the SLA of DO in the different nitrogen treatments? 
DO <- SLA_Datasheet %>%
  filter(Species=="Dichanthelium_oligosanthes")

#ANOVA test
summary( aov(DO$SLA~DO$Treatment) )
# Conclusion: p=0.33013 No sig difference in SLA of DO for different N treatments


### ANOVA: AP, SLA ###
# Is there variance in the SLA of AP in the different nitrogen treatments? 
AP <- SLA_Datasheet %>%
  filter(Species=="Ambrosia_psilostachya")

summary( aov(AP$SLA~AP$Treatment) )
# Conclusion: p=0.0027373 There is moderate significant difference in SLA 
# of AP for different N treatments

TukeyHSD(aov(AP$SLA~AP$Treatment))
# Conclusion: The most significant difference is between the 
# Control and 20g N  (p=0.0023795)



### ANOVA: SM, SLA ###
# Is there variance in the SLA of SM in the different nitrogen treatments? 
SM <- SLA_Datasheet %>%
  filter(Species=="Solidago_missouriensis")

summary( aov(SM$SLA~SM$Treatment) )
#Conclusion: p=0.3432, No sig difference in SLA of SM for different N treatments


### ANOVA: AG, SLA ###
# Is there variance in the SLA of AG in the different nitrogen treatments? 
AG <- SLA_Datasheet %>%
  filter(Species=="Andropogon_gerardii")

summary( aov(AG$SLA~AG$Treatment) )
#Conclusion: p=0.00090845, there is sig difference in SLA of AG for different N treatments

TukeyHSD(aov(AG$SLA~AG$Treatment))
# Conclusion: The most significant difference is between the 
# Control and 10g N  (p=0.0003911139)


### ANOVA: SN, SLA ###
# Is there variance in the SLA of SN in the different nitrogen treatments? 
SN <- SLA_Datasheet %>%
  filter(Species=="Sorghastrum_nutans")

summary( aov(SN$SLA~SN$Treatment) )
#Conclusion: p=0.55436, no sig difference in SLA of SN for different N treatments



#adjust p-values
pvalues <- c(0.33013, 0.0027373, 0.3432, 0.00090845, 0.55436 )
p.adjust(pvalues,method="BH")
#           0.42900000 0.00684325 0.42900000 0.00454225 0.55436000





# see what are the relationship between SLA and other variables 
# take the sum of these FIRST!

# linear regression models, pearsons, other 










#### ALL Traits ####

#load data 
library(readr)
All_Traits <- read_csv("All_Traits_Datasheet.csv")
View(All_Traits_Datasheet)

#Filter out each Species 
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





#### Test if there is difference in Trait 2, Aboveground Weight ####

# Is there variance in the Aboveground Weight of DO in the different nitrogen treatments?
test1 <- summary( aov(DO_Traits$Aboveground_Dry_Weight_g~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$Aboveground_Dry_Weight_g~DO_Traits$Treatment))
#Conclusion: p=0.000996, there is sig difference, most significant is Control vs 10g p=0.0010

# Is there variance in the Aboveground Weight of AP in the different nitrogen treatments?
test2 <- summary( aov(AP_Traits$Aboveground_Dry_Weight_g~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$Aboveground_Dry_Weight_g~AP_Traits$Treatment))
#Conclusion: p=0.00675; there is sig, most significnace between Control and 20g p=0.0031

# Is there variance in the Aboveground Weight of SM in the different nitrogen treatments?
test3 <- summary( aov(SM_Traits$Aboveground_Dry_Weight_g~SM_Traits$Treatment) )
test1 <- TukeyHSD(aov(SM_Traits$Aboveground_Dry_Weight_g~SM_Traits$Treatment))
# Conclusion: p= 0.00213; there is significant difference, most significance between Control and 20g, p=0.0148137

# Is there variance in the Aboveground Weight of SN in the different nitrogen treatments?
test4 <- summary( aov(SN_Traits$Aboveground_Dry_Weight_g~SN_Traits$Treatment) )
# Conclusion: p= 0.301; no significant difference 

# Is there variance in the Aboveground Weight of AG in the different nitrogen treatments?
test5 <- summary( aov(AG_Traits$Aboveground_Dry_Weight_g~AG_Traits$Treatment) )
# Conclusion: p= 0.117; no significant difference 


#adjust p-values
pvalues <- c(0.000996, 0.00675, 0.00213, 0.301, 0.117)
p.adjust(pvalues,method="BH")
#            0.004980 0.011250 0.005325 0.301000 0.146250




#### Test if there is difference in Trait 3, Leaf Number ####

# Is there variance in the Leaf Number of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Leaf_Number~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$Leaf_Number~DO_Traits$Treatment))
#Conclusion: p=0.0026, there is sig difference, most significant is Control vs 10g p=0.0019978

# Is there variance in the Leaf Number of AP in the different nitrogen treatments?
summary( aov(AP_Traits$Leaf_Number~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$Leaf_Number~AP_Traits$Treatment))
#Conclusion: p=4.28e-05; there is very sig difference; most significance between Control and 20g p=0.0000222

# Is there variance in the Leaf Number of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Leaf_Number~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$Leaf_Number~SM_Traits$Treatment))
# Conclusion: p= 0.135; not significant
###### IS THERE DATA MISSING? 6 rows??? 

# Is there variance in the Leaf Number of SN in the different nitrogen treatments?
summary( aov(SN_Traits$Leaf_Number~SN_Traits$Treatment) )
# Conclusion: p= 0.826; no significant difference 

# Is there variance in the Leaf Number of AG in the different nitrogen treatments?
summary( aov(AG_Traits$Leaf_Number~AG_Traits$Treatment) )
# Conclusion: p= 0.0794; no significant difference 

#adjust p-values
pvalues <- c(0.0026,4.28e-05, 0.135, 0.826, 0.0794)
p.adjust(pvalues,method="BH")
# Adjusted: 0.0065000 0.0002140 0.1687500 0.8260000 0.1323333





#### Test if there is difference in Trait 4, Number of Senescing Leaves ####

# Is there variance in the Number of Senescing Leaves of DO in the different nitrogen treatments?
summary( aov(DO_Traits$S_Leaf_Stage~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$S_Leaf_Stage~DO_Traits$Treatment))
#Conclusion: p=0.0092, there is sig difference, most significant is Control vs 20g p=0.0167

# Is there variance in the Number of Senescing Leaves of AP in the different nitrogen treatments?
summary( aov(AP_Traits$S_Leaf_Stage~AP_Traits$Treatment) )
#Conclusion: p=0.486; there is no sig difference

# Is there variance in the Number of Senescing Leaves of SM in the different nitrogen treatments?
summary( aov(SM_Traits$S_Leaf_Stage~SM_Traits$Treatment) )
# Conclusion: p= 0.162; not significant
###### IS THERE DATA MISSING? 7 rows??? 

# Is there variance in the Number of Senescing Leaves of SN in the different nitrogen treatments?
summary( aov(SN_Traits$S_Leaf_Stage~SN_Traits$Treatment) )
# Conclusion: p= 0.976; no significant difference 

# Is there variance in the Number of Senescing Leaves of AG in the different nitrogen treatments?
summary( aov(AG_Traits$S_Leaf_Stage~AG_Traits$Treatment) )
# Conclusion: p= 0.0918; no significant difference 




### Test if there is difference in trait 4, Number of Emerging Leaves ### 

# Is there variance in the Number of Emerging Leaves of DO in the different nitrogen treatments?
summary( aov(DO_Traits$E_Leaf_Stage~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$E_Leaf_Stage~DO_Traits$Treatment))
#Conclusion: p=0.00294, there is sig difference, most significant is Control vs 10g p=0.0039472

# Is there variance in the Number of Emerging Leaves of AP in the different nitrogen treatments?
summary( aov(AP_Traits$E_Leaf_Stage~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$E_Leaf_Stage~AP_Traits$Treatment))
#Conclusion: p=0.000235, there is sig difference, most significant is Control vs 20g p=0.0001638

# Is there variance in the Number of Emerging Leaves of SM in the different nitrogen treatments?
summary( aov(SM_Traits$E_Leaf_Stage~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$E_Leaf_Stage~SM_Traits$Treatment))
View(SM_Traits)
#Conclusion: p=0.0023, there is sig difference, most significant is Control vs 20g p=0.0021333
###### DATA MISSING? 22 rows??? 

# Is there variance in the Number of Emerging Leaves of SN in the different nitrogen treatments?
summary( aov(SN_Traits$E_Leaf_Stage~SN_Traits$Treatment) )
# Conclusion: p= 0.502; no significant difference 

# Is there variance in the Number of Emerging Leaves of AG in the different nitrogen treatments?
summary( aov(AG_Traits$E_Leaf_Stage~AG_Traits$Treatment) )
# Conclusion: p= 0.619; no significant difference 




### Test if there is difference in trait 5, Number of Fully Developed Leaves ### 

# Is there variance in the Number of Fully Dev Leaves of DO in the different nitrogen treatments?
summary( aov(DO_Traits$F_Leaf_Stage~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$F_Leaf_Stage~DO_Traits$Treatment))
#Conclusion: p=0.029, there is sig difference, most significant is Control vs 10g p=0.0155334

# Is there variance in the Number of Fully Dev Leaves of AP in the different nitrogen treatments?
summary( aov(AP_Traits$F_Leaf_Stage~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$F_Leaf_Stage~AP_Traits$Treatment))
#Conclusion: p=0.000144, there is sig difference, most significant is Control vs 20g p=0.0000711

# Is there variance in the Number of Fully Dev Leaves of SM in the different nitrogen treatments?
summary( aov(SM_Traits$F_Leaf_Stage~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$F_Leaf_Stage~SM_Traits$Treatment))
#Conclusion: p=0.245; no significant difference 
###### DATA MISSING? 7 rows??? 

# Is there variance in the Number of Fully Dev Leaves of SN in the different nitrogen treatments?
summary( aov(SN_Traits$F_Leaf_Stage~SN_Traits$Treatment) )
# Conclusion: p= 0.785; no significant difference 

# Is there variance in the Number of Fully Dev Leaves of AG in the different nitrogen treatments?
summary( aov(AG_Traits$F_Leaf_Stage~AG_Traits$Treatment) )
# Conclusion: p= 0.259; no significant difference






### Test if there is difference in trait 6, Number of Flowers ### 

# Is there variance in the Number of Flowers of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Flower_Number~DO_Traits$Treatment) )
#Conclusion: p=0.789; no significant difference

# Is there variance in the Number of Flowers of AP in the different nitrogen treatments?
#NOTE:  Ambrosia mostly NOT flowering

# Is there variance in the Number of Flowers of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Flower_Number~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$Flower_Number~SM_Traits$Treatment))
#Conclusion: p=0.000269, there is sig difference, most significant is Control vs 20g p=0.0003765

# Is there variance in the Number of Flowers of SN in the different nitrogen treatments?
#NOTE:  Sorgh mostly NOT flowering

# Is there variance in the Number of Flowers of AG in the different nitrogen treatments?
#NOTE:  Andro mostly NOT flowering




### Test if there is difference in Number of Senescing Flowers ### 
# NOTE: there were not enough senecsing flowers to count 




### Test if there is difference in trait 7, Number of Emerging Flowers ### 

# Is there variance in the Number of Emerging Flowers of DO in the different nitrogen treatments?
summary( aov(DO_Traits$E_Flower_Stage~DO_Traits$Treatment) )
#Conclusion: 
# NOTES: ERROR With NAs - need to fix

# Is there variance in the Number of Flowers of AP in the different nitrogen treatments?
#NOTE:  Ambrosia mostly NOT flowering

# Is there variance in the Number of Flowers of SM in the different nitrogen treatments?
summary( aov(SM_Traits$E_Flower_Stage~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$E_Flower_Stage~SM_Traits$Treatment))
#Conclusion: 
# NOTES: ERROR With NAs - need to fix

# Is there variance in the Number of Flowers of SN in the different nitrogen treatments?
#NOTE:  Sorgh mostly NOT flowering

# Is there variance in the Number of Flowers of AG in the different nitrogen treatments?
#NOTE:  Andro mostly NOT flowering






### Test if there is difference in trait 8, Number of Fully Dev Flowers ### 

# Is there variance in the Number of Fully Dev Flowers of DO in the different nitrogen treatments?
summary( aov(DO_Traits$F_Flower_Stage~DO_Traits$Treatment) )
#Conclusion: 
# NOTES: ERROR With NAs - need to fix

# Is there variance in the Number of Fully Dev Flowers of AP in the different nitrogen treatments?
#NOTE:  Ambrosia mostly NOT flowering

# Is there variance in the Number of Flowers of SM in the different nitrogen treatments?
summary( aov(SM_Traits$F_Flower_Stage~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$F_Flower_Stage~SM_Traits$Treatment))
#Conclusion: 
# NOTES: ERROR With NAs - need to fix

# Is there variance in the Number of Fully Dev Flowers of SN in the different nitrogen treatments?
#NOTE:  Sorgh mostly NOT flowering

# Is there variance in the Number of Fully Dev Flowers of AG in the different nitrogen treatments?
#NOTE:  Andro mostly NOT flowering



### Test if there is difference in trait 9, Plant Height ### 

# Is there variance in the Plant Height of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Plant_Height_cm~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$Plant_Height_cm~DO_Traits$Treatment))
#Conclusion: p=0.0152, there is sig difference, most significant is Control vs 10g p=0.0142798

# Is there variance in the Plant Height of AP in the different nitrogen treatments?
summary( aov(AP_Traits$Plant_Height_cm~AP_Traits$Treatment) )
#Conclusion: p=0.0269, there is sig difference

# Is there variance in the Plant Height of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Plant_Height_cm~SM_Traits$Treatment) )
TukeyHSD(aov(SM_Traits$Plant_Height_cm~SM_Traits$Treatment))
#Conclusion: p=0.0032; there is sig difference
###### DATA MISSING? 6 rows??? 

# Is there variance in the Plant Height of SN in the different nitrogen treatments?
summary( aov(SN_Traits$Plant_Height_cm~SN_Traits$Treatment) )
TukeyHSD(aov(SN_Traits$Plant_Height_cm~SN_Traits$Treatment) )
# Conclusion: p= 0.0204; there is significant difference, most significant is Control vs 20g p=0.0174746


# Is there variance in the Plant Height of AG in the different nitrogen treatments?
summary( aov(AG_Traits$Plant_Height_cm~AG_Traits$Treatment) )
# Conclusion: p= 0.357; no significant difference

## ADJUST P-VALUES ... ???? 
p <- c(0.0152, 0.0269, 0.0032, 0.0204, 0.357)
p.adjust(p, method = "hochberg", n = length(p))
# 0.0538 0.0538 0.0160 0.0538 0.3570





### Test if there is difference in trait 10, Plant Area ### 

# Is there variance in the Plant Area of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Plant_Area~DO_Traits$Treatment) )
TukeyHSD(aov(DO_Traits$Plant_Area~DO_Traits$Treatment))
#Conclusion: p=0.00769, there is sig difference, most significant is Control vs 10g p=0.0036825

# Is there variance in the Plant Area of AP in the different nitrogen treatments?
summary( aov(AP_Traits$Plant_Area~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$Plant_Area~AP_Traits$Treatment))
#Conclusion: p=0.00849, there is sig difference, most significant is Control vs 20g p=0.0245492

# Is there variance in the Plant Area of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Plant_Area~SM_Traits$Treatment) )
#Conclusion: p=0.0174, there is significant difference 

# Is there variance in the Plant Area of SN in the different nitrogen treatments?
summary( aov(SN_Traits$Plant_Area~SN_Traits$Treatment) )
# Conclusion: p= 0.133; no significant difference 

# Is there variance in the Plant Area of AG in the different nitrogen treatments?
summary( aov(AG_Traits$Plant_Area~AG_Traits$Treatment) )
# Conclusion: p= 0.65; no significant difference






### Test if there is difference in trait 11, Belowground Weight (of Focal species) ### 

# Is there variance in the Belowground Weight of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Focal_Root_Weight_g~DO_Traits$Treatment) )
#Conclusion: p=0.273, no sig difference

# Is there variance in the Belowground Weight of AP in the different nitrogen treatments?
summary( aov(AP_Traits$Focal_Root_Weight_g~AP_Traits$Treatment) )
TukeyHSD(aov(AP_Traits$Focal_Root_Weight_g~AP_Traits$Treatment))
#Conclusion: p=0.0491, there is sig difference, most significant is Control vs 20g p=0.0437492

# Is there variance in the Belowground Weight of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Focal_Root_Weight_g~SM_Traits$Treatment) )
#Conclusion: p=0.0722, no significant difference 

# Is there variance in the Belowground Weight of SN in the different nitrogen treatments?
summary( aov(SN_Traits$Focal_Root_Weight_g~SN_Traits$Treatment) )
# Conclusion: p= 0.723, no significant difference 

# Is there variance in the Belowground Weight of AG in the different nitrogen treatments?
summary( aov(AG_Traits$Focal_Root_Weight_g~AG_Traits$Treatment) )
# Conclusion: p= 0.549; no significant difference





### Test if there is difference in trait 12, Total Belowground Weight ### 

# Is there variance in the Total Belowground Weight of DO in the different nitrogen treatments?
summary( aov(DO_Traits$Total_Belowground_Weight_g~DO_Traits$Treatment) )
#Conclusion: p=0.367, no sig difference

# Is there variance in the Total Belowground Weight of AP in the different nitrogen treatments?
summary( aov(AP_Traits$Total_Belowground_Weight_g~AP_Traits$Treatment) )
#Conclusion: p=0.218, no sig difference

# Is there variance in the Total Belowground Weight of SM in the different nitrogen treatments?
summary( aov(SM_Traits$Total_Belowground_Weight_g~SM_Traits$Treatment) )
#Conclusion: p=0.0604, no significant difference 

# Is there variance in the Total Belowground Weight of SN in the different nitrogen treatments?
summary( aov(SN_Traits$Total_Belowground_Weight_g~SN_Traits$Treatment) )
# Conclusion: p= 0.163, no significant difference 

# Is there variance in the Total Belowground Weight of AG in the different nitrogen treatments?
summary( aov(AG_Traits$Total_Belowground_Weight_g~AG_Traits$Treatment) )
# Conclusion: p= 0.644; no significant difference





### Test if there is difference in trait 13, Specific Root Length ### 

# Is there variance in the SRL of DO in the different nitrogen treatments?
summary( aov(DO_Traits$SRL~DO_Traits$Treatment) )
#Conclusion: p=0.666, no sig difference

# Is there variance in the SRL of AP in the different nitrogen treatments?
summary( aov(AP_Traits$SRL~AP_Traits$Treatment) )
#Conclusion: p=0.425, no sig difference

# Is there variance in the SRL of SM in the different nitrogen treatments?
summary( aov(SM_Traits$SRL~SM_Traits$Treatment) )
#Conclusion: p=0.394, no significant difference 
### DATA MISSING 

# Is there variance in the SRL of SN in the different nitrogen treatments?
summary( aov(SN_Traits$SRL~SN_Traits$Treatment) )
# Conclusion: p= 0.136, no significant difference 

# Is there variance in the SRL of AG in the different nitrogen treatments?
summary( aov(AG_Traits$SRL~AG_Traits$Treatment) )
# Conclusion: p= 0.206; no significant difference


p <- c(0.666, 0.425,0.394,0.136,0.206)
p.adjust(p, method = "hochberg", n = length(p))






### SQ1 ####

# Regression 1: Total Belowground Weight against Focal Root Weight for DO
# linear

plot(DO_Traits$Focal_Root_Weight_g, DO_Traits$Total_Belowground_Weight_g,
     pch = 16, cex= 1, col="darkgreen",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "D. oligosanthese")
abline(lm(DO_Traits$Total_Belowground_Weight_g ~ DO_Traits$Focal_Root_Weight_g),col="red", lty=7, lwd=2)
text(locator(), labels = c("p= 0.8806"))
text(locator(), labels = c("r= 0.02226508"))

cor.test(DO_Traits$Focal_Root_Weight_g, DO_Traits$Total_Belowground_Weight_g, method="pearson")
# p= 0.8806, r=0.02226508
# Nearly no linear relationship

#non-linear test
cor.test(DO_Traits$Focal_Root_Weight_g, DO_Traits$Total_Belowground_Weight_g, method="spearman")
#Conclusion: p-value = 0.5532, rho = 0.08749457 


#change to numeric
is.numeric(DO_Traits$Focal_Root_Weight_g)
DO_Traits$Focal_Root_Weight_g <- as.numeric(as.character(DO_Traits$Focal_Root_Weight_g))




# Regression 1: Total Belowground Weight against Focal Root Weight for AP

plot(AP_Traits$Focal_Root_Weight_g, AP_Traits$Total_Belowground_Weight_g,
     pch = 16, cex= 1, col="darkgreen",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "A. psylostachia")
abline(lm(AP_Traits$Total_Belowground_Weight_g ~ AP_Traits$Focal_Root_Weight_g),col="red", lty=7, lwd=2)
text(locator(), labels = c("p= 0.3943"))
text(locator(), labels = c("r= 0.1286265"))

#linear test
cor.test(AP_Traits$Focal_Root_Weight_g, AP_Traits$Total_Belowground_Weight_g, method="pearson")
#Conclusion:p= 0.3943 (r not significant), r= 0.1286265

#non-linear test
cor.test(AP_Traits$Focal_Root_Weight_g, AP_Traits$Total_Belowground_Weight_g, method="spearman")
#Conclusion: p = 0.5507 (rho not significant) , rho =  0.09028956 



# Regression 1: Total Belowground Weight against Focal Root Weight for SM
plot(SM_Traits$Focal_Root_Weight_g, SM_Traits$Total_Belowground_Weight_g,
     pch = 16, cex= 1, col="darkgreen",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "S. missouriensis")
abline(lm(SM_Traits$Total_Belowground_Weight_g ~ SM_Traits$Focal_Root_Weight_g),col="red", lty=7, lwd=2)
text(locator(), labels = c("p= 0.4138"))
text(locator(), labels = c("r= 0.129483"))

#linear test
cor.test(SM_Traits$Focal_Root_Weight_g, SM_Traits$Total_Belowground_Weight_g, method="pearson")
#Conclusion:p= 0.4138 (r not significant), r= 0.129483

#non-linear test
cor.test(SM_Traits$Focal_Root_Weight_g, SM_Traits$Total_Belowground_Weight_g, method="pearson")
# p= 0.4138 (r is not significant), r=0.129483



# Regression 1: Total Belowground Weight against Focal Root Weight for AG
plot(AG_Traits$Focal_Root_Weight_g, AG_Traits$Total_Belowground_Weight_g,
     pch = 16, cex= 1, col="darkgreen",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "A. gerardii")
abline(lm(AG_Traits$Total_Belowground_Weight_g ~ AG_Traits$Focal_Root_Weight_g),col="red", lty=7, lwd=2)
text(locator(), labels = c("p= 0.01635"))
text(locator(), labels = c("r= 0.348553"))

cor.test(AG_Traits$Focal_Root_Weight_g, AG_Traits$Total_Belowground_Weight_g, method="pearson")
# p= 0.01635 (r is significant), r=0.348553
# Conclusion: Slightly positive relationship where as AG Total Belowground increases, so does Focal Root Biomass

#non-linear test
cor.test(AG_Traits$Focal_Root_Weight_g, AG_Traits$Total_Belowground_Weight_g, method="pearson")
# p= 0.5591 (r is not significant), r= -0.08740395 




# Regression 1: Total Belowground Weight against Focal Root Weight for SN
plot(SN_Traits$Focal_Root_Weight_g, SN_Traits$Total_Belowground_Weight_g,
     pch = 16, cex= 1, col="darkgreen",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "S. nutans")
abline(lm(SN_Traits$Total_Belowground_Weight_g ~ SN_Traits$Focal_Root_Weight_g),col="red", lty=7, lwd=2)
text(locator(), labels = c("p= 0.852"))
text(locator(), labels = c("r= 0.02765121"))

cor.test(SN_Traits$Focal_Root_Weight_g, SN_Traits$Total_Belowground_Weight_g, method="pearson")
# p= 0.852 (r is not significant), r=0.02765121 


#change to numeric
is.numeric(SN_Traits$Focal_Root_Weight_g)
SN_Traits$Focal_Root_Weight_g <- as.numeric(as.character(SN_Traits$Focal_Root_Weight_g))

#non-linear test
cor.test(SN_Traits$Focal_Root_Weight_g, SN_Traits$Total_Belowground_Weight_g, method="spearman")
#Conclusion: p-value = 0.7678 (rho not significant); rho = -0.04363873 


#Combine all graphs into one 
par(mfrow=c(3,2))




####  Regression 2: SRL against Focal Root Weight ####


# Regression 2: SRL against Focal Root Weight for DO
plot(DO_Traits$Focal_Root_Weight_g, DO_Traits$SRL,
     pch = 16, cex= 1, col="steelblue",
     xlab="Focal Root Biomass (g)", 
     ylab= "SRL", 
     main= "D. oligosanthese")
abline(lm(DO_Traits$SRL ~ DO_Traits$Focal_Root_Weight_g),col="blue", lty=7, lwd=2)
text(locator(), labels = c("p= 1.246e-05"))
text(locator(), labels = c("r= -0.5905368"))

#test of linear relationship
cor.test(DO_Traits$Focal_Root_Weight_g, DO_Traits$SRL, method="pearson")
#Conclusion: p= 1.246e-05 (r is significant), r= -0.5905368
# Moderately negative slope, mod neg relationship 
# The relationship is negative because as SRL increases, the Focal Root Biomass decreases. 

#test of non-linear relationship
cor.test(DO_Traits$Focal_Root_Weight_g, DO_Traits$SRL, method="spearman")
#Conclusion: p-value = 1.734e-07, rho= -0.6995837


# Regression 2: SRL against Focal Root Weight for AP
plot(AP_Traits$Focal_Root_Weight_g, AP_Traits$SRL,
     pch = 16, cex= 1, col="steelblue",
     xlab="Focal Root Biomass (g)", 
     ylab= "SRL", 
     main= "A. psylostachia")
abline(lm(AP_Traits$SRL ~ AP_Traits$Focal_Root_Weight_g),col="blue", lty=7, lwd=2)
text(locator(), labels = c("p= 0.0003327"))
text(locator(), labels = c("r= -0.5451118"))

#test of linear relationship
cor.test(AP_Traits$Focal_Root_Weight_g, AP_Traits$SRL, method="pearson")
#Conclusion: p= 0.0003327 (r is significant) r= -0.5451118
# Moderately negative slope, mod neg relationship 
# The relationship is negative because as SRL increases, the Focal Root Biomass decreases. 

#test of non-linear relationship
cor.test(AP_Traits$Focal_Root_Weight_g, AP_Traits$SRL, method="spearman")
#Conclusion: p-value = 6.86e-05, rho= -0.5933499


# Regression 2: SRL against Focal Root Weight for SM
plot(SM_Traits$Focal_Root_Weight_g, SM_Traits$SRL,
     pch = 16, cex= 1, col="steelblue",
     xlab="Focal Root Biomass (g)", 
     ylab= "SRL", 
     main= "S. missouriensis")
abline(lm(SM_Traits$SRL ~ SM_Traits$Focal_Root_Weight_g),col="blue", lty=7, lwd=2)
text(locator(), labels = c("p= 0.0149"))
text(locator(), labels = c("r= -0.4720473"))

#test of linear relationship
cor.test(SM_Traits$Focal_Root_Weight_g, SM_Traits$SRL, method="pearson")
#Conclusion: p= 0.0149 (r is significant) r= -0.4720473
# Moderately negative slope, mod neg relationship 
# The relationship is negative because as SRL increases, the Focal Root Biomass decreases. 

#test of non-linear relationship
cor.test(SM_Traits$Focal_Root_Weight_g, SM_Traits$SRL, method="spearman")
#Conclusion: p-value = 0.001878 (rho is significant), rho= -0.5890598



# Regression 2: SRL against Focal Root Weight for AG
plot(AG_Traits$Focal_Root_Weight_g, AG_Traits$SRL,
     pch = 16, cex= 1, col="steelblue",
     xlab="Focal Root Biomass (g)", 
     ylab= "SRL", 
     main= "A. gerardii")
abline(lm(AG_Traits$SRL ~ AG_Traits$Focal_Root_Weight_g),col="blue", lty=7, lwd=2)
text(locator(), labels = c("p= 0.2957"))
text(locator(), labels = c("r= 0.165238"))

#test of linear relationship
cor.test(AG_Traits$Focal_Root_Weight_g, AG_Traits$SRL, method="pearson")
#Conclusion: p= 0.2957 (r is not significant) r= 0.165238

#test of non-linear relationship
cor.test(AG_Traits$Focal_Root_Weight_g, AG_Traits$SRL, method="spearman")
#Conclusion: p-value = 0.2297 (rho is not significant), rho= 0.1890446


# Regression 2: SRL against Focal Root Weight for SN
plot(SN_Traits$Focal_Root_Weight_g, SN_Traits$SRL,
     pch = 16, cex= 1, col="steelblue",
     xlab="Focal Root Biomass (g)", 
     ylab= "SRL", 
     main= "S. nutans")
abline(lm(SN_Traits$SRL ~ SN_Traits$Focal_Root_Weight_g),col="blue", lty=7, lwd=2)
text(locator(), labels = c("p= 1.433e-05"))
text(locator(), labels = c("r= -0.5925739"))

#test of linear relationship
cor.test(SN_Traits$Focal_Root_Weight_g, SN_Traits$SRL, method="pearson")
#Conclusion: p= 1.433e-05 (r is significant) r= -0.5925739 
# Moderately negative slope, mod neg relationship 
# The relationship is negative because as SRL increases, the Focal Root Biomass decreases. 

#test of non-linear relationship
cor.test(SN_Traits$Focal_Root_Weight_g, SN_Traits$SRL, method="spearman")
#Conclusion: p-value = 8.843e-07 (rho is significant), rho= -0.6685785 




#Combine all graphs into one 
par(mfrow=c(3,2))






# Regression 3: Belowground %N against Focal Root Weight
# Regression 4: Belowground %C against Focal Root Weight





All_Traits <- read_csv("All_Traits_Datasheet.csv")


#change to numeric
is.numeric(All_Traits$Focal_Root_Weight_g)
All_Traits$Focal_Root_Weight_g <- as.numeric(as.character(All_Traits$Focal_Root_Weight_g))


DO_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes")

DO_data_Control <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control")

DO_data_10N <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="10")
View(DO_data_10N)

DO_data_20N <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="20")
View(DO_data_20N)

#messing around
plot(DO_Traits$Focal_Root_Weight_g, DO_Traits$Total_BeG_Weight_g)
plot(DO_Traits$Log_Focal_Root_Weight, DO_Traits$Log_Total_BeG_Weight)
abline(lm(DO_Traits$Log_Total_BeG_Weight ~ DO_Traits$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)



### DO Regressions, Total BeG biomass against Focal Root biomass
# CONTROL
plot(DO_data_Control$Log_Focal_Root_Weight, DO_data_Control$Log_Total_BeG_Weight)
abline(lm(DO_data_Control$Log_Total_BeG_Weight ~ DO_data_Control$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)



plot(DO_data_Control$Log_Focal_Root_Weight, DO_data_Control$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "DO: Control, Log Values")
abline(lm(DO_data_Control$Log_Total_BeG_Weight ~ DO_data_Control$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)

cor.test(DO_data_Control$Log_Focal_Root_Weight, DO_data_Control$Log_Total_BeG_Weight, method="pearson")
# p-value = 0.9295, r= -0.02246683



# 10g N treatment

plot(DO_data_10N$Log_Focal_Root_Weight, DO_data_10N$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "DO: 10 g N, Log Values")
abline(lm(DO_data_10N$Log_Total_BeG_Weight ~ DO_data_10N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)
cor.test(DO_data_10N$Log_Focal_Root_Weight,DO_data_10N$Log_Total_BeG_Weight, method="pearson")
# p=0.01709, r= 0.5538411

cor.test(DO_data_10N$Focal_Root_Weight_g, DO_data_10N$Total_BeG_Weight_g, method="pearson")
# p=0.2128, r= 0.3085817




# 20g N treatment
plot(DO_data_20N$Log_Focal_Root_Weight, DO_data_20N$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="Focal Root Biomass (g)", 
     ylab= "Total Belowground Biomass (g)", 
     main= "DO: 20 g N, Log Values")
abline(lm(DO_data_20N$Log_Total_BeG_Weight ~ DO_data_20N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)
cor.test(DO_data_20N$Log_Focal_Root_Weight,DO_data_20N$Log_Total_BeG_Weight, method="pearson")
# p=0.01709, r= 0.5538411

cor.test(DO_data_10N$Focal_Root_Weight_g, DO_data_10N$Total_BeG_Weight_g, method="pearson")
# p=0.2128, r= 0.3085817




# regressions for all species sorted by treatment
Traits_Control <- All_Traits %>%
  filter(Treatment=="Control")

Traits_10N <- All_Traits %>%
  filter(Treatment=="10")

Traits_20N <- All_Traits %>%
  filter(Treatment=="20")



# Control treatment
plot(Traits_Control$Log_Focal_Root_Weight, Traits_Control$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="log of Focal Root Biomass (g)", 
     ylab= "log og Total Belowground Biomass (g)", 
     main= "Control")
abline(lm(Traits_Control$Log_Total_BeG_Weight ~ Traits_Control$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)

cor.test(Traits_Control$Log_Focal_Root_Weight, Traits_Control$Log_Total_BeG_Weight, method="pearson")
#Conclusion: p-value = 0.001357, r=0.3420095 


# 10g N treatment
plot(Traits_10N$Log_Focal_Root_Weight, Traits_10N$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="log of Focal Root Biomass (g)", 
     ylab= "log of Total Belowground Biomass (g)", 
     main= "10 g N")
abline(lm(Traits_10N$Log_Total_BeG_Weight ~ Traits_10N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)

cor.test(Traits_10N$Log_Focal_Root_Weight, Traits_10N$Log_Total_BeG_Weight, method="pearson")
#Conclusion: p-value = 4.291e-05, r=0.4216376

# 20g N treatment
plot(Traits_20N$Log_Focal_Root_Weight, Traits_20N$Log_Total_BeG_Weight,
     pch = 16, cex= 1, col="gold",
     xlab="log of Focal Root Biomass (g)", 
     ylab= "log of Total Belowground Biomass (g)", 
     main= "20 g N")
abline(lm(Traits_20N$Log_Total_BeG_Weight ~ Traits_20N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)

cor.test(Traits_20N$Log_Focal_Root_Weight, Traits_20N$Log_Total_BeG_Weight, method="pearson")
#Conclusion: p-value = 0.3407, r=0.1801717






# SRL 
plot(Traits_Control$Log_Focal_Root_Weight, Traits_Control$Log_SRL,
     pch = 16, cex= 1, col="gold",
     xlab=" log of Focal Root Biomass (g)", 
     ylab= "log of SRL", 
     main= "Control")
abline(lm(Traits_Control$Log_SRL ~ Traits_Control$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)
text(locator(), labels = c("p= 3.7e-09"))
text(locator(), labels = c("r= -0.6043242"))

cor.test(Traits_Control$Log_Focal_Root_Weight, Traits_Control$Log_SRL, method="pearson")
#Conclusion: p-value = 3.7e-09, r=-0.6043242  


# SRL 
plot(Traits_10N$Log_Focal_Root_Weight, Traits_10N$Log_SRL,
     pch = 16, cex= 1, col="gold",
     xlab=" log of Focal Root Biomass (g)", 
     ylab= "log of SRL", 
     main= "10 g N")
abline(lm(Traits_10N$Log_SRL ~ Traits_10N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)
text(locator(), labels = c("p= 1.66e-10"))
text(locator(), labels = c("r= -0.6367622"))

cor.test(Traits_10N$Log_Focal_Root_Weight, Traits_10N$Log_SRL, method="pearson")
#Conclusion: p-value =1.66e-10, r=-0.6367622  



# SRL 
plot(Traits_20N$Log_Focal_Root_Weight, Traits_20N$Log_SRL,
     pch = 16, cex= 1, col="gold",
     xlab=" log of Focal Root Biomass (g)", 
     ylab= "log of SRL", 
     main= "20 g N")
abline(lm(Traits_20N$Log_SRL ~ Traits_20N$Log_Focal_Root_Weight),col="black", lty=7, lwd=2)
text(locator(), labels = c("p= 0.008517"))
text(locator(), labels = c("r= -0.5144127"))

cor.test(Traits_20N$Log_Focal_Root_Weight, Traits_20N$Log_SRL, method="pearson")
#Conclusion: p-value = 0.008517, r= -0.5144127

#Combine all graphs into one 
par(mfrow=c(1,3))
