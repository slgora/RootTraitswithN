#### Thesis Results Analysis- Linear Regressions ####
###  Gora_Thesis-Results-Analysis_Regressions.R
###  by Sarah Gora
###  Date created: Jan 13, 2021

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


#install packages
install.packages("dplyr")
library(dplyr)
library(readr)

#load Data
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet.csv")

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


# Filter out Control, 10g N for each species
DO_N0_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control")
DO_N10_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="10")
AP_N0_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Treatment=="Control")
AP_N10_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Treatment=="10")
SM_N0_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Treatment=="Control")
SM_N10_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Treatment=="10")
AG_N0_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Treatment=="Control")
AG_N10_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Treatment=="10")
SN_N0_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Treatment=="Control")
SN_N10_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Treatment=="10")

# Linear regression models for each TRAIT

# All species combined into a multi-panel
# one graph showing trait 1, for each species
# combined into a multi-panel



# Regression 
# anova 
# Run regressions for each species 


# DO, regressions
# is the relationship linear between trait A and trait B?
# are there tradeoffs?

#colors
# green: #769370, 10N
# gray: #BDB2A7, Control
pal2 <- c("#BDB2A7","#769370")




#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs SRL
model_DO_SLAvSRL <- lm(formula = SRL~SLA*Treatment+(1/Experiment)+(1/Plot), data = DO_Traits)
summary(model_DO_SLAvSRL)
# overall trend
# p= 0.5023, not significant
# R2= -0.01224
# 10N Treatment
# p= 0.571, not significant

plot(DO_Traits$SLA, DO_Traits$SRL,
     pch=20, 
     xlab="SLA",
     ylab="SRL",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_SLAvSRL), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.502"), font=2)
text(locator(), labels = c("R2=-0.012"), font=2)



##### FINAL

par(mfrow = c(1,1))

plot(AG_Traits$SRL, AG_Traits$SLA,
     pch=20, 
     xlab="SRL",
     ylab="SLA",
     main= "A. gerardii")
model_AG_SRLvSLA <- lm(formula = SLA~SRL*Treatment+(1/Experiment)+(1/Plot), data = AG_Traits)
abline(lm(model_AG_SRLvSLA), lwd=4, lty=2)





# LM for AP: SLA vs SRL
model_AP_SLAvSRL <- lm(formula = SRL~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvSRL)
# overall trend
# p= 0.05294, not significant
# R2= 0.1607
# 10N Treatment
# p= 0.9448, not significant

plot(AP_Traits$SLA, AP_Traits$SRL,
     pch=20, 
     xlab="SLA",
     ylab="SRL",
     ylim = c(0,20),
     main= "Ambrosia psylostachia")
abline(lm(model_AP_SLAvSRL), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$SRL~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$SRL~AP_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.052"),  font = 2)
text(locator(), labels = c("R2=0.161"), font = 2)







# LM for SM: SLA vs SRL
model_SM_SLAvSRL <- lm(formula = SRL~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvSRL)
# overall trend
# p= 0.1421, not significant
# R2= 0.1098
# 10N Treatment
# p= 0.71929, not significant

plot(SM_Traits$SLA, SM_Traits$SRL,
     pch=20, 
     xlab="SLA",
     ylab="SRL",
     main= "Solidago missouriensis")
abline(lm(model_SM_SLAvSRL), lwd=4, lty=2)
# abline(lm(SM_N0_Traits$SRL~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
# abline(lm(SM_N10_Traits$SRL~SM_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Treatment, 10g N"), fill = "#769370", bg = "white")
text(locator(), labels = c("p=0.142"), font = 2)
text(locator(), labels = c("R2=0.110"), font = 2)




# LM for AG: SLA vs SRL
model_AG_SLAvSRL <- lm(formula = SRL~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvSRL)
# overall trend
# p= 0.4028, not significant
# R2=0.01248 
# 10N Treatment
# p= 0.247, not significant

plot(SM_Traits$SLA, SM_Traits$SRL,
     pch=20, 
     xlab="SLA",
     ylab="SRL",
     main= "Andropogon gerardii")
abline(lm(model_AG_SLAvSRL), lwd=4, lty=2)
# abline(lm(AG_N0_Traits$SRL~AG_N0_Traits$SLA), lwd=4, col="#BDB2A7")
# abline(lm(AG_N10_Traits$SRL~AG_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Treatment, 10g N"), fill = "#769370", bg = "white")
text(locator(), labels = c("p=0.403"), font = 2)
text(locator(), labels = c("R2=0.012"), font = 2)




# LM for SN: SLA vs SRL
model_SN_SLAvSRL <- lm(formula = SRL~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvSRL)
# overall trend
# p= 0.6846, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.346, not significant

plot(SN_Traits$SLA, SN_Traits$SRL,
     pch=20, 
     xlab="SLA",
     ylab="SRL",
     main= "Sorghastrum nutans")
abline(lm(model_SN_SLAvSRL), lwd=4, lty=2)
# abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
# abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Treatment, 10g N"), fill = "#769370", bg = "white")
text(locator(), labels = c("p=0.685"), font = 2)
text(locator(), labels = c("R2=-0.049"), font = 2)






#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Focal Root Weight
model_DO_SLAvFocalRoot <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvFocalRoot)
# overall trend
# p= 0.6846, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.041, * significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.041 *, significant
# Control: p=0.243
# R2= 0.02255

plot(DO_Traits$SLA, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_SN_SLAvFocalRoot), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.243"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.041 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.023"), font = 2)



# LM for AP: SLA vs Focal Root Weight
model_AP_SLAvFocalRoot <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvFocalRoot)
# overall trend
# p= 0.6846, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0204, * significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0204 *, significant
# Control: p=0.6454 
# R2= -0.01385

plot(AP_Traits$SLA, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Focal_Root_Weight_g~AP_Traits$SLA), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.645"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.020 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.014"), font = 2)




# LM for SM: SLA vs Focal Root Weight
model_SM_SLAvFocalRoot <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvFocalRoot)
# overall trend
# p= 0.1283, not significant
# R2=0.1147 
# 10N Treatment
# p= 0.4598, not significant

plot(SM_Traits$SLA, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(lm(model_SM_SLAvFocalRoot), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Focal_Root_Weight_g~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Focal_Root_Weight_g~SM_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.128"), font = 2)
text(locator(), labels = c("R2=0.115"), font = 2)



# LM for AG: SLA vs Focal Root Weight
model_AG_SLAvFocalRoot <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvFocalRoot)
# overall trend
# p= 0.025, *significant
# R2=0.230
# 10N Treatment
# p= 0.39914, not significant

plot(AG_Traits$SLA, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
abline(lm(model_AG_SLAvFocalRoot), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Focal_Root_Weight_g~AG_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Focal_Root_Weight_g~AG_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.025 *"), font = 2)
text(locator(), labels = c("R2=0.230"), font = 2)


# LM for SN: SLA vs Focal Root Weight
model_SN_SLAvFocalRoot <- lm(formula = Focal_Root_Weight_g~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvFocalRoot)
# overall trend
# p= 0.208, not significant
# R2=0.06459 
# 10N Treatment
# p=0.2591, not significant

plot(SN_Traits$SLA, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Focal Root Weight (g)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_SLAvFocalRoot), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$Focal_Root_Weight_g~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$Focal_Root_Weight_g~SN_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.208"), font = 2)
text(locator(), labels = c("R2=0.065"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Total Root Weight
model_DO_SLAvTotalRoot <- lm(formula = Total_BeG_Weight_g~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvTotalRoot)
# overall trend
# p= 0.689, not significant
# R2=-0.05026
# 10N Treatment
# p= 0.0528, not significant

plot(DO_Traits$SLA, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Total Root Weight (g)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_SLAvTotalRoot), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Total_BeG_Weight_g~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Total_BeG_Weight_g~DO_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.689"), font = 2)
text(locator(), labels = c("R2=-0.050"), font = 2)



# LM for AP: SLA vs Total Root Weight
model_AP_SLAvTotalRoot <- lm(formula = Total_BeG_Weight_g~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_DO_SLAvTotalRoot)
# overall trend
# p= 0.689, not significant
# R2=-0.05026 
# 10N Treatment
# p= 0.0528, not significant

plot(AP_Traits$SLA, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Total Root Weight (g)",
     main= "Ambrosia psylostachia")
abline(lm(AP_Traits$Total_BeG_Weight_g~AP_Traits$SLA), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Total_BeG_Weight_g~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Total_BeG_Weight_g~AP_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.689"), font = 2)
text(locator(), labels = c("R2=-0.050"), font = 2)



# LM for SM: SLA vs Total Root Weight
model_SM_SLAvTotalRoot <- lm(formula = Total_BeG_Weight_g~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvTotalRoot)
# overall trend
# p= 0.0234, * significant
# R2= 0,228
# 10N Treatment
# p= 0.2708 not significant

plot(SM_Traits$SLA, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Total Root Weight (g)",
     main= "Solidago missouriensis")
abline(lm(SM_Traits$Total_BeG_Weight_g~SM_Traits$SLA), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Total_BeG_Weight_g~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Total_BeG_Weight_g~AP_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.023 *"), font = 2)
text(locator(), labels = c("R2=0.228"), font = 2)



# LM for AG: SLA vs Total Root Weight
model_AG_SLAvTotalRoot <- lm(formula = Total_BeG_Weight_g~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvTotalRoot)
# overall trend
# p= 0.1479, not significant
# R2=0.1066 
# 10N Treatment
# p= 0.0781, not significant


plot(AG_Traits$SLA, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Total Root Weight (g)",
     main= "Andropogon gerardii")
abline(lm(AG_Traits$Total_BeG_Weight_g~AG_Traits$SLA), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Total_BeG_Weight_g~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Total_BeG_Weight_g~AP_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.148"), font = 2)
text(locator(), labels = c("R2=0.107"), font = 2)


# LM for SN: SLA vs Total Root Weight
model_SN_SLAvTotalRoot <- lm(formula = Total_BeG_Weight_g~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvTotalRoot)
# overall trend
# p= 0.01736, * significant
# R2=0.2134  
# 10N Treatment
# p= 0.11036, not significant

plot(AG_Traits$SLA, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="SLA",
     ylab="Total Root Weight (g)",
     main= "Sorghastrum nutans")
abline(lm(SN_Traits$Total_BeG_Weight_g~SN_Traits$SLA), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$Total_BeG_Weight_g~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$Total_BeG_Weight_g~SN_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.017 *"), font = 2)
text(locator(), labels = c("R2=0.213"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Root_Diam_Max
model_DO_SLAvRootDiamMax <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvRootDiamMax)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0176, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0176 *, significant
# Control: p=0.7736
# R2= 0.2332

plot(DO_Traits$SLA, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="SLA",
     ylab="Max Root Diameter (cm)",
     main= "Dicanthelium oligosanthes")
#abline(lm(DO_Traits$Root_Diam_Max~DO_Traits$SLA), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Max~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Max~DO_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.774"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.018 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.233"), font = 2)



# LM for AP: SLA vs Root_Diam_Max
model_AP_SLAvRootDiamMax <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvRootDiamMax)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.000507, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0176 *, significant
# Control: p=0.076343
# R2= 0.01763

plot(AP_Traits$SLA, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="SLA",
     ylab="Max Root Diameter (cm)",
     main= "Ambrosia psylostachia")
# abline(lm(AP_Traits$Root_Diam_Max~AP_Traits$SLA), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Max~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Max~AP_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.076"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0005 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.018"), font = 2)


# LM for SM: SLA vs Root_Diam_Max
model_SM_SLAvRootDiamMax <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvRootDiamMax)
# overall trend
# p= 0.6644, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0592, not significant

plot(SM_Traits$SLA, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="SLA",
     ylab="Max Root Diameter (cm)",
     main= "Solidago missouriensis")
abline(lm(SM_Traits$Root_Diam_Max~SM_Traits$SLA), lwd=4, lty=2)
# abline(lm(SM_N0_Traits$Root_Diam_Max~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
# abline(lm(SM_N10_Traits$Root_Diam_Max~SM_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.664"), font = 2)
text(locator(), labels = c("R2=-0.054"), font = 2)


# LM for AG: SLA vs Root_Diam_Max
model_AG_SLAvRootDiamMax <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvRootDiamMax)
# overall trend
# p= 0.5648, not significant
# R2=-0.03009 
# 10N Treatment
# p= 0.394, not significant

plot(AG_Traits$SLA, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="SLA",
     ylab="Max Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(lm(AG_Traits$Root_Diam_Max~AG_Traits$SLA), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Root_Diam_Max~AG_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Root_Diam_Max~AG_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.565"), font = 2)
text(locator(), labels = c("R2=-0.0301"), font = 2)

# LM for SN: SLA vs Root_Diam_Max
model_SN_SLAvRootDiamMax <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvRootDiamMax)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0119, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~SLA*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0119 *, significant
# Control: p=0.2905 
# R2= -0.006533

plot(SN_Traits$SLA, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="SLA",
     ylab="Max Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Max~SN_Traits$SLA), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Max~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Max~SN_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.291"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.012 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.007"), font = 2)











#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Root_Diam_Mean
model_DO_SLAvRootDiamMean <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvRootDiamMean)
# overall trend
# p= 0.7963, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0003, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0003 *, significant
# Control: p=0.582425 
# R2=-0.08477

plot(DO_Traits$SLA, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="SLA",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Root_Diam_Mean~DO_Traits$SLA), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Mean~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Mean~DO_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.582"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0003 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.085"), font = 2)


# LM for AP: SLA vs Root_Diam_Mean
model_AP_SLAvRootDiamMean <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvRootDiamMean)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.003, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.003 *, significant
# Control: p=0.455
# R2= -0.01322

plot(AP_Traits$SLA, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="SLA",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Root_Diam_Mean~AP_Traits$SLA), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Mean~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Mean~AP_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.455"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.003 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.013"), font = 2)


# LM for SM: SLA vs Root_Diam_Mean
model_SM_SLAvRootDiamMean <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvRootDiamMean)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.00334, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00334 *, significant
# Control: p=0.0372*, significant 
# R2= 0.07603

plot(SM_Traits$SLA, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="SLA",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Root_Diam_Mean~SM_Traits$SLA), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Mean~SM_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.037 *"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.003 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.076"), font = 2)


# LM for AG: SLA vs Root_Diam_Mean
model_AG_SLAvRootDiamMean <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvRootDiamMean)
# overall trend
# p= 0.1843, not significant
# R2= 0.09081 
# 10N Treatment
# p=0.119, not significant

plot(AG_Traits$SLA, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="SLA",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(lm(AG_Traits$Root_Diam_Mean~AG_Traits$SLA), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Root_Diam_Mean~AG_N10_Traits$SLA), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.184"), font = 2)
text(locator(), labels = c("R2=0.091"), font = 2)


# LM for SN: SLA vs Root_Diam_Mean
model_SN_SLAvRootDiamMean <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvRootDiamMean)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0134, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~SLA*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0134 *, significant
# Control: p=0.6917
# R2= -0.006533

plot(SN_Traits$SLA, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="SLA",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Mean~SN_Traits$SLA), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Mean~SN_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.692"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.013 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.018"), font = 2)








#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Aboveground_Dry_Weight_g vs SRL
model_DO_AbGvSRL <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGvSRL)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 1.78e-05, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.78e-05 *, significant
# Control: p=0.1822
# R2= 0.05055

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$SRL,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$SRL~DO_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Root_Diam_Mean), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$Root_Diam_Mean), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c(col="#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.182"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=1.78e-05 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.051"), font = 2)

# LM for AP: Aboveground_Dry_Weight_g vs SRL
model_AP_AbGvSRL <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGvSRL)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 3.49e-07, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=3.49e-07 *, significant
# Control: p=0.1280
# R2= 0.1091 

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$SRL,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="SRL",
     main= "Ambrosia psylostachia")
abline(lm(AP_N0_Traits$SRL~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c(col="#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.128"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=3.49e-07 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.1091 "), font = 2)


# LM for SM: Aboveground_Dry_Weight_g vs SRL
model_SM_AbGvSRL <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGvSRL)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.0134, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=8.31e-05 *, significant
# Control: p=0.931
# R2= 0.01686

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$SRL,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="SRL",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$SRL~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SM_N0_Traits$SRL~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$SRL~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c(col="#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.931"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=8.31e-05 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.017"), font = 2)


# LM for AG: Aboveground_Dry_Weight_g vs SRL
model_AG_AbGvSRL <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGvSRL)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 2.87e-10, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.87e-10 *, significant
# Control: p=0.3505
# R2= 0.1191

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$SRL,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(lm(SM_Traits$SRL~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AG_N0_Traits$SRL~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$SRL~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c(col="#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.351"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=2.87e-10 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.119"), font = 2)


# LM for SN: Aboveground_Dry_Weight_g vs SRL
model_SN_AbGvSRL <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGvSRL)
# overall trend
# p= 0.01284, not significant
# R2=-0.04936 
# 10N Treatment
# p= 0.000837, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000837 *, significant
# Control: p=0.418797 
# R2= 0.02948

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$SRL,
     pch=20, 
     xlab="Aboveground_Dry_Weight_g",
     ylab="SRL",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$SRL~SN_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SM_N0_Traits$SRL~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c(col="#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.419"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0008 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.029"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Aboveground_Dry_Weight_g vs Focal_Root_Weight_g
model_DO_AbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGvFocalRoot)
# overall trend
# p= 0.005, *significant
# R2= 0.2717 
# 10N Treatment
# p= 0.258563, not significant

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Focal Root Weight (g)",
     main="Dicanthelium oligosantheses")
abline(lm(DO_Traits$Focal_Root_Weight_g~DO_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.005 *"), font = 2)
text(locator(), labels = c("R2=0.272"), font = 2)



# LM for AP: Aboveground_Dry_Weight_g vs Focal_Root_Weight_g
model_AP_AbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGvFocalRoot)
# overall trend
# p= 0.002 , * significant
# R2=0.3304 
# 10N Treatment
# p= 0.1007, not significant

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylim=c(0.01, 0.30),
     xlab="Aboveground Dry Weight (g)",
     ylab="Focal Root Weight (g)",
     main="Ambrosia psylostachia")
abline(lm(AP_Traits$Focal_Root_Weight_g~AP_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.002 *"), font = 2)
text(locator(), labels = c("R2=0.330"), font = 2)


# LM for SM: Aboveground_Dry_Weight_g vs Focal_Root_Weight_g
model_SM_AbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGvFocalRoot)
# overall trend
# p= 7.41e-05, * significant
# R2=0.4811
# 10N Treatment
# p= 0.635020 , not significant

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Focal Root Weight (g)",
     main="Solidago missouriensis")
abline(lm(SM_Traits$Focal_Root_Weight_g~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Focal_Root_Weight_g~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Focal_Root_Weight_g~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=7.41e-05 *"), font = 2)
text(locator(), labels = c("R2=0.481"), font = 2)


# LM for AG: Aboveground_Dry_Weight_g vs Focal_Root_Weight_g
model_AG_AbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGvFocalRoot)
# overall trend
# p= 0.02022, * significant
# R2=0.2102 
# 10N Treatment
# p= 0.33916, not significant

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Focal Root Weight (g)",
     main="Andropogon gerardii")
abline(lm(AG_Traits$Focal_Root_Weight_g~AG_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Focal_Root_Weight_g~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Focal_Root_Weight_g~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
# legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.020 *"), font = 2)
text(locator(), labels = c("R2=0.210"), font = 2)


# LM for SN: Aboveground_Dry_Weight_g vs Focal_Root_Weight_g
model_SN_AbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGvFocalRoot)
# overall trend
# p= 0.0001722, * significant
# R2=-0.04936 
# 10N Treatment
# p= 0.000102, * significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000102 *, significant
# Control: p=0.362367  
# R2= 0.4611

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Focal Root Weight (g)",
     main="Sorghastrum nutans")
#abline(lm(SN_Traits$Focal_Root_Weight_g~SN_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Focal_Root_Weight_g~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Focal_Root_Weight_g~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.362"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0001 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.461"), font = 2)






#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Aboveground_Dry_Weight_g vs Total_BeG_Weight_g
model_DO_AbGvTotalRoot <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGvTotalRoot)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 0.004088, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.004088*, significant
# Control: p=0.283184  
# R2= 0.004761

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Total Belowground Mass (g)",
     main="Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Total_BeG_Weight_g~DO_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Total_BeG_Weight_g~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Total_BeG_Weight_g~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.283"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.005"), font = 2)

# LM for AP: Aboveground_Dry_Weight_g vs Total_BeG_Weight_g
model_AP_AbGvTotalRoot <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGvTotalRoot)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 0.000593, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000593*, significant
# Control: p=0.483774  
# R2=-0.05562

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Total Belowground Mass (g)",
     main="Ambrosia psylostachia")
#abline(lm(AP_Traits$Total_BeG_Weight_g~AP_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Total_BeG_Weight_g~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Total_BeG_Weight_g~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.484"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0006 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.056"), font = 2)


# LM for SM: Aboveground_Dry_Weight_g vs Total_BeG_Weight_g
model_SM_AbGvTotalRoot <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGvTotalRoot)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 0.00588, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00588*, significant
# Control: p=0.28524 
# R2= -0.01632

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Total Belowground Mass (g)",
     main="Solidago missouriensis")
#abline(lm(SM_Traits$Total_BeG_Weight_g~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Total_BeG_Weight_g~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Total_BeG_Weight_g~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.285"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.006 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.0163"), font = 2)

# LM for AG: Aboveground_Dry_Weight_g vs Total_BeG_Weight_g
model_AG_AbGvTotalRoot <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGvTotalRoot)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 0.02337, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.02337*, significant
# Control: p=0.01887  
# R2= 0.1346

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Total Belowground Mass (g)",
     main="Andropogon gerardii")
#abline(lm(AG_Traits$Total_BeG_Weight_g~AG_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Total_BeG_Weight_g~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Total_BeG_Weight_g~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.019 *"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.023 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.135"), font = 2)


# LM for SN: Aboveground_Dry_Weight_g vs Total_BeG_Weight_g
model_SN_AbGvTotalRoot <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGvTotalRoot)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 0.000194, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000194*, significant
# Control: p=0.851503 
# R2=-0.044

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Total Belowground Mass (g)",
     main="Sorghastrum nutans")
#abline(lm(SN_Traits$Total_BeG_Weight_g~SN_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Total_BeG_Weight_g~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Total_BeG_Weight_g~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.852"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.044"), font = 2)









#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Aboveground_Dry_Weight_g vs Root_Diam_Max
model_DO_AbGvRootDiamMax <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGvRootDiamMax)
# overall trend
# p= 0.02986, * significant
# R2=0.1739
# 10N Treatment
# p= 0.000122, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000122*, significant
# Control: p=0.508344
# R2=0.002637

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Max Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Root_Diam_Max~DO_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Mean~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Mean~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.508"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.0001 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.003"), font = 2)


# LM for AP: Aboveground_Dry_Weight_g vs Root_Diam_Max
model_AP_AbGvRootDiamMax <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGvRootDiamMax)
# overall trend
# p= 0.1519, not significant
# R2=0.1739
# 10N Treatment
# p= 0.00252, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00252, significant
# Control: p=0.851503 
# R2=0.08376

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Root_Diam_Max,
     pch=20, 
     ylim=c(0.07, 0.45),
     xlab="Aboveground Dry Weight (g)",
     ylab="Max Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Root_Diam_Max~AP_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Max~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Max~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.852"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.003 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.084"), font = 2)


# LM for SM: Aboveground_Dry_Weight_g vs Root_Diam_Max
model_SM_AbGvRootDiamMax <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGvRootDiamMax)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 1.98e-05, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.98e-05*, significant
# Control: p=0.18424 
# R2=0.226

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Max Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Root_Diam_Max~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Max~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Max~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.184"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=1.98e-05 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.226"), font = 2)


# LM for AG: Aboveground_Dry_Weight_g vs Root_Diam_Max
model_AG_AbGvRootDiamMax <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGvRootDiamMax)
# overall trend
# p= 0.03686, not significant
# R2=0.1739
# 10N Treatment
# p= 7.04e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=7.04e-06*, significant
# Control: p= 0.544 
# R2=0.1791

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Max Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(AG_Traits$Root_Diam_Max~AG_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Max~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Max~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p= 0.544"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=7.04e-06 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.179"), font = 2)


# LM for SN: Aboveground_Dry_Weight_g vs Root_Diam_Max
model_SN_AbGvRootDiamMax <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGvRootDiamMax)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 1.86e-07, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.86e-07, * significant
# Control: p=0.8833 
# R2=0.00779

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Max Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Max~SN_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Max~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Max~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.883"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=1.86e-07 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.008"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Aboveground_Dry_Weight_g vs Root_Diam_Mean
model_DO_AbGvRootDiamMean <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGvRootDiamMean)
# overall trend
# p= 0.03686, not significant
# R2=0.1739
# 10N Treatment
# p= 2.98e-09, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.98e-09, * significant
# Control: p=0.688 
# R2=-0.06095

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Root_Diam_Mean~DO_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Mean~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Mean~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.688"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=2.98e-09 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.061"), font = 2)


# LM for AP: Aboveground_Dry_Weight_g vs Root_Diam_Mean
model_AP_AbGvRootDiamMean <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGvRootDiamMean)
# overall trend
# p= 0.03686, not significant
# R2=0.1739
# 10N Treatment
# p= 0.00522 * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00522, * significant
# Control: p=0.0423 * 
# R2=0.03514

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
# abline(lm(AP_Traits$Root_Diam_Mean~AP_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Mean~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Mean~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.042 *"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=0.005 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.035"), font = 2)


# LM for SM: Aboveground_Dry_Weight_g vs Root_Diam_Mean
model_SM_AbGvRootDiamMean <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGvRootDiamMean)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 1.39e-07, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.39e-07, * significant
# Control: p=0.256 
# R2=0.04356 

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Root_Diam_Mean,
     pch=20, 
     ylim=c(0.03, 0.11),
     xlab="Aboveground Dry Weight (g)",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Root_Diam_Mean~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Mean~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.256"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=1.39e-07 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.044"), font = 2)

# LM for AG: Aboveground_Dry_Weight_g vs 
model_AG_AbGvRootDiamMean <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGvRootDiamMean)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 6.76e-07, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=6.76e-07, * significant
# Control: p=0.17328 
# R2=0.4117 

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Root_Diam_Mean,
     pch=20, 
     ylim=c(0.025,0.115),
     xlab="Aboveground Dry Weight (g)",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(SM_Traits$Root_Diam_Mean~SM_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Mean~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Mean~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.173"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=6.76e-07 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=0.412"), font = 2)


# LM for SN: Aboveground_Dry_Weight_g vs Root_Diam_Mean
model_SN_AbGvRootDiamMean <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGvRootDiamMean)
# overall trend
# p= 0.03686, * significant
# R2=0.1739
# 10N Treatment
# p= 5.85e-05, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.85e-05, * significant
# Control: p=0.8833 
# R2=-0.06095

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Aboveground Dry Weight (g)",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Mean~SN_Traits$Aboveground_Dry_Weight_g), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Mean~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Mean~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7","#769370"), bg = "white")
text(locator(), labels = c("p=0.884"), font = 2, col="#BDB2A7")
text(locator(), labels = c("p=5.85e-05 *"), font = 2, col="#769370")
text(locator(), labels = c("R2=-0.061"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs Focal_Root_Weight_g
model_DO_LeafNumvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvFocalRoot)
# overall trend
# p= 0.04808, * significant
# R2=0.1591
# 10N Treatment
# p= 0.4680, not significant

plot(DO_Traits$Leaf_Number, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosantheses")
abline(lm(DO_Traits$Focal_Root_Weight_g~DO_Traits$Leaf_Number), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.048 *"), font = 2)
text(locator(), labels = c("R2=0.159"), font = 2)


# LM for AP: Leaf_Number vs Focal_Root_Weight_g
model_AP_LeafNumvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvFocalRoot)
# overall trend
# p= 1.298e-05, * significant
# R2=0.1739
# 10N Treatment
# p= 0.4680, not significant

plot(AP_Traits$Leaf_Number, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     ylim=c(0.01, 0.30),
     xlab="Leaf Number",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
abline(lm(AP_Traits$Focal_Root_Weight_g~AP_Traits$Leaf_Number), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=1.30e-05 *"), font = 2)
text(locator(), labels = c("R2=0.499"), font = 2)


# LM for SM: Leaf_Number vs Focal_Root_Weight_g
model_SM_LeafNumvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvFocalRoot)
# overall trend
# p= 0.07865, not significant
# R2=0.1591
# 10N Treatment
# p= 0.118, not significant

plot(SM_Traits$Leaf_Number, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(lm(SM_Traits$Focal_Root_Weight_g~SM_Traits$Leaf_Number), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.079"), font = 2)
text(locator(), labels = c("R2=0.151"), font = 2)



# LM for AG: Leaf_Number vs Focal_Root_Weight_g
model_AG_LeafNumvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvFocalRoot)
# overall trend
# p= 0.1471, not significant
# R2=0.09157
# 10N Treatment
# p= 0.118, not significant

plot(AG_Traits$Leaf_Number, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
abline(lm(AG_Traits$Focal_Root_Weight_g~AG_Traits$Leaf_Number), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.147"), font = 2)
text(locator(), labels = c("R2=0.092"), font = 2)

# LM for SN: Leaf_Number vs Focal_Root_Weight_g
model_SN_LeafNumvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvFocalRoot)
# overall trend
# p= 2.681e-06, * significant
# R2=0.525
# 10N Treatment
# p= 0.05948, not significant

plot(SN_Traits$Leaf_Number, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Focal Root Weight (g)",
     main= "Soghastrum nutans")
abline(lm(SN_Traits$Focal_Root_Weight_g~SN_Traits$Leaf_Number), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=2.68e-06 *"), font = 2)
text(locator(), labels = c("R2=0.525"), font = 2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs Total_BeG_Weight_g
model_DO_LeafNumvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvTotalRoot)
# overall trend
# p= 2.681e-06, * significant
# R2= -0.0406
# 10N Treatment
# p= 0.0109, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0109, * significant
# Control: p=0.9974 
# R2=--0.0406

plot(DO_Traits$Leaf_Number, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Total Belowground Mass (g)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Total_BeG_Weight_g~DO_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Total_BeG_Weight_g~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Total_BeG_Weight_g~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.997"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0109 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.041"), font=2)


# LM for AP: Leaf_Number vs Total_BeG_Weight_g
model_AP_LeafNumvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvTotalRoot)
# overall trend
# p= 2.681e-06, * significant
# R2= -0.0406
# 10N Treatment
# p= 0.00996, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00996, * significant
# Control: p=0.9974 
# R2=0.07928

plot(AP_Traits$Leaf_Number, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Total Belowground Mass (g)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Total_BeG_Weight_g~AP_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Total_BeG_Weight_g~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Total_BeG_Weight_g~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.915"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.010 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.079"), font=2)

# LM for SM: Leaf_Number vs Total_BeG_Weight_g
model_SM_LeafNumvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvTotalRoot)
# overall trend
# p= 2.681e-06, * significant
# R2= -0.0406
# 10N Treatment
# p= 0.00972, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00972, * significant
# Control: p=0.88376
# R2=0.01592

plot(SM_Traits$Leaf_Number, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Total Belowground Mass (g)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Total_BeG_Weight_g~SM_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Total_BeG_Weight_g~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Total_BeG_Weight_g~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.884"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.010 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.016"), font=2)


# LM for AG: Leaf_Number vs Total_BeG_Weight_g
model_AG_LeafNumvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvTotalRoot)
# overall trend
# p= 2.681e-06, * significant
# R2= -0.0406
# 10N Treatment
# p= 0.0473, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0473, * significant
# Control: p=0.1072
# R2=0.05243

plot(AG_Traits$Leaf_Number, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Total Belowground Mass (g)",
     main= "Andropogon gerardii")
#abline(lm(AG_Traits$Total_BeG_Weight_g~AG_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Total_BeG_Weight_g~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Total_BeG_Weight_g~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.107"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.047 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.052"), font=2)


# LM for SN: Leaf_Number vs Total_BeG_Weight_g
model_SN_LeafNumvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvTotalRoot)
# overall trend
# p= not significant
# R2= -0.0406
# 10N Treatment
# p= 0.000792, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000792, * significant
# Control: p=0.694383 
# R2=--0.0406

plot(SN_Traits$Leaf_Number, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Number",
     ylab="Total Belowground Mass (g)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Total_BeG_Weight_g~SN_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Total_BeG_Weight_g~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Total_BeG_Weight_g~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.694"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0008 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.031"), font=2)





#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs SRL
model_DO_LeafNumvSRL <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvSRL)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 0.00303, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00303, * significant
# Control: p=0.98926
# R2=-0.1296

class(DO_Traits$Treatment)

par(mfrow=c(1,1))

plot(DO_Traits$Leaf_Number, DO_Traits$SRL,
     pch=20, 
     xlab="Leaf Number",
     ylab="SRL",
     main= "Dicanthelium oligosanthes")
#abline(lm(DO_Traits$Leaf_Number~DO_Traits$SRL), lwd=4, lty=2)
abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.989"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.003 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.130"), font=2)


# LM for AP: Leaf_Number vs SRL
model_AP_LeafNumvSRL <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvSRL)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 2.59e-06, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.59e-06, * significant
# Control: p=0.4977
# R2=0.143

plot(AP_Traits$Leaf_Number, AP_Traits$SRL,
     pch=20, 
     xlab="Leaf Number",
     ylab="SRL",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$SRL~AP_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AP_N0_Traits$SRL~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$SRL~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.498"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.59e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.143"), font=2)


# LM for SM: Leaf_Number vs SRL
model_SM_LeafNumvSRL <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvSRL)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 0.0105, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0105, * significant
# Control: p=0.8616
# R2=-0.07873

plot(SM_Traits$Leaf_Number, SM_Traits$SRL,
     pch=20, 
     xlab="Leaf Number",
     ylab="SRL",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$SRL~SM_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SM_N0_Traits$SRL~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$SRL~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.862"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.011 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.079"), font=2)

# LM for AG: Leaf_Number vs SRL
model_AG_LeafNumvSRL <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvSRL)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 2.94e-07, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.94e-07, * significant
# Control: p=0.8755
# R2= 0.1223

plot(AG_Traits$Leaf_Number, AG_Traits$SRL,
     pch=20, 
     xlab="Leaf Number",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(lm(AG_Traits$SRL~AG_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AG_N0_Traits$SRL~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$SRL~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.876"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.9e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.122"), font=2)

# LM for SN: Leaf_Number vs SRL
model_SN_LeafNumvSRL <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvSRL)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 0.00447, * significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00447, * significant
# Control: p=0.22540
# R2=0.046

plot(SN_Traits$Leaf_Number, SN_Traits$SRL,
     pch=20, 
     xlab="Leaf Number",
     ylab="SRL",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$SRL~SN_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.225"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.046"), font=2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs Root_Diam_Max
model_DO_LeafNumvRootDiamMax <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvRootDiamMax)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 0.00183, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 2.34e-05, * significant
# Control: p=0.300
# R2=0.0002329

plot(DO_Traits$Leaf_Number, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Number",
     ylab="Max Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Root_Diam_Max~DO_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Max~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Max~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.300"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.34e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.0002"), font=2)


# LM for AP: Leaf_Number vs Root_Diam_Max
model_AP_LeafNumvRootDiamMax <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvRootDiamMax)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 5.24e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.24e-06, * significant
# Control: p=0.1332
# R2=0.2521

plot(AP_Traits$Leaf_Number, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Number",
     ylab="Max Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Root_Diam_Max~AP_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Max~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Max~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.133"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.24e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.252"), font=2)

# LM for SM: Leaf_Number vs Root_Diam_Max
model_SM_LeafNumvRootDiamMax <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvRootDiamMax)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p=0.00041 , * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00041, * significant
# Control: p=0.45441
# R2=0.02883

plot(SM_Traits$Leaf_Number, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Number",
     ylab="Max Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Root_Diam_Max~SM_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Max~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Max~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.454"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.029"), font=2)


# LM for AG: Leaf_Number vs Root_Diam_Max
model_AG_LeafNumvRootDiamMax <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvRootDiamMax)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 7.44e-05, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=7.44e-05, * significant
# Control: p=0.012
# R2=0.012

plot(AG_Traits$Leaf_Number, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Number",
     ylab="Max Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(AG_Traits$Root_Diam_Max~AG_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Max~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Max~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.012"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=7.4e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.012"), font=2)


# LM for SN: Leaf_Number vs Root_Diam_Max
model_SN_LeafNumvRootDiamMax <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvRootDiamMax)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 5.69e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.69e-06, * significant
# Control: p= 0.531
# R2=0.03907

plot(SN_Traits$Leaf_Number, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Number",
     ylab="Max Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Max~SN_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Max~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Max~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.531"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.69e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.039"), font=2)







#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs Root_Diam_Mean
model_DO_LeafNumvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvRootDiamMean)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 3.37e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 3.37e-06, * significant
# Control: p=0.7729
# R2=0.07777

plot(DO_Traits$Leaf_Number, DO_Traits$Root_Diam_Mean,
     pch=20, 
     ylim=c(0.02, 0.07),
     xlab="Leaf Number",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(lm(DO_Traits$Root_Diam_Mean~DO_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Mean~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Mean~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.773"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.041 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.078"), font=2)


# LM for AP: Leaf_Number vs Root_Diam_Mean
model_AP_LeafNumvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvRootDiamMean)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 2.28e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.28e-06, * significant
# Control: p=0.776
# R2=-0.034

View(AP_Traits)

plot(AP_Traits$Leaf_Number, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Number",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(AP_Traits$Root_Diam_Mean~AP_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Mean~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Mean~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.776"), col="#BDB2A7", font=2)
text(locator(), labels = c("p=2.2e-06 *"), col="#769370", font=2)
text(locator(), labels = c("R2=-0.034"), font=2)


# LM for SM: Leaf_Number vs Root_Diam_Mean
model_SM_LeafNumvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvRootDiamMean)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 6.01e-06, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=6.01e-06, * significant
# Control: p=0.432
# R2=-0.0584

plot(SM_Traits$Leaf_Number, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Number",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(SM_Traits$Root_Diam_Mean~SM_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Mean~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.432"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=6.0e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.058"), font=2)


# LM for AG: Leaf_Number vs Root_Diam_Mean
model_AG_LeafNumvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvRootDiamMean)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 0.00402, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00402, * significant
# Control: p=0.00373
# R2=0.3145 

plot(AG_Traits$Leaf_Number, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Number",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(AG_Traits$Root_Diam_Mean~AG_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Mean~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Mean~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.203"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.315"), font=2, col="#769370")


# LM for SN: Leaf_Number vs Root_Diam_Mean
model_SN_LeafNumvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvRootDiamMean)
# overall trend
# p= , not significant
# R2= 
# 10N Treatment
# p= 4.01e-05, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=4.01e-05, * significant
# Control: p=0.417270
# R2=0.06726

plot(SN_Traits$Leaf_Number, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Number",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(SN_Traits$Root_Diam_Mean~SN_Traits$Leaf_Number), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Mean~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Mean~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.417"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=4.01e-05 * "), font=2, col="#769370")
text(locator(), labels = c("R2=0.067"), font=2)



#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Height_cm vs Focal_Root_Weight_g
model_DO_HeightvFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvFocalRoot)
# overall trend
# p= 0.1337, not significant
# R2: 0.112
# 10N Treatment
# p= 0.7977 , not significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(DO_Traits$Plant_Height_cm, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosanthes")
abline(model_DO_HeightvFocalRoot, lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Plant_Height_cm~DO_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Plant_Height_cm~DO_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.134"), font=2)
text(locator(), labels = c("R2= 0.112"), font=2)



# LM for AP: Plant_Height_cm vs Focal_Root_Weight_g
model_AP_HeightvFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvFocalRoot)
# overall trend
# p= 0.002482, not significant
# R2: 
# 10N Treatment
# p=  , * significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(AP_Traits$Plant_Height_cm, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
abline(model_AP_HeightvFocalRoot, lwd=4, lty=2) 
#abline(lm(AP_N0_Traits$Plant_Height_cm~AP_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Plant_Height_cm~AP_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.002 * "), font=2)
text(locator(), labels = c("R2=0.347"), font=2)


# LM for SM: Plant_Height_cm vs Focal_Root_Weight_g
model_SM_HeightvFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvFocalRoot)
# overall trend
# p= 0.01195, * significant
# R2: 0.2652
# 10N Treatment
# p=  , not significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(SM_Traits$Plant_Height_cm, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(model_SM_HeightvFocalRoot, lwd=4, lty=2) 
#abline(lm(SM_N0_Traits$Plant_Height_cm~SM_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Plant_Height_cm~SM_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.012 *"), font=2)
text(locator(), labels = c("R2=0.265"), font=2)



# LM for AG: Plant_Height_cm vs Focal_Root_Weight_g
model_AG_HeightvFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvFocalRoot)
# overall trend
# p= 0.2969, not significant
# R2: 0.04636
# 10N Treatment
# p=  , not significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(AG_Traits$Plant_Height_cm, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
abline(model_AG_HeightvFocalRoot, lwd=4, lty=2) 
#abline(lm(AG_N0_Traits$Plant_Height_cm~AG_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Plant_Height_cm~AG_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.296"), font=2)
text(locator(), labels = c("R2=0.046"), font=2)


# LM for SN: Plant_Height_cm vs Focal_Root_Weight_g
model_SN_HeightvFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvFocalRoot)
# overall trend
# p= 0.03904, * significant
# R2: 0.1939
# 10N Treatment
# p=  ,not significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(SN_Traits$Plant_Height_cm, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Focal Root Weight (g)",
     main= "Sorghastrum nutans")
abline(model_SN_HeightvFocalRoot, lwd=4, lty=2) 
#abline(lm(SN_N0_Traits$Plant_Height_cm~SN_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$Plant_Height_cm~SN_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.039 *"), font=2)
text(locator(), labels = c("R2= 0.194"), font=2)





#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Height_cm vs Total_BeG_Weight_g
model_DO_HeightvTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvTotalRoot)
# overall trend
# p= 0.2537, not significant
# R2: 0.05968
# 10N Treatment
# p=  , not significant

reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= , * significant
# Control: p=
# R2=

plot(DO_Traits$Plant_Height_cm, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosantheses")
abline(model_DO_HeightvTotalRoot, lwd=4, lty=2) 
text(locator(), labels = c("p=0.254"), font=2)
text(locator(), labels = c("R2=0.060"), font=2)



# LM for AP: Plant_Height_cm vs Total_BeG_Weight_g
model_AP_HeightvTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvTotalRoot)
# overall trend
# p= 0.03904, * significant
# R2: 0.037
# 10N Treatment
# p=  ,not significant

plot(AP_Traits$Plant_Height_cm, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
abline(model_AP_HeightvTotalRoot, lwd=4, lty=2) 
text(locator(), labels = c("p=0.328"), font=2)
text(locator(), labels = c("R2=0.037"), font=2)


# LM for SM: Plant_Height_cm vs Total_BeG_Weight_g
model_SM_HeightvTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvTotalRoot)
# overall trend
# p= 0.2705 not significant
# R2: 0.1939
# 10N Treatment
# p=  ,not significant

plot(SM_Traits$Plant_Height_cm, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
abline(model_SM_HeightvTotalRoot, lwd=4, lty=2) 
text(locator(), labels = c("p=0.271"), font=2)
text(locator(), labels = c("R2=0.052"), font=2)



# LM for AG: Plant_Height_cm vs Total_BeG_Weight_g
model_AG_HeightvFocalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvFocalRoot)
# overall trend
# p= 0.6435, not significant
# R2: -0.04692 
# 10N Treatment
# p=  ,not significant

plot(AG_Traits$Plant_Height_cm, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
abline(model_AG_HeightvFocalRoot, lwd=4, lty=2) 
text(locator(), labels = c("p=0.644"), font=2)
text(locator(), labels = c("R2=-0.047"), font=2)



# LM for SN: Plant_Height_cm vs Total_BeG_Weight_g
model_SN_HeightvTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvTotalRoot)
# overall trend
# p= 0.4345, * significant
# R2: 0.0057
# 10N Treatment
# p=  ,not significant

plot(SN_Traits$Plant_Height_cm, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
abline(model_SN_HeightvTotalRoot, lwd=4, lty=2) 
text(locator(), labels = c("p=0.435"), font=2)
text(locator(), labels = c("R2=0.0057"), font=2)




# LM for DO: Plant_Height_cm vs SRL
# LM for AP: Plant_Height_cm vs SRL
# LM for SM: Plant_Height_cm vs SRL
# LM for AG: Plant_Height_cm vs SRL
# LM for SN: Plant_Height_cm vs SRL

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Height_cm vs SRL
model_DO_HeightvSRL <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvSRL)
# overall trend
# p= , not significant
# R2: 
# 10N Treatment
# p=

reg1 <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 0.000154, * significant
# Control: p=0.211905
# R2= 0.148 

plot(DO_Traits$Plant_Height_cm, DO_Traits$SRL,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_HeightvSRL, lwd=4, lty=2) 
abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.212"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#769370")
text(locator(), labels = c("R2= 0.148 "), font=2)




# LM for AP: Plant_Height_cm vs SRL
model_AP_HeightvSRL <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvSRL)
# overall trend
# p= 
# R2: 

reg1 <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 0.000154, * significant
# Control: p=0.4158
# R2= 0.148 

plot(AP_Traits$Plant_Height_cm, AP_Traits$SRL,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="SRL",
     main= "Ambrosia psylostachia")
#abline(model_AP_HeightvSRL, lwd=4, lty=2) 
abline(lm(AP_N0_Traits$SRL~AP_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$SRL~AP_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.416"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.017 "), font=2)


# LM for SM: Plant_Height_cm vs SRL
model_SM_HeightvSRL <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvSRL)
# overall trend
# p= 0.1203, not significant
# R2: 0.08829

reg1 <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 0.000589, * significant
# Control: p=0.896382
# R2= 0.02016

plot(SM_Traits$Plant_Height_cm, SM_Traits$SRL,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="SRL",
     main= "Solidago missouriensis")
#abline(model_SM_HeightvSRL, lwd=4, lty=2) 
abline(lm(SM_N0_Traits$SRL~SM_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$SRL~SM_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.896"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0006 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.0202 "), font=2)



# LM for AG: Plant_Height_cm vs SRL
model_AG_HeightvSRL <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvSRL)
# overall trend
# p= 
# R2= 

reg1 <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p= 0.00929, * significant
# Control: p=0.33748
# R2= 0.0027

plot(AG_Traits$Plant_Height_cm, AG_Traits$SRL,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(model_AG_HeightvSRL, lwd=4, lty=2) 
abline(lm(AG_N0_Traits$SRL~AG_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$SRL~AG_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.337"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.003"), font=2)



# LM for SN: Plant_Height_cm vs SRL
model_SN_HeightvSRL <- lm(formula = SRL~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvSRL)
# overall trend
# p=  0.4448, not significant
# R2= -0.02428

plot(SN_Traits$Plant_Height_cm, SN_Traits$SRL,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="SRL",
     main= "Sorghastrum nutans")
abline(model_SN_HeightvSRL, lwd=4, lty=2) 
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$SRL~DO_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0002 *"), font=2)
text(locator(), labels = c("R2=0.003"), font=2)





################# run under the new models after this point #################



# LM for DO: Plant_Height_cm vs Root_Diam_Max
# LM for AP: Plant_Height_cm vs Root_Diam_Max
# LM for SM: Plant_Height_cm vs Root_Diam_Max
# LM for AG: Plant_Height_cm vs Root_Diam_Max
# LM for SN: Plant_Height_cm vs Root_Diam_Max

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Height_cm vs Root_Diam_Max
model_DO_HeightvRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvRootDiamMax)
# overall trend
# p= 0.2545 *, significant
# R2= 0.06092
# 10N Treatment
# p= not significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=
# Control: p=
# R2= 

plot(DO_Traits$Plant_Height_cm, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Max Root Diameter",
     main= "Dicanthelium oligosantheses")
abline(model_DO_HeightvRootDiamMax, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=DO_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.255"), font=2)
text(locator(), labels = c("R2=0.061"), font=2)



# LM for AP: Plant_Height_cm vs Root_Diam_Max
model_AP_HeightvRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvRootDiamMax)
# overall trend
# p= 6.302e-06, * significant
# R2= -0.06408
# 10N Treatment
# p= , significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=
# Control: p=
# R2=

plot(AP_Traits$Plant_Height_cm, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Maximum Root Diameter",
     main= "Ambrosia psylostachia")
abline(model_AP_HeightvRootDiamMax, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=6.3e-06 *"), font=2)
text(locator(), labels = c("R2=0.556"), font=2)



# LM for SM: Plant_Height_cm vs Root_Diam_Max
model_SM_HeightvRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvRootDiamMax)
# overall trend
# p= 0.2281, not significant
# R2: 0.04596

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00889 *
# Control: p=0.28762
# R2= 0.1547

plot(SM_Traits$Plant_Height_cm, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Maximum Root Diameter",
     main= "Solidago missouriensis")
#abline(model_SM_HeightvRootDiamMax, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Height_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Plant_Height_cm, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.287"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.1547"), font=2)




# LM for AG: Plant_Height_cm vs Root_Diam_Max
model_AG_HeightvRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvRootDiamMax)
# overall trend
# p= 0.2787, not significant
# R2= 0.03127
# 10N Treatment
# p= not significant

plot(AG_Traits$Plant_Height_cm, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Max Root Diameter",
     main= "Andropogon gerardii")
abline(model_AG_HeightvRootDiamMax, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=AG_N10_Traits), lwd=4, col="#769370")
text(locator(), labels = c("p=0.278"), font=2)
text(locator(), labels = c("R2=0.054"), font=2)



# LM for SN: Plant_Height_cm vs Root_Diam_Max
model_SN_HeightvRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvRootDiamMax)
# overall trend
# p= 0.718, not significant
# R2= -0.0643
# 10N Treatment
# p= not significant

plot(SN_Traits$Plant_Height_cm, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Max Root Diameter",
     main= "Sorghastrum nutans")
abline(model_SN_HeightvRootDiamMax, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=SN_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Plant_Height_cm, data=SN_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.718"), font=2)
text(locator(), labels = c("R2=-0.064"), font=2)






# LM for DO: Plant_Height_cm vs Root_Diam_Mean
# LM for AP: Plant_Height_cm vs Root_Diam_Mean
# LM for SM: Plant_Height_cm vs Root_Diam_Mean
# LM for AG: Plant_Height_cm vs Root_Diam_Mean
# LM for SN: Plant_Height_cm vs Root_Diam_Mean

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Height_cm vs Root_Diam_Mean
model_DO_HeightvRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvRootDiamMean)
# overall trend
# p= 0.5363, not significant
# R2= -0.01871
# 10N Treatment
# p= 1.04e-05 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.17e-06 *
# Control: p=0.156
# R2= 0.01365

plot(DO_Traits$Plant_Height_cm, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Mean Root Diameter",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_HeightvRootDiamMax, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Height_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Height_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.156"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.04e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.014"), font=2)



# LM for AP: Plant_Height_cm vs Root_Diam_Mean
model_AP_HeightvRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvRootDiamMean)
# overall trend
# p= 1.098e-07, * significant
# R2= 0.6522
# 10N Treatment
# p= not significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=
# Control: p=
# R2= 

plot(AP_Traits$Plant_Height_cm, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Mean Root Diameter",
     main= "Ambrosia psylostachia")
abline(model_AP_HeightvRootDiamMean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=1.1e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.652"), font=2)



# LM for SM: Plant_Height_cm vs Root_Diam_Mean
model_SM_HeightvRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvRootDiamMean)
# overall trend
# p= 0.3382, not significant
# R2: 0.01719
# 10N Treatment
# p= 0.000226 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0002 *
# Control: p=0.419
# R2= 0.03656

plot(SM_Traits$Plant_Height_cm, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Mean Root Diameter",
     main= "Solidago missouriensis")
#abline(model_SM_HeightvRootDiamMean, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Height_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Height_cm, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.419"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.037"), font=2)




# LM for AG: Plant_Height_cm vs Root_Diam_Mean
model_AG_HeightvRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvRootDiamMean)
# overall trend
# p= 0.3813, not significant
# R2= 0.1064
# 10N Treatment
# p= not significant  

plot(AG_Traits$Plant_Height_cm, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Mean Root Diameter",
     main= "Andropogon gerardii")
abline(model_AG_HeightvRootDiamMean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.381"), font=2)
text(locator(), labels = c("R2=0.021"), font=2)



# LM for SN: Plant_Height_cm vs Root_Diam_Mean
model_SN_HeightvRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvRootDiamMean)
# overall trend
# p= 0.9878, not significant
# R2= -0.1727
# 10N Treatment
# p= not significant

plot(SN_Traits$Plant_Height_cm, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Mean Root Diameter",
     main= "Sorghastrum nutans")
abline(model_SN_HeightvRootDiamMean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=SN_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Plant_Height_cm, data=SN_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.988"), font=2, col="#769370")
text(locator(), labels = c("R2=--0.173"), font=2)





########## From this point down needs to be done ########

# LM for DO: Plant_Area vs Focal_Root_Weight_g
# LM for AP: Plant_Area vs Focal_Root_Weight_g
# LM for SM: Plant_Area vs Focal_Root_Weight_g
# LM for AG: Plant_Area vs Focal_Root_Weight_g
# LM for SN: Plant_Area vs Focal_Root_Weight_g


#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area vs Focal_Root_Weight_g
model_DO_AreavFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavFocalRoot)
# overall trend
# p= 0.01212, significant
# R2= 0.1837
# 10N Treatment
# p= 0.03784 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.03784 *
# Control: p=0.81788
# R2= 0.3507

plot(DO_Traits$Plant_Area, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_AreavFocalRoot, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Plant_Area, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Focal_Root_Weight_g~Plant_Area, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.550"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.043 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.292"), font=2)



# LM for AP: Plant_Area vs Focal_Root_Weight_g
model_AP_AreavFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AreavFocalRoot)
# overall trend
# p= 5.948e-06 *, significant
# R2= 0.458
# 10N Treatment
# p= 0.00542 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00542 *
# Control: p=0.41678
# R2= 0.4342

plot(AP_Traits$Plant_Area, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
#abline(model_AP_AreavFocalRoot, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Plant_Area, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Focal_Root_Weight_g~Plant_Area, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.742"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.007 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.419"), font=2)



# LM for SM: Plant_Area vs Focal_Root_Weight_g
model_SM_AreavFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AreavFocalRoot)
# overall trend
# p= 0.010 * significant
# R2: 0.276
# 10N Treatment
# p= , not significant

plot(SM_Traits$Plant_Area, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(model_SM_AreavFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~Plant_Area, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~Plant_Area, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.001 *"), font=2)
text(locator(), labels = c("R2=0.276"), font=2)




# LM for AG: Plant_Area vs Focal_Root_Weight_g
model_AG_AreavFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AreavFocalRoot)
# overall trend
# p= 0.2024, not significant
# R2= 0.0465
# 10N Treatment
# p= 0.0348 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0348 *
# Control: p=0.718 
# R2= 0.063

plot(AG_Traits$Plant_Area, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
#abline(model_AG_AreavFocalRoot, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Plant_Area, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Focal_Root_Weight_g~Plant_Area, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.708 "), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.030 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.084"), font=2)



# LM for SN: Plant_Area vs Focal_Root_Weight_g
model_SN_AreavFocalRoot <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavFocalRoot)
# overall trend
# p= 0.3889, not significant
# R2= 0.004907
# 10N Treatment
# p= 2.68e-05 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.68e-05 *
# Control: p=0.426
# R2= 0.1024

plot(SN_Traits$Plant_Area, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Focal Root Weight (g)",
     main= "Sorghastrum nutans")
#abline(model_SN_AreavFocalRoot, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Plant_Area, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Focal_Root_Weight_g~Plant_Area, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.301"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.92e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.224"), font=2)







# LM for DO: Plant_Area vs Total_BeG_Weight_g
# LM for AP: Plant_Area vs Total_BeG_Weight_g
# LM for SM: Plant_Area vs Total_BeG_Weight_g
# LM for AG: Plant_Area vs Total_BeG_Weight_g
# LM for SN: Plant_Area vs Total_BeG_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area vs Total_BeG_Weight_g
model_DO_AreavTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavTotalRoot)
# overall trend
# p= 0.5356, not significant
# R2= -0.01783
# 10N Treatment
# p= 4.32e-05 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0088 *
# Control: p=0.152
# R2= 0.0075

plot(DO_Traits$Plant_Area, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_AreavTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Plant_Area, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~Plant_Area, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.085"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0007 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.039"), font=2)



# LM for AP: Plant_Area vs Total_BeG_Weight_g
model_AP_AreavTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AreavTotalRoot)
# overall trend
# p= 0.09959, not significant
# R2= 0.08835
# 10N Treatment
# p= 2.82e-05 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00455 *
# Control: p=0.2146
# R2= 0.1332

plot(AP_Traits$Plant_Area, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
#abline(model_AP_AreavTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Plant_Area, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~Plant_Area, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.168"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0001 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.108"), font=2)



# LM for SM: Plant_Area vs Total_BeG_Weight_g
model_SM_AreavTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AreavTotalRoot)
# overall trend
# p= 0.1032, not significant
# R2: 0.09526
# 10N Treatment
# p= 0.000164,* significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000164 *
# Control: p=0.269350
# R2= 0.2996

plot(SM_Traits$Plant_Area, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
#abline(model_SM_AreavTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Plant_Area, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~Plant_Area, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.424"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.017 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.017"), font=2)




# LM for AG: Plant_Area vs Total_BeG_Weight_g
model_AG_AreavTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AreavTotalRoot)
# overall trend
# p= 0.7076, not significant
# R2= -0.04173
# 10N Treatment
# p= 2.67e-06 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.67e-06 *
# Control: p=0.498
# R2= 0.2418

plot(AG_Traits$Plant_Area, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
#abline(model_AG_AreavTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Plant_Area, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~Plant_Area, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.080"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.078"), font=2)



# LM for SN: Plant_Area vs Total_BeG_Weight_g
model_SN_AreavTotalRoot <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavTotalRoot)
# overall trend
# p= 0.2644, not significant
# R2= 0.02956
# 10N Treatment
# p= 1.69e-09 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Plant_Area*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.69e-09
# Control: p=0.3012
# R2= -0.02139

plot(SN_Traits$Plant_Area, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
#abline(model_SN_AreavTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Plant_Area, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Total_BeG_Weight_g~Plant_Area, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.715"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.56e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.048"), font=2)







# LM for DO: Plant_Area vs SRL
# LM for AP: Plant_Area vs SRL
# LM for SM: Plant_Area vs SRL
# LM for AG: Plant_Area vs SRL
# LM for SN: Plant_Area vs SRL

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area vs SRL
model_DO_AreavSRL <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavSRL)
# overall trend
# p= 0.2045, not significant
# R2= 0.04484
# 10N Treatment
# p= 8.21e-07 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=8.21e-07 *
# Control: p=0.0914
# R2= 0.04484

plot(DO_Traits$Plant_Area, DO_Traits$SRL,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_AreavSRL, lwd=4, lty=2) 
abline(lm(SRL~Plant_Area, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Plant_Area, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.137"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.28e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.133"), font=2)



# LM for AP: Plant_Area vs SRL
model_AP_AreavSRL <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AreavSRL)
# overall trend
# p= 0.07618, not significant
# R2= 0.1028
# 10N Treatment
# p= 5.05e-09 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.05e-09 *
# Control: p=0.0657
# R2= 0.08006

plot(AP_Traits$Plant_Area, AP_Traits$SRL,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Ambrosia psylostachia")
#abline(model_AP_AreavSRL, lwd=4, lty=2) 
abline(lm(SRL~Plant_Area, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Plant_Area, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.102"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.5e-08 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.060"), font=2)



# LM for SM: Plant_Area vs SRL
model_SM_AreavSRL <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AreavSRL)
# overall trend
# p= 0.354, not significant
# R2: 0.01366
# 10N Treatment
# p= 4.5e-05 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=4.5e-05 *
# Control: p=0.523
# R2= 0.5182

plot(SM_Traits$Plant_Area, SM_Traits$SRL,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Solidago missouriensis")
#abline(model_SM_AreavSRL, lwd=4, lty=2) 
abline(lm(SRL~Plant_Area, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Plant_Area, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.850"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0008 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.045"), font=2)




# LM for AG: Plant_Area vs SRL
model_AG_AreavSRL <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AreavSRL)
# overall trend
# p= 0.187, not significant
# R2= 0.05118
# 10N Treatment
# p= 4.65e-11 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=4.65e-11 *, significant
# Control: p=0.4246
# R2= -0.01936

plot(AG_Traits$Plant_Area, AG_Traits$SRL,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(model_AG_AreavSRL, lwd=4, lty=2) 
abline(lm(SRL~Plant_Area, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Plant_Area, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.743"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.8e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.050"), font=2)



# LM for SN: Plant_Area vs SRL
model_SN_AreavSRL <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavSRL)
# overall trend
# p= 0.8073, not significant
# R2= -0.05378
# 10N Treatment
# p= 2.09e-09 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Plant_Area*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.09e-09 *, significant
# Control: p=0.635
# R2= -0.02377

plot(SN_Traits$Plant_Area, SN_Traits$SRL,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Sorghastrum nutans")
#abline(model_SN_AreavSRL, lwd=4, lty=2) 
abline(lm(SRL~Plant_Area, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(SRL~Plant_Area, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.308"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.3e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.022"), font=2)







# LM for DO: Plant_Area vs Root_Diam_Max
# LM for AP: Plant_Area vs Root_Diam_Max
# LM for SM: Plant_Area vs Root_Diam_Max
# LM for AG: Plant_Area vs Root_Diam_Max
# LM for SN: Plant_Area vs Root_Diam_Max

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area vs Root_Diam_Max
model_DO_AreavRootDiamMax <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavRootDiamMax)
# overall trend
# p= 0.03675, not significant
# R2= 0.04484
# 10N Treatment
# p= 8.26e-07 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=8.26e-07 *
# Control: p=0.363
# R2= 0.0114

plot(DO_Traits$Plant_Area, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Area, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Plant_Area, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.794"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.17e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.050"), font=2)



# LM for AP: Plant_Area vs Root_Diam_Max
model_AP_AreavRoot_Diam_Max <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AreavRoot_Diam_Max)
# overall trend
# p= 0.2283, not significant
# R2= 0.04027
# 10N Treatment
# p= 2.57e-06 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.57e-06 *, significant
# Control: p=0.4323 
# R2= 0.01383

plot(AP_Traits$Plant_Area, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Root_Diam_Max",
     main= "Ambrosia psylostachia")
#abline(model_AP_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Area, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Plant_Area, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.253"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.7e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.014"), font=2)



# LM for SM: Plant_Area vs Root_Diam_Max
model_SM_AreavRoot_Diam_Max <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AreavRoot_Diam_Max)
# overall trend
# p= 0.6227, not significant
# R2: -0.03502
# 10N Treatment
# p= 2.69e-09 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.69e-09 *
# Control: p=0.774
# R2= 0.002614

plot(SM_Traits$Plant_Area, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Maximum Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(model_SM_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Area, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Plant_Area, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.107"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0003 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.177"), font=2)




# LM for AG: Plant_Area vs Root_Diam_Max
model_AG_AreavRoot_Diam_Max <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AreavRoot_Diam_Max)
# overall trend
# p= 0.5982, not significant
# R2= -0.02765
# 10N Treatment
# p= 2.89e-10 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.89e-10 *, significant
# Control: p=0.179
# R2= 0.02034

plot(AG_Traits$Plant_Area, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Maximum Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(model_AG_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Area, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Plant_Area, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.139"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.28e-08 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.012"), font=2)



# LM for SN: Plant_Area vs SRL
model_SN_AreavRoot_Diam_Max <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavRoot_Diam_Max)
# overall trend
# p= 0.8617, not significant
# R2= -0.06123
# 10N Treatment
# p< 2e-16 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Plant_Area*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p< 2e-16 *, significant
# Control: p=0.861
# R2= -0.02377

plot(SN_Traits$Plant_Area, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Maximum Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(model_SN_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Plant_Area, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Root_Diam_Max~Plant_Area, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.927"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.0e-10 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.090"), font=2)





# LM for DO: Plant_Area vs Root_Diam_Mean
# LM for AP: Plant_Area vs Root_Diam_Mean
# LM for SM: Plant_Area vs Root_Diam_Mean
# LM for AG: Plant_Area vs Root_Diam_Mean
# LM for SN: Plant_Area vs Root_Diam_Mean

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area vs Root_Diam_Mean
model_DO_AreavRootDiamMean <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavRootDiamMean)
# overall trend
# p= 0.2184, not significant
# R2= 0.04301
# 10N Treatment
# p= 8.84e-13 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=8.84e-13 *
# Control: p=0.512
# R2= 0.01693

plot(DO_Traits$Plant_Area, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Mean Root Diameter",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Area, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Area, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.283"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.5e-13 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.141"), font=2)



# LM for AP: Plant_Area vs Root_Diam_Mean
model_AP_AreavRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AreavRoot_Diam_Mean)
# overall trend
# p= 0.6435, not significant
# R2= -0.03395
# 10N Treatment
# p= 3.33e-06 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=3.33e-06 *, significant
# Control: p=0.220 
# R2= -0.02696

plot(AP_Traits$Plant_Area, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(model_AP_AreavRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Area, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Area, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.095"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.5e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.007"), font=2)



# LM for SM: Plant_Area vs Root_Diam_Mean
model_SM_AreavRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AreavRoot_Diam_Mean)
# overall trend
# p= 0.6774, not significant
# R2: -0.04359
# 10N Treatment
# p= 1.64e-11 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.64e-11 *
# Control: p=0.774
# R2= -0.02399

plot(SM_Traits$Plant_Area, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(model_SM_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Area, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Area, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.336"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.2e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.023"), font=2)




# LM for AG: Plant_Area vs Root_Diam_Max
model_AG_AreavRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AreavRoot_Diam_Mean)
# overall trend
# p= 0.08763, not significant
# R2= 0.09528 
# 10N Treatment
# p= 5.84e-14 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.84e-14 *, significant
# Control: p=0.05
# R2= 0.05912

plot(AG_Traits$Plant_Area, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(model_AG_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Area, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Plant_Area, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.182"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.9e-11 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.030"), font=2)



# LM for SN: Plant_Area vs Root_Diam_Mean
model_SN_AreavRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavRoot_Diam_Mean)
# overall trend
# p= 0.9369, not significant
# R2= -0.07304
# 10N Treatment
# p= 5.9e-14 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Plant_Area*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.9e-14 *, significant
# Control: p=0.456
# R2= -0.04354

plot(SN_Traits$Plant_Area, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Plant Area (cm^3)",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(model_SN_AreavSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Plant_Area, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Root_Diam_Mean~Plant_Area, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.727"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.6e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.076"), font=2)









# LM for DO: Leaf_Thickness_cm vs Focal_Root_Weight_g
# LM for AP: Leaf_Thickness_cm vs Focal_Root_Weight_g
# LM for SM: Leaf_Thickness_cm vs Focal_Root_Weight_g
# LM for AG: Leaf_Thickness_cm vs Focal_Root_Weight_g
# LM for SN: Leaf_Thickness_cm vs Focal_Root_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Leaf_Thickness_cm vs Focal_Root_Weight_g
model_DO_LeafThvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvFocalRoot)
# overall trend
# p= 0.4252, not significant
# R2= -0.00117
# 10N Treatment
# p= 0.000413 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000413 *
# Control: p=0.150916
# R2= 0.04177

plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.151"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.033"), font=2)



# LM for AP: Leaf thickness vs Focal_Root_Weight_g
model_AP_LeafThvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvFocalRoot)
# overall trend
# p= 0.06518 not significant
# R2= 0.1785
# 10N Treatment
# p= 0.1649, not significant


plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
abline(model_AP_LeafThvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.065"), font=2)
text(locator(), labels = c("R2=0.148"), font=2)



# LM for SM: Leaf_Thickness_cm vs Focal_Root_Weight_g
model_SM_LeafThvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvFocalRoot)
# overall trend
# p= 0.05715 significant
# R2: 0.1516 
# 10N Treatment
# p= 0.5130, not significant

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(model_SM_LeafThvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.057"), font=2)
text(locator(), labels = c("R2=0.172"), font=2)




# LM for AG: Leaf_Thickness_cm vs Focal_Root_Weight_g
model_AG_LeafThvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvFocalRoot)
# overall trend
# p= 0.01313, *significant
# R2= 0.06157
# 10N Treatment
# p= 0.6772, not significant

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
abline(model_AG_LeafThvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.013 *"), font=2)
text(locator(), labels = c("R2=0.267"), font=2)



# LM for SN: Leaf_Thickness_cm vs Focal_Root_Weight_g
model_SN_LeafThvFocalRoot <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AreavFocalRoot)
# overall trend
# p= 0.3889, not significant
# R2= 0.004907
# 10N Treatment
# p= 2.68e-05 *, significant

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=2.68e-05 *, significant
# Control: p=0.321
# R2= 0.1269

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Focal Root Weight (g)",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Focal_Root_Weight_g~Leaf_Thickness_cm, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.321"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.68e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.127"), font=2)







# LM for DO: Leaf_Thickness_cm vs Total_BeG_Weight_g
# LM for AP: Leaf_Thickness_cm vs Total_BeG_Weight_g
# LM for SM: Leaf_Thickness_cm vs Total_BeG_Weight_g
# LM for AG: Leaf_Thickness_cm vs Total_BeG_Weight_g
# LM for SN: Leaf_Thickness_cm vs Total_BeG_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Thickness_cm vs Total_BeG_Weight_g
model_DO_LeafThvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvTotalRoot)
# overall trend
# p= 0.5318, not significant
# R2= -0.0173
# 10N Treatment
# p= 0.02879 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00117 *
# Control: p=0.36312
# R2= 0.05146

plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.838"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.029 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.049"), font=2)



# LM for AP: Leaf thickness vs Total_BeG_Weight_g
model_AP_LeafThvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvTotalRoot)
# overall trend
# p= 0.3349, not significant
# R2= 0.01553
# 10N Treatment
# p= 0.000889 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00117 *
# Control: p=0.497
# R2= -0.02937 

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
abline(model_AP_LeafThvTotalRoot, lwd=4, lty=2) 
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.663"), font=2, col="#BDB2A7")
text(locator(), labels = c("R2=0.047"), font=2)



# LM for SM: Leaf_Thickness_cm vs Total_BeG_Weight_g
model_SM_LeafThvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvTotalRoot)
# overall trend
# p= 0.1132, not significant
# R2: 0.08969 
# 10N Treatment
# p= 0.2672, not significant

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
abline(model_SM_LeafThvTotalRoot, lwd=4, lty=2) 
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.180"), font=2)
text(locator(), labels = c("R2=0.088"), font=2)




# LM for AG: Leaf_Thickness_cm vs Total_BeG_Weight_g
model_AG_LeafThvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvTotalRoot)
# overall trend
# p= 0.2976, not significant
# R2= 0.00172
# 10N Treatment
# p= 0.00786 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00786 *
# Control: p=0.22488
# R2= -0.01434 

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
abline(model_AG_LeafThvTotalRoot, lwd=4, lty=2) 
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.299"), font=2)
text(locator(), labels = c("R2=0.044"), font=2)



# LM for SN: Leaf_Thickness_cm vs Total_BeG_Weight_g
model_SN_LeafThvTotalRoot <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvTotalRoot)
# overall trend
# p= 0.2777, not significant
# R2= 0.02654
# 10N Treatment
# p= 0.0181 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0146 *, significant
# Control: p=0.3181
# R2= -0.01937

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Total_BeG_Weight_g~Leaf_Thickness_cm, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.405"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.017 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.0008"), font=2)






# LM for DO: Leaf_Thickness_cm vs SRL
# LM for AP: Leaf_Thickness_cm vs SRL
# LM for SM: Leaf_Thickness_cm vs SRL
# LM for AG: Leaf_Thickness_cm vs SRL
# LM for SN: Leaf_Thickness_cm vs SRL

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Thickness_cm vs SRL
model_DO_LeafThvSRL <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvSRL)
# overall trend
# p= 0.5318, not significant
# R2= -0.03143
# 10N Treatment
# p= 0.000395 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000395 *
# Control: p=0.364113
# R2= -0.04421

plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$SRL,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~Leaf_Thickness_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Leaf_Thickness_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.718"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.019 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.044"), font=2)



# LM for AP: Leaf thickness vs SRL
model_AP_LeafThvSRL <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvSRL)
# overall trend
# p= 0.07637, not significant
# R2= 0.1026
# 10N Treatment
# p= 0.0208 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.69e-07 *
# Control: p=0.0769
# R2= 0.01265

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$SRL,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="SRL",
     main= "Ambrosia psylostachia")
#abline(model_AP_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~Leaf_Thickness_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Leaf_Thickness_cm, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.709"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.021 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.016"), font=2)



# LM for SM: Leaf_Thickness_cm vs SRL
model_SM_LeafThvSRL <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvSRL)
# overall trend
# p= 0.5633, not significant
# R2= -0.02543 
# 10N Treatment
# p= 0.175, not significant

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$SRL,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="SRL",
     main= "Solidago missouriensis")
abline(model_SM_LeafThvSRL, lwd=4, lty=2) 
#abline(lm(SRL~Leaf_Thickness_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(SRL~Leaf_Thickness_cm, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.785"), font=2)
text(locator(), labels = c("R2=-0.084"), font=2)




# LM for AG: Leaf_Thickness_cm vs SRL
model_AG_LeafThvSRL <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvSRL)
# overall trend
# p= 0.1846, not significant
# R2= 0.06048
# 10N Treatment
# p= 0.000785 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000785 *
# Control: p=0.208730
# R2= -0.002452 

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$SRL,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(model_AG_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~Leaf_Thickness_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~Leaf_Thickness_cm, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.414"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.008 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.018"), font=2)



# LM for SN: Leaf_Thickness_cm vs SRL
model_SN_LeafThvSRL <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvSRL)
# overall trend
# p= 0.9076, not significant
# R2= -0.06811
# 10N Treatment
# p= 0.00901 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00901 *, significant
# Control: p=0.66698
# R2= -0.04593

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$SRL,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="SRL",
     main= "Sorghastrum nutans")
abline(model_SN_LeafThvSRL, lwd=4, lty=2) 
#abline(lm(SRL~Leaf_Thickness_cm, data=SN_N0_Traits), lwd=4, col="#769370")
#abline(lm(SRL~Leaf_Thickness_cm, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.920"), font=2)
text(locator(), labels = c("R2=-0.105"), font=2)






# LM for DO: Leaf_Thickness_cm vs Root_Diam_Max
# LM for AP: Leaf_Thickness_cm vs Root_Diam_Max
# LM for SM: Leaf_Thickness_cm vs Root_Diam_Max
# LM for AG: Leaf_Thickness_cm vs Root_Diam_Max
# LM for SN: Leaf_Thickness_cm vs Root_Diam_Max


#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Leaf_Thickness_cm vs Root_Diam_Max
model_DO_LeafThvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvRoot_Diam_Max)
# overall trend
# p= 0.0267, not significant
# R2= 0.1519
# 10N Treatment
# p= 0.000754 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000754 *
# Control: p=0.442385
# R2= 0.06147 

plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Maximum Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.126"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0003 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.104"), font=2)



# LM for AP: Leaf thickness vs SRL
model_AP_LeafThvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvRoot_Diam_Max)
# overall trend
# p= 0.5064, not significant
# R2= -0.0142 
# 10N Treatment
# p= 0.0016 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0016 *
# Control: p=0.6427
# R2= -0.03549

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Maximum Root Diameter (cm)",
     main= "Ambrosia psylostachia")
abline(model_AP_LeafThvRoot_Diam_Max, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.698"), font=2)
text(locator(), labels = c("R2=-0.054"), font=2)



# LM for SM: Leaf_Thickness_cm vs Root_Diam_Max
model_SM_LeafThvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvRoot_Diam_Max)
# overall trend
# p= 0.7946, not significant
# R2= -0.06181 
# 10N Treatment
# p= 0.043, *significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.043 *
# Control: p=0.678
# R2= -0.04828

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Maximum Root Diameter (cm)",
     main= "Solidago missouriensis")
abline(model_SM_LeafThvRoot_Diam_Max, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.697"), font=2)
text(locator(), labels = c("R2=-0.062"), font=2)




# LM for AG: Leaf_Thickness_cm vs Root_Diam_Max
model_AG_LeafThvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvRoot_Diam_Max)
# overall trend
# p= 0.4554, not significant
# R2= -0.006775
# 10N Treatment
# p= 0.00525 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000785 *
# Control: p=0.25278
# R2= 0.06793 

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Maximum Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(model_AG_LeafThvRoot_Diam_Max, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.716"), font=2)
text(locator(), labels = c("R2=0.068"), font=2)



# LM for SN: Leaf_Thickness_cm vs Root_Diam_Max
model_SN_LeafThvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvRoot_Diam_Max)
# overall trend
# p= 0.01391,* significant
# R2= 0.1777
# 10N Treatment
# p= 0.078749, not significant

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Maximum Root Diameter (cm)",
     main= "Sorghastrum nutans")
abline(model_SN_LeafThvRoot_Diam_Max, lwd=4, lty=2) 
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=SN_N0_Traits), lwd=4, col="#769370")
#abline(lm(Root_Diam_Max~Leaf_Thickness_cm, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.060"), font=2)
text(locator(), labels = c("R2=0.147"), font=2)







# LM for DO: Leaf_Thickness_cm vs Root_Diam_Mean
# LM for AP: Leaf_Thickness_cm vs Root_Diam_Mean
# LM for SM: Leaf_Thickness_cm vs Root_Diam_Mean
# LM for AG: Leaf_Thickness_cm vs Root_Diam_Mean
# LM for SN: Leaf_Thickness_cm vs Root_Diam_Mean

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Thickness_cm vs Root_Diam_Mean
model_DO_LeafThvRootDiamMean <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvRootDiamMean)
# overall trend
# p= 0.374, not significant
# R2= 0.007965
# 10N Treatment
# p= 3.83e-12 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=3.83e-12 *
# Control: p=0.772
# R2= 0.04474

plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.361"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=8.09e-11 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.033"), font=2)



# LM for AP: Leaf Thickness vs Root_Diam_Mean
model_AP_LeafThvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvRoot_Diam_Mean)
# overall trend
# p= 0.4898, not significant
# R2= -0.01163
# 10N Treatment
# p= 0.00403 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00403 *, significant
# Control: p=0.15771
# R2= -0.01371

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
abline(model_AP_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.823"), font=2)
text(locator(), labels = c("R2=-0.083"), font=2)



# LM for SM: Leaf_Thickness_cm vs Root_Diam_Mean
model_SM_LeafThvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvRoot_Diam_Mean)
# overall trend
# p= 0.9065, not significant
# R2: -0.08084
# 10N Treatment
# p= 0.00085 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00085 *
# Control: p=0.66102
# R2= -0.065

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
abline(model_SM_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.582"), font=2)
text(locator(), labels = c("R2=-0.034"), font=2)




# LM for AG: Leaf_Thickness_cm vs Root_Diam_Max
model_AG_LeafThvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvRoot_Diam_Mean)
# overall trend
# p= 0.02636, * significant
# R2= 0.1798 
# 10N Treatment
# p= 0.0003 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0003 *, significant
# Control: p=0.0111, *significant
# R2= 0.1648

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(model_AG_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.034 *"), font=2)
text(locator(), labels = c("R2=0.165"), font=2)



# LM for SN: Leaf_Thickness_cm vs Root_Diam_Mean
model_SN_LeafThvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvRoot_Diam_Mean)
# overall trend
# p= 0.1939, not significant
# R2= 0.04792
# 10N Treatment
# p= 0.0937, not significant

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
abline(model_SN_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=SN_N0_Traits), lwd=4, col="#769370")
#abline(lm(Root_Diam_Mean~Leaf_Thickness_cm, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.459"), font=2)
text(locator(), labels = c("R2=0.003"), font=2)



# LM for DO: F_Leaf_Stage vs Focal_Root_Weight_g
# LM for AP: F_Leaf_Stage vs Focal_Root_Weight_g
# LM for SM: F_Leaf_Stage vs Focal_Root_Weight_g
# LM for AG: F_Leaf_Stage vs Focal_Root_Weight_g
# LM for SN: F_Leaf_Stage vs Focal_Root_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Focal_Root_Weight_g
model_DO_FLeafvFocalRoot <- lm(formula = Focal_Root_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvFocalRoot)
# overall trend
# p= 0.02043, * significant
# R2= 0.231
# 10N Treatment
# p= 0.35477, not significant

plot(DO_Traits$F_Leaf_Stage, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Focal Root Weight (g)",
     main= "Dicanthelium oligosantheses")
abline(model_DO_FLeafvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=DO_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.020 *"), font=2)
text(locator(), labels = c("R2=0.231"), font=2)



# LM for AP: F_Leaf_Stage vs Focal_Root_Weight_g
model_AP_FLeafvFocalRoot <- lm(formula = Focal_Root_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvFocalRoot)
# overall trend
# p= 0.002549, * significant
# R2= 0.346
# 10N Treatment
# p= 0.226965, not significant

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Focal Root Weight (g)",
     main= "Ambrosia psylostachia")
abline(model_AP_FLeafvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=AP_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.002 *"), font=2)
text(locator(), labels = c("R2=0.346"), font=2)



# LM for SM: F_Leaf_Stage vs Focal_Root_Weight_g
model_SM_FLeafvFocalRoot <- lm(formula = Focal_Root_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvFocalRoot)
# overall trend
# p= 0.1713 ,  not significant
# R2: 0.0918
# 10N Treatment
# p= 0.4192, not significant

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Focal Root Weight (g)",
     main= "Solidago missouriensis")
abline(model_SM_FLeafvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=SM_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.171"), font=2)
text(locator(), labels = c("R2=0.092"), font=2)


# LM for AG: F_Leaf_Stage vs Focal_Root_Weight_g
model_AG_FLeafvFocalRoot <- lm(formula = Focal_Root_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvFocalRoot)
# overall trend
# p= 0.510, not significant
# R2= 0.0140
# 10N Treatment
# p= 0.072, not significant

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Focal Root Weight (g)",
     main= "Andropogon gerardii")
abline(model_AG_FLeafvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=AG_N10_Traits), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.510"), font=2)
text(locator(), labels = c("R2=0.014"), font=2)


# LM for SN: F_Leaf_Stage vs Focal_Root_Weight_g
model_SN_FLeafvFocalRoot <- lm(formula = Focal_Root_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvFocalRoot)
# overall trend
# p= 0.0008, * significant
# R2= 0.377
# 10N Treatment
# p= not significant

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Focal Root Weight (g)",
     main= "Sorghastrum nutans")
abline(model_SN_FLeafvFocalRoot, lwd=4, lty=2) 
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=SN_N0_Traits), lwd=4, col="#769370")
#abline(lm(Focal_Root_Weight_g~F_Leaf_Stage, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0008 *"), font=2)
text(locator(), labels = c("R2=0.377"), font=2)






# LM for DO: F_Leaf_Stage vs Total_BeG_Weight_g
# LM for AP: F_Leaf_Stage vs Total_BeG_Weight_g
# LM for SM: F_Leaf_Stage vs Total_BeG_Weight_g
# LM for AG: F_Leaf_Stage vs Total_BeG_Weight_g
# LM for SN: F_Leaf_Stage vs Total_BeG_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Focal_Root_Weight_g
model_DO_FLeafvTotalRoot <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvTotalRoot)
# overall trend
# p= 0.5337, not significant
# R2= -0.01757
# 10N Treatment
# p= 0.0155, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0155 *, significant
# Control: p=0.377
# R2= -0.01956


plot(DO_Traits$F_Leaf_Stage, DO_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_FLeafvTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.377"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.016 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.020"), font=2)



# LM for AP: F_Leaf_Stage vs Total_BeG_Weight_g
model_AP_FLeafvTotalRoot <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvTotalRoot)
# overall trend
# p= 0.3571, not significant
# R2= 0.01117
# 10N Treatment
# p= 0.01143, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.01143 *, significant
# Control: p=0.574
# R2= 0.02549

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
#abline(model_AP_FLeafvTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=-0.593"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.011 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.016"), font=2)



# LM for SM: F_Leaf_Stage vs Total_BeG_Weight_g
model_SM_FLeafvTotalRoot <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvTotalRoot)
# overall trend
# p= 0.1173,  not significant
# R2= 0.08753
# 10N Treatment
# p= 0.0058, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.0058 *, significant
# Control: p=0.258
# R2= -0.004914

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
#abline(model_SM_FLeafvTotalRoot, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.642"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.006 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.005"), font=2)


# LM for AG: F_Leaf_Stage vs Focal_Root_Weight_g
model_AG_FLeafvTotalRoot <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvTotalRoot)
# overall trend
# p= 0.7905, not significant
# R2= -0.05274
# 10N Treatment
# p= 0.000425 *, significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000425 *, significant
# Control: p=0.337
# R2= -0.05881

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
#abline(model_AG_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.337"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.059"), font=2)


# LM for SN: F_Leaf_Stage vs Total_BeG_Weight_g
model_SN_FLeafvTotalRoot <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvTotalRoot)
# overall trend
# p= 0.2512, not significant
# R2= 0.03265 
# 10N Treatment
# p= 0.0046, * significant

# 10N and Control
reg1 <- lm(formula = Total_BeG_Weight_g~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=6.06e-05 *, significant
# Control: p=0.2908
# R2= 0.4077

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Total_BeG_Weight_g,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Total_BeG_Weight_g~F_Leaf_Stage, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.952"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=v0.005 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.030"), font=2)


# LM for DO: F_Leaf_Stage vs Root_Diam_Max
# LM for AP: F_Leaf_Stage vs Root_Diam_Max
# LM for SM: F_Leaf_Stage vs Root_Diam_Max
# LM for AG: F_Leaf_Stage vs Root_Diam_Max
# LM for SN: F_Leaf_Stage vs Root_Diam_Max

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Root_Diam_Max
model_DO_FLeafvRootDiamMax <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvRootDiamMax)
# overall trend
# p= 0.09537, not significant
# R2= 0.1208
# 10N Treatment
# p= 0.00109 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00109 *
# Control: p=0.87591
# R2= 0.01525

plot(DO_Traits$F_Leaf_Stage, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Maximum Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.876"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.001 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.015"), font=2)



# LM for AP: F_Leaf_Stage vs Root_Diam_Max
model_AP_FLeafvRoot_Diam_Max <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvRoot_Diam_Max)
# overall trend
# p= 0.4436, not significant
# R2= -0.004226
# 10N Treatment
# p= 0.000945 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000945 *, significant
# Control: p=0.780233
# R2=0.04016 

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Maximum Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(model_AP_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.044 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.040"), font=2)



# LM for SM: F_Leaf_Stage vs Root_Diam_Max
model_SM_FLeafvRoot_Diam_Max <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvRoot_Diam_Max)
# overall trend
# p= 0.4861, not significant
# R2: -0.01224
# 10N Treatment
# p= 0.000512 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000512 *
# Control: p=0.481082
# R2= 0.0389

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Maximum Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(model_SM_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.481"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0005 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.039"), font=2)




# LM for AG: F_Leaf_Stage vs Root_Diam_Max
model_AG_FLeafvRoot_Diam_Max <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvRoot_Diam_Max)
# overall trend
# p= 0.4823, not significant
# R2= -0.01047 
# 10N Treatment
# p= 1.43e-06 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.43e-06 *, significant
# Control: p=0.692
# R2= 0.01256

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Maximum Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(model_AG_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.692"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.43e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.013"), font=2)



# LM for SN: F_Leaf_Stage vs Root_Diam_Max
model_SN_FLeafvRoot_Diam_Max <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvRoot_Diam_Max)
# overall trend
# p= 0.8733, not significant
# R2= -0.06291
# 10N Treatment
# p= 1.35e-10, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=5.48e-06 *, significant
# Control: p=0.802
# R2= -0.09165

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Maximum Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Root_Diam_Max~F_Leaf_Stage, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.802"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.48e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.092"), font=2)







# LM for DO: F_Leaf_Stage vs Root_Diam_Mean
# LM for AP: F_Leaf_Stage vs Root_Diam_Mean
# LM for SM: F_Leaf_Stage vs Root_Diam_Mean
# LM for AG: F_Leaf_Stage vs Root_Diam_Mean
# LM for SN: F_Leaf_Stage vs Root_Diam_Mean

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Focal_Root_Weight_g
model_DO_FLeafvRootDiamMean <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvRootDiamMean)
# overall trend
# p= 0.1468, not significant
# R2= 0.09918
# 10N Treatment
# p= 1.07e-07*, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.07e-07 *
# Control: p=0.260
# R2= 0.1137

plot(DO_Traits$F_Leaf_Stage, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.260"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.07e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2= 0.114"), font=2)



# LM for AP: F_Leaf_Stage vs Root_Diam_Mean
model_AP_FLeafvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvRoot_Diam_Mean)
# overall trend
# p= 0.2459, not significant
# R2= 0.05448
# 10N Treatment
# p= 0.00497 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00497 *, significant
# Control: p=0.02419
# R2= 0.05375

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(model_AP_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.024"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.005 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.054"), font=2)



# LM for SM: F_Leaf_Stage vs Root_Diam_Mean
model_SM_FLeafvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvRoot_Diam_Mean)
# overall trend
# p= 0.2886, not significant
# R2: 0.0473
# 10N Treatment
# p= 1.6e-06 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.6e-06 *
# Control: p=0.0978
# R2= 0.09811

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(model_SM_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.10"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.6e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.098"), font=2)




# LM for AG: F_Leaf_Stage vs Root_Diam_Mean
model_AG_FLeafvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvRoot_Diam_Mean)
# overall trend
# p= 0.01065, * significant
# R2= 0.1973
# 10N Treatment
# p= 1.37e-07 *, significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~F_Leaf_Stage+Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=1.37e-07 *, significant
# Control: p=0.416288, * significant
# R2= 0.2197

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(model_AG_LeafThvSRL, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.416"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.37e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.2197"), font=2)



# LM for SN: F_Leaf_Stage vs Root_Diam_Mean
model_SN_FLeafvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvRoot_Diam_Mean)
# overall trend
# p= 0.9937, *significant
# R2= -0.1459 
# 10N Treatment
# p= 0.0002, * significant

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000229 *, significant
# Control: p=0.926797
# R2= -0.07009

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(Root_Diam_Mean~F_Leaf_Stage, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.927"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.070"), font=2)






# LM for DO: F_Leaf_Stage vs SRL
# LM for AP: F_Leaf_Stage vs SRL
# LM for SM: F_Leaf_Stage vs SRL
# LM for AG: F_Leaf_Stage vs SRL
# LM for SN: F_Leaf_Stage vs SRL

#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: F_Leaf_Stage vs SRL
model_DO_FLeafvSRL <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvSRL)
# overall trend
# p= 0.7114, not significant
# R2= -0.05486
# 10N Treatment
# p= 0.000878 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.000878 *
# Control: p=0.801364
# R2= 0.00424

plot(DO_Traits$F_Leaf_Stage, DO_Traits$SRL,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="SRL",
     main= "Dicanthelium oligosantheses")
#abline(model_DO_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~F_Leaf_Stage, data=DO_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~F_Leaf_Stage, data=DO_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.801"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0009 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.004"), font=2)



# LM for AP: F_Leaf_Stage vs Root_Diam_Mean
model_AP_FLeafvSRL <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvSRL)
# overall trend
# p= 0.3031, not significant
# R2= 0.03676
# 10N Treatment
# p= 6.42e-06 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=6.42e-06 *, significant
# Control: p=0.4023
# R2= 0.07054

plot(AP_Traits$F_Leaf_Stage, AP_Traits$SRL,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="SRL",
     main= "Ambrosia psylostachia")
#abline(model_AP_LeafThvRoot_Diam_Mean, lwd=4, lty=2) 
abline(lm(SRL~F_Leaf_Stage, data=AP_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~F_Leaf_Stage, data=AP_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.402"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=6.42e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.071"), font=2)



# LM for SM: F_Leaf_Stage vs Root_Diam_Mean
model_SM_FLeafvSRL <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvSRL)
# overall trend
# p= 0.5685, not significant
# R2= -0.03029
# 10N Treatment
# p= 0.007 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.007 *
# Control: p=0.329
# R2= -0.02307

plot(SM_Traits$F_Leaf_Stage, SM_Traits$SRL,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="SRL",
     main= "Solidago missouriensis")
#abline(model_SM_AreavRoot_Diam_Max, lwd=4, lty=2) 
abline(lm(SRL~F_Leaf_Stage, data=SM_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~F_Leaf_Stage, data=SM_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.329"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.007 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.0231"), font=2)




# LM for AG: F_Leaf_Stage vs SRL
model_AG_FLeafvSRL <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvSRL)
# overall trend
# p= 0.1646, not significant
# R2= 0.08355
# 10N Treatment
# p= 3.47e-10 *, significant

# 10N and Control
reg1 <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=3.47e-10 *, significant
# Control: p=0.5251
# R2= 0.09234 

plot(AG_Traits$F_Leaf_Stage, AG_Traits$SRL,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(model_AG_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~F_Leaf_Stage, data=AG_N0_Traits), lwd=4, col="#BDB2A7")
abline(lm(SRL~F_Leaf_Stage, data=AG_N10_Traits), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.525 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.47e-10 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.092"), font=2)



# LM for SN: F_Leaf_Stage vs SRL
model_SN_FLeafvSRL <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvSRL)
# overall trend
# p= 0.9305, not significant
# R2= -0.109
# 10N Treatment
# p= 0.00361, * significant

# 10N and Control
reg1 <- lm(formula = SRL~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)
# N10: p=0.00361 *, significant
# Control: p=0.55497
# R2= -0.04233

plot(SN_Traits$F_Leaf_Stage, SN_Traits$SRL,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="SRL",
     main= "Sorghastrum nutans")
#abline(model_SN_LeafThvSRL, lwd=4, lty=2) 
abline(lm(SRL~F_Leaf_Stage, data=SN_N0_Traits), lwd=4, col="#769370")
abline(lm(SRL~F_Leaf_Stage, data=SN_N10_Traits), lwd=4, col="#BDB2A7")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.555"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.042"), font=2)











# LM for DO: F_Leaf_Stage vs Percent_N_BeG
# LM for AP: F_Leaf_Stage vs Percent_N_BeG
# LM for SM: F_Leaf_Stage vs Percent_N_BeG
# LM for AG: F_Leaf_Stage vs Percent_N_BeG
# LM for SN: F_Leaf_Stage vs Percent_N_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Percent_N_BeG
model_DO_FLeafvN_BegG <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvN_BegG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$F_Leaf_Stage, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.144"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.1e-10 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.373"), font=2)


# LM for AP: F_Leaf_Stage vs Percent_N_BeG
model_AP_FLeafvN_BegG <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.5e-12 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.578"), font=2)




# LM for SM: F_Leaf_Stage vs Percent_N_BeG
model_SM_FLeafvN_BegG <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.210"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=7.1e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.113"), font=2)




# LM for AG: F_Leaf_Stage vs Percent_N_BeG
model_AG_FLeafvN_BegG <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.006 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.3e-11 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.376"), font=2)


# LM for SN: F_Leaf_Stage vs Percent_N_BeG
model_SN_FLeafvN_BegG <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.537"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.2e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.526"), font=2)





# LM for DO: F_Leaf_Stage vs Percent_C_BeG
# LM for AP: F_Leaf_Stage vs Percent_C_BeG
# LM for SM: F_Leaf_Stage vs Percent_C_BeG
# LM for AG: F_Leaf_Stage vs Percent_C_BeG
# LM for SN: F_Leaf_Stage vs Percent_C_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: F_Leaf_Stage vs Percent_C_BeG
model_DO_FLeafvC_BegG <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_FLeafvC_BegG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$F_Leaf_Stage, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.572"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.011"), font=2)


# LM for AP: F_Leaf_Stage vs Percent_C_BeG
model_AP_FLeafvC_BegG <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_FLeafvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$F_Leaf_Stage, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.828"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.002"), font=2)




# LM for SM: F_Leaf_Stage vs Percent_C_BeG
model_SM_FLeafvC_BegG <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_FLeafvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$F_Leaf_Stage, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.833"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.109"), font=2)




# LM for AG: F_Leaf_Stage vs Percent_C_BeG
model_AG_FLeafvC_BegG <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_FLeafvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$F_Leaf_Stage, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.308"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.032"), font=2)


# LM for SN: F_Leaf_Stage vs Percent_N_BeG
model_SN_FLeafvC_BegG <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_FLeafvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~F_Leaf_Stage*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$F_Leaf_Stage, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Number of Fully Developed Leaves",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$F_Leaf_Stage), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$F_Leaf_Stage), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.536"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.038"), font=2)






# LM for DO: SLA vs Percent_C_BeG
# LM for AP: SLA vs Percent_C_BeG
# LM for SM: SLA vs Percent_C_BeG
# LM for AG: SLA vs Percent_C_BeG
# LM for SN: SLA vs Percent_C_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Percent_C_BeG
model_DO_SLAvC_BeG <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvC_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$SLA, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.852"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.026"), font=2)


# LM for AP: SLA vs Percent_C_BeG
model_AP_SLAvC_BegG <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$SLA, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.868"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.028"), font=2)




# LM for SM: SLA vs Percent_C_BeG
model_SM_SLAvC_BegG <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$SLA, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.111"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.052"), font=2)




# LM for AG: SLA vs Percent_C_BeG
model_AG_SLAvC_BegG <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$SLA, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$SLA), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.829"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.8e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.001"), font=2)


# LM for SN: F_Leaf_Stage vs Percent_N_BeG
model_SN_SLAvC_BegG <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~SLA*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$SLA, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$SLA), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.640"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.062"), font=2)






# LM for DO: SLA vs Percent_N_BeG
# LM for AP: SLA vs Percent_N_BeG
# LM for SM: SLA vs Percent_N_BeG
# LM for AG: SLA vs Percent_N_BeG
# LM for SN: SLA vs Percent_N_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: SLA vs Percent_N_BeG
model_DO_SLAvN_BeG <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_SLAvN_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$SLA, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.128"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.016 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.260"), font=2)


# LM for AP: SLA vs Percent_N_BeG
model_AP_SLAvN_BegG <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_SLAvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$SLA, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.356"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.3e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.529"), font=2)


# LM for SM: SLA vs Percent_N_BeG
model_SM_SLAvN_BegG <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_SLAvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$SLA, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.073"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.134"), font=2)




# LM for AG: SLA vs Percent_N_BeG
model_AG_SLAvN_BegG <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_SLAvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$SLA, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
abline(lm(model_AG_SLAvN_BegG), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$SLA), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$SLA), lwd=4, col="#769370")
#legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=5.3e-05 *"), font=2)
text(locator(), labels = c("R2=0.492"), font=2)


# LM for SN: SLA vs Percent_N_BeG
model_SN_SLAvN_BegG <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_SLAvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~SLA*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$SLA, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="SLA",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$SLA), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$SLA), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.378"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.519"), font=2)



colnames(DO_Traits)



# LM for DO: Leaf_Thickness_cm vs Percent_C_BeG
# LM for AP: Leaf_Thickness_cm vs Percent_C_BeG
# LM for SM: Leaf_Thickness_cm vs Percent_C_BeG
# LM for AG: Leaf_Thickness_cm vs Percent_C_BeG
# LM for SN: Leaf_Thickness_cm vs Percent_C_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Thickness_cm vs Percent_C_BeG
model_DO_LeafThvC_BeG <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvC_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.728"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.057"), font=2)


# LM for AP: Leaf_Thickness_cm vs Percent_C_BeG
model_AP_LeafThvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.846"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.047"), font=2)




# LM for SM: Leaf_Thickness_cm vs Percent_C_BeG
model_SM_LeafThvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.848"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.8e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.111"), font=2)




# LM for AG: SLA vs Percent_C_BeG
model_AG_LeafThvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.023 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.07e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.235"), font=2)


# LM for SN: F_Leaf_Stage vs Percent_N_BeG
model_SN_LeafThvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.590"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.074"), font=2)






# LM for DO: Leaf_Thickness_cm vs Percent_N_BeG
# LM for AP: Leaf_Thickness_cm vs Percent_N_BeG
# LM for SM: Leaf_Thickness_cm vs Percent_N_BeG
# LM for AG: Leaf_Thickness_cm vs Percent_N_BeG
# LM for SN: Leaf_Thickness_cm vs Percent_N_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Thickness_cm vs Percent_N_BeG
model_DO_LeafThvN_BeG <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafThvN_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Leaf_Thickness_cm, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.643"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.2e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.272"), font=2)


# LM for AP: Leaf_Thickness_cm vs Percent_N_BeG
model_AP_LeafThvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafThvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Leaf_Thickness_cm, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.153"), font=2, col="#BDB2A7")
text(locator(), labels = c("p= 0.0003 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.653"), font=2)




# LM for SM: Leaf_Thickness_cm vs Percent_N_BeG
model_SM_LeafThvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafThvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Leaf_Thickness_cm, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.420"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.030 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.112"), font=2)




# LM for AG: SLA vs Percent_N_BeG
model_AG_LeafThvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafThvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Leaf_Thickness_cm, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.009 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.6e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.438"), font=2)


# LM for SN: Leaf thickness vs Percent_N_BeG
model_SN_LeafThvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafThvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Thickness_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Leaf_Thickness_cm, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Thickness (cm)",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Leaf_Thickness_cm), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Leaf_Thickness_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.022 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.1e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.510"), font=2)












colnames(DO_Traits)

# LM for DO: Plant_Area_cm^2 vs Percent_C_BeG
# LM for AP: Plant_Area_cm^2 vs Percent_C_BeG
# LM for SM: Plant_Area_cm^2 vs Percent_C_BeG
# LM for AG: Plant_Area_cm^2 vs Percent_C_BeG
# LM for SN: Plant_Area_cm^2 vs Percent_C_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Plant_Area_cm^2 vs Percent_C_BeG
model_DO_AreavC_BeG <- lm(formula = Percent_C_BeG~Plant_Area*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AreavC_BeG)
# overall trend

View(DO_Traits)


# LM for DO: Plant_Area_cm^2 vs Percent_N_BeG
# LM for AP: Plant_Area_cm^2 vs Percent_N_BeG
# LM for SM: Plant_Area_cm^2 vs Percent_N_BeG
# LM for AG: Plant_Area_cm^2 vs Percent_N_BeG
# LM for SN: Plant_Area_cm^2 vs Percent_N_BeG








#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Plant_Height_cm vs Percent_N_BeG
# LM for AP: Plant_Height_cm vs Percent_N_BeG
# LM for SM: Plant_Height_cm vs Percent_N_BeG
# LM for AG: Plant_Height_cm vs Percent_N_BeG
# LM for SN: Plant_Height_cm vs Percent_N_BeG

# LM for DO: Plant_Height_cm vs Percent_N_BeG
model_DO_HeightvN_BeG <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvN_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Plant_Height_cm, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.949"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.301"), font=2)


# LM for AP: Plant_Height_cm vs Percent_N_BeG
model_AP_HeightvN_BegG <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Plant_Height_cm, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.007 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.7e-5 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.564"), font=2)




# LM for SM: Plant_Height_cm vs Percent_N_BeG
model_SM_HeightvN_BegG <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Plant_Height_cm, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.062"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.4e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.147"), font=2)




# LM for AG: Plant_Height_cm vs Percent_N_BeG
model_AG_HeightvN_BegG <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Plant_Height_cm, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.002"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.6e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.455"), font=2)


# LM for SN: Plant_Height_cm vs Percent_N_BeG
model_SN_HeightvN_BegG <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Plant_Height_cm, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.548"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.011 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.500"), font=2)



#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Plant_Height_cm vs Percent_N_BeG
# LM for AP: Plant_Height_cm vs Percent_N_BeG
# LM for SM: Plant_Height_cm vs Percent_N_BeG
# LM for AG: Plant_Height_cm vs Percent_N_BeG
# LM for SN: Plant_Height_cm vs Percent_N_BeG

model_DO_HeightvC_BeG <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_HeightvC_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Plant_Height_cm, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.648"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.024"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_HeightvC_BegG <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_HeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Plant_Height_cm, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.831"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.025"), font=2)




# LM for SM: Plant_Height_cm vs Percent_C_BeG
model_SM_HeightvC_BegG <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_HeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Plant_Height_cm, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.683"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.101"), font=2)




# LM for AG: Plant_Height_cm vs Percent_C_BeG
model_AG_HeightvC_BegG <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_HeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Plant_Height_cm, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.181"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.2e-11 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.029"), font=2)


# LM for SN: Plant_Height_cm vs Percent_C_BeG
model_SN_HeightvC_BegG <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_HeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Plant_Height_cm*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Plant_Height_cm, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Plant Height (cm)",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$Plant_Height_cm), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$Plant_Height_cm), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.573"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.80"), font=2)








# LM for DO: Leaf_Number vs Percent_N_BeG
# LM for AP: Leaf_Number vs Percent_N_BeG
# LM for SM: Leaf_Number vs Percent_N_BeG
# LM for AG: Leaf_Number vs Percent_N_BeG
# LM for SN: Leaf_Number vs Percent_N_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Leaf_Number vs Percent_N_BeG
model_DO_LeafNumvN_BeG <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvN_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Leaf_Number, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.049 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.4e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.330"), font=2)


# LM for AP: Plant_Height_cm vs Percent_N_BeG
model_AP_LeafNumvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Leaf_Number, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=1.3e-05 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.1e-12 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.635"), font=2)


# LM for SM: Leaf_Number vs Percent_N_BeG
model_SM_LeafNumvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Leaf_Number, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.316"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.4e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.110"), font=2)


# LM for AG: Leaf_Number vs Percent_N_BeG
model_AG_LeafNumvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Leaf_Number, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf_Number",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.073"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.9e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.405"), font=2)


# LM for SN: Leaf_Number vs Percent_N_BeG
model_SN_LeafNumvN_BegG <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Leaf_Number, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.295"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.8e-06 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.556"), font=2)






#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Leaf_Number vs Percent_C_BeG
# LM for AP: Leaf_Number vs Percent_C_BeG
# LM for SM: Leaf_Number vs Percent_C_BeG
# LM for AG: Leaf_Number vs Percent_C_BeG
# LM for SN: Leaf_Number vs Percent_C_BeG

model_DO_LeafNumvC_BeG <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_LeafNumvC_BeG)
# overall trend
#

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)


plot(DO_Traits$Leaf_Number, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.517"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.056"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_LeafNumvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_LeafNumvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent__BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Leaf_Number, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.970"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.019"), font=2)


# LM for SM: Leaf_Number vs Percent_C_BeG
model_SM_LeafNumvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_LeafNumvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Leaf_Number, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.747"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.092"), font=2)


# LM for AG: Leaf_Number vs Percent_C_BeG
model_AG_LeafNumvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_LeafNumvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Leaf_Number, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf_Number",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.648"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.024"), font=2)


# LM for SN: Leaf_Number vs Percent_C_BeG
model_SN_LeafNumvC_BegG <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_LeafNumvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Leaf_Number*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Leaf_Number, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Number",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$Leaf_Number), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$Leaf_Number), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.754"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.027"), font=2)






# LM for DO: Aboveground_Dry_Weight_g vs Percent_C_BeG
# LM for AP: Aboveground_Dry_Weight_g vs Percent_C_BeG
# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG

#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_DO_AbGWeightvC_BeG <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGWeightvC_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.679"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.064"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_AbGWeightvC_BegG <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGWeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent__BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.388"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.064"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_AbGWeightvC_BegG <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGWeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.747"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.092"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_AG_AbGWeightvC_BegG <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGWeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.648"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.024"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SN_AbGWeightvC_BegG <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGWeightvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.754"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.027"), font=2)








#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Aboveground_Dry_Weight_g vs Percent_N_BeG
# LM for AP: Aboveground_Dry_Weight_g vs Percent_N_BeG
# LM for SM: Aboveground_Dry_Weight_g vs Percent_N_BeG
# LM for AG: Aboveground_Dry_Weight_g vs Percent_N_BeG
# LM for SN: Aboveground_Dry_Weight_g vs Percent_N_BeG


# LM for DO: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_DO_AbGWeightvN_BeG <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_AbGWeightvN_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Aboveground_Dry_Weight_g, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.014 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=5.2e-11 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.367"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_AbGWeightvN_BegG <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_AbGWeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=7.7e-13 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.585"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_N_BeG
model_SM_AbGWeightvN_BegG <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_AbGWeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Aboveground_Dry_Weight_g, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.034 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=4.0e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.137"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_N_BeG
model_AG_AbGWeightvN_BegG <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_AbGWeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Aboveground_Dry_Weight_g, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.001 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.9e-106 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.395"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_N_BeG
model_SN_AbGWeightvN_BegG <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_AbGWeightvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Aboveground_Dry_Weight_g*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Aboveground_Dry_Weight_g, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Aboveground Biomass (g)",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Aboveground_Dry_Weight_g), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.012 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=2.6e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.507"), font=2)




#put into a 5-panel
par(mfrow = c(2,3))




# LM for DO: Percent_N_AbG vs Percent_C_BeG
# LM for AP: Percent_N_AbG vs Percent_C_BeG
# LM for SM: Percent_N_AbG vs Percent_C_BeG
# LM for AG: Percent_N_AbG vs Percent_C_BeG
# LM for SN: Percent_N_AbG vs Percent_C_BeG

model_DO_NAbGvC_BeG <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvC_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.063"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.023"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_NAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Aboveground_Dry_Weight_g, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.640"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.170"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_NAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.147"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.170"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_AG_NAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.398"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=4.3e-13 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.145"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SN_NAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_C_BeG~SN_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_C_BeG~SN_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p= 0.450"), font=2, col="#BDB2A7")
text(locator(), labels = c("p<2e-16 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.071"), font=2)



#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Percent_N_AbG vs Percent_N_BeG
# LM for AP: Percent_N_AbG vs Percent_N_BeG
# LM for SM: Percent_N_AbG vs Percent_N_BeG
# LM for AG: Percent_N_AbG vs Percent_N_BeG
# LM for SN: Percent_N_AbG vs Percent_N_BeG

model_DO_NAbGvN_BeG <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvN_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Percent_N_BeG~DO_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Percent_N_BeG~DO_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.025"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.4e-05 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.300"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_NAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.104"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.640"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_NAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
#abline(lm(model_SM_FLeafvC_BegG), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Percent_N_BeG~SM_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Percent_N_BeG~SM_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.813"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.013 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.149"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_AG_NAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
#abline(lm(model_AG_FLeafvN_BegG), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Percent_N_BeG~AG_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Percent_N_BeG~AG_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.329"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.012 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.375"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SN_NAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
abline(lm(model_SN_NAbGvN_BegG), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=3.3e-11 *"), font=2)
text(locator(), labels = c("R2=0.738"), font=2)






#put into a 5-panel
par(mfrow = c(2,3))




# LM for DO: Percent_C_AbG vs Percent_C_BeG
# LM for AP: Percent_C_AbG vs Percent_C_BeG
# LM for SM: Percent_C_AbG vs Percent_C_BeG
# LM for AG: Percent_C_AbG vs Percent_C_BeG
# LM for SN: Percent_C_AbG vs Percent_C_BeG


model_DO_CAbGvC_BeG <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvC_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent C",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvC_BeG), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.769"), font=2)
text(locator(), labels = c("R2=-0.067"), font=2)


# LM for AP: Plant_Height_cm vs Percent_C_BeG
model_AP_CAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent C",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_CAbGvC_BegG), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Percent_C_BeG~AP_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Percent_C_BeG~AP_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.293"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=3.2e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.039"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_CAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent C",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvC_BegG), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.744"), font=2)
text(locator(), labels = c("R2=-0.073"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_AG_CAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent C",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvC_BegG), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.528"), font=2)
text(locator(), labels = c("R2=-0.018"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SN_CAbGvC_BegG <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvC_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_C_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Percent_C_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent C",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvC_BegG), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("bottomright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.229"), font=2)
text(locator(), labels = c("R2=0.057"), font=2)




#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Percent_C_AbG vs Percent_N_BeG
# LM for AP: Percent_C_AbG vs Percent_N_BeG
# LM for SM: Percent_C_AbG vs Percent_N_BeG
# LM for AG: Percent_C_AbG vs Percent_N_BeG
# LM for SN: Percent_C_AbG vs Percent_N_BeG


model_DO_CAbGvN_BeG <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvN_BeG)

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent N",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvN_BeG), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Percent_C_BeG~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=9.4e-06 *"), font=2)
text(locator(), labels = c("R2=0.491"), font=2)


# LM for AP: 
model_AP_CAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent N",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvN_BegG), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0002 *"), font=2)
text(locator(), labels = c("R2=0.406"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_CAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent N",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvN_BegG), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.074"), font=2)
text(locator(), labels = c("R2=0.159"), font=2)


# LM for AG: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_AG_CAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent N",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvN_BegG), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=2.7e-05 *"), font=2)
text(locator(), labels = c("R2=0.461"), font=2)


# LM for SN: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SN_CAbGvN_BegG <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvN_BegG)
# overall trend

# 10N and Control
reg1 <- lm(formula = Percent_N_BeG~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Percent_N_BeG,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Root Percent N",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_CAbGvN_BegG), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Percent_N_BeG~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Percent_N_BeG~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.596"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=9.7e-09 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.520"), font=2)






# LM for DO: Percent_C_AbG vs SRL
# LM for AP: Percent_C_AbG vs SRL
# LM for SM: Percent_C_AbG vs SRL
# LM for AG: Percent_C_AbG vs SRL
# LM for SN: Percent_C_AbG vs SRL

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_CAbGvSRL <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvSRL)

# 10N and Control
reg1 <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="SRL",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvSRL), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.279"), font=2)
text(locator(), labels = c("R2=0.042"), font=2)


# LM for AP: 
model_AP_CAbGvSRL <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="SRL",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvSRL), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.491"), font=2)
text(locator(), labels = c("R2=-0.010"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_CAbGvSRL <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="SRL",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvSRL), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.679"), font=2)
text(locator(), labels = c("R2=-0.057"), font=2)


# LM for AG:
model_AG_CAbGvSRL <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="SRL",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvSRL), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.007 *"), font=2)
text(locator(), labels = c("R2=0.261"), font=2)


# LM for SN: 
model_SN_CAbGvSRL <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="SRL",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvSRL), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.220"), font=2)
text(locator(), labels = c("R2=0.060"), font=2)



#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Percent_N_AbG vs SRL
# LM for AP: Percent_N_AbG vs SRL
# LM for SM: Percent_N_AbG vs SRL
# LM for AG: Percent_N_AbG vs SRL
# LM for SN: Percent_N_AbG vs SRL

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_NAbGvSRL <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvSRL)

# 10N and Control
reg1 <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="SRL",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_NAbGvSRL), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.243"), font=2)
text(locator(), labels = c("R2=0.053"), font=2)


# LM for AP: 
model_AP_NAbGvSRL <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="SRL",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_NAbGvSRL), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.483"), font=2)
text(locator(), labels = c("R2=-0.008"), font=2)


# LM for SM: Aboveground_Dry_Weight_g vs Percent_C_BeG
model_SM_NAbGvSRL <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="SRL",
     main= "Solidago missouriensis")
abline(lm(model_SM_NAbGvSRL), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.841"), font=2)
text(locator(), labels = c("R2=-0.100"), font=2)


# LM for AG:
model_AG_NAbGvSRL <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="SRL",
     main= "Andropogon gerardii")
#abline(lm(model_AG_NAbGvSRL), lwd=4, lty=2)
abline(lm(AG_N0_Traits$SRL~AG_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$SRL~AG_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.529"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.033 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.023"), font=2)


# LM for SN: 
model_SN_NAbGvSRL <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvSRL)
# overall trend

# 10N and Control
reg1 <- lm(formula = SRL~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$SRL,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="SRL",
     main= "Sorghastrum nutans")
abline(lm(model_SN_NAbGvSRL), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.934"), font=2)
text(locator(), labels = c("R2=-0.110"), font=2)





# LM for DO: Percent_C_AbG vs Total_Belowground_Weight_g
# LM for AP: Percent_C_AbG vs Total_Belowground_Weight_g
# LM for SM: Percent_C_AbG vs Total_Belowground_Weight_g
# LM for AG: Percent_C_AbG vs Total_Belowground_Weight_g
# LM for SN: Percent_C_AbG vs Total_Belowground_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))


model_DO_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvTotalRoot)

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.412"), font=2)
text(locator(), labels = c("R2=0.0076"), font=2)


# LM for AP: 
model_AP_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Percent_N_BeG~AP_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Percent_N_BeG~AP_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.472"), font=2)
text(locator(), labels = c("R2=-0.006"), font=2)


# LM for SM: 
model_SM_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.280"), font=2)
text(locator(), labels = c("R2=0.049"), font=2)


# LM for AG:
model_AG_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.533"), font=2)
text(locator(), labels = c("R2=-0.019"), font=2)


# LM for SN: 
model_SN_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.493"), font=2)
text(locator(), labels = c("R2=-0.010"), font=2)




# LM for DO: Percent_N_AbG vs Total_Belowground_Weight_g
# LM for AP: Percent_N_AbG vs Total_Belowground_Weight_g
# LM for SM: Percent_N_AbG vs Total_Belowground_Weight_g
# LM for AG: Percent_N_AbG vs Total_Belowground_Weight_g
# LM for SN: Percent_N_AbG vs Total_Belowground_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_NAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvTotalRoot)

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Total Belowground Biomass (g)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_NAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$SRL~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Percent_C_BeG~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.149"), font=2)
text(locator(), labels = c("R2=0.088"), font=2)


# LM for AP: 
model_AP_NAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Total Belowground Biomass (g)",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_NAbGvTotalRoot), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Total_Belowground_Weight_g~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Total_Belowground_Weight_g~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.934"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.016 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.080"), font=2)


# LM for SM: 
model_SM_NAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Total Belowground Biomass (g)",
     main= "Solidago missouriensis")
abline(lm(model_SM_NAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.200"), font=2)
text(locator(), labels = c("R2=0.079"), font=2)


# LM for AG:
model_AG_NAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
abline(lm(model_AG_NAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.893"), font=2)
text(locator(), labels = c("R2=-0.099"), font=2)


# LM for SN: 
model_SN_NAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_NAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.035 *"), font=2)
text(locator(), labels = c("R2=0.177"), font=2)






# LM for DO: Percent_C_AbG vs Focal_Root_Weight_g
# LM for AP: Percent_C_AbG vs Focal_Root_Weight_g
# LM for SM: Percent_C_AbG vs Focal_Root_Weight_g
# LM for AG: Percent_C_AbG vs Focal_Root_Weight_g
# LM for SN: Percent_C_AbG vs Focal_Root_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_CAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvFocalRoot)

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Focal Root Biomass (g)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.472"), font=2)
text(locator(), labels = c("R2=-0.006"), font=2)


# LM for AP: 
model_AP_CAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Focal Root Biomass (g)",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0007 *"), font=2)
text(locator(), labels = c("R2=0.365"), font=2)


# LM for SM: 
model_SM_CAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Focal Root Biomass (g)",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.063"), font=2)
text(locator(), labels = c("R2=0.166"), font=2)


# LM for AG:
model_AG_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.533"), font=2)
text(locator(), labels = c("R2=-0.019"), font=2)


# LM for SN: 
model_SN_CAbGvTotalRoot <- lm(formula = Total_Belowground_Weight_g~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvTotalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Total_Belowground_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Total_Belowground_Weight_g,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Total Belowground Biomass (g)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvTotalRoot), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.493"), font=2)
text(locator(), labels = c("R2=-0.010"), font=2)







#put into a 5-panel
par(mfrow = c(2,3))


# LM for DO: Percent_N_AbG vs Focal_Root_Weight_g
# LM for AP: Percent_N_AbG vs Focal_Root_Weight_g
# LM for SM: Percent_N_AbG vs Focal_Root_Weight_g
# LM for AG: Percent_N_AbG vs Focal_Root_Weight_g
# LM for SN: Percent_N_AbG vs Focal_Root_Weight_g

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_NAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvFocalRoot)

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Focal Root Biomass (g)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_NAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.148"), font=2)
text(locator(), labels = c("R2=0.088"), font=2)


# LM for AP: 
model_AP_NAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Focal Root Biomass (g)",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_NAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.286"), font=2)
text(locator(), labels = c("R2=0.042"), font=2)


# LM for SM: 
model_SM_NAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Focal Root Biomass (g)",
     main= "Solidago missouriensis")
abline(lm(model_SM_NAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.228"), font=2)
text(locator(), labels = c("R2=0.067"), font=2)


# LM for AG:
model_AG_NAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Focal Root Biomass (g)",
     main= "Andropogon gerardii")
abline(lm(model_AG_NAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.590"), font=2)
text(locator(), labels = c("R2=-0.031"), font=2)


# LM for SN: 
model_SN_NAbGvFocalRoot <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Focal_Root_Weight_g~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Focal_Root_Weight_g,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Focal Root Biomass (g)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_NAbGvFocalRoot), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.660"), font=2)
text(locator(), labels = c("R2=-0.044"), font=2)






# LM for DO: Percent_C_AbG vs Root_Diam_Max
# LM for AP: Percent_C_AbG vs Root_Diam_Max
# LM for SM: Percent_C_AbG vs Root_Diam_Max
# LM for AG: Percent_C_AbG vs Root_Diam_Max
# LM for SN: Percent_C_AbG vs Root_Diam_Max

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_CAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvRoot_Diam_Max)

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Maximum Root Diameter (cm)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvRoot_Diam_Max), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.009 *"), font=2)
text(locator(), labels = c("R2=0.251"), font=2)


# LM for AP: 
model_AP_CAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Maximum Root Diameter (cm)",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvRoot_Diam_Max), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.277"), font=2)
text(locator(), labels = c("R2=0.045"), font=2)


# LM for SM: 
model_SM_CAbGvFocalRoot <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvFocalRoot)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Maximum Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(model_SM_CAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Max~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Max~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.330"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.020 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.116"), font=2)


# LM for AG:
model_AG_CAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Maximum Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvRoot_Diam_Max), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.740"), font=2)
text(locator(), labels = c("R2=-0.064"), font=2)


# LM for SN: 
model_SN_CAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Maximum Root Diameter (cm)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvRoot_Diam_Max), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.898"), font=2)
text(locator(), labels = c("R2=-0.010"), font=2)




# LM for DO: Percent_N_AbG vs Root_Diam_Max
# LM for AP: Percent_N_AbG vs Root_Diam_Max
# LM for SM: Percent_N_AbG vs Root_Diam_Max
# LM for AG: Percent_N_AbG vs Root_Diam_Max
# LM for SN: Percent_N_AbG vs Root_Diam_Max

#put into a 5-panel
par(mfrow = c(2,3))

model_DO_NAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvRoot_Diam_Max)

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Maximum Root Diameter (cm)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_NAbGvRoot_Diam_Max), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=7.8e-06 *"), font=2)
text(locator(), labels = c("R2=-0.504"), font=2)


# LM for AP: 
model_AP_NAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Maximum Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Max~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Max~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.021 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.998"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.100"), font=2)


# LM for SM: 
model_SM_NAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Maximum Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(model_SM_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Max~SM_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Max~SM_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.720"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.166"), font=2)


# LM for AG:
model_AG_NAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Maximum Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(model_AG_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Max~AG_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Max~AG_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.110"), font=2)
text(locator(), labels = c("p=0.006 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.023"), font=2)


# LM for SN: 
model_SN_NAbGvRoot_Diam_Max <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvRoot_Diam_Max)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Max~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Root_Diam_Max,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Maximum Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Max~SN_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Max~SN_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.018 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0007 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.125"), font=2)






#put into a 5-panel
par(mfrow = c(2,3))

# LM for DO: Percent_C_AbG vs Root_Diam_Mean
# LM for AP: Percent_C_AbG vs Root_Diam_Mean
# LM for SM: Percent_C_AbG vs Root_Diam_Mean
# LM for AG: Percent_C_AbG vs Root_Diam_Mean
# LM for SN: Percent_C_AbG vs Root_Diam_Mean

#put into a 5-panel
par(mfrow = c(2,3))


model_DO_CAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_CAbGvRoot_Diam_Mean)

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_C_AbG, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosanthes")
abline(lm(model_DO_CAbGvRoot_Diam_Mean), lwd=4, lty=2)
#abline(lm(DO_N0_Traits$Focal_Root_Weight_g~DO_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(DO_N10_Traits$Focal_Root_Weight_g~DO_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.424"), font=2)
text(locator(), labels = c("R2=0.006"), font=2)


# LM for AP: 
model_AP_CAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_CAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_C_AbG, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
abline(lm(model_AP_CAbGvRoot_Diam_Mean), lwd=4, lty=2)
#abline(lm(AP_N0_Traits$Focal_Root_Weight_g~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AP_N10_Traits$Focal_Root_Weight_g~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.795"), font=2)
text(locator(), labels = c("R2=-0.076"), font=2)


# LM for SM: 
model_SM_CAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_CAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_C_AbG, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
abline(lm(model_SM_CAbGvRoot_Diam_Mean), lwd=4, lty=2)
#abline(lm(SM_N0_Traits$Percent_C_BeG~SM_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SM_N10_Traits$Percent_C_BeG~SM_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.797"), font=2)
text(locator(), labels = c("R2=-0.087"), font=2)


# LM for AG:
model_AG_CAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_CAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_C_AbG, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
abline(lm(model_AG_CAbGvRoot_Diam_Mean), lwd=4, lty=2)
#abline(lm(AG_N0_Traits$Percent_C_BeG~AG_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(AG_N10_Traits$Percent_C_BeG~AG_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.224"), font=2)
text(locator(), labels = c("R2=-0.062"), font=2)


# LM for SN: 
model_SN_CAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_C_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_CAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_C_AbG, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent C",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
abline(lm(model_SN_CAbGvRoot_Diam_Mean), lwd=4, lty=2)
#abline(lm(SN_N0_Traits$SRL~SN_N0_Traits$Percent_C_AbG), lwd=4, col="#BDB2A7")
#abline(lm(SN_N10_Traits$SRL~SN_N10_Traits$Percent_C_AbG), lwd=4, col="#769370")
#legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.942"), font=2)
text(locator(), labels = c("R2=-0.113"), font=2)




# LM for DO: Percent_N_AbG vs Root_Diam_Mean
# LM for AP: Percent_N_AbG vs Root_Diam_Mean
# LM for SM: Percent_N_AbG vs Root_Diam_Mean
# LM for AG: Percent_N_AbG vs Root_Diam_Mean
# LM for SN: Percent_N_AbG vs Root_Diam_Mean


#put into a 5-panel
par(mfrow = c(2,3))


model_DO_NAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = DO_Traits)
summary(model_DO_NAbGvRoot_Diam_Mean)

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(DO_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(DO_Traits$Percent_N_AbG, DO_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Mean Root Diameter (cm)",
     main= "Dicanthelium oligosanthes")
#abline(lm(model_DO_NAbGvRoot_Diam_Mean), lwd=4, lty=2)
abline(lm(DO_N0_Traits$Root_Diam_Mean~DO_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(DO_N10_Traits$Root_Diam_Mean~DO_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.239"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.018 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.116"), font=2)


# LM for AP: 
model_AP_NAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = AP_Traits)
summary(model_AP_NAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AP_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AP_Traits$Percent_N_AbG, AP_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Mean Root Diameter (cm)",
     main= "Ambrosia psylostachia")
#abline(lm(model_AP_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(AP_N0_Traits$Root_Diam_Mean~AP_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AP_N10_Traits$Root_Diam_Mean~AP_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.547"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.007 *"), font=2, col="#769370")
text(locator(), labels = c("R2=-0.004"), font=2)


# LM for SM: 
model_SM_NAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = SM_Traits)
summary(model_SM_NAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SM_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SM_Traits$Percent_N_AbG, SM_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Mean Root Diameter (cm)",
     main= "Solidago missouriensis")
#abline(lm(model_SM_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(SM_N0_Traits$Root_Diam_Mean~SM_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SM_N10_Traits$Root_Diam_Mean~SM_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.234"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=0.0002 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.008"), font=2)


# LM for AG:
model_AG_NAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = AG_Traits)
summary(model_AG_NAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(AG_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(AG_Traits$Percent_N_AbG, AG_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Mean Root Diameter (cm)",
     main= "Andropogon gerardii")
#abline(lm(model_AG_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(AG_N0_Traits$Root_Diam_Mean~AG_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(AG_N10_Traits$Root_Diam_Mean~AG_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.260"), font=2)
text(locator(), labels = c("p=0.004 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.049"), font=2)


# LM for SN: 
model_SN_NAbGvRoot_Diam_Mean <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = SN_Traits)
summary(model_SN_NAbGvRoot_Diam_Mean)
# overall trend

# 10N and Control
reg1 <- lm(formula = Root_Diam_Mean~Percent_N_AbG*Treatment+(1/Experiment), data = subset(SN_Traits, Treatment=="Control"|Treatment=="10"))
summary(reg1)

plot(SN_Traits$Percent_N_AbG, SN_Traits$Root_Diam_Mean,
     pch=20, 
     xlab="Leaf Percent N",
     ylab="Mean Root Diameter (cm)",
     main= "Sorghastrum nutans")
#abline(lm(model_SN_NAbGvRoot_Diam_Max), lwd=4, lty=2)
abline(lm(SN_N0_Traits$Root_Diam_Mean~SN_N0_Traits$Percent_N_AbG), lwd=4, col="#BDB2A7")
abline(lm(SN_N10_Traits$Root_Diam_Mean~SN_N10_Traits$Percent_N_AbG), lwd=4, col="#769370")
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill =c("#BDB2A7", "#769370"), bg = "white")
text(locator(), labels = c("p=0.0001 *"), font=2, col="#BDB2A7")
text(locator(), labels = c("p=1.0e-07 *"), font=2, col="#769370")
text(locator(), labels = c("R2=0.300"), font=2)









#####################################################################

# REGRESSIONS for Q3 
# only on AG


# latest data, May trait drop model
All_Traits_O <- read_csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_Datasheet_O2.csv")
View(All_Traits_O)

# subset only AG out 
AG_Traits_0 <- subset(All_Traits_O, Species=="Andropogon_gerardii")
View(AG_Traits_0)

# Subset SN
SN_Traits_0 <- subset(All_Traits_O, Species=="Sorghastrum_nutans")
SN_N20_Traits_0 <- subset(All_Traits_O, Treatment=="20")

#Dichanthelium_oligosanthes
#	Ambrosia_psilostachya
# Solidago_missouriensis


# Subset AP
AP_Traits_0 <- subset(All_Traits_O, Species=="Ambrosia_psilostachya")


#Subset trts
AG_N0_Traits_0 <- subset(AG_Traits_0, Treatment=="Control")
AG_N2_Traits_0 <- subset(AG_Traits_0, Treatment=="2.5")
AG_N10_Traits_0 <- subset(AG_Traits_0, Treatment=="10")
AG_N20_Traits_0 <- subset(AG_Traits_0, Treatment=="20")

AP_N0_Traits_0 <- subset(AP_Traits_0, Treatment=="Control")
AP_N2_Traits_0 <- subset(AP_Traits_0, Treatment=="2.5")
AP_N10_Traits_0 <- subset(AP_Traits_0, Treatment=="10")
AP_N20_Traits_0 <- subset(AP_Traits_0, Treatment=="20")





# colors used 
trtcol = c("#BDB2A7","#6E687E","#769370","#F1C646")

# "Control"= "#BDB2A7" (tan)
# "2.5N"= "#6E687E" (grey)
# "10N"= "#769370" (green)
# "20N"= "#F1C646" (yelow)
       

       
       

# FINAL Q3 Powerpoint Graph 

## AG 

# AG: TRAIT PAIR 1- Aboveground Biomass vs Root Biomass
regall_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AG_Traits_0)
summary(regall_1)

reg0_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AG_N0_Traits_0)
summary(reg0_1)

reg2_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g, data = AG_N2_Traits_0)
summary(reg2_1)

reg10_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AG_N10_Traits_0)
summary(reg10_1)

reg20_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g, data = SN_N20_Traits_0)
summary(reg20_1)

#par(mfrow = c(1,2))
par(mfrow = c(1,3))


#PLOT
plot(AG_Traits_0$Focal_Root_Weight_g, AG_Traits_0$Aboveground_Dry_Weight_g,
     pch=20, 
     xlab="Root Biomass (g)",
     ylab="Aboveground Biomass (g)",
     main= "A. gerardii")
#overall black dashed line
abline(lm(regall_1), lwd=4, lty=2)
#control line
abline(lm(reg0_1), lwd=4, col="#BDB2A7")
#N2.5 line 
abline(lm(reg2_1), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_1), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_1), lwd=4, col="#F1C646")





# AG: TRAIT PAIR 2- SLA vs SRL
regall_2 <- lm(formula = SLA~SRL*Treatment+(1/Experiment), data = AG_Traits_0)
summary(regall_2)

reg0_2 <- lm(formula = SLA~SRL+(1/Experiment), data = AG_N0_Traits_0)
summary(reg0_2)

reg2_2 <- lm(formula = SLA~SRL, data = AG_N2_Traits_0)
summary(reg2_2)

reg10_2 <- lm(formula = SLA~SRL+(1/Experiment), data = AG_N10_Traits_0)
summary(reg10_2)

reg20_2 <- lm(formula = SLA~SRL, data = AG_N20_Traits_0)
summary(reg20_2)

#PLOT
plot(AG_Traits_0$SRL, AG_Traits_0$SLA,
     pch=20, 
     xlab="Specific Root Length (SRL)",
    ylab="Specific Leaf Area (SLA)",
     main= "A. gerardii")
#overall black dashed line
abline(lm(regall_2), lwd=4, lty=2)
#control line
abline(lm(reg0_2), lwd=4, col="#BDB2A7")
#N2.5 line 
abline(lm(reg2_2), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_2), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_2), lwd=4, col="#F1C646")

# Add Legend 
#legend("bottomright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")






# AG: TRAIT PAIR 3- Leaf_N vs Root_N
regall_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AG_Traits_0)
summary(regall_3)

reg0_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AG_N0_Traits_0)
summary(reg0_3)

reg2_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AG_N2_Traits_0)
summary(reg2_3)

reg10_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AG_N10_Traits_0)
summary(reg10_3)

reg20_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AG_N20_Traits_0)
summary(reg20_3)

#PLOT
plot(AG_Traits_0$PercentN_BeG, AG_Traits_0$PercentN_AbG,
     pch=20, 
     xlab="Root N (%)",
     ylab="Leaf N (%)",
     main= "A. gerardii")
#overall black dashed line
abline(lm(regall_3), lwd=4, lty=2)
#control line
abline(lm(reg0_3), lwd=4, col="#BDB2A7")
#N2.5 line 
abline(lm(reg2_3), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_3), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_3), lwd=4, col="#F1C646")

# Add Legend 







########## AP ############


par(mfrow = c(1,4))


# AP: TRAIT PAIR 1- Aboveground Biomass vs Root Biomass
regall_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AP_Traits_0)
summary(regall_1)

reg0_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AP_N0_Traits_0)
summary(reg0_1)

reg2_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g, data = AP_N2_Traits_0)
summary(reg2_1)

reg10_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g+(1/Experiment), data = AP_N10_Traits_0)
summary(reg10_1)

reg20_1 <- lm(formula = Aboveground_Dry_Weight_g~Focal_Root_Weight_g, data = AP_N20_Traits_0)
summary(reg20_1)


#PLOT
plot(AP_Traits_0$Focal_Root_Weight_g, AP_Traits_0$Aboveground_Dry_Weight_g,
     pch=20, 
     xlab="Root Biomass (g)",
     ylab="Aboveground Biomass (g)",
     main = substitute(italic('A. psilostachya')))
#control line
abline(lm(reg0_1), lwd=4, col="#BDB2A7")
#N2.5 line 
abline(lm(reg2_1), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_1), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_1), lwd=4, col="#F1C646")
#overall black dashed line
abline(lm(regall_1), lwd=4, lty=2)





# AG: TRAIT PAIR 2- SLA vs SRL
regall_2 <- lm(formula = SLA~SRL*Treatment+(1/Experiment), data = AP_Traits_0)
summary(regall_2)

reg0_2 <- lm(formula = SLA~SRL+(1/Experiment), data = AP_N0_Traits_0)
summary(reg0_2)

reg2_2 <- lm(formula = SLA~SRL, data = AP_N2_Traits_0)
summary(reg2_2)

reg10_2 <- lm(formula = SLA~SRL+(1/Experiment), data = AP_N10_Traits_0)
summary(reg10_2)

reg20_2 <- lm(formula = SLA~SRL, data = AP_N20_Traits_0)
summary(reg20_2)

#PLOT
plot(AP_Traits_0$SRL, AP_Traits_0$SLA,
     pch=20, 
     xlab="Specific Root Length (SRL)",
     ylab="Specific Leaf Area (SLA)",
     main = substitute(italic('A. psilostachya')))
#control line
abline(lm(reg0_2), lwd=4, col="#F1C646")
#N2.5 line 
abline(lm(reg2_2), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_2), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_2), lwd=4, col="#BDB2A7")
#overall black dashed line
abline(lm(regall_2), lwd=4, lty=2)




# AG: TRAIT PAIR 3- Leaf_N vs Root_N
regall_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AP_Traits_0)
summary(regall_3)

reg0_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AP_N0_Traits_0)
summary(reg0_3)

reg2_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AP_N2_Traits_0)
summary(reg2_3)

reg10_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AP_N10_Traits_0)
summary(reg10_3)

reg20_3 <- lm(formula = PercentN_AbG~PercentN_BeG, data = AP_N20_Traits_0)
summary(reg20_3)

#PLOT
plot(AP_Traits_0$PercentN_BeG, AP_Traits_0$PercentN_AbG,
     pch=20, 
     xlab="Root N (%)",
     ylab="Leaf N (%)",
     main = substitute(italic('A. psilostachya')))
#control line
abline(lm(reg0_3), lwd=4, col="#BDB2A7")
#N2.5 line 
abline(lm(reg2_3), lwd=4, col="#6E687E")
#N10 line
abline(lm(reg10_3), lwd=4, col="#769370")
#N20 line
abline(lm(reg20_3), lwd=4, col="#F1C646") 
#overall black dashed line
abline(lm(regall_3), lwd=4, lty=2)




# Add Legend
plot.new()
legend("center",cex = 1.5, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")





### Slide with figure of ANOVA bar plot 


par(mfrow = c(1,1))



# ANOVA bar plot


#Labels
catlables2 <-  c( "","DO","","","","AP","","","","SM","","","","AG","","","","SN","","")
trtcol <- c("#BDB2A7", "#6E687E" ,"#769370" ,"#F1C646")

abcd <- barplot(height = FRic_Change_matrix,                     
                beside = TRUE,
                #names=catlables2,
                col= trtcol,
                main="ANOVA",
                xlab="Treatment",
                ylab="Trait")
abline(h = c(0), lty = 1, lwd=3)


trtcol23 <- c("#BDB2A7", "#769370")
              
count2 <- c(5, 10)
catlab2 <- c("N0", "N10")

barplot(count2, 
        col= trtcol23,
        main="ANOVA",
        names= catlab2, 
        ylab="Trait", 
        xlab="Treatment")
abline(h = c(0), lty = 1, lwd=1)




# regression
plot(AP_Traits_0$Treatment2, AP_Traits_0$Aboveground_Dry_Weight_g,
     pch=20, 
     ylab="Trait",
     xlab="Treatment",
     main = "Regression (GLM)")
regularGLM <- lm(formula = Aboveground_Dry_Weight_g~Treatment2, data = AP_Traits_0)
abline(lm(regularGLM), lwd=4, lty=2, col="blue")
       




