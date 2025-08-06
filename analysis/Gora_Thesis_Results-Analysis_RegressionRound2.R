#### Thesis Results Analysis- Linear Regressions, Kevin Wilcox Suggestion ####
###  Gora_Thesis-Results-Analysis_RegressionsRound2.R
###  by Sarah Gora
###  Date created: March 7, 2022

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


#install packages
install.packages("stats")
library(stats)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("tidyverse")
library(tidyverse)
library(magrittr)
install.packages("ggplot2")
library(ggplot2)
install.packages("rlang")
library(rlang)


detach("package:stats")  # Unload stats


#load Data
All_Traits <- read.csv("~/Desktop/Thesis_Data/ALL_Traits_Datasheet.csv")
View(All_Traits)

# Subset out data by species 
DO_Traits <- subset(All_Traits, Species=="Dichanthelium_oligosanthes")
AP_Traits <- subset(All_Traits, Species=="Ambrosia_psilostachya")
SM_Traits <- subset(All_Traits, Species=="Solidago_missouriensis")
AG_Traits <- subset(All_Traits, Species=="Andropogon_gerardii")
SN_Traits <- subset(All_Traits, Species=="Sorghastrum_nutans")


# Subset out data by species for N0N10  
DO_N0N10_Traits <- subset(DO_Traits, Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits <- subset(AP_Traits, Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits <- subset(SM_Traits, Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits <- subset(AG_Traits, Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits <- subset(SN_Traits, Treatment=="Control"| Treatment=="10")


# Subset out data by species for Change  
DO_Ch_Traits <- subset(DO_Traits, Experiment=="Change")
AP_Ch_Traits <- subset(AP_Traits, Experiment=="Change")
SM_Ch_Traits <- subset(SM_Traits, Experiment=="Change")
AG_Ch_Traits <- subset(AG_Traits, Experiment=="Change")
SN_Ch_Traits <- subset(SN_Traits, Experiment=="Change")


View(SN_Ch_Traits)

#put into a 5-panel
par(mfrow = c(2,3))




# LM for SLA - DO
reg_DO_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Experiment), data = DO_Traits)
summary(reg_DO_SLAvTrt)

plot(DO_Traits$Treatment2, DO_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     main= "D. oligosanthes")
abline(lm(reg_DO_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.747"), font=2)
text(locator(), labels = c("R2=-0.019"), font=2)



# LM for SLA - AP
reg_AP_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Experiment), data = AP_Traits)
summary(reg_AP_SLAvTrt)

plot(AP_Traits$Treatment2, AP_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     main= "A. psilostachia")
abline(lm(reg_AP_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.086"), font=2)
text(locator(), labels = c("R2=0.043"), font=2)


# LM for SLA - SM
reg_SM_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Experiment), data = SM_Traits)
summary(reg_SM_SLAvTrt)

plot(SM_Traits$Treatment2, SM_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     main= "S. missouriensis")
abline(lm(reg_SM_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.136"), font=2)
text(locator(), labels = c("R2=0.031"), font=2)


# LM for SLA - AG
reg_AG_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Experiment), data = AG_Traits)
summary(reg_AG_SLAvTrt)

plot(AG_Traits$Treatment2, AG_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     main= "A. gerardii")
abline(lm(reg_AG_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.033 *"), font=2)
text(locator(), labels = c("R2=0.087"), font=2)


# LM for SLA - SN
reg_SN_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Experiment), data = SN_Traits)
summary(reg_SN_SLAvTrt)

plot(SN_Traits$Treatment2, SN_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     main= "S. nutans")
abline(lm(reg_SN_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.820"), font=2)
text(locator(), labels = c("R2=-0.021"), font=2)















########## Change Data ###########

### LM for SLA vs Treatments


#put into a 5-panel
par(mfrow = c(2,3))

# LM for SLA - DO  
reg_DO_Ch_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_SLAvTrt)

plot(DO_Ch_Traits$Treatment2, DO_Ch_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     xaxt='n', 
     xlim = c(0.0, 20.5),
     main= "D. oligosanthes")
axis(side=1, at=seq(0.0, 20.5, by=0.5))
abline(lm(reg_DO_Ch_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.252"), font=2)
text(locator(), labels = c("R2=0.016"), font=2)



# LM for SLA - AP
reg_AP_Ch_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_SLAvTrt)

plot(AP_Ch_Traits$Treatment2, AP_Ch_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     xaxt='n', 
     xlim = c(0.0, 20.5),
     main= "A. psilostachia")
axis(side=1, at=seq(0.0, 20.5, by=0.5))
abline(lm(reg_AP_Ch_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.219"), font=2)
text(locator(), labels = c("R2=0.025"), font=2)


# LM for SLA - SM
reg_SM_Ch_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_SLAvTrt)

plot(SM_Ch_Traits$Treatment2, SM_Ch_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     xaxt='n', 
     xlim = c(0.0, 20.5),
     main= "S. missouriensis")
axis(side=1, at=seq(0.0, 20.5, by=0.5))
abline(lm(reg_SM_Ch_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.145"), font=2)
text(locator(), labels = c("R2=0.056"), font=2)


# LM for SLA - AG
reg_AG_Ch_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Block), data = AG_Ch_Traits)
summary(reg_AG_Ch_SLAvTrt)

plot(AG_Ch_Traits$Treatment2, AG_Ch_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     xaxt='n', 
     xlim = c(0.0, 20.5),
     main= "A. gerardii")
axis(side=1, at=seq(0.0, 20.5, by=0.5))
abline(lm(reg_AG_Ch_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.154"), font=2)
text(locator(), labels = c("R2=0.049"), font=2)


# LM for SLA - SN
reg_SN_Ch_SLAvTrt <- lm(formula = SLA~Treatment2+(1/Block), data = SN_Ch_Traits)
summary(reg_SN_Ch_SLAvTrt)

plot(SN_Ch_Traits$Treatment2, SN_Ch_Traits$SLA,
     pch=20, 
     xlab="Nitrogen Treatment",
     ylab="SLA",
     xaxt='n', 
     xlim = c(0.0, 20.5),
     main= "S. nutans")
axis(side=1, at=seq(0.0, 20.5, by=0.5))
abline(lm(reg_SN_Ch_SLAvTrt), lwd=4, lty=2)
text(locator(), labels = c("p=0.733"), font=2)
text(locator(), labels = c("R2=-0.040"), font=2)


















### ignore everything above 


## Linear Models 

## linear vs second order 
## lowest AIC score is best 

# plot
plot(x = plant trait, y = treatment)

# lm(formula= y~x)
# lm(formula= treatment~trait)


# how to run the line for second order poly 

# EXAMPLE: Draw polynomial regression curve
lines(sort(AG_Ch_Traits$Treatment2),                 
      fitted(reg_AG_Ch_SLAvTrt2)[order(AG_Ch_Traits$Treatment2)],
      col = "red",
      lwd=3.5,
      type = "l")



######### SLA for each Species ##########

## change data only 
## x- treatment
## y- trait 




# predict all X (Treatment) data:
fakeX_DOTrt2 <- seq(min(DO_Ch_Traits$Treatment2), max(DO_Ch_Traits$Treatment2), by = 0.1 )
fakeX_APTrt2 <- seq(min(AP_Ch_Traits$Treatment2), max(AP_Ch_Traits$Treatment2), by = 0.1 )
fakeX_SMTrt2 <- seq(min(SM_Ch_Traits$Treatment2), max(SM_Ch_Traits$Treatment2), by = 0.1 )
fakeX_AGTrt2 <- seq(min(AG_Ch_Traits$Treatment2), max(AG_Ch_Traits$Treatment2), by = 0.1 )
fakeX_SNTrt2 <- seq(min(SN_Ch_Traits$Treatment2), max(SN_Ch_Traits$Treatment2), by = 0.1 )


#put into a 5-panel
par(mfrow = c(2,3))


# DO 
# run linear model 
reg_DO_Ch_SLAvTrt <- lm(SLA~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_SLAvTrt)
AIC(reg_DO_Ch_SLAvTrt)
# AIC: 184.0559

reg_DO_Ch_SLAvTrt <- aov(SLA~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_SLAvTrt, "means")

# run second order Linear model 
reg_DO_Ch_SLAvTrt2 <- lm(SLA ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_SLAvTrt2)
summary(reg_DO_Ch_SLAvTrt2)
# AIC: 182.5521 ** LOWEST

# predict based on best AIC: 
fake_X_Treatment2 <- seq(min(DO_Ch_Traits$Treatment2), max(DO_Ch_Traits$Treatment2), by = 0.1 )
fake_Y_SLA_poly2 <- predict(reg_DO_Ch_SLAvTrt2, list(Treatment2= fake_X_Treatment2,
                                                     type="response", se.fit=F))

plot(DO_Ch_Traits$SLA~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="SLA",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fake_Y_SLA_poly2~fake_X_Treatment2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.114"), font=2)
text(locator(), labels = c("R2=0.110"), font=2)


# AP 
# run linear model 
reg_AP_Ch_SLAvTrt <- lm(SLA~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_SLAvTrt)
AIC(reg_AP_Ch_SLAvTrt)
# AIC: 204.2214 *** LOWEST

# reg_AP_Ch_SLAvTrt <- aov(SLA~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_SLAvTrt, "means")

# run second order Linear model 
reg_AP_Ch_SLAvTrt2 <- glm(SLA ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_SLAvTrt2)
# AIC: 205.6677

plot(AP_Ch_Traits$SLA~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SLA", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_SLAvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.219"), font=2)
text(locator(), labels = c("R2=0.025"), font=2)


# SM
# run linear model 
reg_SM_Ch_SLAvTrt <- lm(SLA~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_SLAvTrt)
AIC(reg_SM_SLAvTrt)
# AIC: 284.0765 

reg_SM_Ch_SLAvTrt <- aov(SLA~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_SLAvTrt, "means")

# run second order Linear model 
reg_SM_Ch_SLAvTrt2 <- glm(SLA ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_SLAvTrt2)
summary(reg_SM_Ch_SLAvTrt2)
# AIC: 139.4393 *** LOWEST

# predict based on best AIC: 
fakeX_SMTrt2 <- seq(min(SM_Ch_Traits$Treatment2), max(SM_Ch_Traits$Treatment2), by = 0.1 )
fakeY_SMSLA_poly2 <- predict(reg_SM_Ch_SLAvTrt2, list(Treatment2= fakeX_SMTrt2,
                                                     type="response", se.fit=F))

plot(SM_Ch_Traits$SLA~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SLA", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
lines(fakeY_SMSLA_poly2~fakeX_SMTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.291"), font=2)
text(locator(), labels = c("R2=0.028"), font=2)


# AG
# run linear model 
reg_AG_Ch_SLAvTrt <- lm(SLA~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_SLAvTrt)
summary(reg_AG_Ch_SLAvTrt)
# AIC: 158.9413

reg_AG_Ch_SLAvTrt <- aov(SLA~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_SLAvTrt, "means")

# run second order Linear model 
reg_AG_Ch_SLAvTrt2 <- glm(SLA~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_SLAvTrt2)
summary(reg_AG_Ch_SLAvTrt2)
# AIC: 157.3708 *** LOWEST

# predict based on best AIC: 
fakeX_AGTrt2 <- seq(min(AG_Ch_Traits$Treatment2), max(AG_Ch_Traits$Treatment2), by = 0.1 )
fakeY_AGSLA_poly2 <- predict(reg_AG_Ch_SLAvTrt2, list(Treatment2= fakeX_AGTrt2,
                                                      type="response", se.fit=F))

plot(AG_Ch_Traits$SLA~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SLA", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
lines(fakeY_AGSLA_poly2~fakeX_AGTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.078"), font=2)
text(locator(), labels = c("R2=0.141"), font=2)



# SN
# run linear model 
reg_SN_Ch_SLAvTrt <- lm(SLA~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_SLAvTrt)
summary(reg_SN_Ch_SLAvTrt)
# AIC: 170.811 *** LOWEST

reg_SN_Ch_SLAvTrt <- aov(SLA~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_SLAvTrt, "means")

# run second order Linear model 
reg_SN_Ch_SLAvTrt2 <- glm(SLA ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_SLAvTrt2)
# AIC: 327.0412

plot(SN_Ch_Traits$SLA~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SLA", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_SLAvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.733"), font=2)
text(locator(), labels = c("R2=-0.040"), font=2)






############ Leaf_Thickness_cm #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_LeafThvTrt <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_LeafThvTrt)
AIC(reg_DO_Ch_LeafThvTrt)
# AIC: -178.8786

reg_DO_Ch_LeafThvTrt <- aov(Leaf_Thickness_cm~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_LeafThvTrt, "means")

# run second order Linear model 
reg_DO_Ch_LeafThvTrt2 <- glm(Leaf_Thickness_cm ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_LeafThvTrt2)
summary(reg_DO_Ch_LeafThvTrt2)
# AIC: -179.0105 ** LOWEST

# predict based on best AIC: 
fakeY_DOLeafTh_poly2 <- predict(reg_DO_Ch_LeafThvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                      type="response", se.fit=F))

# plot
plot(DO_Ch_Traits$Leaf_Thickness_cm~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Leaf Thickness (cm)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DOLeafTh_poly2~fakeX_DOTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.356"), font=2)
text(locator(), labels = c("R2=0.007"), font=2)


# AP 
# run linear model 
reg_AP_Ch_LeafThvTrt <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_LeafThvTrt)
AIC(reg_AP_Ch_LeafThvTrt)
# AIC: -119.6728 *** LOWEST

reg_AP_Ch_LeafThvTrt <- aov(Leaf_Thickness_cm~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_LeafThvTrt, "means")

# run second order Linear model 
reg_AP_Ch_LeafThvTrt2 <- lm(Leaf_Thickness_cm ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_LeafThvTrt2)
# AIC: -118.5739

plot(AP_Ch_Traits$Leaf_Thickness_cm~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Thickness (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_LeafThvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.018 *"), font=2)
text(locator(), labels = c("R2=0.194"), font=2)


# SM
# run linear model 
reg_SM_Ch_LeafThvTrt <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_LeafThvTrt)
AIC(reg_SM_Ch_LeafThvTrt)
# AIC: -153.8311 *** LOWEST

reg_SM_Ch_LeafThvTrt <- aov(Leaf_Thickness_cm~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_LeafThvTrt, "means")

# run second order Linear model 
reg_SM_Ch_LeafThvTrt2 <- glm(Leaf_Thickness_cm ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_LeafThvTrt2)
summary(reg_SM_Ch_LeafThvTrt2)
# AIC:-152.6778 

plot(SM_Ch_Traits$Leaf_Thickness_cm~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Thickness (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_LeafThvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.141"), font=2)
text(locator(), labels = c("R2=0.057"), font=2)


# AG
# run linear model 
reg_AG_Ch_LeafThvTrt <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_LeafThvTrt)
summary(reg_AG_Ch_LeafThvTrt)
# AIC: -152.6357 *** LOWEST

reg_AG_Ch_LeafThvTrt <- aov(Leaf_Thickness_cm~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_LeafThvTrt, "means")

# run second order Linear model 
reg_AG_Ch_LeafThvTrt2 <- lm(Leaf_Thickness_cm~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_LeafThvTrt2)
summary(reg_AG_Ch_LeafThvTrt2)
# AIC: -151.2496 

plot(AG_Ch_Traits$Leaf_Thickness_cm~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Thickness (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_LeafThvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.667"), font=2)
text(locator(), labels = c("R2=-0.037"), font=2)



# SN
# run linear model 
reg_SN_Ch_LeafThvTrt <- lm(Leaf_Thickness_cm~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_LeafThvTrt)
summary(reg_SN_Ch_LeafThvTrt)
# AIC: -175.0848

reg_SN_Ch_LeafThvTrt <- aov(Leaf_Thickness_cm~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_LeafThvTrt, "means")

# run second order Linear model 
reg_SN_Ch_LeafThvTrt2 <- glm(Leaf_Thickness_cm ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_LeafThvTrt2)
summary(reg_SN_Ch_LeafThvTrt2)
# AIC: -175.8213 *** LOWEST

# predict based on best AIC: 
fakeY_SNLeafTh_poly2 <- predict(reg_SN_Ch_LeafThvTrt2, list(Treatment2= fakeX_SNTrt2,
                                                            type="response", se.fit=F))

# plot
plot(SN_Ch_Traits$Leaf_Thickness_cm~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Thickness (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
lines(fakeY_SNLeafTh_poly2~fakeX_SNTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.206"), font=2)
text(locator(), labels = c("R2=0.058"), font=2)






############ Aboveground_Dry_Weight_g #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_AbGWeightvTrt <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_AbGWeightvTrt)
AIC(reg_DO_Ch_AbGWeightvTrt)
# AIC: -8.347425

reg_DO_Ch_AbGWeightvTrt <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_AbGWeightvTrt, "means")

# run second order Linear model 
reg_DO_Ch_AbGWeightvTrt2 <- glm(Aboveground_Dry_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_AbGWeightvTrt2)
summary(reg_DO_Ch_AbGWeightvTrt2)
# AIC: -11.26307 ** LOWEST

# predict based on best AIC: 
fakeY_DOAbGWeight_poly2 <- predict(reg_DO_Ch_AbGWeightvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                            type="response", se.fit=F))

# plot
plot(DO_Ch_Traits$Aboveground_Dry_Weight_g~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Aboveground Biomass (g)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DOAbGWeight_poly2~fakeX_SNTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.114"), font=2)
text(locator(), labels = c("R2=0.109"), font=2)


# AP 
# run linear model 
reg_AP_Ch_AbGWeightvTrt <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_AbGWeightvTrt)
AIC(reg_AP_Ch_AbGWeightvTrt)
# AIC: 46.37704 *** LOWEST

reg_AP_Ch_AbGWeightvTrt <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_AbGWeightvTrt, "means")

# run second order Linear model 
reg_AP_Ch_AbGWeightvTrt2 <- lm(Aboveground_Dry_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_AbGWeightvTrt2)
# AIC: 47.5625

plot(AP_Ch_Traits$Aboveground_Dry_Weight_g~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_AbGWeightvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.035 *"), font=2)
text(locator(), labels = c("R2=0.151"), font=2)


# SM
# run linear model 
reg_SM_Ch_AbGWeightvTrt <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_AbGWeightvTrt)
AIC(reg_SM_Ch_AbGWeightvTrt)
# AIC: 107.1043 *** LOWEST

reg_SM_Ch_AbGWeightvTrt <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_AbGWeightvTrt, "means")

# run second order Linear model 
reg_SM_Ch_AbGWeightvTrt2 <- lm(Aboveground_Dry_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_AbGWeightvTrt2)
summary(reg_SM_Ch_AbGWeightvTrt2)
# AIC: 108.6653 

plot(SM_Ch_Traits$Aboveground_Dry_Weight_g~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_AbGWeightvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.172"), font=2)
text(locator(), labels = c("R2=0.043"), font=2)


# AG
# run linear model 
reg_AG_Ch_AbGWeightvTrt <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_AbGWeightvTrt)
summary(reg_AG_Ch_AbGWeightvTrt)
# AIC: 51.10059 *** LOWEST

reg_AG_Ch_AbGWeightvTrt <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_AbGWeightvTrt, "means")

# run second order Linear model 
reg_AG_Ch_AbGWeightvTrt2 <- lm(Aboveground_Dry_Weight_g~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_AbGWeightvTrt2)
summary(reg_AG_Ch_AbGWeightvTrt2)
# AIC: 52.79698 

plot(AG_Ch_Traits$Aboveground_Dry_Weight_g~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_AbGWeightvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.026 *"), font=2)
text(locator(), labels = c("R2=0.171"), font=2)



# SN
# run linear model 
reg_SN_Ch_AbGWeightvTrt <- lm(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_AbGWeightvTrt)
summary(reg_SN_Ch_AbGWeightvTrt)
# AIC: 54.52117 *** LOWEST

reg_SN_Ch_AbGWeightvTrt <- aov(Aboveground_Dry_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_AbGWeightvTrt, "means")

# run second order Linear model 
reg_SN_Ch_AbGWeightvTrt2 <- lm(Aboveground_Dry_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_AbGWeightvTrt2)
summary(reg_SN_Ch_AbGWeightvTrt2)
# AIC: 56.51912 

plot(SN_Ch_Traits$Aboveground_Dry_Weight_g~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Aboveground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_AbGWeightvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.022 *"), font=2)
text(locator(), labels = c("R2=0.180"), font=2)




############ Leaf_Number #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_LeafNumvTrt <- lm(Leaf_Number~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_LeafNumvTrt)
AIC(reg_DO_Ch_LeafNumvTrt)
model.tables(reg_DO_Ch_LeafNumvTrt, "means")
# AIC: 114.9596 ** LOWEST

# run second order Linear model 
reg_DO_Ch_LeafNumvTrt2 <- aov(Leaf_Number ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_LeafNumvTrt2)
summary(reg_DO_Ch_LeafNumvTrt2)
# AIC: 116.9137

plot(DO_Ch_Traits$Leaf_Number~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Leaf Number",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_LeafNumvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.080"), font=2)
text(locator(), labels = c("R2=0.094"), font=2)


# AP 
# run linear model 
reg_AP_Ch_LeafNumvTrt <- lm(Leaf_Number~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_LeafNumvTrt)
AIC(reg_AP_Ch_LeafNumvTrt)
# AIC: 183.1096 *** LOWEST

reg_AP_Ch_LeafNumvTrt <- aov(Leaf_Number~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_LeafNumvTrt, "means")


# run second order Linear model 
reg_AP_Ch_LeafNumvTrt2 <- lm(Leaf_Number ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_LeafNumvTrt2)
# AIC: 183.2404

plot(AP_Ch_Traits$Leaf_Number~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Number", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_LeafNumvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.554"), font=2)


# SM
# run linear model 
reg_SM_Ch_LeafNumvTrt <- lm(Leaf_Number~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_LeafNumvTrt)
AIC(reg_SM_Ch_LeafNumvTrt)
# AIC: 203.1434 *** LOWEST

reg_SM_Ch_LeafNumvTrt <- aov(Leaf_Number~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_LeafNumvTrt, "means")

# run second order Linear model 
reg_SM_Ch_LeafNumvTrt2 <- lm(Leaf_Number ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_LeafNumvTrt2)
summary(reg_SM_Ch_LeafNumvTrt2)
# AIC: 204.6996 

plot(SM_Ch_Traits$Leaf_Number~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Number", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_LeafNumvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.256"), font=2)
text(locator(), labels = c("R2=0.016"), font=2)


# AG
# run linear model 
reg_AG_Ch_LeafNumvTrt <- lm(Leaf_Number~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_LeafNumvTrt)
summary(reg_AG_Ch_LeafNumvTrt)
# AIC: 157.3578 *** LOWEST

reg_AG_Ch_LeafNumvTrt <- aov(Leaf_Number~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_LeafNumvTrt, "means")

# run second order Linear model 
reg_AG_Ch_LeafNumvTrt2 <- lm(Leaf_Number~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_LeafNumvTrt2)
summary(reg_AG_Ch_LeafNumvTrt2)
# AIC: 158.4287 

plot(AG_Ch_Traits$Leaf_Number~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Number", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_LeafNumvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.010 *"), font=2)
text(locator(), labels = c("R2=0.229"), font=2)



# SN
# run linear model 
reg_SN_Ch_LeafNumvTrt <- lm(Leaf_Number~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_LeafNumvTrt)
summary(reg_SN_Ch_LeafNumvTrt)
# AIC: 135.292 *** LOWEST

reg_SN_Ch_LeafNumvTrt <- aov(Leaf_Number~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_LeafNumvTrt, "means")

# run second order Linear model 
reg_SN_Ch_LeafNumvTrt2 <- lm(Leaf_Number ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_LeafNumvTrt2)
summary(reg_SN_Ch_LeafNumvTrt2)
# AIC: 136.392 

plot(SN_Ch_Traits$Leaf_Number~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Number", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_LeafNumvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.436"), font=2)
text(locator(), labels = c("R2=-0.016"), font=2)






############ Plant_Height_cm #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_HeightvTrt <- lm(Plant_Height_cm~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_HeightvTrt)
AIC(reg_DO_Ch_HeightvTrt)
# AIC: 159.35 

reg_DO_Ch_HeightvTrt <- aov(Plant_Height_cm~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_HeightvTrt, "means")

# run second order Linear model 
reg_DO_Ch_HeightvTrt2 <- glm(Plant_Height_cm ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_HeightvTrt2)
summary(reg_DO_Ch_HeightvTrt2)
# AIC: 158.8714 ** LOWEST

# predict based on best AIC: 
fakeY_DOHeight_poly2 <- predict(reg_DO_Ch_HeightvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                                  type="response", se.fit=F))


plot(DO_Ch_Traits$Plant_Height_cm~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Plant Height (cm)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DOHeight_poly2~fakeX_DOTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.315"), font=2)
text(locator(), labels = c("R2=0.019"), font=2)


# AP 
# run linear model 
reg_AP_Ch_HeightvTrt <- lm(Plant_Height_cm~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_HeightvTrt)
AIC(reg_AP_Ch_HeightvTrt)
# AIC: 142.7582 *** LOWEST

reg_AP_Ch_HeightvTrt <- aov(Plant_Height_cm~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_HeightvTrt, "means")

# run second order Linear model 
reg_AP_Ch_HeightvTrt2 <- lm(Plant_Height_cm ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_HeightvTrt2)
# AIC: 144.6363

plot(AP_Ch_Traits$Plant_Height_cm~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Plant Height (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_HeightvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.348"), font=2)


# SM
# run linear model 
reg_SM_Ch_HeightvTrt <- lm(Plant_Height_cm~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_HeightvTrt)
AIC(reg_SM_Ch_HeightvTrt)
# AIC: 178.0006 *** LOWEST

reg_SM_Ch_HeightvTrt <- aov(Plant_Height_cm~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_HeightvTrt, "means")

# run second order Linear model 
reg_SM_Ch_HeightvTrt2 <- lm(Plant_Height_cm ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_HeightvTrt2)
summary(reg_SM_Ch_HeightvTrt2)
# AIC: 179.5496 

plot(SM_Ch_Traits$Plant_Height_cm~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Plant Height (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_HeightvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.764"), font=2)
text(locator(), labels = c("R2=-0.043"), font=2)


# AG
# run linear model 
reg_AG_Ch_HeightvTrt <- lm(Plant_Height_cm~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_HeightvTrt)
summary(reg_AG_Ch_HeightvTrt)
# AIC: 185.5892 *** LOWEST

reg_AG_Ch_HeightvTrt <- aov(Plant_Height_cm~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_HeightvTrt, "means")

# run second order Linear model 
reg_AG_Ch_HeightvTrt2 <- lm(Plant_Height_cm~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_HeightvTrt2)
summary(reg_AG_Ch_HeightvTrt2)
# AIC: 187.0458 

plot(AG_Ch_Traits$Plant_Height_cm~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Plant Height (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_HeightvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.203"), font=2)
text(locator(), labels = c("R2=0.030"), font=2)



# SN
# run linear model 
reg_SN_Ch_HeightvTrt <- lm(Plant_Height_cm~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_HeightvTrt)
summary(reg_SN_Ch_HeightvTrt)
# AIC: 194.9921 *** LOWEST

reg_SN_Ch_HeightvTrt <- aov(Plant_Height_cm~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_HeightvTrt, "means")

# run second order Linear model 
reg_SN_Ch_HeightvTrt2 <- lm(Plant_Height_cm ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_HeightvTrt2)
summary(reg_SN_Ch_HeightvTrt2)
# AIC: 196.9872 

plot(SN_Ch_Traits$Plant_Height_cm~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Plant Height (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_HeightvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.036 *"), font=2)
text(locator(), labels = c("R2=0.147"), font=2)





############ Focal_Root_Weight_g #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_RootWtvTrt <- lm(Focal_Root_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_RootWtvTrt)
AIC(reg_DO_Ch_RootWtvTrt)
# AIC: -12.09483 ** LOWEST

reg_DO_Ch_RootWtvTrt <- aov(Focal_Root_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_RootWtvTrt, "means")

# run second order Linear model 
reg_DO_Ch_RootWtvTrt2 <- lm(Focal_Root_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_RootWtvTrt2)
summary(reg_DO_Ch_RootWtvTrt2)
# AIC: -10.09713 

plot(DO_Ch_Traits$Focal_Root_Weight_g~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Focal Root Biomass (g)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_RootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.715"), font=2)
text(locator(), labels = c("R2=-0.039"), font=2)


# AP 
# run linear model 
reg_AP_Ch_RootWtvTrt <- lm(Focal_Root_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_RootWtvTrt)
AIC(reg_AP_Ch_RootWtvTrt)
# AIC:-61.35022 *** LOWEST

reg_AP_Ch_RootWtvTrt <- aov(Focal_Root_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_RootWtvTrt, "means")

# run second order Linear model 
reg_AP_Ch_RootWtvTrt2 <- lm(Focal_Root_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_RootWtvTrt2)
# AIC: -60.06655

plot(AP_Ch_Traits$Focal_Root_Weight_g~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Focal Root Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_RootWtvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.005 *"), font=2)
text(locator(), labels = c("R2=0.280"), font=2)


# SM
# run linear model 
reg_SM_Ch_RootWtvTrt <- lm(Focal_Root_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_RootWtvTrt)
AIC(reg_SM_Ch_RootWtvTrt)
# AIC: 30.29631 *** LOWEST

reg_SM_Ch_RootWtvTrt <- aov(Focal_Root_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_RootWtvTrt, "means")


# run second order Linear model 
reg_SM_Ch_RootWtvTrt2 <- lm(Focal_Root_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_RootWtvTrt2)
summary(reg_SM_Ch_RootWtvTrt2)
# AIC: 32.29585 

plot(SM_Ch_Traits$Focal_Root_Weight_g~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Focal Root Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_RootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.193"), font=2)
text(locator(), labels = c("R2=0.036"), font=2)


# AG
# run linear model 
reg_AG_Ch_RootWtvTrt <- lm(Focal_Root_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_RootWtvTrt)
summary(reg_AG_Ch_RootWtvTrt)
# AIC: 28.48727 *** LOWEST

reg_AG_Ch_RootWtvTrt <- aov(Focal_Root_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_RootWtvTrt, "means")


# run second order Linear model 
reg_AG_Ch_RootWtvTrt2 <- lm(Focal_Root_Weight_g~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_RootWtvTrt2)
summary(reg_AG_Ch_RootWtvTrt2)
# AIC: 28.83626 

plot(AG_Ch_Traits$Focal_Root_Weight_g~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Focal Root Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_RootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.818"), font=2)
text(locator(), labels = c("R2=-0.045"), font=2)



# SN
# run linear model 
reg_SN_Ch_RootWtvTrt <- lm(Focal_Root_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_RootWtvTrt)
summary(reg_SN_Ch_RootWtvTrt)
# AIC: 1.463806 *** LOWEST

reg_SN_Ch_RootWtvTrt <- aov(Focal_Root_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_RootWtvTrt, "means")

# run second order Linear model 
reg_SN_Ch_RootWtvTrt2 <- lm(Focal_Root_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_RootWtvTrt2)
summary(reg_SN_Ch_RootWtvTrt2)
# AIC: 3.281737 

plot(SN_Ch_Traits$Focal_Root_Weight_g~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Focal Root Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_RootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.845"), font=2)
text(locator(), labels = c("R2=-0.044"), font=2)





############ Total_Belowground_Weight_g #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_TRootWtvTrt <- lm(Total_Belowground_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_TRootWtvTrt)
AIC(reg_DO_Ch_TRootWtvTrt)
# AIC: 100.0179 ** LOWEST

reg_DO_Ch_TRootWtvTrt <- aov(Total_Belowground_Weight_g~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_TRootWtvTrt, "means")

# run second order Linear model
reg_DO_Ch_TRootWtvTrt2 <- lm(Total_Belowground_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_TRootWtvTrt2)
summary(reg_DO_Ch_TRootWtvTrt2)
# AIC: 101.6024 

plot(DO_Ch_Traits$Total_Belowground_Weight_g~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Total Belowground Biomass (g)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_TRootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.221"), font=2)
text(locator(), labels = c("R2=0.025"), font=2)


# AP 
# run linear model 
reg_AP_Ch_TRootWtvTrt <- lm(Total_Belowground_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_TRootWtvTrt)
AIC(reg_AP_Ch_TRootWtvTrt)
# AIC: 78.12447 *** LOWEST

reg_AP_Ch_TRootWtvTrt <- aov(Total_Belowground_Weight_g~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_TRootWtvTrt, "means")

# run second order Linear model 
reg_AP_Ch_TRootWtvTrt2 <- lm(Total_Belowground_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_TRootWtvTrt2)
# AIC: 78.5077

plot(AP_Ch_Traits$Total_Belowground_Weight_g~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Total Belowground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_TRootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.880"), font=2)
text(locator(), labels = c("R2=-0.046"), font=2)


# SM
# run linear model 
reg_SM_Ch_TRootWtvTrt <- lm(Total_Belowground_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_TRootWtvTrt)
AIC(reg_SM_Ch_TRootWtvTrt)
# AIC: 94.08027 *** LOWEST

reg_SM_Ch_TRootWtvTrt <- aov(Total_Belowground_Weight_g~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_TRootWtvTrt, "means")

# run second order Linear model 
reg_SM_Ch_TRootWtvTrt2 <- lm(Total_Belowground_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_TRootWtvTrt2)
summary(reg_SM_Ch_RootWtvTrt2)
# AIC: 95.24565 

plot(SM_Ch_Traits$Total_Belowground_Weight_g~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Total Belowground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_TRootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.564"), font=2)
text(locator(), labels = c("R2=-0.031"), font=2)


# AG
# run linear model 
reg_AG_Ch_TRootWtvTrt <- lm(Total_Belowground_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_TRootWtvTrt)
summary(reg_AG_Ch_TRootWtvTrt)
# AIC: 89.55296 *** LOWEST

reg_AG_Ch_TRootWtvTrt <- aov(Total_Belowground_Weight_g~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_TRootWtvTrt, "means")

# run second order Linear model 
reg_AG_Ch_TRootWtvTrt2 <- lm(Total_Belowground_Weight_g~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_TRootWtvTrt2)
summary(reg_AG_Ch_TRootWtvTrt2)
# AIC: 91.53866 

plot(AG_Ch_Traits$Total_Belowground_Weight_g~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Total Belowground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_TRootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.969"), font=2)
text(locator(), labels = c("R2=-0.048"), font=2)



# SN
# run linear model 
reg_SN_Ch_TRootWtvTrt <- lm(Total_Belowground_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_TRootWtvTrt)
summary(reg_SN_Ch_TRootWtvTrt)
# AIC: 94.04792 *** LOWEST

reg_SN_Ch_TRootWtvTrt <- aov(Total_Belowground_Weight_g~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_TRootWtvTrt, "means")

# run second order Linear model 
reg_SN_Ch_TRootWtvTrt2 <- lm(Total_Belowground_Weight_g ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_TRootWtvTrt2)
summary(reg_SN_Ch_TRootWtvTrt2)
# AIC: 95.76917 

plot(SN_Ch_Traits$Total_Belowground_Weight_g~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Total Belowground Biomass (g)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_TRootWtvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.229"), font=2)
text(locator(), labels = c("R2=0.023"), font=2)




############ SRL #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_SRLvTrt <- lm(SRL~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_SRLvTrt)
AIC(reg_DO_Ch_SRLvTrt)
# AIC: 161.4478 ** LOWEST

reg_DO_Ch_SRLvTrt <- aov(SRL~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_SRLvTrt, "means")


# run second order Linear model 
reg_DO_Ch_SRLvTrt2 <- glm(SRL ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_SRLvTrt2)
summary(reg_DO_Ch_SRLvTrt2)
# AIC: 163.3743 

plot(DO_Ch_Traits$SRL~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="SRL",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_SRLvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.963"), font=2)
text(locator(), labels = c("R2=-0.045"), font=2)


# AP 
# run linear model 
reg_AP_Ch_SRLvTrt <- lm(SRL~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_SRLvTrt)
AIC(reg_AP_Ch_SRLvTrt)
# AIC: 142.0515 

reg_AP_Ch_SRLvTrt <- aov(SRL~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_SRLvTrt, "means")



# run second order Linear model 
reg_AP_Ch_SRLvTrt2 <- glm(SRL ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_SRLvTrt2)
summary(reg_AP_Ch_SRLvTrt2)
# AIC: 138.1772 *** LOWEST

# predict based on best AIC: 
fakeY_APSRL_poly2 <- predict(reg_AP_Ch_SRLvTrt2, list(Treatment2= fakeX_APTrt2,
                                                      type="response", se.fit=F))

plot(AP_Ch_Traits$SRL~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SRL", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
lines(fakeY_APSRL_poly2~fakeX_APTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.074"), font=2)
text(locator(), labels = c("R2=0.152"), font=2)


# SM
# run linear model 
reg_SM_Ch_SRLvTrt <- lm(SRL~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_SRLvTrt)
AIC(reg_SM_Ch_SRLvTrt)
# AIC: 59.8522 *** LOWEST

reg_SM_Ch_SRLvTrt <- aov(SRL~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_SRLvTrt, "means")

# run second order Linear model 
reg_SM_Ch_SRLvTrt2 <- lm(SRL ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_SRLvTrt2)
summary(reg_SM_Ch_SRLvTrt2)
# AIC: 61.33047 

plot(SM_Ch_Traits$SRL~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SRL", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_SRLvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.925"), font=2)
text(locator(), labels = c("R2=-0.047"), font=2)


# AG
# run linear model 
reg_AG_Ch_SRLvTrt <- lm(SRL~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_SRLvTrt)
summary(reg_AG_Ch_SRLvTrt)
# AIC: 100.642 

reg_AG_Ch_SRLvTrt <- aov(SRL~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_SRLvTrt, "means")

# run second order Linear model 
reg_AG_Ch_SRLvTrt2 <- glm(SRL~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_SRLvTrt2)
summary(reg_AG_Ch_SRLvTrt2)
# AIC: 99.20518  *** LOWEST

# predict based on best AIC: 
fakeY_AGSRL_poly2 <- predict(reg_AG_Ch_SRLvTrt2, list(Treatment2= fakeX_AGTrt2,
                                                                  type="response", se.fit=F))


plot(AG_Ch_Traits$SRL~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SRL", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
lines(fakeY_AGSRL_poly2~fakeX_AGTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.206"), font=2)
text(locator(), labels = c("R2=0.061"), font=2)



# SN
# run linear model 
reg_SN_Ch_SRLvTrt <- lm(SRL~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_SRLvTrt)
summary(reg_SN_Ch_SRLvTrt)
# AIC: 141.4244 *** LOWEST


reg_SN_Ch_SRLvTrt <- aov(SRL~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_SRLvTrt, "means")

# run second order Linear model 
reg_SN_Ch_SRLvTrt2 <- lm(SRL ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_SRLvTrt2)
summary(reg_SN_Ch_TSRLvTrt2)
# AIC: 142.2945 

plot(SN_Ch_Traits$SRL~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="SRL", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_SRLvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.329"), font=2)
text(locator(), labels = c("R2=-0.001"), font=2)






############ Root_Diam_Max #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_RootMaxvTrt <- lm(Root_Diam_Max~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_RootMaxvTrt)
AIC(reg_DO_Ch_RootMaxvTrt)
# AIC: -71.21143 

reg_DO_Ch_RootMaxvTrt <- aov(Root_Diam_Max~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_RootMaxvTrt, "means")

# run second order Linear model 
reg_DO_Ch_RootMaxvTrt2 <- glm(Root_Diam_Max ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_RootMaxvTrt2)
summary(reg_DO_Ch_RootMaxvTrt2)
# AIC: -71.66384 ** LOWEST

# predict based on best AIC: 
fakeY_DORootMax_poly2 <- predict(reg_DO_Ch_RootMaxvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                                  type="response", se.fit=F))

plot(DO_Ch_Traits$Root_Diam_Max~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Maximum Root Diameter (cm)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DORootMax_poly2~fakeX_SNTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.302"), font=2)
text(locator(), labels = c("R2=0.023"), font=2)


# AP 
# run linear model 
reg_AP_Ch_RootMaxvTrt <- lm(Root_Diam_Max~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_RootMaxvTrt)
AIC(reg_AP_Ch_RootMaxvTrt)
# AIC: -58.87996 *** LOWEST

reg_AP_Ch_RootMaxvTrt <- aov(Root_Diam_Max~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_RootMaxvTrt, "means")

# run second order Linear model 
reg_AP_Ch_RootMaxvTrt2 <- glm(Root_Diam_Max ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_RootMaxvTrt2)
summary(reg_AP_Ch_RootMaxvTrt2)
# AIC: -56.89237

plot(AP_Ch_Traits$Root_Diam_Max~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Maximum Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_RootMaxvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.045 *"), font=2)
text(locator(), labels = c("R2=0.152"), font=2)


# SM
# run linear model 
reg_SM_Ch_RootMaxvTrt <- lm(Root_Diam_Max~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_RootMaxvTrt)
AIC(reg_SM_Ch_RootMaxvTrt)
# AIC: -23.40221 *** LOWEST

reg_SM_Ch_RootMaxvTrt <- aov(Root_Diam_Max~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_AP_Ch_RootMaxvTrt, "means")

# run second order Linear model 
reg_SM_Ch_RootMaxvTrt2 <- lm(Root_Diam_Max ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_RootMaxvTrt2)
summary(reg_SM_Ch_RootMaxvTrt2)
# AIC: -21.96439 

plot(SM_Ch_Traits$Root_Diam_Max~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Maximum Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_RootMaxvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.614"), font=2)
text(locator(), labels = c("R2=-0.035"), font=2)


# AG
# run linear model 
reg_AG_Ch_RootMaxvTrt <- lm(Root_Diam_Max~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_RootMaxvTrt)
summary(reg_AG_Ch_RootMaxvTrt)
# AIC: -24.55222

reg_AG_Ch_RootMaxvTrt <- aov(Root_Diam_Max~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_RootMaxvTrt, "means")

# run second order Linear model 
reg_AG_Ch_RootMaxvTrt2 <- glm(Root_Diam_Max~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_RootMaxvTrt2)
summary(reg_AG_Ch_RootMaxvTrt2)
# AIC: -27.09289  *** LOWEST

# predict based on best AIC: 
fakeY_AGRootMax_poly2 <- predict(reg_AG_Ch_RootMaxvTrt2, list(Treatment2= fakeX_AGTrt2,
                                                      type="response", se.fit=F))


plot(AG_Ch_Traits$Root_Diam_Max~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Maximum Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
lines(fakeY_AGRootMax_poly2~fakeX_AGTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.092"), font=2)
text(locator(), labels = c("R2=0.140"), font=2)



# SN
# run linear model 
reg_SN_Ch_RootMaxvTrt <- lm(Root_Diam_Max~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_RootMaxvTrt)
summary(reg_SN_Ch_RootMaxvTrt)
# AIC: -34.42705 *** LOWEST

reg_SN_Ch_RootMaxvTrt <- aov(Root_Diam_Max~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_AG_Ch_RootMaxvTrt, "means")

# run second order Linear model 
reg_SN_Ch_RootMaxvTrt2 <- lm(Root_Diam_Max ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_RootMaxvTrt2)
summary(reg_SN_Ch_RootMaxvTrt2)
# AIC: -33.28725 

plot(SN_Ch_Traits$Root_Diam_Max~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Maximum Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_RootMaxvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.213"), font=2)
text(locator(), labels = c("R2=0.027"), font=2)







############ Root_Diam_Mean #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_RootMeanvTrt <- lm(Root_Diam_Mean~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_RootMeanvTrt)
AIC(reg_DO_Ch_RootMeanvTrt)
# AIC: -172.8293 

reg_DO_Ch_RootMeanvTrt <- aov(Root_Diam_Mean~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_RootMeanvTrt, "means")

# run second order Linear model 
reg_DO_Ch_RootMeanvTrt2 <- lm(Root_Diam_Mean ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_RootMeanvTrt2)
summary(reg_DO_Ch_RootMeanvTrt2)
# AIC: -173.4341 ** LOWEST

# predict based on best AIC: 
fakeY_DORootMean_poly2 <- predict(reg_DO_Ch_RootMeanvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                              type="response", se.fit=F))

plot(DO_Ch_Traits$Root_Diam_Mean~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Mean Root Diameter (cm)",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DORootMean_poly2~fakeX_DOTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.132"), font=2)
text(locator(), labels = c("R2=0.097"), font=2)


# AP 
# run linear model 
reg_AP_Ch_RootMeanvTrt <- lm(Root_Diam_Mean~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_RootMeanvTrt)
AIC(reg_AP_Ch_RootMeanvTrt)
# AIC: -136.4202 

reg_AP_Ch_RootMeanvTrt <- aov(Root_Diam_Mean~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_RootMeanvTrt, "means")

# run second order Linear model 
reg_AP_Ch_RootMeanvTrt2 <- glm(Root_Diam_Mean ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_RootMeanvTrt2)
summary(reg_AP_Ch_RootMeanvTrt2)
# AIC: -137.6921 *** LOWEST

# predict based on best AIC: 
fakeY_APRootMean_poly2 <- predict(reg_AP_Ch_RootMeanvTrt2, list(Treatment2= fakeX_APTrt2,
                                                                type="response", se.fit=F))

plot(AP_Ch_Traits$Root_Diam_Mean~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Mean Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
lines(fakeY_APRootMean_poly2~fakeX_APTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.226"), font=2)
text(locator(), labels = c("R2=0.052"), font=2)


# SM
# run linear model 
reg_SM_Ch_RootMeanvTrt <- lm(Root_Diam_Mean~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_RootMeanvTrt)
AIC(reg_SM_Ch_RootMeanvTrt)
# AIC: -125.1643 *** LOWEST

reg_SM_Ch_RootMeanvTrt <- aov(Root_Diam_Mean~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_RootMeanvTrt, "means")

# run second order Linear model 
reg_SM_Ch_RootMeanvTrt2 <- lm(Root_Diam_Mean ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_RootMeanvTrt2)
summary(reg_SM_Ch_RootMeanvTrt2)
# AIC: -123.2209 

plot(SM_Ch_Traits$Root_Diam_Mean~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Mean Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_RootMeanvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.975"), font=2)
text(locator(), labels = c("R2=-0.097"), font=2)


# AG
# run linear model 
reg_AG_Ch_RootMeanvTrt <- lm(Root_Diam_Mean~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_RootMeanvTrt)
summary(reg_AG_Ch_RootMeanvTrt)
# AIC: -112.9679

reg_AG_Ch_RootMeanvTrt <- aov(Root_Diam_Mean~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_RootMeanvTrt, "means")

# run second order Linear model 
reg_AG_Ch_RootMeanvTrt2 <- glm(Root_Diam_Mean~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_RootMeanvTrt2)
summary(reg_AG_Ch_RootMeanvTrt2)
# AIC: -116.4182  *** LOWEST

# predict based on best AIC: 
fakeY_AGRootMean_poly2 <- predict(reg_AG_Ch_RootMeanvTrt2, list(Treatment2= fakeX_AGTrt2,
                                                              type="response", se.fit=F))


plot(AG_Ch_Traits$Root_Diam_Mean~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Mean Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
lines(fakeY_AGRootMean_poly2~fakeX_AGTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.077"), font=2)
text(locator(), labels = c("R2=0.157"), font=2)



# SN
# run linear model 
reg_SN_Ch_RootMeanvTrt <- lm(Root_Diam_Mean~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_RootMeanvTrt)
summary(reg_SN_Ch_RootMeanvTrt)
# AIC: -173.7496 *** LOWEST

reg_SN_Ch_RootMeanvTrt <- aov(Root_Diam_Mean~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_RootMeanvTrt, "means")

# run second order Linear model 
reg_SN_Ch_RootMeanvTrt2 <- lm(Root_Diam_Mean ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_RootMeanvTrt2)
summary(reg_SN_Ch_RootMeanvTrt2)
# AIC: -172.2659 

plot(SN_Ch_Traits$Root_Diam_Mean~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Mean Root Diameter (cm)", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_RootMeanvTrt), lwd=3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.213"), font=2)
text(locator(), labels = c("R2=0.027"), font=2)





############ Percent_N_AbG #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_NAbGvTrt <- lm(Percent_N_AbG~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_NAbGvTrt)
AIC(reg_DO_Ch_NAbGvTrt)
# AIC: 27.63447 ** LOWEST

reg_DO_Ch_NAbGvTrt <- aov(Percent_N_AbG~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_NAbGvTrt, "means")

# run second order Linear model 
reg_DO_Ch_NAbGvTrt2 <- lm(Percent_N_AbG ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_NAbGvTrt2)
summary(reg_DO_Ch_NAbGvTrt2)
# AIC: 28.359091 

plot(DO_Ch_Traits$Percent_N_AbG~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Leaf Percent N",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_NAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.762"), font=2)


# AP 
# run linear model 
reg_AP_Ch_NAbGvTrt <- lm(Percent_N_AbG~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_NAbGvTrt)
AIC(reg_AP_Ch_NAbGvTrt)
# AIC: 56.23293 

reg_AP_Ch_NAbGvTrt <- aov(Percent_N_AbG~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_NAbGvTrt, "means")

# run second order Linear model 
reg_AP_Ch_NAbGvTrt2 <- glm(Percent_N_AbG ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_NAbGvTrt2)
summary(reg_AP_Ch_NAbGvTrt2)
# AIC: 49.26066 *** LOWEST

# predict based on best AIC: 
fakeY_APNAbG_poly2 <- predict(reg_AP_Ch_NAbGvTrt2, list(Treatment2= fakeX_APTrt2,
                                                                type="response", se.fit=F))

plot(AP_Ch_Traits$Percent_N_AbG~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
lines(fakeY_APNAbG_poly2~fakeX_APTrt2, lwd = 3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.569"), font=2)


# SM
# run linear model 
reg_SM_Ch_NAbGvTrt <- lm(Percent_N_AbG~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_NAbGvTrt)
AIC(reg_SM_Ch_NAbGvTrt)
# AIC: 31.82168 *** LOWEST

reg_SM_Ch_NAbGvTrt <- aov(Percent_N_AbG~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_NAbGvTrt, "means")

# run second order Linear model 
reg_SM_Ch_NAbGvTrt2 <- lm(Percent_N_AbG ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_NAbGvTrt2)
summary(reg_SM_Ch_NAbGvTrt2)
# AIC: 33.79665 

plot(SM_Ch_Traits$Percent_N_AbG~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_NAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.015 *"), font=2)
text(locator(), labels = c("R2=0.216"), font=2)


# AG
# run linear model 
reg_AG_Ch_NAbGvTrt <- lm(Percent_N_AbG~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_NAbGvTrt)
summary(reg_AG_Ch_NAbGvTrt)
# AIC: -4.793751 *** LOWEST

reg_AG_Ch_NAbGvTrt <- aov(Percent_N_AbG~Treatment2+(1/Block), data = AG_Traits)
model.tables(reg_AG_Ch_NAbGvTrt, "means")

# run second order Linear model 
reg_AG_Ch_NAbGvTrt2 <- glm(Percent_N_AbG~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_NAbGvTrt2)
summary(reg_AG_Ch_NAbGvTrt2)
# AIC: -3.658531  

plot(AG_Ch_Traits$Percent_N_AbG~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_NAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.157"), font=2)



# SN
# run linear model 
reg_SN_Ch_NAbGvTrt <- lm(Percent_N_AbG~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_NAbGvTrt)
summary(reg_SN_Ch_NAbGvTrt)
# AIC: -24.19468 *** LOWEST

reg_SN_Ch_NAbGvTrt <- aov(Percent_N_AbG~Treatment2+(1/Block), data = SN_Traits)
model.tables(reg_SN_Ch_NAbGvTrt, "means")

# run second order Linear model 
reg_SN_Ch_NAbGvTrt2 <- lm(Percent_N_AbG ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_NAbGvTrt2)
summary(reg_SN_Ch_NAbGvTrt2)
# AIC: -22.6516 

plot(SN_Ch_Traits$Percent_N_AbG~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_NAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.762"), font=2)




############ Percent_C_AbG #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_CAbGvTrt <- lm(Percent_C_AbG~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_CAbGvTrt)
AIC(reg_DO_Ch_CAbGvTrt)
# AIC: 89.68579 ** LOWEST

reg_DO_Ch_CAbGvTrt <- aov(Percent_C_AbG~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_CAbGvTrt, "means")

# run second order Linear model 
reg_DO_Ch_CAbGvTrt2 <- lm(Percent_C_AbG ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_CAbGvTrt2)
summary(reg_DO_Ch_CAbGvTrt2)
# AIC: 89.88105 

plot(DO_Ch_Traits$Percent_C_AbG~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Leaf Percent C",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_CAbGvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.067"), font=2)
text(locator(), labels = c("R2=0.762"), font=2)


# AP 
# run linear model 
reg_AP_Ch_CAbGvTrt <- lm(Percent_C_AbG~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_CAbGvTrt)
AIC(reg_AP_Ch_CAbGvTrt)
# AIC: 82.1357 *** LOWEST

reg_AP_Ch_CAbGvTrt <- aov(Percent_C_AbG~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_CAbGvTrt, "means")

# run second order Linear model 
reg_AP_Ch_CAbGvTrt2 <- glm(Percent_C_AbG ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_CAbGvTrt2)
summary(reg_AP_Ch_CAbGvTrt2)
# AIC: 84.08737 

plot(AP_Ch_Traits$Percent_C_AbG~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_CAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.569"), font=2)


# SM
# run linear model 
reg_SM_Ch_CAbGvTrt <- lm(Percent_C_AbG~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_CAbGvTrt)
AIC(reg_SM_Ch_CAbGvTrt)
# AIC: 58.17634 *** LOWEST

reg_SM_Ch_CAbGvTrt <- aov(Percent_C_AbG~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_CAbGvTrt, "means")

# run second order Linear model 
reg_SM_Ch_CAbGvTrt2 <- lm(Percent_C_AbG ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_CAbGvTrt2)
summary(reg_SM_Ch_CAbGvTrt2)
# AIC: 60.09016 

plot(SM_Ch_Traits$Percent_C_AbG~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_CAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.002 *"), font=2)
text(locator(), labels = c("R2=0.351"), font=2)


# AG
# run linear model 
reg_AG_Ch_CAbGvTrt <- lm(Percent_C_AbG~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_CAbGvTrt)
summary(reg_AG_Ch_CAbGvTrt)
# AIC: 57.99545 *** LOWEST

reg_AG_Ch_CAbGvTrt <- aov(Percent_C_AbG~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_CAbGvTrt, "means")

# run second order Linear model 
reg_AG_Ch_CAbGvTrt2 <- glm(Percent_C_AbG~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_CAbGvTrt2)
summary(reg_AG_Ch_CAbGvTrt2)
# AIC: 59.63913  

plot(AG_Ch_Traits$Percent_C_AbG~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_CAbGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.028 *"), font=2)
text(locator(), labels = c("R2=0.165"), font=2)



# SN
# run linear model 
reg_SN_Ch_CAbGvTrt <- lm(Percent_C_AbG~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_CAbGvTrt)
summary(reg_SN_Ch_CAbGvTrt)
# AIC: 68.47004 *** LOWEST

reg_SN_Ch_CAbGvTrt <- aov(Percent_C_AbG~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_CAbGvTrt, "means")

# run second order Linear model 
reg_SN_Ch_CAbGvTrt2 <- lm(Percent_C_AbG ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_CAbGvTrt2)
summary(reg_SN_Ch_CAbGvTrt2)
# AIC: 68.96426 

plot(SN_Ch_Traits$Percent_C_AbG~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Leaf Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_CAbGvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.313"), font=2)
text(locator(), labels = c("R2=0.003"), font=2)



############ Percent_N_BeG #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_NBeGvTrt <- lm(Percent_N_BeG~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_NBeGvTrt)
AIC(reg_DO_Ch_NBeGvTrt)
# AIC: 5.940426 ** LOWEST

reg_DO_Ch_NBeGvTrt <- aov(Percent_N_BeG~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_NBeGvTrt, "means")


# run second order Linear model 
reg_DO_Ch_NBeGvTrt2 <- lm(Percent_N_BeG ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_NBeGvTrt2)
summary(reg_DO_Ch_NBeGvTrt2)
# AIC: 7.833662 

plot(DO_Ch_Traits$Percent_N_BeG~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Root Percent N",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_NBeGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.545"), font=2)


# AP 
# run linear model 
reg_AP_Ch_NBeGvTrt <- lm(Percent_N_BeG~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_NBeGvTrt)
AIC(reg_AP_Ch_NBeGvTrt)
# AIC: 23.73835 

reg_AP_Ch_NBeGvTrt <- aov(Percent_N_BeG~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_NBeGvTrt, "means")

# run second order Linear model 
reg_AP_Ch_NBeGvTrt2 <- lm(Percent_N_BeG ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_NBeGvTrt2)
summary(reg_AP_Ch_NBeGvTrt2)
# AIC: 15.96228 *** LOWEST

# predict based on best AIC: 
fakeY_APNBeG_poly2 <- predict(reg_AP_Ch_NBeGvTrt2, list(Treatment2= fakeX_APTrt2,
                                                        type="response", se.fit=F))

plot(AP_Ch_Traits$Percent_N_BeG~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
lines(fakeY_APNBeG_poly2~fakeX_APTrt2, lwd = 3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.479"), font=2)


# SM
# run linear model 
reg_SM_Ch_NBeGvTrt <- lm(Percent_N_BeG~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_NBeGvTrt)
AIC(reg_SM_Ch_NBeGvTrt)
# AIC: 31.50097 *** LOWEST

reg_SM_Ch_NBeGvTrt <- aov(Percent_N_BeG~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_NBeGvTrt, "means")

# run second order Linear model 
reg_SM_Ch_NBeGvTrt2 <- lm(Percent_N_BeG ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_NBeGvTrt2)
summary(reg_SM_Ch_NBeGvTrt2)
# AIC: 33.33775 

plot(SM_Ch_Traits$Percent_N_BeG~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_NBeGvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.067"), font=2)
text(locator(), labels = c("R2=0.110"), font=2)


# AG
# run linear model 
reg_AG_Ch_NBeGvTrt <- lm(Percent_N_BeG~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_NBeGvTrt)
summary(reg_AG_Ch_NBeGvTrt)
# AIC: 4.776251 *** LOWEST

reg_AG_Ch_NBeGvTrt <- aov(Percent_N_BeG~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_NBeGvTrt, "means")

# run second order Linear model 
reg_AG_Ch_NBeGvTrt2 <- glm(Percent_N_BeG~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_NBeGvTrt2)
summary(reg_AG_Ch_NBeGvTrt2)
# AIC: -11.79807  

plot(AG_Ch_Traits$Percent_N_BeG~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_NBeGvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.652"), font=2)



# SN
# run linear model 
reg_SN_Ch_NBeGvTrt <- lm(Percent_N_BeG~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_NBeGvTrt)
summary(reg_SN_Ch_NBeGvTrt)
# AIC: -9.522982 

reg_SN_Ch_NBeGvTrt <- aov(Percent_N_BeG~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_NBeGvTrt, "means")

# run second order Linear model 
reg_SN_Ch_NBeGvTrt2 <- glm(Percent_N_BeG ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_NBeGvTrt2)
summary(reg_SN_Ch_NBeGvTrt2)
# AIC: -8.948685 *** LOWEST

# predict based on best AIC: 
fakeY_SNNBeG_poly2 <- predict(reg_SN_Ch_NBeGvTrt2, list(Treatment2= fakeX_SNTrt2,
                                                                  type="response", se.fit=F))

# plot
plot(SN_Ch_Traits$Percent_N_BeG~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent N", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
lines(fakeY_SNNBeG_poly2~fakeX_SNTrt2, lwd = 3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.801"), font=2)





############ Percent_C_BeG #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_CBeGvTrt <- lm(Percent_C_BeG~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_CBeGvTrt)
AIC(reg_DO_Ch_CBeGvTrt)
# AIC: 130.0464 

reg_DO_Ch_CBeGvTrt <- aov(Percent_C_BeG~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_CBeGvTrt, "means")

# run second order Linear model 
reg_DO_Ch_CBeGvTrt2 <- lm(Percent_C_BeG ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_CBeGvTrt2)
summary(reg_DO_Ch_CBeGvTrt2)
# AIC: 128.0567 ** LOWEST

# predict based on best AIC: 
fakeY_DOCBeG_poly2 <- predict(reg_DO_Ch_CBeGvTrt2, list(Treatment2= fakeX_DOTrt2,
                                                        type="response", se.fit=F))


plot(DO_Ch_Traits$Percent_C_BeG~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Root Percent C",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
lines(fakeY_DOCBeG_poly2~fakeX_DOTrt2, lwd = 3, col = "blue", lty=2)
text(locator(), labels = c("p=0.172"), font=2)
text(locator(), labels = c("R2=0.074"), font=2)


# AP 
# run linear model 
reg_AP_Ch_CBeGvTrt <- lm(Percent_C_BeG~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_CBeGvTrt)
AIC(reg_AP_Ch_CBeGvTrt)
# AIC: 91.39247 *** LOWEST

reg_AP_Ch_CBeGvTrt <- aov(Percent_C_BeG~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_CBeGvTrt, "means")

# run second order Linear model 
reg_AP_Ch_CBeGvTrt2 <- lm(Percent_C_BeG ~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_CBeGvTrt2)
summary(reg_AP_Ch_CBeGvTrt2)
# AIC: 91.6966 

plot(AP_Ch_Traits$Percent_C_BeG~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_CBeGvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.130"), font=2)
text(locator(), labels = c("R2=0.063"), font=2)


# SM
# run linear model
reg_SM_Ch_CBeGvTrt <- lm(Percent_C_BeG~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_CBeGvTrt)
AIC(reg_SM_Ch_CBeGvTrt)
# AIC: 79.07853 

reg_SM_Ch_CBeGvTrt <- aov(Percent_C_BeG~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_CBeGvTrt, "means")

# run second order Linear model 
reg_SM_Ch_CBeGvTrt2 <- glm(Percent_C_BeG ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_CBeGvTrt2)
summary(reg_SM_Ch_CBeGvTrt2)
# AIC: 78.75527 *** LOWEST

# predict based on best AIC: 
fakeY_SMCBeG_poly2 <- predict(reg_SM_Ch_CBeGvTrt2, list(Treatment2= fakeX_SMTrt2,
                                                        type="response", se.fit=F))
                                                                  

plot(SM_Ch_Traits$Percent_C_BeG~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
lines(fakeY_SMCBeG_poly2~fakeX_SMTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.156"), font=2)
text(locator(), labels = c("R2=0.086"), font=2)


# AG
# run linear model 
reg_AG_Ch_CBeGvTrt <- lm(Percent_C_BeG~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_CBeGvTrt)
summary(reg_AG_Ch_CBeGvTrt)
# AIC:  130.3177 

reg_AG_Ch_CBeGvTrt <- aov(Percent_C_BeG~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_CBeGvTrt, "means")

# run second order Linear model 
reg_AG_Ch_CBeGvTrt2 <- glm(Percent_C_BeG~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_CBeGvTrt2)
summary(reg_AG_Ch_CBeGvTrt2)
# AIC: 128.059  *** LOWEST

# predict based on best AIC: 
fakeY_AGCBeG_poly2 <- predict(reg_AG_Ch_CBeGvTrt2, list(Treatment2= fakeX_AGTrt2,
                                                        type="response", se.fit=F))


plot(AG_Ch_Traits$Percent_C_BeG~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
lines(fakeY_AGCBeG_poly2~fakeX_AGTrt2, lwd = 3, col = "blue", lty = 2)
text(locator(), labels = c("p=0.127"), font=2)
text(locator(), labels = c("R2=0.100"), font=2)



# SN
# run linear model 
reg_SN_Ch_CBeGvTrt <- lm(Percent_C_BeG~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_CBeGvTrt)
summary(reg_SN_Ch_CBeGvTrt)
# AIC: 109.7542 *** LOWEST

reg_SN_Ch_CBeGvTrt <- aov(Percent_C_BeG~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_CBeGvTrt, "means")

# run second order Linear model 
reg_SN_Ch_CBeGvTrt2 <- glm(Percent_C_BeG ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_CBeGvTrt2)
summary(reg_SN_Ch_CBeGvTrt2)
# AIC: 110.8653 

# plot
plot(SN_Ch_Traits$Percent_C_BeG~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Root Percent C", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_CBeGvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.762"), font=2)
text(locator(), labels = c("R2=-0.041"), font=2)





############ F_Leaf_Stage #############

#put into a 5-panel
par(mfrow = c(2,3))

# DO 
# run linear model 
reg_DO_Ch_FLeafvTrt <- lm(F_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
summary(reg_DO_Ch_FLeafvTrt)
AIC(reg_DO_Ch_FLeafvTrt)
# AIC: 105.2169 ** LOWEST

reg_DO_Ch_FLeafvTrt <- aov(F_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_FLeafvTrt, "means")

# run second order Linear model 
reg_DO_Ch_FLeafvTrt2 <- lm(F_Leaf_Stage ~ poly(Treatment2, degree = 2, raw = T), data=DO_Ch_Traits, family = gaussian()) 
AIC(reg_DO_Ch_FLeafvTrt2)
summary(reg_DO_Ch_FLeafvTrt2)
# AIC: 105.2491 

plot(DO_Ch_Traits$F_Leaf_Stage~DO_Ch_Traits$Treatment2,
     pch=20,
     ylab="Number of Fully Emerged Leaves",
     xlab="Nitrogen Treatment (g)",
     main= "D. oligosantheses")
abline(lm(reg_DO_Ch_FLeafvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.279"), font=2)
text(locator(), labels = c("R2=0.010"), font=2)


# AP 
# run linear model 
reg_AP_Ch_FLeafvTrt <- lm(F_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
summary(reg_AP_Ch_FLeafvTrt)
AIC(reg_AP_Ch_FLeafvTrt)
# AIC: 144.2135 *** LOWEST

reg_AP_Ch_FLeafvTrt <- aov(F_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_FLeafvTrt, "means")

# run second order Linear model 
reg_AP_Ch_FLeafvTrt2 <- lm(F_Leaf_Stage~ poly(Treatment2, degree = 2, raw = T), data=AP_Ch_Traits, family = gaussian()) 
AIC(reg_AP_Ch_FLeafvTrt2)
summary(reg_AP_Ch_FLeafvTrt2)
# AIC: 143.0487 

plot(AP_Ch_Traits$F_Leaf_Stage~AP_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Number of Fully Emerged Leaves", 
     xlab="Nitrogen Treatment (g)",
     main= "A. psilostachia")
abline(lm(reg_AP_Ch_FLeafvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p<0.001 *"), font=2)
text(locator(), labels = c("R2=0.392"), font=2)


# SM
# run linear model
reg_SM_Ch_FLeafvTrt <- lm(F_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
summary(reg_SM_Ch_FLeafvTrt)
AIC(reg_SM_Ch_FLeafvTrt)
# AIC: 144.4151 *** LOWEST

reg_SM_Ch_FLeafvTrt <- aov(F_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_FLeafvTrt, "means")

# run second order Linear model 
reg_SM_Ch_FLeafvTrt2 <- glm(F_Leaf_Stage ~ poly(Treatment2, degree = 2, raw = T), data=SM_Ch_Traits, family = gaussian()) 
AIC(reg_SM_Ch_FLeafvTrt2)
summary(reg_SM_Ch_FLeafvTrt2)
# AIC: 145.9484 

plot(SM_Ch_Traits$F_Leaf_Stage~SM_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Number of Fully Emerged Leaves", 
     xlab="Nitrogen Treatment (g)",
     main= "S. missouriensis")
abline(lm(reg_SM_Ch_FLeafvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.894"), font=2)
text(locator(), labels = c("R2=-0.047"), font=2)


# AG
# run linear model 
reg_AG_Ch_FLeafvTrt <- lm(F_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_FLeafvTrt)
summary(reg_AG_Ch_FLeafvTrt)
# AIC:  117.7429  *** LOWEST

reg_AG_Ch_FLeafvTrt <- aov(F_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_FLeafvTrt, "means")

# run second order Linear model 
reg_AG_Ch_FLeafvTrt2 <- glm(F_Leaf_Stage~poly(Treatment2, degree = 2, raw = T), data=AG_Ch_Traits, family = gaussian()) 
AIC(reg_AG_Ch_FLeafvTrt2)
summary(reg_AG_Ch_FLeafvTrt2)
# AIC: 119.7193 

plot(AG_Ch_Traits$F_Leaf_Stage~AG_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Number of Fully Emerged Leaves", 
     xlab="Nitrogen Treatment (g)",
     main= "A. gerardii")
abline(lm(reg_AG_Ch_FLeafvTrt), lwd=3, col = "blue")
text(locator(), labels = c("p=0.037 *"), font=2)
text(locator(), labels = c("R2=0.146"), font=2)



# SN
# run linear model 
reg_SN_Ch_FLeafvTrt <- lm(F_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_FLeafvTrt)
summary(reg_SN_Ch_FLeafvTrt)
# AIC: 92.16362 *** LOWEST

reg_SN_Ch_FLeafvTrt <- aov(F_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_FLeafvTrt, "means")

# run second order Linear model 
reg_SN_Ch_FLeafvTrt2 <- glm(F_Leaf_Stage ~ poly(Treatment2, degree = 2, raw = T), data=SN_Ch_Traits, family = gaussian()) 
AIC(reg_SN_Ch_FLeafvTrt2)
summary(reg_SN_Ch_FLeafvTrt2)
# AIC: 92.72516 

# plot
plot(SN_Ch_Traits$F_Leaf_Stage~SN_Ch_Traits$Treatment2,
     pch=20, 
     ylab="Number of Fully Emerged Leaves", 
     xlab="Nitrogen Treatment (g)",
     main= "S. nutans")
abline(lm(reg_SN_Ch_FLeafvTrt), lwd=3, col = "blue", lty=2)
text(locator(), labels = c("p=0.358"), font=2)
text(locator(), labels = c("R2=-0.005"), font=2)






############ Plant_Area #############

# DO
reg_DO_Ch_AreavTrt <- lm(`Plant_Area_cm^2`~Treatment2+(1/Block), data = DO_Ch_Traits)
AIC(reg_DO_Ch_AreavTrt)
summary(reg_DO_Ch_AreavTrt)


reg_DO_Ch_AreavTrt <- aov(`Plant_Area_cm^2`~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_AreavTrt, "means")


#AP 
reg_AP_Ch_AreavTrt <- lm(`Plant_Area_cm^2`~Treatment2+(1/Block), data = AP_Ch_Traits)
AIC(reg_AP_Ch_AreavTrt)
summary(reg_AP_Ch_AreavTrt)


reg_AP_Ch_AreavTrt <- aov(`Plant_Area_cm^2`~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_AreavTrt, "means")

#SM
reg_SM_Ch_AreavTrt <- lm(`Plant_Area_cm^2`~Treatment2+(1/Block), data = SM_Ch_Traits)
AIC(reg_SM_Ch_AreavTrt)
summary(reg_SM_Ch_AreavTrt)


reg_SM_Ch_AreavTrt <- aov(`Plant_Area_cm^2`~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_AreavTrt, "means")

#AG
reg_AG_Ch_AreavTrt <- lm(`Plant_Area_cm^2`~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_AreavTrt)
summary(reg_AG_Ch_AreavTrt)


reg_AG_Ch_AreavTrt <- aov(`Plant_Area_cm^2`~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_AreavTrt, "means")

#SN
reg_SN_Ch_AreavTrt <- lm(`Plant_Area_cm^2`~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_AreavTrt)
summary(reg_SN_Ch_AreavTrt)


reg_SN_Ch_AreavTrt <- aov(`Plant_Area_cm^2`~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_AreavTrt, "means")





############ E_Leaf_Stage #############

# DO
reg_DO_Ch_ELeafvTrt <- lm(E_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
AIC(reg_DO_Ch_ELeafvTrt)
summary(reg_DO_Ch_ELeafvTrt)

reg_DO_Ch_ELeafvTrt <- aov(E_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_ELeafvTrt, "means")


#AP 
reg_AP_Ch_ELeafvTrt <- lm(E_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
AIC(reg_AP_Ch_ELeafvTrt)
summary(reg_AP_Ch_ELeafvTrt)


reg_AP_Ch_ELeafvTrt <- aov(E_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_ELeafvTrt, "means")

#SM
reg_SM_Ch_ELeafvTrt <- lm(E_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
AIC(reg_SM_Ch_ELeafvTrt)
summary(reg_SM_Ch_ELeafvTrt)


reg_SM_Ch_ELeafvTrt <- aov(E_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_ELeafvTrt, "means")

#AG
reg_AG_Ch_ELeafvTrt <- lm(E_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_ELeafvTrt)
summary(reg_AG_Ch_ELeafvTrt)


reg_AG_Ch_ELeafvTrt <- aov(E_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_ELeafvTrt, "means")

#SN
reg_SN_Ch_ELeafvTrt <- lm(E_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_ELeafvTrt)
summary(reg_SN_Ch_ELeafvTrt)


reg_SN_Ch_ELeafvTrt <- aov(E_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_ELeafvTrt, "means")


############ S_Leaf_Stage #############

# DO
reg_DO_Ch_SLeafvTrt <- lm(S_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
AIC(reg_DO_Ch_SLeafvTrt)
summary(reg_DO_Ch_SLeafvTrt)

reg_DO_Ch_SLeafvTrt <- aov(S_Leaf_Stage~Treatment2+(1/Block), data = DO_Ch_Traits)
model.tables(reg_DO_Ch_SLeafvTrt, "means")


#AP 
reg_AP_Ch_SLeafvTrt <- lm(S_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
AIC(reg_AP_Ch_SLeafvTrt)
summary(reg_AP_Ch_SLeafvTrt)


reg_AP_Ch_SLeafvTrt <- aov(S_Leaf_Stage~Treatment2+(1/Block), data = AP_Ch_Traits)
model.tables(reg_AP_Ch_SLeafvTrt, "means")

#SM
reg_SM_Ch_SLeafvTrt <- lm(S_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
AIC(reg_SM_Ch_SLeafvTrt)
summary(reg_SM_Ch_SLeafvTrt)


reg_SM_Ch_SLeafvTrt <- aov(S_Leaf_Stage~Treatment2+(1/Block), data = SM_Ch_Traits)
model.tables(reg_SM_Ch_SLeafvTrt, "means")

#AG
reg_AG_Ch_SLeafvTrt <- lm(S_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
AIC(reg_AG_Ch_SLeafvTrt)
summary(reg_AG_Ch_SLeafvTrt)


reg_AG_Ch_SLeafvTrt <- aov(S_Leaf_Stage~Treatment2+(1/Block), data = AG_Ch_Traits)
model.tables(reg_AG_Ch_SLeafvTrt, "means")

#SN
reg_SN_Ch_SLeafvTrt <- lm(S_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
AIC(reg_SN_Ch_SLeafvTrt)
summary(reg_SN_Ch_SLeafvTrt)


reg_SN_Ch_SLeafvTrt <- aov(S_Leaf_Stage~Treatment2+(1/Block), data = SN_Ch_Traits)
model.tables(reg_SN_Ch_SLeafvTrt, "means")








