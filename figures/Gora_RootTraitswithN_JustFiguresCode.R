#### TRootTraitswithN Figures Code ####
###  Gora_RootTraitswithN_JustFiguresCode.R
###  by Sarah Gora
###  Date created: May 1, 2022

# wd
setwd("~/Users/lucie/Desktop/Thesis_Data/R Code")


#####################################################################
###################### PCA FIGURE CODE ##############################
#####################################################################


##### Packages
library(readr)
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)


# Load Data
# TRAIT DROP MODEL
library(readxl)
All_Traits_O_DroppedTraits <- read_excel("~/Desktop/All_Traits_O_DroppedTraits.xlsx")
View(All_Traits_O_DroppedTraits) 

# Subset out TRAIT DROP model, data by species 
DO_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Dichanthelium_oligosanthes")
AP_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Ambrosia_psilostachya")
SM_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Solidago_missouriensis")
AG_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Andropogon_gerardii")
SN_Traits_O_Drop <- subset(All_Traits_O_DroppedTraits, Species=="Sorghastrum_nutans")

#### Subset out N0N10 for each species Ordination data
DO_N0N10_Traits_O_Drop <- subset(DO_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
AP_N0N10_Traits_O_Drop <- subset(AP_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
SM_N0N10_Traits_O_Drop <- subset(SM_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
AG_N0N10_Traits_O_Drop <- subset(AG_Traits_O_Drop, Treatment2 == "0"| Treatment2== "10")
SN_N0N10_Traits_O_Drop <- subset(SN_Traits_O_Drop,Treatment2 == "0"| Treatment2== "10")

#### Subset out Change for each species Ordination data
DO_Ch_Traits_O_Drop <- subset(DO_Traits_O_Drop, Experiment=="Change")
AP_Ch_Traits_O_Drop <- subset(AP_Traits_O_Drop, Experiment=="Change")
SM_Ch_Traits_O_Drop <- subset(SM_Traits_O_Drop, Experiment=="Change")
AG_Ch_Traits_O_Drop <- subset(AG_Traits_O_Drop, Experiment=="Change")
SN_Ch_Traits_O_Drop <- subset(SN_Traits_O_Drop, Experiment=="Change")






# PCA code 
# set N0N10 color 
cols= c("#769370", "#BDB2A7")

#set Change color
cols4 = c( "#769370", "#6E687E", "#F1C646", "#BDB2A7")
trtcol = c( "#769370", "#6E687E", "#F1C646", "#BDB2A7")



par(mfrow=c(1, 2))

# DO, N0 N10
Rip_DO_Traits <- subset(DO_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)
Rip_DO_Traits_PCA <- prcomp(Rip_DO_Traits, scale = TRUE) 
s1 <- summary(Rip_DO_Traits_PCA)
Rip_DO_Traits_RDA <- rda(Rip_DO_Traits, scale = TRUE)

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s1$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s1$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "10g"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.076"))




# DO, Change
Rip_DO_ChTraits <- subset(DO_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_DO_ChTraits_PCA <- prcomp(Rip_DO_ChTraits, scale = TRUE)
s2 <- summary(Rip_DO_ChTraits_PCA)
Rip_DO_ChTraits_RDA <- rda(Rip_DO_ChTraits, scale = TRUE)

p <- ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s2$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s2$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes", 
              font.main=4)
ordispider(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "2.5g", "10g", "20g"), fill =trtcol, bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.192"))





par(mfrow=c(1, 2))

# AP N0N10
Rip_AP_Traits <- subset(AP_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)
Rip_AP_Traits_PCA <- prcomp(Rip_AP_Traits, scale = TRUE) 
s3 <- summary(Rip_AP_Traits_PCA)
Rip_AP_Traits_RDA <- rda(Rip_AP_Traits, scale = TRUE)
p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s3$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s3$importance[5]*100, 1), "%)", sep = ""),
              main= "A. psilostachya", 
              font.main=4)
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "10g"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.304"))


## AP Change
Rip_AP_ChTraits <- subset(AP_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AP_ChTraits_PCA <- prcomp(Rip_AP_ChTraits, scale = TRUE)
s4 <- summary(Rip_AP_ChTraits_PCA)
Rip_AP_ChTraits_RDA <- rda(Rip_AP_ChTraits, scale = TRUE)

p <- ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s4$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s4$importance[5]*100, 1), "%)", sep = ""),
              main= "A. psilostachya", 
              font.main=4)
ordispider(p, groups = as.factor(AP_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.8)
legend("topleft", inset = 0.01, legend = c("0g", "2.5g", "10g", "20g"), fill =trtcol, bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.121"))






par(mfrow=c(1, 2))

# SM, N0N10
Rip_SM_Traits <- subset(SM_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)
Rip_SM_Traits_PCA <- prcomp(Rip_SM_Traits, scale = TRUE) 
s5 <- summary(Rip_SM_Traits_PCA)
Rip_SM_Traits_RDA <- rda(Rip_SM_Traits, scale = TRUE)
p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s5$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s5$importance[5]*100, 1), "%)", sep = ""),
              main= "S. missouriensis", 
              font.main=4)
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.8)
legend("topleft", inset = 0.01, legend = c("0g", "10g"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.137"))


## SM Change
Rip_SM_ChTraits <- subset(SM_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SM_ChTraits_PCA <- prcomp(Rip_SM_ChTraits, scale = TRUE)
s6 <- summary(Rip_SM_ChTraits_PCA)
Rip_SM_ChTraits_RDA <- rda(Rip_SM_ChTraits, scale = TRUE)

p <- ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s6$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s6$importance[5]*100, 1), "%)", sep = ""),
              main= "S. missouriensis", 
              font.main=4)
ordispider(p, groups = as.factor(SM_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "2.5g", "10g", "20g"), fill =trtcol, bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.742"))




par(mfrow=c(1, 2))

# AG, N0N10
Rip_AG_Traits <- subset(AG_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)
Rip_AG_Traits_PCA <- prcomp(Rip_AG_Traits, scale = TRUE) 
s7 <- summary(Rip_AG_Traits_PCA)
Rip_AG_Traits_RDA <- rda(Rip_AG_Traits, scale = TRUE)
p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s7$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s7$importance[5]*100, 1), "%)", sep = ""),
              main= "A. gerardii", 
              font.main=4)
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "10g"), fill =c("#BDB2A7", "#769370"), bg = "white" , title = "N Trt")
text(locator(), labels = c("p<0.001 *"), font=2)


## AG Change
Rip_AG_ChTraits <- subset(AG_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_AG_ChTraits_PCA <- prcomp(Rip_AG_ChTraits, scale = TRUE)
s8 <- summary(Rip_SM_ChTraits_PCA)
Rip_AG_ChTraits_RDA <- rda(Rip_AG_ChTraits, scale = TRUE)

p <- ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s8$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s8$importance[5]*100, 1), "%)", sep = ""),
              main= "A. gerardii", 
              font.main=4)
ordispider(p, groups = as.factor(AG_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "2.5g", "10g", "20g"), fill =trtcol, bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.043 *"), font=2)





par(mfrow=c(1, 2))

# SN, N0N10
Rip_SN_Traits <- subset(SN_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)
Rip_SN_Traits_PCA <- prcomp(Rip_SN_Traits, scale = TRUE) 
s9 <- summary(Rip_SN_Traits_PCA)
Rip_SN_Traits_RDA <- rda(Rip_SN_Traits, scale = TRUE)
p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s9$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s9$importance[5]*100, 1), "%)", sep = ""),
              main= "S. nutans", 
              font.main=4)
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.8)
legend("topleft", inset = 0.01, legend = c("0g", "10g"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.312"))


## SN Change
Rip_SN_ChTraits <- subset(SN_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)
Rip_SN_ChTraits_PCA <- prcomp(Rip_SN_ChTraits, scale = TRUE)
s10 <- summary(Rip_SN_ChTraits_PCA)
Rip_SN_ChTraits_RDA <- rda(Rip_SN_ChTraits, scale = TRUE)

p <- ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s10$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s10$importance[5]*100, 1), "%)", sep = ""),
              main= "S. nutans", 
              font.main=4)
ordispider(p, groups = as.factor(SN_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.8)
legend("topright", inset = 0.01, legend = c("0g", "2.5g", "10g", "20g"), fill =trtcol, bg = "white", title = "N Trt")
text(locator(), labels = c("p=0.258"))






