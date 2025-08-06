#### RootTraitswithN Results Analysis-Ordinations ####
###  Gora_RootTraitswithN-Results-Analysis_Ordinations.R
###  by Sarah Gora
###  Date created: Jan 25, 2021

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

#install packages
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

#POST HOC TEST: pair-wise test
install.packages("devtools")
library(devtools)
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis", force = TRUE)
library(pairwiseAdonis)


# took out all data ROWS with an NA ...???? 
# ask abt remove all rows or replaces NAs w zeros?
# All-Traits Data for Ordinations

All_Traits_O <- read_csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_Datasheet_O2.csv")


# Eliminated Area from this model 
All_Traits_O2 <- read_csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_elimArea_Datasheet_O2 - All_Traits_elimArea_Datasheet_O2.csv")


#### Filter out Control, 10g N for each species Ordination data
DO_N0N10_Traits_O <- All_Traits_O %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits_O <- All_Traits_O %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits_O <- All_Traits_O %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits_O <- All_Traits_O %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits_O <- All_Traits_O %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")

#### Filter out Change for each species Ordination data
DO_Ch_Traits_O <- All_Traits_O %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")
AP_Ch_Traits_O <- All_Traits_O %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Experiment=="Change")
SM_Ch_Traits_O <- All_Traits_O %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Experiment=="Change")
SN_Ch_Traits_O <- All_Traits_O %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Experiment=="Change")
AG_Ch_Traits_O <- All_Traits_O %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Experiment=="Change")

#### Ordinations #####

## 5 ordinations per data set, N0N10 data and Change data
## 1 ordination plot per species 

# environmental/ habitat variables - perform a PCA, right?





# TRAIT DROP MODEL
install.packages("readxl")
library(readxl)
All_Traits_O_DroppedTraits <- read_excel("~/Desktop/All_Traits_O_DroppedTraits.xlsx")
View(All_Traits_O_DroppedTraits) 

# Note: 09/30/2022
# File was not opening so I read it in as a csv
All_Traits_O_DroppedTraits <- read_csv("~/Desktop/All_Traits_O_DroppedTraits.csv")
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







################# Ordination for N10N10: DO ##################
#### Select the columns of traits you want from the N10,N0 Trait Data for DO
Rip_DO_Traits <- subset(DO_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)

# Note: 09/30/2022
# select the Aboveground Traits you want 
Rip_DO_AbG_Traits <- DO_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)




# check out data
head(Rip_DO_Traits) 
View(Rip_DO_Traits)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_DO_Traits_PCA <- prcomp(Rip_DO_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s <- summary(Rip_DO_Traits_PCA)

# Note: 09/30/2022
Rip_DO_AbG_Traits_PCA <- prcomp(Rip_DO_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s <- summary(Rip_DO_AbG_Traits_PCA)

#plot how much of the variance is explained by each component 
# top 5 axes to consider
plot(Rip_DO_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in DO")

#view sample scores
head(Rip_DO_Traits_PCA$x)



##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_DO_Traits_RDA <- rda(Rip_DO_Traits, scale = TRUE)
ordiplot(Rip_DO_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in DO")

par(mfrow=c(1, 1))

# Note: 09/30/2022
Rip_DO_AbG_Traits_RDA <- rda(Rip_DO_AbG_Traits, scale = TRUE)
ordiplot(Rip_DO_AbG_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in DO for AbG traits only")


##### Ordiplot:
counts <- colSums(Rip_DO_Traits) # compute 
ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for DO") # blank plot space
orditorp(Rip_DO_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_DO_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19, main = "Ordiplot for DO")

##### Group by category plot:
#color by N0 and N10
cols= c("#769370", "#BDB2A7")

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.001"), font=2)



# Note: 09/30/2022, AbG traits
par(bg = "#FFFBC8")
p <- ordiplot(Rip_DO_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")
par(bg = "white")
### wdo the stats too!!!

biplot (Rip_DO_AbG_Traits_RDA, display = 'species', scaling = 1, main= "D. oligosanthes, Abg Traits, N0N10")

##### Elipses plot:

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), display = 'sites', 
              main= "Ordiellipse by Treatment for DO", 
              las=1,
              xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""))
ordiellipse(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")




################# Ordination for N10N10: AP #################
#### Select the columns of traits you want from the N10,N0 Trait Data for AP
Rip_AP_Traits <- subset(AP_N0N10_Traits_O_Drop, select =SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_AP_Traits_PCA <- prcomp(Rip_AP_Traits, scale = TRUE) # scale=TRUE to use correlation matrix

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))

plot (Rip_AP_Traits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance in AP")

#view sample scores
head(Rip_AP_Traits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_AP_Traits_RDA <- rda(Rip_AP_Traits, scale = TRUE)
ordiplot(Rip_AP_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in AP")

##### Ordiplot:
counts <- colSums(Rip_AP_Traits) # compute 
ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for AP") # blank plot space
orditorp(Rip_AP_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_AP_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
#cols = c(sample(colours(), 2)) # create a vector of 2 random colors
cols= c("#769370", "#BDB2A7")

p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for AP, grouped by Treatment")
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

##### Elipses plot:
p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for AP")
ordiellipse(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")




########### Ordination for N10N10: SM #################
#### Select the columns of traits you want from the N10,N0 Trait Data for SM
Rip_SM_Traits <- subset(SM_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_SM_Traits_PCA <- prcomp(Rip_SM_Traits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
par(mfrow=c(2, 3))
par(mfrow=c(1, 1))

plot (Rip_SM_Traits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance in SM")

#view sample scores
head(Rip_SM_Traits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_SM_Traits_RDA <- rda(Rip_SM_Traits, scale = TRUE)
ordiplot(Rip_SM_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in SM")

##### Ordiplot:
counts <- colSums(Rip_SM_Traits) # compute 
ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for SM") # blank plot space
orditorp(Rip_SM_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_SM_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
#cols = c(sample(colours(), 2)) # create a vector of 2 random colors
cols= c("#769370", "#BDB2A7")

p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for SM, grouped by Treatment")
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

##### Elipses plot:
p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SM")
ordiellipse(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")






########### Ordination for N10N10: AG #################
#### Select the columns of traits you want from the N10,N0 Trait Data for AG
Rip_AG_Traits <- subset(AG_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_AG_Traits_PCA <- prcomp(Rip_AG_Traits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
par(mfrow=c(2, 3))
par(mfrow=c(1, 1))

plot (Rip_AG_Traits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance in AG")

#view sample scores
head(Rip_AG_Traits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_AG_Traits_RDA <- rda(Rip_AG_Traits, scale = TRUE)
ordiplot(Rip_AG_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in AG")

##### Ordiplot:
counts <- colSums(Rip_AG_Traits) # compute 
ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for AG") # blank plot space
orditorp(Rip_AG_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_AG_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
# cols = c(sample(colours(), 2)) # create a vector of 2 random colors
cols= c("#769370", "#BDB2A7")

p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for AG, grouped by Treatment")
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), lwd=1.5, col = cols)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

##### Elipses plot:
p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for AG, grouped by Treatment")
ordiellipse(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")




########### Ordination for N10N10: SN #################
#### Select the columns of traits you want from the N10,N0 Trait Data for SN
Rip_SN_Traits <- subset(SN_N0N10_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_SN_Traits_PCA <- prcomp(Rip_SN_Traits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))
plot(Rip_SN_Traits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance in SN")

#view sample scores
head(Rip_SN_Traits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_SN_Traits_RDA <- rda(Rip_SN_Traits, scale = TRUE)
ordiplot(Rip_SN_Traits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var in SN")

##### Ordiplot:
counts <- colSums(Rip_SN_Traits) # compute 
ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for SN") # blank plot space
orditorp(Rip_SN_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_SN_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
# cols = c(sample(colours(), 2)) # create a vector of 2 random colors
cols= c("#769370", "#BDB2A7")

p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for SN, grouped by Treatment")
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

##### Elipses plot:
p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SN")
ordiellipse(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), conf = 0.9, lwd=2.9, col = cols)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")







############# COMBINE ##############
# ordinations, ordispider plots
par(mfrow=c(1, 2))

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), display = 'sites', main = "D. oligosanthes")
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), display = 'sites', main = "A. psilostachia")
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), display = 'sites', main = "S. missouriensis")
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("topleft", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), display = 'sites', main = "A. gerardii")
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white" , title = "Treatment")

p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), display = 'sites', main = "S. nutans")
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")




############# COMBINE ##############
# ordinations, ordiellipse plots
par(mfrow=c(2, 3))

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), display = 'sites', main= "D. oligosanthes")
ordiellipse(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for AP")
ordiellipse(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")


p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SM")
ordiellipse(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for AG, grouped by Treatment")
ordiellipse(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), conf = 0.9, col = cols, lwd=2.9)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")

p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SN")
ordiellipse(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), conf = 0.9, lwd=2.9, col = cols)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white")







################# Ordination for Change: DO ##################
#### Select the columns of traits you want from the Change Trait Data for DO
Rip_DO_ChTraits <- subset(DO_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)

#explore the data structure visually
head(Rip_DO_ChTraits)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_DO_ChTraits_PCA <- prcomp(Rip_DO_ChTraits, scale = TRUE) # scale=TRUE to use correlation matrix

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
par(mfrow=c(1, 1))

plot (Rip_DO_ChTraits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to DO Variance in Change ")

#view sample scores
head(Rip_DO_ChTraits_PCA$x)


##### Basic biplot:
Rip_DO_ChTraits_RDA <- rda(Rip_DO_ChTraits, scale = TRUE)

ordiplot(Rip_DO_ChTraits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var for DO in Change")

##### Ordiplot:
counts <- colSums(Rip_DO_ChTraits) # compute 
ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for DO in Change") # blank plot space
orditorp(Rip_DO_ChTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_DO_ChTraits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
# vector of 4 random colors


# cols= c("#BDB2A7", "#769370")

p <- ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for DO, grouped by Treatment in Change")
ordispider(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)

##### Elipses plot:
p <- ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for DO, by Treatment in Change")
ordiellipse(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)






################# Ordination for Change: AP #################
#### Select the columns of traits you want from the Change Trait Data for AP
Rip_AP_ChTraits <- subset(AP_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_AP_ChTraits_PCA <- prcomp(Rip_AP_ChTraits, scale = TRUE) # scale=TRUE to use correlation matrix

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))

plot (Rip_AP_ChTraits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance for AP in Change")

#view sample scores
head(Rip_AP_ChTraits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_AP_ChTraits_RDA <- rda(Rip_AP_ChTraits, scale = TRUE)
ordiplot(Rip_AP_ChTraits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var for AP in Change")

##### Ordiplot:
counts <- colSums(Rip_AP_ChTraits) # compute 
ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for AP in Change") # blank plot space
orditorp(Rip_AP_ChTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_AP_ChTraits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
#cols= c("#BDB2A7", "#769370")

p <- ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for AP, grouped by Treatment in Change")
ordispider(p, groups = as.factor(AP_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)

##### Elipses plot:
p <- ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for AP, by Treatment in Change")
ordiellipse(p, groups = as.factor(AP_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)






########### Ordination for Change: SM #################
#### Select the columns of traits you want from the N10,N0 Trait Data for SM
Rip_SM_ChTraits <- subset(SM_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_SM_ChTraits_PCA <- prcomp(Rip_SM_ChTraits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))

plot (Rip_SM_ChTraits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance for SM in Change")

#view sample scores
head(Rip_SM_ChTraits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_SM_ChTraits_RDA <- rda(Rip_SM_ChTraits, scale = TRUE)
ordiplot(Rip_SM_ChTraits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var for SM in Change")

##### Ordiplot:
counts <- colSums(Rip_SM_ChTraits) # compute 
ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for SM in Change") # blank plot space
orditorp(Rip_SM_ChTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_SM_ChTraits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
#cols= c("#BDB2A7", "#769370")

p <- ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for SM, grouped by Treatment in Change")
ordispider(p, groups = as.factor(SM_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)

##### Elipses plot:
p <- ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for SM, by Treatment in Change")
ordiellipse(p, groups = as.factor(SM_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)






########### Ordination for Change: AG #################
#### Select the columns of traits you want from the N10,N0 Trait Data for AG
Rip_AG_ChTraits <- subset(AG_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_AG_ChTraits_RDA <- prcomp(Rip_AG_ChTraits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))

plot (Rip_AG_ChTraits_RDA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance for SM in Change")

#view sample scores
head(Rip_AG_ChTraits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_AG_ChTraits_RDA <- rda(Rip_AG_ChTraits, scale = TRUE)
ordiplot(Rip_AG_ChTraits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var for AG in Change")

##### Ordiplot:
counts <- colSums(Rip_AG_ChTraits) # compute 
ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for AG in Change") # blank plot space
orditorp(Rip_AG_ChTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_AG_ChTraits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
#cols= c("#BDB2A7", "#769370")

p <- ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for AG, grouped by Treatment in Change")
ordispider(p, groups = as.factor(AG_Ch_Traits_O_Drop$Treatment), lwd=1.5, col = cols4)

##### Elipses plot:
p <- ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for AG, grouped by Treatment in Change")
ordiellipse(p, groups = as.factor(AG_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)




########### Ordination for Change: SN #################
#### Select the columns of traits you want from the N10,N0 Trait Data for SN
Rip_SN_ChTraits <- subset(SN_Ch_Traits_O_Drop, select = SLA:PercentC_BeG)

#### perform PCA
# scale=TRUE to use correlation matrix
Rip_SN_ChTraits_PCA <- prcomp(Rip_SN_ChTraits, scale = TRUE) 

#plot how much of the variance is explained by each component 
# top 5 axes to consider 
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))
plot(Rip_SN_ChTraits_PCA , npcs = 5, col="#ffff99", main = "Top 5 Axes Contrib. to Variance for SN in Change")

#view sample scores
head(Rip_SN_ChTraits_PCA$x)


##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes

Rip_SN_ChTraits_RDA <- rda(Rip_SN_ChTraits, scale = TRUE)
ordiplot(Rip_SN_ChTraits_RDA, 
         choices = c(1, 2), 
         type = 'text', 
         scaling = 2, 
         main = "Biplot Scores plus Raw Var for SN in Change")

##### Ordiplot:
counts <- colSums(Rip_SN_ChTraits) # compute 
ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for SN in Change") # blank plot space
orditorp(Rip_SN_ChTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_SN_ChTraits_RDA, display='species', priority = counts, col = 'red', pch = 19)

##### Group by category plot:
# cols = c(sample(colours(), 2)) # create a vector of 2 random colors
# cols= c("#BDB2A7", "#769370")

p <- ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "Ordiplot for SN, grouped by Treatment in Change")
ordispider(p, groups = as.factor(SN_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)

##### Elipses plot:
p <- ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse for SN, by Treatment for Change")
ordiellipse(p, groups = as.factor(SN_Ch_Traits_O_Drop$Treatment), conf = 0.9, lwd=2.9, col = cols4)












# colors checked 
cols4 = c( "#769370", "#6E687E", "#F1C646", "#BDB2A7")
trtcol = c( "#769370", "#6E687E", "#F1C646", "#BDB2A7")

legend("bottomright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")








############# COMBINE ##############
# ordinations, ordispider plots
par(mfrow=c(1, 2))

p <- ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), display = 'sites', main = "D. oligosanthes")
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_AP_Traits_RDA, choices = c(1, 2), display = 'sites', main = "A. psilostachia")
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_SM_Traits_RDA, choices = c(1, 2), display = 'sites', main = "S. missouriensis")
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("topleft", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")

p <- ordiplot(Rip_AG_Traits_RDA, choices = c(1, 2), display = 'sites', main = "A. gerardii")
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white" , title = "Treatment")

p <- ordiplot(Rip_SN_Traits_RDA, choices = c(1, 2), display = 'sites', main = "S. nutans")
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment),label = FALSE, col = cols, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "10g N"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")







############### COMBINE ###############
#par(mfrow=c(2, 3))
#par(mfrow=c(1, 1))

p <- ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "D. oligosanthes")
ordispider(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), label = FALSE, col = cols4, lwd=1.5)
legend("bottomright", inset = 0.01, legend = c("0g N", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "A. psilostachia")
ordispider(p, groups = as.factor(AP_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)
legend("topleft", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "S. missouriensis")
ordispider(p, groups = as.factor(SM_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "A. gerardii")
ordispider(p, groups = as.factor(AG_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), display = 'sites',  main = "S. nutans")
ordispider(p, groups = as.factor(SN_Ch_Traits_O_Drop$Treatment), col = cols4, lwd=1.5)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")











############# COMBINE ##############
# ordinations, ordiellipse plots Change
par(mfrow=c(2, 3))

p <- ordiplot(Rip_DO_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by T")
ordiellipse(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_AP_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for AP")
ordiellipse(p, groups = as.factor(AP_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)
legend("topleft", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_SM_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SM")
ordiellipse(p, groups = as.factor(SM_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_AG_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for AG")
ordiellipse(p, groups = as.factor(AG_Ch_Traits_O_Drop$Treatment), conf = 0.9, col = cols4, lwd=2.9)
legend("topright", inset = 0.01, legend = c("0g N", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")

p <- ordiplot(Rip_SN_ChTraits_RDA, choices = c(1, 2), display = 'sites', main= "Ordiellipse by Treatment for SN")
ordiellipse(p, groups = as.factor(SN_Ch_Traits_O_Drop$Treatment), conf = 0.9, lwd=2.9, col = cols4)
legend("topright", inset = 0.01, legend = c("0g N", "2.5g N", "10g N", "20g N"), fill = trtcol, bg = "white", title = "Treatment")









############### COMBINE ###############
par(mfrow=c(2, 3))

plot(Rip_DO_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in DO")
plot(Rip_AP_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in AP")
plot(Rip_SM_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in SM")
plot(Rip_AG_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in AG")
plot(Rip_SN_Traits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in SN")





############### COMBINE ###############
par(mfrow=c(2, 3))

plot(Rip_DO_ChTraits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in DO")
plot(Rip_AP_ChTraits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in AP")
plot(Rip_SM_ChTraits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in SM")
plot(Rip_AG_ChTraits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in AG")
plot(Rip_SN_ChTraits_PCA , npcs = 5, col="grey", main = "Top 5 Axes Contrib. to Variance in SN")










# perMANOVA, initial run

# run on each species (5) for both subsets of data
# All-traits w N0N10
# Change-traits 


# This is just for DO in All-Trait data

# gives you dissimilarity matrix
#dissim <- vegdist(Rip_DO_Traits, method = "bray") 
#y.anosim <- anosim(dissim, DO_N0N10_Traits_O$Treatment) # ANOSIM
#summary(y.anosim)

#run PERMANOVA
# ** use this one
adonis(Rip_DO_Traits~Treatment, data = DO_N0N10_Traits_O, permutations = 1000, method = 'bray') 
# p= 0.000999 




### PERMANOVA & SIMPER intial test run ###

#SIMPER 
# simper analysis calculates the contribution of each trait (%) to the dissimilarity between each two groups
# calculated from the Bray-Curtiss dissimilarity matrix


#Run a SIMPER test comparing data from the Environment_Matrix, to data from the Species_Matrix grouping by "Watershed"

#Rip_DO_Traits <- subset(DO_N0N10_Traits_O, select = SLA:F_Leaf_Stage)


################## DO Ordinations #######################
#run PERMANOVA, All-traits (N0N10) Dataset for DO
adonis(Rip_DO_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.002997, * significant 

# Traits Drop Model
# p= 0.0759
# R2= 0.06785

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_DO_Traits ~ Treatment, permutations = 1000, data = DO_N0N10_Traits_O_Drop, method = 'bray'))
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.002661339, * significant 


###### elim Plant_Area
# p= 0.02198, * significant 


#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_DO_Traits,factors=DO_N0N10_Traits_O_Drop$Treatment)
pair.mod
# adjusted p= 0.002 *



## Traits DROP MODEL
# R2=0.0678
# p=0.091

# All-traits (N0N10) Dataset for DO
SIMPER <- with(DO_N0N10_Traits_O_Drop,simper(Rip_DO_Traits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3:          ###### elim Plant_Area
# Plant Area      Plant_Height_cm 
# SLA             Leaf_Number
# SRL             SRL



## Traits DROP MODEL
# 1. SLA 
# 2. SRL
# 3. F Leaf Stage


#run PERMANOVA Change-traits (N0,N2.5,N10,N20) dataset for DO
adonis(Rip_DO_ChTraits~Treatment, data = DO_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.2537, not significant 


## Traits DROP MODEL
# R2=0.17965
# p=0.1918

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_DO_ChTraits ~ Treatment, permutations = 1000, data = DO_Ch_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.2328442, not significant


#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_DO_ChTraits,factors= DO_Ch_Traits_O_Drop$Treatment)
pair.mod
# Contol v 2.5: p= 0.414
# Contol v 10: p= 1.000
# Contol v 20: p= 1.000


###### elim Plant_Area



# Change-traits (N0,N2.5,N10,N20) dataset for DO
SIMPER <- with(DO_Ch_Traits_O_Drop,simper(Rip_DO_ChTraits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3: Control v 2.5      ###### elim Plant_Area
# Plant Area                      SLA
# SLA                             SRL
# SRL                             Plant_Height_cm 

# top 3: Control v 10      ###### elim Plant_Area
# Plant Area                      SLA
# SLA                             Plant_Height_cm 
# SRL                             SRL

# top 3: Control v 20      ###### elim Plant_Area
# Plant Area                      SLA
# SLA                             SRL
# SRL                             Plant_Height_cm 


## Traits DROP MODEL
# top 3: Control v 2.5     
# SLA, SRL, Root %C 

# top 3: Control v 10      
# SLA, SRL, Root %C 

# top 3: Control v 20 
# SLA, SRL, Root %C 



################## AP Ordinations #######################
#run PERMANOVA, All-traits (N0N10) Dataset for AP
adonis(Rip_AP_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.01399, * significant 

# TRAITS DROP MODEL
# p=0.3037
# R2= 0.03416

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AP_Traits ~ Treatment, permutations = 1000, data = AP_N0N10_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.01449451, * significant

#Run Post-Hoc


###### elim Plant_Area
# p= 0.04296, * significant 


# All-traits (N0N10) Dataset for AP
SIMPER <- with(AP_N0N10_Traits_O_Drop,simper(Rip_AP_Traits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3:         ###### elim Plant_Area
# Plant Area            SLA
# SLA                   Leaf Number
# Leaf Number           Plant_Height_cm

## Traits DROP MODEL
# SLA
# SRL 
# Fully Dev Leaves


#run PERMANOVA, Change-traits (N0,N2.5,N10,N20) dataset for AP
adonis(Rip_AP_ChTraits~Treatment, data = AP_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.003996, * significant

## Traits DROP MODEL
# p= 0.1209
# R2= 0.22372


# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AP_ChTraits ~ Treatment, permutations = 1000, data = AP_Ch_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.004533467, * significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_AP_ChTraits,factors= AP_Ch_Traits_O_Drop$Treatment)
pair.mod
# Contol v 2.5: p= 1.000
# Contol v 10: p= 0.462
# Contol v 20: p= 0.030 *

###### elim Plant_Area
# p= 0.004995, * significant


# Change-traits (N0,N2.5,N10,N20) dataset for AP
SIMPER <- with(AP_Ch_Traits_O_Drop,simper(Rip_AP_ChTraits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3: Control v 2.5       ###### elim Plant_Area
# Plant Area                 SLA
# SLA                        Leaf_Number
# SRL                        Plant_Height_cm

# top 3: Control v 10        ###### elim Plant_Area
# Plant Area                 SLA
# SLA                        Leaf_Number
# SRL                        SRL  

# top 3: Control v 20       ###### elim Plant_Area
# Plant Area                Leaf_Number
# Leaf Number               SLA
# SLA                       Plant_Height_cm


## Traits DROP MODEL
## SLA,  SRL, F Leaf


################## SM Ordinations #######################
#run PERMANOVA, All-traits (N0N10) Dataset for SM
adonis(Rip_SM_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.6953, not significant

# TRAITS DROP MODEL
# P= 0.1369
# R2=0.05787

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SM_Traits ~ Treatment, permutations = 1000, data = SM_N0N10_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.7035375, not significant

#Run Post-Hoc


###### elim Plant_Area
# p= 0.1718, not significant


# All-traits (N0N10) Dataset for SM
SIMPER <- with(SM_N0N10_Traits_O_Drop,simper(Rip_SM_Traits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3:               ###### elim Plant_Area
# Plant Area           Plant_Height_cm
# Plant_Height_cm      Leaf_Number
# Leaf_Number          F_Leaf_Stage

# TRAIT DROP MODEL
# F_Leaf Stage
# SLA
# SRL

#run PERMANOVA Change-traits (N0,N2.5,N10,N20) dataset for SM
adonis(Rip_SM_ChTraits~Treatment, data = SM_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.3676, not significant

# TRAIT DROP MODEL
# P= 0.7423
# R2= 0.10127

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SM_ChTraits ~ Treatment, permutations = 1000, data = SM_Ch_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.3504456, not significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_SM_ChTraits,factors= SM_Ch_Traits_O_Drop$Treatment)
pair.mod
# Contol v 2.5: p= 1.000
# Contol v 10: p= 1.000
# Contol v 20: p= 0.888

###### elim Plant_Area
# p= 0.6923, not significant


# Change-traits (N0,N2.5,N10,N20) dataset for SM
SIMPER <- with(SM_Ch_Traits_O_Drop,simper(Rip_SM_ChTraits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3: Control v 2.5     ###### elim Plant_Area
# Plant Area               Plant_Height_cm
# Plant_Height_cm          Leaf_Number
# Leaf Number              F_Leaf_Stage

# top 3: Control v 10      ###### elim Plant_Area
# Plant Area               Plant_Height_cm
# Plant_Height_cm          Leaf_Number
# Leaf Number              F_Leaf_Stage

# top 3: Control v 20      ###### elim Plant_Area
# Plant Area               Leaf_Number
# Leaf Number              Plant_Height_cm
# Plant_Height_cm          SLA


### TRAIT DROP 
# F Leaf STAGE
# SLA
# Abg Weight 
################## AG Ordinations #######################
#run PERMANOVA, All-traits (N0N10) Dataset for AG
adonis(Rip_AG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.7932, not significant 


# TRAITS DROP 
# p<0.001 **
# R2= 0.17868


# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AG_Traits ~ Treatment, permutations = 1000, data = AG_N0N10_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.7975345, not significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_AG_Traits,factors= AG_N0N10_Traits_O_Drop$Treatment)
pair.mod

###### elim Plant_Area
# p= 0.005994, * significant 


# All-traits (N0N10) Dataset for AG
SIMPER <- with(AG_N0N10_Traits_O_Drop,simper(Rip_AG_Traits,Treatment))
#Print out a summary of the results
summary(SIMPER)       
# top 3:                 ###### elim Plant_Area
# Plant Area             SLA
# SLA                    Plant_Height_cm
# Plant_Height_cm        Leaf_Number


# TRAIT MODEL
# SIMPER: SLA, F leaf stage, Root %C


#run PERMANOVA Change-traits (N0,N2.5,N10,N20) dataset for AG
adonis(Rip_AG_ChTraits~Treatment, data = AG_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
#p= 0.5724, not significant

# TRAIT DROP
# p= 0.043  ***
# p= 0.22402


# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_AG_ChTraits ~ Treatment, permutations = 1000, data = AG_Ch_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.5768691, not significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_AG_ChTraits,factors= AG_Ch_Traits_O_Drop$Treatment)
pair.mod
# Contol v 2.5: p= 1.000
# Contol v 10: p= 1.000
# Contol v 20: p= 1.000

###### elim Plant_Area
# p= 0.1638, not significant

# Change-traits (N0,N2.5,N10,N20) dataset for AG
SIMPER <- with(AG_Ch_Traits_O_Drop,simper(Rip_AG_ChTraits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3: Control v 2.5      ###### elim Plant_Area
# Plant Area                Plant_Height_cm
# Plant_Height_cm           SLA
# SLA                       PercentC_BeG

# top 3: Control v 10      ###### elim Plant_Area
# Plant Area               Plant_Height_cm
# Plant_Height_cm          SLA
# SLA                      Leaf_Number

# top 3: Control v 20      ###### elim Plant_Area
# Plant Area               Plant_Height_cm
# Plant_Height_cm          Leaf_Number
# Leaf_Number              SLA

### TRAIT DROP MODEL
# 0 vs 2.5: SLA, Root %C, SRL
# 0 vs 10: SLA, Root %C, F Leaf 
# 0 vs 20: SLA, Root %C, SRL




################## SN Ordinations #######################
#run PERMANOVA, All-traits (N0N10) Dataset for SN
PERMANOVA <-adonis(Rip_SN_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.06793, not significant

# TRAIT DROP MODEL
# p= 0.3117
# R2= 0.03351


# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SN_Traits ~ Treatment, permutations = 1000, data = SN_N0N10_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.07062937, not significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_SN_Traits,factors= SN_N0N10_Traits_O_Drop$Treatment)
pair.mod

###### elim Plant_Area
# p= 0.03596, * significant


# All-traits (N0N10) Dataset for SN
SIMPER <- with(SN_N0N10_Traits_O_Drop,simper(Rip_SN_Traits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3:               ###### elim Plant_Area
# Plant Area           Plant_Height_cm
# Plant_Height_cm      SLA
# SLA                  Leaf_Number

## TRAIT DROP MODEL
# 0 vs 10: SLA, SRL, F Leaf 





#run PERMANOVA Change-traits (N0,N2.5,N10,N20) dataset for SN
adonis(Rip_SN_ChTraits~Treatment, data = SN_Ch_Traits_O_Drop, permutations = 1000, method = 'bray') 
# p= 0.8362, not significant 

# TRAIT DROP MODEL
# p=0.2577
# R2=0.15854

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_SN_ChTraits ~ Treatment, permutations = 1000, data = SN_Ch_Traits_O, method = 'bray'))

mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# mean p= 0.8442218, not significant

#Run Post-Hoc
pair.mod <- pairwise.adonis(Rip_SN_ChTraits,factors= SN_Ch_Traits_O_Drop$Treatment)
pair.mod
# Contol v 2.5: adj p= 1.000
# Contol v 10: adj p= 1.000
# Contol v 20: adj p= 1.000

###### elim Plant_Area
#p= 0.1289, not significant

# Change-traits (N0,N2.5,N10,N20) dataset for SN
SIMPER <- with(SN_Ch_Traits_O_Drop,simper(Rip_SN_ChTraits,Treatment))
#Print out a summary of the results
summary(SIMPER)
# top 3: Control v 2.5     ###### elim Plant_Area
# Plant Area               Plant_Height_cm
# Plant_Height_cm          SLA
# SLA                      SRL

# top 3: Control v 10     ###### elim Plant_Area
# Plant Area              Plant_Height_cm
# SLA                     SLA
# Plant_Height_cm         SRL

# top 3: Control v 20     ###### elim Plant_Area
# Plant Area              Plant_Height_cm
# Plant_Height_cm         SLA
# SLA                     SRL     


## TRAIT DROP  MODEL
# 0 vs 2.5: SLA, SRL, Root %C
# 0 vs 10: SLA, SRL, Root %C,
# 0 vs 20: SLA, SRL, Root %C





#POST HOC TEST: pair-wise test
install_github("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis", force = TRUE)
library(pairwiseAdonis)

pair.mod <- pairwise.adonis(Rip_DO_Traits,factors=DO_N0N10_Traits_O$Treatment)
pair.mod
# p= 0.001 *
# use for Change to see the pair-wise interactions







#### test for adjusted p-values 



# test with 10! permutations
adonis(Rip_DO_Traits ~ Treatment, permutations = factorial(10), data = DO_N0N10_Traits_O, method = 'bray')

# store 1000 tests with 1000 permutations
tests <- lapply(1:1000, function(i) adonis(Rip_DO_Traits ~ Treatment, permutations = 1000, data = DO_N0N10_Traits_O, method = 'bray'))

# mean p-value
mean(sapply(1:1000, function(i) tests[[i]]$aov.tab$`Pr(>F)`[1]))
# p= 0.002661339 * 





