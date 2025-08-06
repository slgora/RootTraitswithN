#### RootTraitswithN Results Analysis-RDAs Run for ALL Traits ####
###  #### RootTraitswithN Results Analysis-Ordinations ####
###  Gora_RootTraitswithN-Results-Analysis_AllTraitsRDA.R
###  by Sarah Gora
###  Date created: Nov 11, 2023

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
###  by Sarah Gora
###  Date created: Jan 25, 2021

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


#load data
#load all traits data for ordinations with extra traits dropped, removed NAs
library(readr)

# all traits 2 is the latest version - use this: may 2, 2022
All_Traits_O_Renamed <- read.csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_Datasheet_O2.csv")

## Edit the names of traits to be simpler for readability of RDA

## NAMES changes as below:
# Leaf_Thickness_cm  > Leaf_Thickness
# Aboveground_Dry_Weight_g > AbG_Biomas
# Plant_Height_cm > Plant_Height
# Plant_Area > Plant_Vol
# Focal_Root_Weight_g > Focal_Root_Biomass
# Total_BeG_Weight_g  > Total_Root_Biomass
# Root_Diam_Max > Max_Root_Diam
# Root_Diam_Mean > Mean_Root_Diam
# S_Leaf_Stage > LeafStatus_S
# F_Leaf_Stage > LeafStatus_F
# E_Leaf_Stage > LeafStatus_E
# PercentN_AbG > Leaf_PercN
# PercentC_AbG > Leaf_PercC
# PercentN_BeG > Root_PercN
# PercentC_BeG > Root_PercC


# Edit names
colnames(All_Traits_O_Renamed)[11] <- "Leaf_Thickness"
colnames(All_Traits_O_Renamed)[12] <- "AbG_Biomass"
colnames(All_Traits_O_Renamed)[14] <- "Plant_Height"
colnames(All_Traits_O_Renamed)[15] <- "Plant_Vol"
colnames(All_Traits_O_Renamed)[16] <- "Focal_Root_Biomass"
colnames(All_Traits_O_Renamed)[17] <- "Total_Root_Biomass"
colnames(All_Traits_O_Renamed)[19] <- "Max_Root_Diam"
colnames(All_Traits_O_Renamed)[20] <- "Mean_Root_Diam"
colnames(All_Traits_O_Renamed)[26] <- "LeafStatus_S"
colnames(All_Traits_O_Renamed)[21] <- "LeafStatus_F"
colnames(All_Traits_O_Renamed)[27] <- "LeafStatus_E"
colnames(All_Traits_O_Renamed)[22] <- "Leaf_PercN"
colnames(All_Traits_O_Renamed)[23] <- "Leaf_PercC"
colnames(All_Traits_O_Renamed)[24] <- "Root_PercN"
colnames(All_Traits_O_Renamed)[25] <- "Root_PercC"


#### Filter out Control, 10g N for each species
DO_N0N10_Traits_O_Drop <- All_Traits_O_Renamed %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits_O_Drop <- All_Traits_O_Renamed %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits_O_Drop <- All_Traits_O_Renamed %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits_O_Drop <- All_Traits_O_Renamed %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits_O_Drop <- All_Traits_O_Renamed %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")


################# RDA Ordinations for N10N10 ##################
#### Select the columns of traits you want from the N10,N0 Trait Data for all species


# select ALL the Aboveground and Belowground Traits  N0N10
Rip_DO_AllTraits <- DO_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC, SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_AP_AllTraits <- AP_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC, SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_SM_AllTraits <- SM_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC, SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_AG_AllTraits <- AG_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC, SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_SN_AllTraits <- SN_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC, SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)


####### Run Adonis for each Species on All Traits ###################3
# run PERMANOVA, N0N10 ordination data with traits dropped 
# DO - All Traits
adonis(Rip_DO_AllTraits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.08691

# AP - All Traits
adonis(Rip_AP_AllTraits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.2987

# SM - All Traits
adonis(Rip_SM_AllTraits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.1469

# AG - All Traits
adonis(Rip_AG_AllTraits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.001998, * significant

# SN - All Traits
adonis(Rip_SN_AllTraits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.2817

# SCALE - perform an RDA
Rip_DO_AllTraits_RDA <- rda(Rip_DO_AllTraits, scale = TRUE)
Rip_AP_AllTraits_RDA <- rda(Rip_AP_AllTraits, scale = TRUE)
Rip_SM_AllTraits_RDA <- rda(Rip_SM_AllTraits, scale = TRUE)
Rip_AG_AllTraits_RDA <- rda(Rip_AG_AllTraits, scale = TRUE)
Rip_SN_AllTraits_RDA <- rda(Rip_SN_AllTraits, scale = TRUE)



#biplots
biplot (Rip_DO_AllTraits_RDA, display = 'species', scaling = 1, main= "D. oligosanthes")
biplot (Rip_AP_AllTraits_RDA, display = 'species', scaling = 1, main= "A. psilostachya")
biplot (Rip_SM_AllTraits_RDA, display = 'species', scaling = 1, main= "S. missouriensis")
biplot (Rip_AG_AllTraits_RDA, display = 'species', scaling = 1, main= "A. gerardii")
biplot (Rip_SN_AllTraits_RDA, display = 'species', scaling = 1, main= "S. nutans")

par(mfrow=c(2, 3))
par(mfrow=c(1, 1))

text(locator(), labels = c("p= 0.086"), font=2)
text(locator(), labels = c("p= 0.298"), font=2)
text(locator(), labels = c("p= 0.146"), font=2)
text(locator(), labels = c("p= 0.001 *"), font=2)
text(locator(), labels = c("p= 0.281"), font=2)



####### new FACTOEXTRA plots try out #### 

#### perform PCA
# scale=TRUE to use correlation matrix

# All traits summaries N0N10
Rip_DO_AllTraits_PCA <- prcomp(Rip_DO_AllTraits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_AP_AllTraits_PCA <- prcomp(Rip_AP_AllTraits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_SM_AllTraits_PCA <- prcomp(Rip_SM_AllTraits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_AG_AllTraits_PCA <- prcomp(Rip_AG_AllTraits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_SN_AllTraits_PCA <- prcomp(Rip_SN_AllTraits, scale = TRUE) # scale=TRUE to use correlation matrix


###### FACTOEXTRA ########
# Load package
install.packages("factoextra")
library("factoextra")




# DO BiPlots, All Traits N0N10
fviz_pca_biplot(Rip_DO_AllTraits_PCA, 
                repel=TRUE, 
                pointsize=3, 
                geom="point", 
                col.var="red", 
                arrowsize=0.6, 
                labelsize=5, 
                col.ind=DO_N0N10_Traits_O_Drop$Treatment, 
                palette=c("#769370","#BDB2A7"), 
                addEllipses=TRUE, 
                ellipse.type="confidence") + labs(title ="D. oligosanthes")


## Do all plots 


# DO, biplot
a <- fviz_pca_biplot(Rip_DO_AllTraits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
               col.ind=DO_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="D. oligosanthes")
a
# AP, biplot
fviz_pca_biplot(Rip_AP_AllTraits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AP_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. psilostachya")
# SM, biplot
fviz_pca_biplot(Rip_SM_AllTraits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SM_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. missouriensis")
# AG, biplot
fviz_pca_biplot(Rip_AG_AllTraits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AG_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. gerardii")
# SN, biplot
fviz_pca_biplot(Rip_SN_AllTraits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SN_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. nutans")


par(mfrow=c(1, 1))









##### I  dont think this code belw was used ###############

##### Ordiplots II:
counts <- colSums(Rip_DO_AllTraits) # compute 
ordiplot(Rip_DO_AllTraits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for DO") # blank plot space
orditorp(Rip_DO_AllTraits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_DO_AllTraits_RDA, display='species', priority = counts, col = 'red', pch = 19, main = "Ordiplot for DO")

##### Group by category plot:
#color by N0 and N10
cols= c("#769370", "#BDB2A7")

par(mfrow=c(5, 2))
par(mfrow=c(1, 1))


p <- ordiplot(Rip_DO_AllTraits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s1$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s1$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = FALSE, col = cols, lwd=1.5)
legend("topright", inset = 0.01, legend = c("N0", "N10"), fill =c("#BDB2A7", "#769370"), bg = "white", title = "Treatment")


#text(locator(), labels = c("p=0."), font=2)

#########################################################




#### RDA Ordinations for Aboveground & Belowground Separately ######
#### Appendix figure RDAs


# Select Aboveground and Belowground Traits  N0N10
Rip_DO_AbG_Traits <- DO_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC)
Rip_DO_BeG_Traits <- DO_N0N10_Traits_O_Drop %>% select(SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_AP_AbG_Traits <- AP_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC)
Rip_AP_BeG_Traits <- AP_N0N10_Traits_O_Drop %>% select(SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_SM_AbG_Traits <- SM_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC)
Rip_SM_BeG_Traits <- SM_N0N10_Traits_O_Drop %>% select(SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_AG_AbG_Traits <- AG_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC)
Rip_AG_BeG_Traits <- AG_N0N10_Traits_O_Drop %>% select(SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)
Rip_SN_AbG_Traits <- SN_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness, AbG_Biomass, LeafStatus_F, Leaf_PercN, Leaf_PercC)
Rip_SN_BeG_Traits <- SN_N0N10_Traits_O_Drop %>% select(SRL, Mean_Root_Diam, Focal_Root_Biomass, Root_PercN, Root_PercC)


####### Run Adonis for each Species on AbG and BeG Traits ###################3
# run PERMANOVA, N0N10 ordination data with traits dropped 

# DO - AbG & BeG Traits
adonis(Rip_DO_AbG_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.041 *
adonis(Rip_DO_BeG_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.511

# AP - AbG & BeG Traits
adonis(Rip_AP_AbG_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.459
adonis(Rip_AP_BeG_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.051

# SM - AbG & BeG Traits
adonis(Rip_SM_AbG_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.149
adonis(Rip_SM_BeG_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.613

# AG - AbG & BeG Traits
adonis(Rip_AG_AbG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p > 0.001 *
adonis(Rip_AG_BeG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.095

# SN - AbG & BeG Traits
adonis(Rip_SN_AbG_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.251
adonis(Rip_SN_BeG_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
# PERMANOVA Results
# p= 0.371



# SCALE - perform an RDA
Rip_DO_AbG_Traits_RDA <- rda(Rip_DO_AbG_Traits, scale = TRUE)
Rip_DO_BeG_Traits_RDA <- rda(Rip_DO_BeG_Traits, scale = TRUE)

Rip_AP_AbG_Traits_RDA <- rda(Rip_AP_AbG_Traits, scale = TRUE)
Rip_AP_BeG_Traits_RDA <- rda(Rip_AP_BeG_Traits, scale = TRUE)

Rip_SM_AbG_Traits_RDA <- rda(Rip_SM_AbG_Traits, scale = TRUE)
Rip_SM_BeG_Traits_RDA <- rda(Rip_SM_BeG_Traits, scale = TRUE)

Rip_AG_AbG_Traits_RDA <- rda(Rip_SG_AbG_Traits, scale = TRUE)
Rip_AG_BeG_Traits_RDA <- rda(Rip_AG_BeG_Traits, scale = TRUE)

Rip_SN_AbG_Traits_RDA <- rda(Rip_SN_AbG_Traits, scale = TRUE)
Rip_SN_BeG_Traits_RDA <- rda(Rip_SN_BeG_Traits, scale = TRUE)


###### FACTOEXTRA ########
## need to run PCA to use FACTOEXTRA for graph with arrows and individual points

#### perform PCA
# scale=TRUE to use correlation matrix

# All traits summaries N0N10
Rip_DO_AbG_Traits_PCA <- prcomp(Rip_DO_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_DO_BeG_Traits_PCA <- prcomp(Rip_DO_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix

Rip_AP_AbG_Traits_PCA <- prcomp(Rip_AP_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_AP_BeG_Traits_PCA <- prcomp(Rip_AP_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix

Rip_SM_AbG_Traits_PCA <- prcomp(Rip_SM_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_SM_BeG_Traits_PCA <- prcomp(Rip_SM_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix

Rip_AG_AbG_Traits_PCA <- prcomp(Rip_AG_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_AG_BeG_Traits_PCA <- prcomp(Rip_AG_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix

Rip_SN_AbG_Traits_PCA <- prcomp(Rip_SN_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
Rip_SN_BeG_Traits_PCA <- prcomp(Rip_SN_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix



# Run All plots, RDAs AbG and BeG

### DO, AbG biplot
fviz_pca_biplot(Rip_DO_AbG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                     col.ind=DO_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="DO AbG")
# DO, BeG biplot
fviz_pca_biplot(Rip_DO_BeG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=DO_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="DO BeG")
### AP, AbG biplot
fviz_pca_biplot(Rip_AP_AbG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AP_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="AP AbG")
# AP, BeG biplot
fviz_pca_biplot(Rip_AP_BeG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AP_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="AP BeG")
### SM, AbG biplot
fviz_pca_biplot(Rip_SM_AbG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SM_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="SM AbG")
# SM, BeG biplot
fviz_pca_biplot(Rip_SM_BeG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SM_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="SM BeG")
### AG, AbG biplot
fviz_pca_biplot(Rip_AG_AbG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AG_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="AG AbG")
# AG, BeG biplot
fviz_pca_biplot(Rip_AG_BeG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AG_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="AG BeG")
### SN, AbG biplot
fviz_pca_biplot(Rip_SN_AbG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SN_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="SN AbG")
# SN, BeG biplot
fviz_pca_biplot(Rip_SN_BeG_Traits_PCA, repel=TRUE, frame= TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SN_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="SN BeG")


