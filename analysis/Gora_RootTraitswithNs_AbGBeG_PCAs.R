#### RootTraitswithN Results Analysis-PCAs Rerun separately for AbG and BeG ####
###  #### RootTraitswithN Results Analysis-Ordinations ####
###  Gora_RootTraitswithN-Results-Analysis_Ordinations.R
###  by Sarah Gora
###  Date created: Oct 10, 2022

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
library(stats).R
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












################# Ordination for N10N10 ##################
#### Select the columns of traits you want from the N10,N0 Trait Data for DO

# select the Aboveground and Belowground Traits  N0N10
Rip_DO_AbG_Traits <- DO_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_DO_BeG_Traits <- DO_N0N10_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_AP_AbG_Traits <- AP_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_AP_BeG_Traits <- AP_N0N10_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_SM_AbG_Traits <- SM_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_SM_BeG_Traits <- SM_N0N10_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_AG_AbG_Traits <- AG_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_AG_BeG_Traits <- AG_N0N10_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_SN_AbG_Traits <- SN_N0N10_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_SN_BeG_Traits <- SN_N0N10_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)

#change data
Rip_DO_Ch_AbG_Traits <- DO_Ch_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_DO_Ch_BeG_Traits <- DO_Ch_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_AP_Ch_AbG_Traits <- AP_Ch_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_AP_Ch_BeG_Traits <- AP_Ch_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_SM_Ch_AbG_Traits <- SM_Ch_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_SM_Ch_BeG_Traits <- SM_Ch_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_AG_Ch_AbG_Traits <- AG_Ch_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_AG_Ch_BeG_Traits <- AG_Ch_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)
Rip_SN_Ch_AbG_Traits <- SN_Ch_Traits_O_Drop %>% select(SLA, Leaf_Thickness_cm, Aboveground_Dry_Weight_g, F_Leaf_Stage, PercentN_AbG, PercentC_AbG)
Rip_SN_Ch_BeG_Traits <- SN_Ch_Traits_O_Drop %>% select(SRL, Root_Diam_Mean, Focal_Root_Weight_g, PercentN_BeG, PercentC_BeG)



# check out data
View( )

#### perform PCA
# scale=TRUE to use correlation matrix

# Aboveground and Belowground traits summaries N0N10
Rip_DO_AbG_Traits_PCA <- prcomp(Rip_DO_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s <- summary(Rip_DO_AbG_Traits_PCA)
Rip_DO_BeG_Traits_PCA <- prcomp(Rip_DO_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s2 <- summary(Rip_DO_BeG_Traits_PCA)

Rip_AP_AbG_Traits_PCA <- prcomp(Rip_AP_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s3 <- summary(Rip_AP_AbG_Traits_PCA)
Rip_AP_BeG_Traits_PCA <- prcomp(Rip_AP_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s4 <- summary(Rip_AP_BeG_Traits_PCA)

Rip_SM_AbG_Traits_PCA <- prcomp(Rip_SM_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s5 <- summary(Rip_SM_AbG_Traits_PCA)
Rip_SM_BeG_Traits_PCA <- prcomp(Rip_SM_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s6 <- summary(Rip_SM_BeG_Traits_PCA)

Rip_AG_AbG_Traits_PCA <- prcomp(Rip_AG_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s7 <- summary(Rip_AG_AbG_Traits_PCA)
Rip_AG_BeG_Traits_PCA <- prcomp(Rip_AG_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s8 <- summary(Rip_AG_BeG_Traits_PCA)

Rip_SN_AbG_Traits_PCA <- prcomp(Rip_SN_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s9 <- summary(Rip_SN_AbG_Traits_PCA)
Rip_SN_BeG_Traits_PCA <- prcomp(Rip_SN_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s10 <- summary(Rip_SN_BeG_Traits_PCA)

# SCALE - perform an RDA
Rip_DO_AbG_Traits_RDA <- rda(Rip_DO_AbG_Traits, scale = TRUE)
Rip_DO_BeG_Traits_RDA <- rda(Rip_DO_BeG_Traits, scale = TRUE)

Rip_AP_AbG_Traits_RDA <- rda(Rip_AP_AbG_Traits, scale = TRUE)
Rip_AP_BeG_Traits_RDA <- rda(Rip_AP_BeG_Traits, scale = TRUE)

Rip_SM_AbG_Traits_RDA <- rda(Rip_SM_AbG_Traits, scale = TRUE)
Rip_SM_BeG_Traits_RDA <- rda(Rip_SM_BeG_Traits, scale = TRUE)

Rip_AG_AbG_Traits_RDA <- rda(Rip_AG_AbG_Traits, scale = TRUE)
Rip_AG_BeG_Traits_RDA <- rda(Rip_AG_BeG_Traits, scale = TRUE)

Rip_SN_AbG_Traits_RDA <- rda(Rip_SN_AbG_Traits, scale = TRUE)
Rip_SN_BeG_Traits_RDA <- rda(Rip_SN_BeG_Traits, scale = TRUE)

##### Basic biplot:
# plots scores plus raw data variables 
# helps visualize the variables that contribute MOST to resulting transformed PC axes
# Biplot for DO AbG, N0N10
biplot (Rip_DO_AbG_Traits_RDA, display = 'species', scaling = 1, main= "D. oligosanthes, AbG Traits, N0N10")
biplot (Rip_DO_BeG_Traits_RDA, display = 'species', scaling = 1, main= "D. oligosanthes, BeG Traits, N0N10")

biplot (Rip_AP_AbG_Traits_RDA, display = 'species', scaling = 1, main= "A. psilostachya, AbG Traits, N0N10")
biplot (Rip_AP_BeG_Traits_RDA, display = 'species', scaling = 1, main= "A. psilostachya, BeG Traits, N0N10")

biplot (Rip_SM_AbG_Traits_RDA, display = 'species', scaling = 1, main= "S. missouriensis, AbG Traits, N0N10")
biplot (Rip_SM_BeG_Traits_RDA, display = 'species', scaling = 1, main= "S. missouriensis, BeG Traits, N0N10")

biplot (Rip_AG_AbG_Traits_RDA, display = 'species', scaling = 1, main= "A. gerardii, AbG Traits, N0N10")
biplot (Rip_AG_BeG_Traits_RDA, display = 'species', scaling = 1, main= "A. gerardii, BeG Traits, N0N10")

biplot (Rip_SN_AbG_Traits_RDA, display = 'species', scaling = 1, main= "S. nutans, AbG Traits, N0N10")
biplot (Rip_SN_BeG_Traits_RDA, display = 'species', scaling = 1, main= "S. nutans, BeG Traits, N0N10")


par(mfrow=c(5, 2))
par(mfrow=c(1, 1))


###### Trying FACTOEXTRA ########
# Load package
install.packages("factoextra")
library("factoextra")

View(Rip_SN_Traits)

# DO BiPlots (NEW), Abg and BeG
fviz_pca_biplot(Rip_DO_AbG_Traits_PCA, 
                repel=TRUE, 
                pointsize=3, 
                geom="point", 
                col.var="red", 
                arrowsize=0.6, 
                labelsize=5, 
                col.ind=DO_N0N10_Traits_O_Drop$Treatment, 
                palette=c("#769370","#BDB2A7"), 
                addEllipses=TRUE, 
                ellipse.type="confidence") + labs(title ="D. oligosanthes Aboveground Traits")

fviz_pca_biplot(Rip_DO_BeG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=DO_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="D. oligosanthes Belowground Traits")

# AP BiPlots (NEW), Abg and BeG
fviz_pca_biplot(Rip_AP_AbG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AP_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. psilostachya Aboveground Traits")

fviz_pca_biplot(Rip_AP_BeG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AP_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. psilostachya Belowground Traits")

# SM BiPlots (NEW), Abg and BeG
fviz_pca_biplot(Rip_SM_AbG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SM_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. missouriensis Aboveground Traits")

fviz_pca_biplot(Rip_SM_BeG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SM_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. missouriensis Belowground Traits")

# SN BiPlots (NEW), Abg and BeG
fviz_pca_biplot(Rip_SN_AbG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SN_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. nutans Aboveground Traits")

fviz_pca_biplot(Rip_SN_BeG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=SN_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="S. nutans Belowground Traits")

# AG BiPlots (NEW), Abg and BeG
fviz_pca_biplot(Rip_AG_AbG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AG_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. gerardii Aboveground Traits")

fviz_pca_biplot(Rip_AG_BeG_Traits_PCA, repel=TRUE, pointsize=3, geom="point", col.var="red", arrowsize=0.6, labelsize=5, 
                col.ind=AG_N0N10_Traits_O_Drop$Treatment, palette=c("#769370","#BDB2A7"), addEllipses=TRUE, ellipse.type="confidence") + labs(title ="A. gerardii Belowground Traits")



##### Ordiplot:
counts <- colSums(Rip_DO_Traits) # compute 
ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for DO") # blank plot space
orditorp(Rip_DO_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_DO_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19, main = "Ordiplot for DO")

##### Group by category plot:
#color by N0 and N10
cols= c("#769370", "#BDB2A7")

par(mfrow=c(5, 2))

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



# Note: DO: AbG traits, N0N10
par(mfrow=c(1, 2))

#par(bg = "#FFFBC8")
p <- ordiplot(Rip_DO_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

#DO BeG
p <- ordiplot(Rip_DO_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s2$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s2$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

?ordiplot

# AP
par(mfrow=c(1,2))

p <- ordiplot(Rip_AP_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s3$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s3$importance[5]*100, 1), "%)", sep = ""),
              main= "A. psilostachya, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)


p <- ordiplot(Rip_AP_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s4$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s4$importance[5]*100, 1), "%)", sep = ""),
              main= "A. psylostachya, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(AP_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)



## SM
p <- ordiplot(Rip_SM_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s5$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s5$importance[5]*100, 1), "%)", sep = ""),
              main= "S. missouriensis, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

#SM BeG
p <- ordiplot(Rip_SM_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s6$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s6$importance[5]*100, 1), "%)", sep = ""),
              main= "S. missouriensis, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(SM_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

# AG
par(mfrow=c(2, 2))
p <- ordiplot(Rip_AG_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s7$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s7$importance[5]*100, 1), "%)", sep = ""),
              main= "A. gerardii, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

#AG BeG
p <- ordiplot(Rip_AG_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s8$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s8$importance[5]*100, 1), "%)", sep = ""),
              main= "A. gerardii, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(AG_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)


# SN

p <- ordiplot(Rip_SN_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s9$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s9$importance[5]*100, 1), "%)", sep = ""),
              main= "S. nutans, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)

#SN BeG
p <- ordiplot(Rip_SN_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s10$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s10$importance[5]*100, 1), "%)", sep = ""),
              main= "S. nutans, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(SN_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)





####### Run Adonis for each Abg and BeG ###################3
# run PERMANOVA, N0N10 ordination data with traits dropped 
# DO -AbG
PERMANOVA <-adonis(Rip_DO_AbG_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.047, * slightly significant

# DO -BeG
PERMANOVA <-adonis(Rip_DO_BeG_Traits~Treatment, data = DO_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.510, not significant


# AP -AbG
PERMANOVA <-adonis(Rip_AP_AbG_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.448 , nearly significant

# AP -BeG
PERMANOVA <-adonis(Rip_AP_BeG_Traits~Treatment, data = AP_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.0599, not significant


# SM -AbG
PERMANOVA <-adonis(Rip_SM_AbG_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.149 , not significant

# SM -BeG
PERMANOVA <-adonis(Rip_SM_BeG_Traits~Treatment, data = SM_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.0594, not significant



#  AG-AbG
PERMANOVA <-adonis(Rip_AG_AbG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= <0.001 *** , very significant

# AG -BeG
PERMANOVA <-adonis(Rip_AG_BeG_Traits~Treatment, data = AG_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.089, not significant



#  SN-AbG
PERMANOVA <-adonis(Rip_SN_AbG_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.236, not significant

# AG -BeG
PERMANOVA <-adonis(Rip_SN_BeG_Traits~Treatment, data = SN_N0N10_Traits_O_Drop, permutations = 1000, method = 'bray') 
PERMANOVA
# p= 0.351, not significant





# Note: DO: AbG traits, N0N10
par(mfrow=c(1, 1))

#### perform PCA
# scale=TRUE to use correlation matrix

# Aboveground and Belowground traits summaries Change
Rip_DO_Ch_AbG_Traits_PCA <- prcomp(Rip_DO_Ch_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s11 <- summary(Rip_DO_Ch_AbG_Traits_PCA)
Rip_DO_Ch_BeG_Traits_PCA <- prcomp(Rip_DO_Ch_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s12 <- summary(Rip_DO_Ch_BeG_Traits_PCA)

Rip_AP_Ch_AbG_Traits_PCA <- prcomp(Rip_AP_Ch_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s13 <- summary(Rip_AP_Ch_AbG_Traits_PCA)
Rip_AP_Ch_BeG_Traits_PCA <- prcomp(Rip_AP_Ch_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s14 <- summary(Rip_AP_Ch_BeG_Traits_PCA)

Rip_SM_Ch_AbG_Traits_PCA <- prcomp(Rip_SM_Ch_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s15 <- summary(Rip_SM_Ch_AbG_Traits_PCA)
Rip_SM_Ch_BeG_Traits_PCA <- prcomp(Rip_SM_Ch_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s16 <- summary(Rip_SM_Ch_BeG_Traits_PCA)

Rip_AG_Ch_AbG_Traits_PCA <- prcomp(Rip_AG_Ch_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s17 <- summary(Rip_AG_Ch_AbG_Traits_PCA)
Rip_AG_Ch_BeG_Traits_PCA <- prcomp(Rip_AG_Ch_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s18 <- summary(Rip_AG_Ch_BeG_Traits_PCA)

Rip_SN_Ch_AbG_Traits_PCA <- prcomp(Rip_SN_Ch_AbG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s19 <- summary(Rip_SN_Ch_AbG_Traits_PCA)
Rip_SN_Ch_BeG_Traits_PCA <- prcomp(Rip_SN_Ch_BeG_Traits, scale = TRUE) # scale=TRUE to use correlation matrix
s20 <- summary(Rip_SN_Ch_BeG_Traits_PCA)

# SCALE - perform an RDA, Change data
Rip_DO_Ch_AbG_Traits_RDA <- rda(Rip_DO_Ch_AbG_Traits, scale = TRUE)
Rip_DO_Ch_BeG_Traits_RDA <- rda(Rip_DO_Ch_BeG_Traits, scale = TRUE)

Rip_AP_Ch_AbG_Traits_RDA <- rda(Rip_AP_Ch_AbG_Traits, scale = TRUE)
Rip_AP_Ch_BeG_Traits_RDA <- rda(Rip_AP_Ch_BeG_Traits, scale = TRUE)

Rip_SM_Ch_AbG_Traits_RDA <- rda(Rip_SM_Ch_AbG_Traits, scale = TRUE)
Rip_SM_Ch_BeG_Traits_RDA <- rda(Rip_SM_Ch_BeG_Traits, scale = TRUE)

Rip_AG_Ch_AbG_Traits_RDA <- rda(Rip_AG_Ch_AbG_Traits, scale = TRUE)
Rip_AG_Ch_BeG_Traits_RDA <- rda(Rip_AG_Ch_BeG_Traits, scale = TRUE)

Rip_SN_Ch_AbG_Traits_RDA <- rda(Rip_SN_Ch_AbG_Traits, scale = TRUE)
Rip_SN_Ch_BeG_Traits_RDA <- rda(Rip_SN_Ch_BeG_Traits, scale = TRUE)



### plot change data 
##### Ordiplot:
counts <- colSums(Rip_DO_Ch_AbG_Traits) # compute 
ordiplot(Rip_DO_Traits_RDA, choices = c(1, 2), type = 'none', main = "Ordiplot for DO") # blank plot space
orditorp(Rip_DO_Traits_RDA, display = 'sites', col = 'blue', pch = 19)
orditorp(Rip_DO_Traits_RDA, display='species', priority = counts, col = 'red', pch = 19, main = "Ordiplot for DO")

##### Group by category plot:
#color by N0 and N10
cols= c("#769370", "#BDB2A7")

#par(bg = "#FFFBC8")
p <- ordiplot(Rip_DO_AbG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes, Abg Traits", 
              font.main=4)
ordispider(p, groups = as.factor(DO_Ch_Traits_O_Drop$Treatment), label = TRUE, lwd=1.5)


#DO BeG
p <- ordiplot(Rip_DO_BeG_Traits_RDA, choices = c(1, 2), 
              display = 'sites', 
              las=1,
              xlab=paste("PCA 1 (", round(s2$importance[2]*100, 1), "%)", sep = ""), 
              ylab=paste("PCA 2 (", round(s2$importance[5]*100, 1), "%)", sep = ""),
              main= "D. oligosanthes, BeG Traits", 
              font.main=4)
ordispider(p, groups = as.factor(DO_N0N10_Traits_O_Drop$Treatment), label = TRUE, col = cols, lwd=1.5)


