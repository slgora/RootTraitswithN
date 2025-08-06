#### RootTraitswithN Results Analysis- Stacked Bar Chart, Added MC Data ####
###  Gora_RootTraitswithN-Results-Analysis_StackedBarChart_MC.R
###  by Sarah Gora
###  Date created: February 17, 2023

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


# install packages
library(ggplot2)
library(dplyr)
library(tidyr)

# create N0N10 dataframe 
# changed to 8 BeG traits (including MC)

# DO: 8/11 AbG, 1/8 BeG 72.73, -12.50
# AP: 5/11 AbG, 1/8 BeG 45.45, -12.50
# SM: 1/11 AbG, 1/8 BeG 9.10, -12.50
# AG: 1/11 AbG, 1/8 BeG 9.10, -12.50
# SN: 1/11 AbG, 1/8 BeG 9.10, -12.50

#plot 
df_barchart_N0N10_MC <- data.frame(Species = rep(c("DO", "AP", "SM", "AG", "SN")),
                                Traits = rep(c("Aboveground","Belowground"),times=5),
                                Percent_Responsive_to_N = c(
                                  72.73, -12.50, 
                                  9.10, -12.50, 
                                  9.10, -12.50, 
                                  45.5, -12.50,
                                  9.10, -12.50))
df_barchart_N0N10_MC_1 <- df_barchart_N0N10_MC   # Replicate original data
df_barchart_N0N10_MC_1$Species <- factor(df_barchart_N0N10_MC_1$Species,                                    # Change ordering manually
                                      levels = c("DO", "AP", "SM", "AG", "SN"))

my_plot_MC <- ggplot(df_barchart_N0N10_MC_1, aes(x=Species, y=Percent_Responsive_to_N, fill=Traits, group=Traits,group=Species)) + ylim(-20, 100)+
  geom_bar(position="stack", stat="identity", colour = "black")

myplot_MC_theme <- my_plot_MC + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
a_MC <- myplot_MC_theme + scale_fill_manual(values=c("darkolivegreen4", "bisque4")) + geom_hline(yintercept=0)+ ggtitle("Trait response to 10g N")+labs(y= "Traits Responsive to N")

a_MC

# add % labels to N0N10plot
bar_labels_a_MC <- c("72.7%","12.5%","9.1%","12.5%","9.1%","12.5%","45.5%","12.5%","9.1%","12.5%")

a_MC_labled <- a_MC + geom_text(aes(label = bar_labels_a_MC),size = 4, fontface = "bold", vjust = 1.5,color="white") + 
  geom_text(aes(label = bar_labels_a_MC),size = 4, fontface = "bold", vjust = -1.5,color="white")

a_MC_labled


#########  Change Barchart ######## 
# based on regressions
# grouped + stacked, barchart 

# create Change dataframe, adding in MC as BeG 8th trait

# DO: 6/11 AbG, 1/8 BeG   54.5, -12.5
# AP: 9/11 AbG, 2/8 BeG   81.8, -25.0
# SM: 2/11 AbG, 0/8 BeG   18.2, 0
# AG: 5/11 AbG, 1/8 BeG   45.5, -12.5
# SN: 3/11 AbG, 1/8 BeG   27.3, -12.5

# * 8 traits BeG also in focal 


df_barchart_Ch_MC <- data.frame(Species = rep(c("DO","DO", "AP","AP", "SM", "SM","AG", "AG", "SN", "SN")),
                             Traits = rep(c("Aboveground","Belowground"),times=5),
                             Percent_Responsive_to_N = c(
                               54.5, -12.5, #
                               81.8, -25.0, #4
                               18.2, 0,    #1 
                               45.5, -12.5, 
                               27.3, -12.5))

df_barchart_Ch_MC_1 <- df_barchart_Ch_MC   # Replicate original data
df_barchart_Ch_MC_1$Species <- factor(df_barchart_Ch_MC_1$Species,                                    # Change ordering manually
                                   levels = c("DO", "AP", "SM", "AG", "SN"))

my_plot_Ch_MC <- ggplot(df_barchart_Ch_MC_1, aes(x=Species, y=Percent_Responsive_to_N, fill=Traits, group=Traits,group=Species)) + ylim(-35, 100) + 
  geom_bar(position="stack", stat="identity", colour = "black")

myplot_Ch_MC_theme <- my_plot_Ch_MC + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                      panel.background = element_blank(), axis.line = element_line(colour = "black"))
b_MC <- myplot_Ch_MC_theme + scale_fill_manual(values=c("darkolivegreen4", "bisque4")) + geom_hline(yintercept=0) + ggtitle("Trait response to N gradients")+labs(y= "Traits Responsive to N")

b_MC

# add labels 
bar_labels_b_MC <- c("54.5%","12.5%","81.8%","25.0%","18.2%"," ","45.5%","12.5%","27.3%","12.5%")

# add % labels to Change plot
b_MC_labled <- b_MC + geom_text(aes(label = bar_labels_b_MC),size = 4, fontface = "bold", vjust = 1.5,color="white") + geom_text(aes(label = bar_labels_b_MC),size = 4, fontface = "bold", vjust = -1.5,color="white")
b_MC_labled







## COMBINE
#remove legend from N0N10 plot
a_MC_lab_nolegend <- a_MC_labled + theme(legend.position="none")

#remove legend from Change plot
b_MC_lab_nolegend <- b_MC_labled + theme(legend.position="none")


# Package for Combining Plots
install.packages("ggpubr")
library(ggpubr)

ggarrange(a_MC_lab_nolegend, b_MC_lab_nolegend)


####### Kevin Wilcox Suggestion: 
# June 2023, SG working
# add panel to barchart 
# graph log response ratio to show effect size 


# 1. Calculate means for each trait value in 10N and 0N
# by species



#load Data 
# all traits data
All_Traits <- read.csv("~/Desktop/Thesis_Data/ALL_Traits_Datasheet.csv")
View(All_Traits)

# Subset out data by species
DO_Traits <- subset(All_Traits, Species=="Dichanthelium_oligosanthes")
AP_Traits <- subset(All_Traits, Species=="Ambrosia_psilostachya")
SM_Traits <- subset(All_Traits, Species=="Solidago_missouriensis")
AG_Traits <- subset(All_Traits, Species=="Andropogon_gerardii")
SN_Traits <- subset(All_Traits, Species=="Sorghastrum_nutans")

#subset by 0N and 10N
DO_N0_Traits <- subset(DO_Traits, Treatment=="Control")
DO_N10_Traits <- subset(DO_Traits, Treatment=="10")
AP_N0_Traits <- subset(AP_Traits, Treatment=="Control")
AP_N10_Traits <- subset(AP_Traits, Treatment=="10")
SM_N0_Traits <- subset(SM_Traits, Treatment=="Control")
SM_N10_Traits <- subset(SM_Traits, Treatment=="10")
AG_N0_Traits <- subset(AG_Traits, Treatment=="Control")
AG_N10_Traits <- subset(AG_Traits, Treatment=="10")
SN_N0_Traits <- subset(SN_Traits, Treatment=="Control")
SN_N10_Traits <- subset(SN_Traits, Treatment=="10")


# 18 Traits:

# 1. SLA
# 2. Leaf_Thickness_cm
# 3. Aboveground_Dry_Weight_g
# 4. Leaf_Number
# 5. S_Leaf_Stage
# 6. E_Leaf_Stage                # ABG (11)
# 7. F_Leaf_Stage
# 8. Plant_Height_cm
# 9. Plant_Area_cm.2
# 10. Percent_N_AbG
# 11. Percent_C_AbG

# 12. Focal_Root_Weight_g
# 13. Total_Belowground_Weight_g
# 14. SRL
# 15. Root_Diam_Max             # BEG (7)
# 16. Root_Diam_Mean
# 17. Percent_N_BeG
# 18. Percent_C_BeG

# Trait 1: SLA
mean_DO_N0_SLA <- mean(DO_N0_Traits$SLA, trim = 0, na.rm = TRUE)
mean_DO_N10_SLA <- mean(DO_N10_Traits$SLA, trim = 0, na.rm = TRUE)
mean_AP_N0_SLA <- mean(AP_N0_Traits$SLA, trim = 0, na.rm = TRUE)
mean_AP_N10_SLA <- mean(AP_N10_Traits$SLA, trim = 0, na.rm = TRUE)
mean_SM_N0_SLA <- mean(SM_N0_Traits$SLA, trim = 0, na.rm = TRUE)
mean_SM_N10_SLA <- mean(SM_N10_Traits$SLA, trim = 0, na.rm = TRUE)
mean_AG_N0_SLA <- mean(AG_N0_Traits$SLA, trim = 0, na.rm = TRUE)
mean_AG_N10_SLA <- mean(AG_N10_Traits$SLA, trim = 0, na.rm = TRUE)
mean_SN_N0_SLA <- mean(SN_N0_Traits$SLA, trim = 0, na.rm = TRUE)
mean_SN_N10_SLA <- mean(SN_N10_Traits$SLA, trim = 0, na.rm = TRUE)

# Trait 2. Leaf_Thickness_cm
mean_DO_N0_LeafTh <- mean(DO_N0_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_DO_N10_LeafTh <- mean(DO_N10_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_AP_N0_LeafTh <- mean(AP_N0_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_AP_N10_LeafTh <- mean(AP_N10_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_SM_N0_LeafTh <- mean(SM_N0_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_SM_N10_LeafTh <- mean(SM_N10_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_AG_N0_LeafTh <- mean(AG_N0_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_AG_N10_LeafTh <- mean(AG_N10_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_SN_N0_LeafTh <- mean(SN_N0_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)
mean_SN_N10_LeafTh <- mean(SN_N10_Traits$Leaf_Thickness_cm, trim = 0, na.rm = TRUE)

# 3. Aboveground_Dry_Weight_g
mean_DO_N0_AbgBio <- mean(DO_N0_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_DO_N10_AbgBio <- mean(DO_N10_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N0_AbgBio <- mean(AP_N0_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N10_AbgBio <- mean(AP_N10_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N0_AbgBio <- mean(SM_N0_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N10_AbgBio <- mean(SM_N10_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N0_AbgBio <- mean(AG_N0_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N10_AbgBio <- mean(AG_N10_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N0_AbgBio <- mean(SN_N0_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N10_AbgBio <- mean(SN_N10_Traits$Aboveground_Dry_Weight_g, trim = 0, na.rm = TRUE)

# 4. Leaf_Number
mean_DO_N0_LeafNum <- mean(DO_N0_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_DO_N10_LeafNum <- mean(DO_N10_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_AP_N0_LeafNum <- mean(AP_N0_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_AP_N10_LeafNum <- mean(AP_N10_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_SM_N0_LeafNum <- mean(SM_N0_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_SM_N10_LeafNum <- mean(SM_N10_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_AG_N0_LeafNum <- mean(AG_N0_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_AG_N10_LeafNum <- mean(AG_N10_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_SN_N0_LeafNum <- mean(SN_N0_Traits$Leaf_Number, trim = 0, na.rm = TRUE)
mean_SN_N10_LeafNum <- mean(SN_N10_Traits$Leaf_Number, trim = 0, na.rm = TRUE)

# 5. S_Leaf_Stage
mean_DO_N0_SLeaf <- mean(DO_N0_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_DO_N10_SLeaf <- mean(DO_N10_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N0_SLeaf <- mean(AP_N0_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N10_SLeaf <- mean(AP_N10_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N0_SLeaf <- mean(SM_N0_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N10_SLeaf <- mean(SM_N10_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N0_SLeaf <- mean(AG_N0_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N10_SLeaf <- mean(AG_N10_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N0_SLeaf <- mean(SN_N0_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N10_SLeaf <- mean(SN_N10_Traits$S_Leaf_Stage, trim = 0, na.rm = TRUE)

# 6. E_Leaf_Stage
mean_DO_N0_ELeaf <- mean(DO_N0_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_DO_N10_ELeaf <- mean(DO_N10_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N0_ELeaf <- mean(AP_N0_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N10_ELeaf <- mean(AP_N10_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N0_ELeaf <- mean(SM_N0_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N10_ELeaf <- mean(SM_N10_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N0_ELeaf <- mean(AG_N0_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N10_ELeaf <- mean(AG_N10_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N0_ELeaf <- mean(SN_N0_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N10_ELeaf <- mean(SN_N10_Traits$E_Leaf_Stage, trim = 0, na.rm = TRUE)

# 7. F_Leaf_Stage
mean_DO_N0_FLeaf <- mean(DO_N0_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_DO_N10_FLeaf <- mean(DO_N10_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N0_FLeaf <- mean(AP_N0_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AP_N10_FLeaf <- mean(AP_N10_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N0_FLeaf <- mean(SM_N0_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SM_N10_FLeaf <- mean(SM_N10_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N0_FLeaf <- mean(AG_N0_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_AG_N10_FLeaf <- mean(AG_N10_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N0_FLeaf <- mean(SN_N0_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)
mean_SN_N10_FLeaf <- mean(SN_N10_Traits$F_Leaf_Stage, trim = 0, na.rm = TRUE)

# 8. Plant_Height_cm
mean_DO_N0_Height <- mean(DO_N0_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_DO_N10_Height <- mean(DO_N10_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_AP_N0_Height <- mean(AP_N0_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_AP_N10_Height <- mean(AP_N10_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_SM_N0_Height <- mean(SM_N0_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_SM_N10_Height <- mean(SM_N10_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_AG_N0_Height <- mean(AG_N0_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_AG_N10_Height <- mean(AG_N10_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_SN_N0_Height <- mean(SN_N0_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)
mean_SN_N10_Height <- mean(SN_N10_Traits$Plant_Height_cm, trim = 0, na.rm = TRUE)

# 9. Plant_Area_cm.2
mean_DO_N0_Vol <- mean(DO_N0_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_DO_N10_Vol <- mean(DO_N10_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_AP_N0_Vol <- mean(AP_N0_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_AP_N10_Vol <- mean(AP_N10_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_SM_N0_Vol <- mean(SM_N0_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_SM_N10_Vol <- mean(SM_N10_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_AG_N0_Vol <- mean(AG_N0_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_AG_N10_Vol <- mean(AG_N10_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_SN_N0_Vol <- mean(SN_N0_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)
mean_SN_N10_Vol <- mean(SN_N10_Traits$Plant_Area_cm.2, trim = 0, na.rm = TRUE)

# 10. Percent_N_AbG
mean_DO_N0_LeafN <- mean(DO_N0_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_DO_N10_LeafN <- mean(DO_N10_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_AP_N0_LeafN <- mean(AP_N0_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_AP_N10_LeafN <- mean(AP_N10_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_SM_N0_LeafN <- mean(SM_N0_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_SM_N10_LeafN <- mean(SM_N10_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_AG_N0_LeafN <- mean(AG_N0_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_AG_N10_LeafN <- mean(AG_N10_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_SN_N0_LeafN <- mean(SN_N0_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)
mean_SN_N10_LeafN <- mean(SN_N10_Traits$Percent_N_AbG, trim = 0, na.rm = TRUE)

# 11. Percent_C_AbG
mean_DO_N0_LeafC <- mean(DO_N0_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_DO_N10_LeafC <- mean(DO_N10_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_AP_N0_LeafC <- mean(AP_N0_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_AP_N10_LeafC <- mean(AP_N10_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_SM_N0_LeafC <- mean(SM_N0_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_SM_N10_LeafC <- mean(SM_N10_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_AG_N0_LeafC <- mean(AG_N0_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_AG_N10_LeafC <- mean(AG_N10_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_SN_N0_LeafC <- mean(SN_N0_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)
mean_SN_N10_LeafC <- mean(SN_N10_Traits$Percent_C_AbG, trim = 0, na.rm = TRUE)

# 12. Focal_Root_Weight_g
mean_DO_N0_FocalRt <- mean(DO_N0_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_DO_N10_FocalRt <- mean(DO_N10_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N0_FocalRt <- mean(AP_N0_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N10_FocalRt <- mean(AP_N10_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N0_FocalRt <- mean(SM_N0_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N10_FocalRt <- mean(SM_N10_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N0_FocalRt <- mean(AG_N0_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N10_FocalRt <- mean(AG_N10_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N0_FocalRt <- mean(SN_N0_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N10_FocalRt <- mean(SN_N10_Traits$Focal_Root_Weight_g, trim = 0, na.rm = TRUE)

# 13. Total_Belowground_Weight_g
mean_DO_N0_BegBio <- mean(DO_N0_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_DO_N10_BegBio <- mean(DO_N10_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N0_BegBio <- mean(AP_N0_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_AP_N10_BegBio <- mean(AP_N10_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N0_BegBio <- mean(SM_N0_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_SM_N10_BegBio <- mean(SM_N10_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N0_BegBio <- mean(AG_N0_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_AG_N10_BegBio <- mean(AG_N10_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N0_BegBio <- mean(SN_N0_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)
mean_SN_N10_BegBio <- mean(SN_N10_Traits$Total_Belowground_Weight_g, trim = 0, na.rm = TRUE)

# 14. SRL
mean_DO_N0_SRL <- mean(DO_N0_Traits$SRL, trim = 0, na.rm = TRUE)
mean_DO_N10_SRL <- mean(DO_N10_Traits$SRL, trim = 0, na.rm = TRUE)
mean_AP_N0_SRL <- mean(AP_N0_Traits$SRL, trim = 0, na.rm = TRUE)
mean_AP_N10_SRL <- mean(AP_N10_Traits$SRL, trim = 0, na.rm = TRUE)
mean_SM_N0_SRL <- mean(SM_N0_Traits$SRL, trim = 0, na.rm = TRUE)
mean_SM_N10_SRL <- mean(SM_N10_Traits$SRL, trim = 0, na.rm = TRUE)
mean_AG_N0_SRL <- mean(AG_N0_Traits$SRL, trim = 0, na.rm = TRUE)
mean_AG_N10_SRL <- mean(AG_N10_Traits$SRL, trim = 0, na.rm = TRUE)
mean_SN_N0_SRL <- mean(SN_N0_Traits$SRL, trim = 0, na.rm = TRUE)
mean_SN_N10_SRL <- mean(SN_N10_Traits$SRL, trim = 0, na.rm = TRUE)

# 15. Root_Diam_Max
mean_DO_N0_RtDiamMax <- mean(DO_N0_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_DO_N10_RtDiamMax <- mean(DO_N10_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_AP_N0_RtDiamMax <- mean(AP_N0_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_AP_N10_RtDiamMax <- mean(AP_N10_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_SM_N0_RtDiamMax <- mean(SM_N0_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_SM_N10_RtDiamMax <- mean(SM_N10_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_AG_N0_RtDiamMax <- mean(AG_N0_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_AG_N10_RtDiamMax <- mean(AG_N10_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_SN_N0_RtDiamMax <- mean(SN_N0_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)
mean_SN_N10_RtDiamMax <- mean(SN_N10_Traits$Root_Diam_Max, trim = 0, na.rm = TRUE)

# 16. Root_Diam_Mean
mean_DO_N0_RtDiamMean <- mean(DO_N0_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_DO_N10_RtDiamMean <- mean(DO_N10_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_AP_N0_RtDiamMean <- mean(AP_N0_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_AP_N10_RtDiamMean <- mean(AP_N10_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_SM_N0_RtDiamMean <- mean(SM_N0_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_SM_N10_RtDiamMean <- mean(SM_N10_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_AG_N0_RtDiamMean <- mean(AG_N0_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_AG_N10_RtDiamMean <- mean(AG_N10_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_SN_N0_RtDiamMean <- mean(SN_N0_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)
mean_SN_N10_RtDiamMean <- mean(SN_N10_Traits$Root_Diam_Mean, trim = 0, na.rm = TRUE)

# 17. Percent_N_BeG
mean_DO_N0_RootN <- mean(DO_N0_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_DO_N10_RootN <- mean(DO_N10_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_AP_N0_RootN <- mean(AP_N0_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_AP_N10_RootN <- mean(AP_N10_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_SM_N0_RootN <- mean(SM_N0_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_SM_N10_RootN <- mean(SM_N10_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_AG_N0_RootN <- mean(AG_N0_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_AG_N10_RootN <- mean(AG_N10_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_SN_N0_RootN <- mean(SN_N0_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)
mean_SN_N10_RootN <- mean(SN_N10_Traits$Percent_N_BeG, trim = 0, na.rm = TRUE)

# 18. Percent_C_BeG
mean_DO_N0_RootC <- mean(DO_N0_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_DO_N10_RootC <- mean(DO_N10_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_AP_N0_RootC <- mean(AP_N0_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_AP_N10_RootC <- mean(AP_N10_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_SM_N0_RootC <- mean(SM_N0_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_SM_N10_RootC <- mean(SM_N10_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_AG_N0_RootC <- mean(AG_N0_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_AG_N10_RootC <- mean(AG_N10_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_SN_N0_RootC <- mean(SN_N0_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)
mean_SN_N10_RootC <- mean(SN_N10_Traits$Percent_C_BeG, trim = 0, na.rm = TRUE)


# 2. For each trait, calculate log response ratios for each species
# log(mean_10N/mean_0N)

##### Log Response Ratios #####

# Trait 1: log SLA
log_DO_SLA <- log(mean_DO_N10_SLA/mean_DO_N0_SLA)
log_AP_SLA <- log(mean_AP_N10_SLA/mean_AP_N0_SLA)
log_SM_SLA <- log(mean_SM_N10_SLA/mean_SM_N0_SLA)
log_AG_SLA <- log(mean_AG_N10_SLA/mean_AG_N0_SLA)
log_SN_SLA <- log(mean_SN_N10_SLA/mean_SN_N0_SLA)

# Trait 2: log Leaf Thickness
log_DO_LeafTh <- log(mean_DO_N10_LeafTh/mean_DO_N0_LeafTh)
log_AP_LeafTh <- log(mean_AP_N10_LeafTh/mean_AP_N0_LeafTh)
log_SM_LeafTh <- log(mean_SM_N10_LeafTh/mean_SM_N0_LeafTh)
log_AG_LeafTh <- log(mean_AG_N10_LeafTh/mean_AG_N0_LeafTh)
log_SN_LeafTh <- log(mean_SN_N10_LeafTh/mean_SN_N0_LeafTh)

# Trait 3. log Aboveground_Dry_Weight_g
log_DO_AbgBio <- log(mean_DO_N10_AbgBio/mean_DO_N0_AbgBio)
log_AP_AbgBio <- log(mean_AP_N10_AbgBio/mean_AP_N0_AbgBio)
log_SM_AbgBio <- log(mean_SM_N10_AbgBio/mean_SM_N0_AbgBio)
log_AG_AbgBio <- log(mean_AG_N10_AbgBio/mean_AG_N0_AbgBio)
log_SN_AbgBio <- log(mean_SN_N10_AbgBio/mean_SN_N0_AbgBio)

# Trait 4. log Leaf_Number 
log_DO_LeafNum <- log(mean_DO_N10_LeafNum/mean_DO_N0_LeafNum)
log_AP_LeafNum <- log(mean_AP_N10_LeafNum/mean_AP_N0_LeafNum)
log_SM_LeafNum <- log(mean_SM_N10_LeafNum/mean_SM_N0_LeafNum)
log_AG_LeafNum <- log(mean_AG_N10_LeafNum/mean_AG_N0_LeafNum)
log_SN_LeafNum <- log(mean_SN_N10_LeafNum/mean_SN_N0_LeafNum)

# Trait 5. log S_Leaf_Stage  
log_DO_SLeaf <- log(mean_DO_N10_SLeaf/mean_DO_N0_SLeaf)
log_AP_SLeaf <- log(mean_AP_N10_SLeaf/mean_AP_N0_SLeaf)
log_SM_SLeaf <- log(mean_SM_N10_SLeaf/mean_SM_N0_SLeaf)
log_AG_SLeaf <- log(mean_AG_N10_SLeaf/mean_AG_N0_SLeaf)
log_SN_SLeaf <- log(mean_SN_N10_SLeaf/mean_SN_N0_SLeaf)

# Trait 6. log E_Leaf_Stage  
log_DO_ELeaf <- log(mean_DO_N10_ELeaf/mean_DO_N0_ELeaf)
log_AP_ELeaf <- log(mean_AP_N10_ELeaf/mean_AP_N0_ELeaf)
log_SM_ELeaf <- log(mean_SM_N10_ELeaf/mean_SM_N0_ELeaf)
log_AG_ELeaf <- log(mean_AG_N10_ELeaf/mean_AG_N0_ELeaf)
log_SN_ELeaf <- log(mean_SN_N10_ELeaf/mean_SN_N0_ELeaf)

# Trait 7. log F_Leaf_Stage  
log_DO_FLeaf <- log(mean_DO_N10_FLeaf/mean_DO_N0_FLeaf)
log_AP_FLeaf <- log(mean_AP_N10_FLeaf/mean_AP_N0_FLeaf)
log_SM_FLeaf <- log(mean_SM_N10_FLeaf/mean_SM_N0_FLeaf)
log_AG_FLeaf <- log(mean_AG_N10_FLeaf/mean_AG_N0_FLeaf)
log_SN_FLeaf <- log(mean_SN_N10_FLeaf/mean_SN_N0_FLeaf)

# Trait 8. log Plant_Height_cm  
log_DO_Height <- log(mean_DO_N10_Height/mean_DO_N0_Height)
log_AP_Height <- log(mean_AP_N10_Height/mean_AP_N0_Height)
log_SM_Height <- log(mean_SM_N10_Height/mean_SM_N0_Height)
log_AG_Height <- log(mean_AG_N10_Height/mean_AG_N0_Height)
log_SN_Height <- log(mean_SN_N10_Height/mean_SN_N0_Height)

# Trait 9. log Plant_Area_cm.2  
log_DO_Vol <- log(mean_DO_N10_Vol/mean_DO_N0_Vol)
log_AP_Vol <- log(mean_AP_N10_Vol/mean_AP_N0_Vol)
log_SM_Vol <- log(mean_SM_N10_Vol/mean_SM_N0_Vol)
log_AG_Vol <- log(mean_AG_N10_Vol/mean_AG_N0_Vol)
log_SN_Vol <- log(mean_SN_N10_Vol/mean_SN_N0_Vol)

# Trait 10. log Percent_N_AbG  
log_DO_LeafN <- log(mean_DO_N10_LeafN/mean_DO_N0_LeafN)
log_AP_LeafN <- log(mean_AP_N10_LeafN/mean_AP_N0_LeafN)
log_SM_LeafN <- log(mean_SM_N10_LeafN/mean_SM_N0_LeafN)
log_AG_LeafN <- log(mean_AG_N10_LeafN/mean_AG_N0_LeafN)
log_SN_LeafN <- log(mean_SN_N10_LeafN/mean_SN_N0_LeafN)

# Trait 11. log Percent_C_AbG  
log_DO_LeafC <- log(mean_DO_N10_LeafC/mean_DO_N0_LeafC)
log_AP_LeafC <- log(mean_AP_N10_LeafC/mean_AP_N0_LeafC)
log_SM_LeafC <- log(mean_SM_N10_LeafC/mean_SM_N0_LeafC)
log_AG_LeafC <- log(mean_AG_N10_LeafC/mean_AG_N0_LeafC)
log_SN_LeafC <- log(mean_SN_N10_LeafC/mean_SN_N0_LeafC)

# Trait 12. log Focal_Root_Weight_g  
log_DO_FocalRt <- log(mean_DO_N10_FocalRt/mean_DO_N0_FocalRt)
log_AP_FocalRt <- log(mean_AP_N10_FocalRt/mean_AP_N0_FocalRt)
log_SM_FocalRt <- log(mean_SM_N10_FocalRt/mean_SM_N0_FocalRt)
log_AG_FocalRt <- log(mean_AG_N10_FocalRt/mean_AG_N0_FocalRt)
log_SN_FocalRt <- log(mean_SN_N10_FocalRt/mean_SN_N0_FocalRt)

# Trait 13. log Total_Belowground_Weight_g  
log_DO_BegBio <- log(mean_DO_N10_BegBio/mean_DO_N0_BegBio)
log_AP_BegBio <- log(mean_AP_N10_BegBio/mean_AP_N0_BegBio)
log_SM_BegBio <- log(mean_SM_N10_BegBio/mean_SM_N0_BegBio)
log_AG_BegBio <- log(mean_AG_N10_BegBio/mean_AG_N0_BegBio)
log_SN_BegBio <- log(mean_SN_N10_BegBio/mean_SN_N0_BegBio)

# Trait 14. log SRL
log_DO_SRL <- log(mean_DO_N10_SRL/mean_DO_N0_SRL)
log_AP_SRL <- log(mean_AP_N10_SRL/mean_AP_N0_SRL)
log_SM_SRL <- log(mean_SM_N10_SRL/mean_SM_N0_SRL)
log_AG_SRL <- log(mean_AG_N10_SRL/mean_AG_N0_SRL)
log_SN_SRL <- log(mean_SN_N10_SRL/mean_SN_N0_SRL)

# Trait 15. log Root_Diam_Max  
log_DO_RtDiamMax <- log(mean_DO_N10_RtDiamMax/mean_DO_N0_RtDiamMax)
log_AP_RtDiamMax <- log(mean_AP_N10_RtDiamMax/mean_AP_N0_RtDiamMax)
log_SM_RtDiamMax <- log(mean_SM_N10_RtDiamMax/mean_SM_N0_RtDiamMax)
log_AG_RtDiamMax <- log(mean_AG_N10_RtDiamMax/mean_AG_N0_RtDiamMax)
log_SN_RtDiamMax <- log(mean_SN_N10_RtDiamMax/mean_SN_N0_RtDiamMax)

# Trait 16. log Root_Diam_Mean 
log_DO_RtDiamMean <- log(mean_DO_N10_RtDiamMean/mean_DO_N0_RtDiamMean)
log_AP_RtDiamMean <- log(mean_AP_N10_RtDiamMean/mean_AP_N0_RtDiamMean)
log_SM_RtDiamMean <- log(mean_SM_N10_RtDiamMean/mean_SM_N0_RtDiamMean)
log_AG_RtDiamMean <- log(mean_AG_N10_RtDiamMean/mean_AG_N0_RtDiamMean)
log_SN_RtDiamMean <- log(mean_SN_N10_RtDiamMean/mean_SN_N0_RtDiamMean)

# Trait 17. log Percent_N_BeG  
log_DO_RootN <- log(mean_DO_N10_RootN/mean_DO_N0_RootN)
log_AP_RootN <- log(mean_AP_N10_RootN/mean_AP_N0_RootN)
log_SM_RootN <- log(mean_SM_N10_RootN/mean_SM_N0_RootN)
log_AG_RootN <- log(mean_AG_N10_RootN/mean_AG_N0_RootN)
log_SN_RootN <- log(mean_SN_N10_RootN/mean_SN_N0_RootN)

# Trait 18. log Percent_C_BeG 
log_DO_RootC <- log(mean_DO_N10_RootC/mean_DO_N0_RootC)
log_AP_RootC <- log(mean_AP_N10_RootC/mean_AP_N0_RootC)
log_SM_RootC <- log(mean_SM_N10_RootC/mean_SM_N0_RootC)
log_AG_RootC <- log(mean_AG_N10_RootC/mean_AG_N0_RootC)
log_SN_RootC <- log(mean_SN_N10_RootC/mean_SN_N0_RootC)




# Combine into a dataframe??
# columns= species 
# rows= traits (log values)


### testing 

Species <- c("D. oligosanthes", 
             "A. psilostachya",
             "S. missouriensis",
             "A. gerardii",
             "S. nutans",
             "D. oligosanthes", 
             "A. psilostachya",
             "S. missouriensis",
             "A. gerardii",
             "S. nutans")
Traits <- c("SLA",
                "SLA",
                "SLA",
                "SLA",
                "SLA",
                "SRL",
                "SRL",
                "SRL",
                "SRL",
                "SRL")
traitcat <- c("Aboveground",
              "Aboveground",
              "Aboveground",
              "Aboveground",
              "Aboveground",
              "Belowground",
              "Belowground",
              "Belowground",
              "Belowground",
              "Belowground")
logtraits <- c(log_DO_SLA, 
               log_AP_SLA, 
               log_SM_SLA, 
               log_AG_SLA,
               log_SN_SLA,
               log_DO_SRL,
               log_AP_SRL,
               log_SM_SRL,
               log_AG_SRL,
               log_SN_SRL)

data2 <- data.frame(Species,
                    Traits,
                    traitcat,
                    logtraits)

#ggplot(data2, aes(x=Species, y=traitted2)) + theme_bw()

# ggplot(data2, aes(col= traitcat, y = logtraits, x = as.factor(Species)))+
#  geom_point(position = position_dodge(0.4), size = 4)+
#  theme_bw()
  
#color by N0 and N10
cols= c("#769370", "#BDB2A7")
  
ggplot(data2, aes(y = logtraits, x = as.factor(Species)))+
  geom_point(aes(shape=Traits, color=traitcat), position = position_dodge(0.4), size = 4)+
  scale_color_manual(values=c("#769370", "#BDB2A7"), name="Trait Category")+
  scale_x_discrete(name = "Species", label = Species)+
   theme_bw()

# scale_y_discrete(name = "Effect Size (Log Response Ratio)")+
  

# 3. Plot these values for each trait 
# x = Species 
# y = Effect Size (Log Response Ratio)
# one value for each trait within each species
# shapes = traits
# green = aboveground traits
# brown = belowground traits

# Group by Experiment 
# (n=3) replication
# gives mean and error around the response ratios
# group_by(Experiment)
