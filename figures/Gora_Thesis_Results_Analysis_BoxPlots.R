#### Thesis Results Analysis- Box Plots ####
###  Gora_Thesis_Results_Analysis_BoxPlots.R
###  by Sarah Gora
###  Date created: Dec 30, 2021

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

# load packages
install.packages("gt")
library(gt)


devtools::install_github("katiejolly/nationalparkcolors")


#load Data
library(readr)
All_Traits <- read_csv("~/Desktop/Thesis_Data/All_Traits_Datasheet.csv")
View(All_Traits)

# Subset out data by species 
DO_Traits <- subset(All_Traits, Species=="Dichanthelium_oligosanthes")
AP_Traits <- subset(All_Traits, Species=="Ambrosia_psilostachya")
SM_Traits <- subset(All_Traits, Species=="Solidago_missouriensis")
AG_Traits <- subset(All_Traits, Species=="Andropogon_gerardii")
SN_Traits <- subset(All_Traits, Species=="Sorghastrum_nutans")


# Subset out data by species for N0  
DO_N0_Traits <- subset(DO_Traits, Treatment=="Control")
AP_N0_Traits <- subset(AP_Traits, Treatment=="Control")
SM_N0_Traits <- subset(SM_Traits, Treatment=="Control")
AG_N0_Traits <- subset(AG_Traits, Treatment=="Control")
SN_N0_Traits <- subset(SN_Traits, Treatment=="Control")

# Subset out data by species for N10  
DO_N10_Traits <- subset(DO_Traits, Treatment=="10")
AP_N10_Traits <- subset(AP_Traits, Treatment=="10")
SM_N10_Traits <- subset(SM_Traits, Treatment=="10")
AG_N10_Traits <- subset(AG_Traits, Treatment=="10")
SN_N10_Traits <- subset(SN_Traits, Treatment=="10")



#### stop subset out ... 







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


View(DO_Traits)
# BoxPlots 
# Multi-panel

# for each species -control vs treatment?
# for each trait?


# Make a box plot for SLA 

View(DO_N0)
#DO 
DO_N0 <- which(All_Traits$Species == "Dichanthelium_oligosanthes" | All_Traits$Treatment == "Control")
DO_N10 <- which(All_Traits$Species == "Dichanthelium_oligosanthes" | All_Traits$Treatment == "10")
AP_N0 <- which(All_Traits$Species == "Ambrosia_psilostachya" | All_Traits$Treatment == "Control")
AP_N10 <- which(All_Traits$Species == "Ambrosia_psilostachya" | All_Traits$Treatment == "10")
SM_N0 <- which(All_Traits$Species == "Solidago_missouriensis" | All_Traits$Treatment == "Control")
SM_N10 <- which(All_Traits$Species == "Solidago_missouriensis" | All_Traits$Treatment == "10")
AG_N0 <- which(All_Traits$Species == "Andropogon_gerardii" | All_Traits$Treatment == "Control")
AG_N10 <- which(All_Traits$Species == "Andropogon_gerardii" | All_Traits$Treatment == "10")
SN_N0 <- which(All_Traits$Species == "Sorghastrum_nutans" | All_Traits$Treatment == "Control")
SN_N10 <- which(All_Traits$Species == "Sorghastrum_nutans" | All_Traits$Treatment == "10")


cat4 <- c("N0", "N10","N0", "N10","N0", "N10","N0", "N10","N0", "N10")

boxplot(DO_Traits$SLA[DO_N0], DO_Traits$SLA[DO_N10],
        AP_Traits$SLA[AP_N0], AP_Traits$SLA[AP_N10],
        SM_Traits$SLA[SM_N0], SM_Traits$SLA[SM_N10],
        AG_Traits$SLA[AG_N0], AG_Traits$SLA[AG_N10],
        SN_Traits$SLA[SN_N0], SN_Traits$SLA[SN_N10],
        names= cat4,
        xlab="Treatment", 
        ylab="Average SLA", 
        main= "SLA by Treatment", 
        pch=20)




#DO
cat2 <- c("N0", "N10")
boxplot(All_Traits$SLA[DO_N0], All_Traits$SLA[DO_N10],
        names= cat2,
        xlab="DO", 
        ylab="SLA", 
        pch=8)
#AP
boxplot(All_Traits$SLA[AP_N0], All_Traits$SLA[AP_N10],
        names= cat2,
        xlab="AP", 
        ylab="SLA",
        pch=8)
#SM
boxplot(All_Traits$SLA[SM_N0], All_Traits$SLA[SM_N10],
        names= cat2,
        xlab="SM", 
        ylab="SLA",
        pch=8)
#SN
boxplot(All_Traits$SLA[SN_N0], All_Traits$SLA[SN_N10],
        names= cat2,
        xlab="SN", 
        ylab="SLA",
        pch=8)
#AG
boxplot(All_Traits$SLA[AG_N0], All_Traits$SLA[AG_N10],
        names= cat2,
        xlab="AG", 
        ylab="SLA",
        pch=8)


#Combine all graphs into one 
par(mfrow=c(3,2))






#setting colors
library(nationalparkcolors)
pal <- park_palette("Redwoods", 2)
library("RColorBrewer")

brewer.pal(5, pal)




#MULTI-PANEL BOX PLOTS

#Code for Control, 10N
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

### SLA statistics ###
summary(aov(DO_Traits$SLA~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$SLA~DO_Traits$Treatment))
# p= 0.8040425 

View(DO_Traits)



aov_SLA_DO <- aov(SLA~Treatment+(1/Experiment) + Error(1/Experiment), data=DO_N0N10_Traits)
summary(aov_SLA_DO)

summary(aov(AP_Traits$SLA~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$SLA~AP_Traits$Treatment))
# p= 0.9715708

summary(aov(SM_Traits$SLA~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$SLA~SM_Traits$Treatment))
# p= 0.5087415

summary(aov(AG_Traits$SLA~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$SLA~AG_Traits$Treatment))
# p= 0.0188641

summary(aov(SN_Traits$SLA~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$SLA~SN_Traits$Treatment))
# p= 0.8505109

### SLA figure ###

# Redwoods palette: 
# green: #769370, 10N
# gray: #BDB2A7, Control
# yellow: #F1C646, 2.5N
# purple: #6E687E 20N
# orange: #F17236

pal <- park_palette("Redwoods", 5)
# gray= control, green= 10N

pal2 <- c("#BDB2A7","#769370")

cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")
boxplot(DO_N0_Traits$SLA, DO_N10_Traits$SLA,
        AP_N0_Traits$SLA, AP_N10_Traits$SLA,
        SM_N0_Traits$SLA, SM_N10_Traits$SLA,
        AG_N0_Traits$SLA, AG_N10_Traits$SLA,
        SN_N0_Traits$SLA, SN_N10_Traits$SLA,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylim=c(20,120),
        ylab="SLA", 
        main= "SLA across Species in N Additions", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.804"))
text(locator(), labels = c("p=0.972"))
text(locator(), labels = c("p=0.508"))
text(locator(), labels = c("p=0.019 *"))
text(locator(), labels = c("p=0.851"))




### Plant AbG Weight ###
colnames(DO_Traits)

summary(aov(DO_Traits$Aboveground_Dry_Weight_g~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Aboveground_Dry_Weight_g~DO_Traits$Treatment))
# p= 0.001

summary(aov(AP_Traits$Aboveground_Dry_Weight_g~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Aboveground_Dry_Weight_g~AP_Traits$Treatment))
# p= 0.673

summary(aov(SM_Traits$Aboveground_Dry_Weight_g~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Aboveground_Dry_Weight_g~SM_Traits$Treatment))
# p= 0.911

summary(aov(AG_Traits$Aboveground_Dry_Weight_g~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Aboveground_Dry_Weight_g~AG_Traits$Treatment))
# p= 0.717

summary(aov(SN_Traits$Aboveground_Dry_Weight_g~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Aboveground_Dry_Weight_g~SN_Traits$Treatment))
# p= 0.894

### AbG Weight Figure ###

pal <- park_palette("Redwoods", 5)
# gray= control, green= 10N

pal2 <- c("#BDB2A7","#769370")

cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")
boxplot(DO_N0_Traits$Aboveground_Dry_Weight_g, DO_N10_Traits$Aboveground_Dry_Weight_g,
        AP_N0_Traits$Aboveground_Dry_Weight_g, AP_N10_Traits$Aboveground_Dry_Weight_g,
        SM_N0_Traits$Aboveground_Dry_Weight_g, SM_N10_Traits$Aboveground_Dry_Weight_g,
        AG_N0_Traits$Aboveground_Dry_Weight_g, AG_N10_Traits$Aboveground_Dry_Weight_g,
        SN_N0_Traits$Aboveground_Dry_Weight_g, SN_N10_Traits$Aboveground_Dry_Weight_g,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylim=c(0,5),
        ylab="Aboveground Biomass (g)", 
        main= "Aboveground Biomass", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.001 *"))
text(locator(), labels = c("p=0.673"))
text(locator(), labels = c("p=0.911"))
text(locator(), labels = c("p=0.717"))
text(locator(), labels = c("p=0.894"))




### Leaf_Number ###
#Leaf Number Stats
colnames(DO_Traits)

summary(aov(DO_Traits$Leaf_Number~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Leaf_Number~DO_Traits$Treatment))
# p= 0.002

summary(aov(AP_Traits$Leaf_Number~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Leaf_Number~AP_Traits$Treatment))
# p= 0.075

summary(aov(SM_Traits$Leaf_Number~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Leaf_Number~SM_Traits$Treatment))
# p= 0.999

summary(aov(AG_Traits$Leaf_Number~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Leaf_Number~AG_Traits$Treatment))
# p= 0.622

summary(aov(SN_Traits$Leaf_Number~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Leaf_Number~SN_Traits$Treatment))
# p= 0.853

### Leaf_Number Figure ###

# gray= control, green= 10N
pal2 <- c("#BDB2A7","#769370")
cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")

boxplot(DO_N0_Traits$Leaf_Number, DO_N10_Traits$Leaf_Number,
        AP_N0_Traits$Leaf_Number, AP_N10_Traits$Leaf_Number,
        SM_N0_Traits$Leaf_Number, SM_N10_Traits$Leaf_Number,
        AG_N0_Traits$Leaf_Number, AG_N10_Traits$Leaf_Number,
        SN_N0_Traits$Leaf_Number, SN_N10_Traits$Leaf_Number,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0,60),
        xlab="Species", 
        ylab="Leaf Number", 
        main= " ", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")



boxplot(DO_N0_Traits$Leaf_Number)
text(locator(), labels = c("p<0.001 *"))
text(locator(), labels = c("p=0.029 *"))
text(locator(), labels = c("p=0.929"))
text(locator(), labels = c("p=0.396"))
text(locator(), labels = c("p=0.587"))





### Plant_Height_cm ###

#Plant_Height_cm Stats
colnames(DO_Traits)

summary(aov(DO_Traits$Plant_Height_cm~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Plant_Height_cm~DO_Traits$Treatment))
# p= 0.122

summary(aov(AP_Traits$Plant_Height_cm~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Plant_Height_cm~AP_Traits$Treatment))
# p= 0.153

summary(aov(SM_Traits$Plant_Height_cm~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Plant_Height_cm~SM_Traits$Treatment))
# p= 0.570

summary(aov(AG_Traits$Plant_Height_cm~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Plant_Height_cm~AG_Traits$Treatment))
# p= 0.868

summary(aov(SN_Traits$Plant_Height_cm~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Plant_Height_cm~SN_Traits$Treatment))
# p= 0.183

### Plant_Height_cm Figure ###

# gray= control, green= 10N
pal2 <- c("#BDB2A7","#769370")
cat3 <- c("          DO", "", "          AP", "", "          SM", "", "          AG", "", "          SN", "")

boxplot(DO_N0_Traits$Plant_Height_cm, DO_N10_Traits$Plant_Height_cm,
        AP_N0_Traits$Plant_Height_cm, AP_N10_Traits$Plant_Height_cm,
        SM_N0_Traits$Plant_Height_cm, SM_N10_Traits$Plant_Height_cm,
        AG_N0_Traits$Plant_Height_cm, AG_N10_Traits$Plant_Height_cm,
        SN_N0_Traits$Plant_Height_cm, SN_N10_Traits$Plant_Height_cm,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylim=c(0,100),
        ylab="Plant Height (cm)", 
        main= "Plant Height", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.045 *"))
text(locator(), labels = c("p=0.053"))
text(locator(), labels = c("p=0.241"))
text(locator(), labels = c("p=0.467"))
text(locator(), labels = c("p=0.021 *"))








# gotta add p-values ..... 


# Trait Focal_Root_Weight_g
boxplot(DO_Traits$Focal_Root_Weight_g[DO_N0], DO_Traits$Focal_Root_Weight_g[DO_N10],
        AP_Traits$Focal_Root_Weight_g[AP_N0], AP_Traits$Focal_Root_Weight_g[AP_N10],
        SM_Traits$Focal_Root_Weight_g[SM_N0], SM_Traits$Focal_Root_Weight_g[SM_N10],
        AG_Traits$Focal_Root_Weight_g[AG_N0], AG_Traits$Focal_Root_Weight_g[AG_N10],
        SN_Traits$Focal_Root_Weight_g[SN_N0], SN_Traits$Focal_Root_Weight_g[SN_N10],
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0,1.5),
        xlab="Species", 
        ylab="Root Weight (g)", 
        main= "Root Mass by Species in N Additions vs Control", 
        pch=20,
        col=pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
                                                                                     

#Redo.. 

### Focal_Root_Weight_g ###

#Focal_Root_Weight_g Stats

summary(aov(DO_Traits$Focal_Root_Weight_g~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Focal_Root_Weight_g~DO_Traits$Treatment))
# p=0.313

summary(aov(AP_Traits$Focal_Root_Weight_g~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Focal_Root_Weight_g~AP_Traits$Treatment))
# p= 0.815

summary(aov(SM_Traits$Focal_Root_Weight_g~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Focal_Root_Weight_g~SM_Traits$Treatment))
# p= 0.999

summary(aov(AG_Traits$Focal_Root_Weight_g~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Focal_Root_Weight_g~AG_Traits$Treatment))
# p= 0.873

summary(aov(SN_Traits$Focal_Root_Weight_g~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Focal_Root_Weight_g~SN_Traits$Treatment))
# p= 0.928

### Focal_Root_Weight_g Figure ###

boxplot(DO_N0_Traits$Focal_Root_Weight_g, DO_N10_Traits$Focal_Root_Weight_g,
        AP_N0_Traits$Focal_Root_Weight_g, AP_N10_Traits$Focal_Root_Weight_g,
        SM_N0_Traits$Focal_Root_Weight_g, SM_N10_Traits$Focal_Root_Weight_g,
        AG_N0_Traits$Focal_Root_Weight_g, AG_N10_Traits$Focal_Root_Weight_g,
        SN_N0_Traits$Focal_Root_Weight_g, SN_N10_Traits$Focal_Root_Weight_g,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylim=c(0,1.5),
        ylab="Root Biomass (g)", 
        main="Focal Root Biomass", 
        pch=20,
        col=pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.093"))
text(locator(), labels = c("p=0.333"))
text(locator(), labels = c("p=0.937"))
text(locator(), labels = c("p=0.450"))
text(locator(), labels = c("p=0.563"))




### SRL ###

#SRL Stats

summary(aov(DO_Traits$SRL~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$SRL~DO_Traits$Treatment))
# p=0.999

summary(aov(AP_Traits$SRL~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$SRL~AP_Traits$Treatment))
# p= 0.545

summary(aov(SM_Traits$SRL~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$SRL~SM_Traits$Treatment))
# p= 0.999

summary(aov(AG_Traits$SRL~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$SRL~AG_Traits$Treatment))
# p= 0.988

summary(aov(SN_Traits$SRL~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$SRL~SN_Traits$Treatment))
# p=0.938

### SRL Figure ###

boxplot(DO_Traits$SRL[DO_N0], DO_Traits$SRL[DO_N10],
        AP_Traits$SRL[AP_N0], AP_Traits$SRL[AP_N10],
        SM_Traits$SRL[SM_N0], SM_Traits$SRL[SM_N10],
        AG_Traits$SRL[AG_N0], AG_Traits$SRL[AG_N10],
        SN_Traits$SRL[SN_N0], SN_Traits$SRL[SN_N10],
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0,17),
        xlab="Species", 
        ylab="SRL", 
        main= "SRL (Specific Root Length)", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.412"))
text(locator(), labels = c("p=0.241"))
text(locator(), labels = c("p=0.656"))
text(locator(), labels = c("p=0.365"))
text(locator(), labels = c("p=0.600"))




### Total_BeG_Weight_g ###

# Total_BeG_Weight_g Stats

summary(aov(DO_Traits$Total_BeG_Weight_g~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Total_BeG_Weight_g~DO_Traits$Treatment))
# p=0.797

summary(aov(AP_Traits$Total_BeG_Weight_g~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Total_BeG_Weight_g~AP_Traits$Treatment))
# p= 0.937

summary(aov(SM_Traits$Total_BeG_Weight_g~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Total_BeG_Weight_g~SM_Traits$Treatment))
# p= 0.624

summary(aov(AG_Traits$Total_BeG_Weight_g~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Total_BeG_Weight_g~AG_Traits$Treatment))
# p=0.928

summary(aov(SN_Traits$Total_BeG_Weight_g~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Total_BeG_Weight_g~SN_Traits$Treatment))
# p=0.740


### Total_BeG_Weight_g Figure ###

boxplot(DO_Traits$Total_BeG_Weight_g[DO_N0], DO_Traits$Total_BeG_Weight_g[DO_N10],
        AP_Traits$Total_BeG_Weight_g[AP_N0], AP_Traits$Total_BeG_Weight_g[AP_N10],
        SM_Traits$Total_BeG_Weight_g[SM_N0], SM_Traits$Total_BeG_Weight_g[SM_N10],
        AG_Traits$Total_BeG_Weight_g[AG_N0], AG_Traits$Total_BeG_Weight_g[AG_N10],
        SN_Traits$Total_BeG_Weight_g[SN_N0], SN_Traits$Total_BeG_Weight_g[SN_N10],
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylab="Total Belowground Biomass (g)", 
        main= "Total Belowground Biomass", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.277"))
text(locator(), labels = c("p=0.547"))
text(locator(), labels = c("p=0.192"))
text(locator(), labels = c("p=0.518"))
text(locator(), labels = c("p=0.266"))







### Plant_Area ###

# Plant_Area Stats
summary(aov(DO_Traits$Plant_Area~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Plant_Area~DO_Traits$Treatment))
# p=0.003

summary(aov(AP_Traits$Plant_Area~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Plant_Area~AP_Traits$Treatment))
# p=0.049

summary(aov(SM_Traits$Plant_Area~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Plant_Area~SM_Traits$Treatment))
# p=0.894

summary(aov(AG_Traits$Plant_Area~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Plant_Area~AG_Traits$Treatment))
# p=0.934

summary(aov(SN_Traits$Plant_Area~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Plant_Area~SN_Traits$Treatment))
# p=0.885

#had to change to take off scientific formatting.. .didnt work for graph
format(DO_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(AP_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(SM_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(AG_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(SN_Traits$Plant_Area, scientific = FALSE, big.mark = ',')

format(DO_N0_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(DO_N10_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(AP_N0_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(AP_N10_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(SM_N0_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(SM_N10_Traits$Plant_Area, scientific = FALSE, big.mark = ',')

format(AG_N0_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(AG_N10_Traits$Plant_Area, scientific = FALSE, big.mark = ',')

format(SN_N0_Traits$Plant_Area, scientific = FALSE, big.mark = ',')
format(SN_N10_Traits$Plant_Area, scientific = FALSE, big.mark = ',')


### Plant Area Figure ###

boxplot(DO_N0_Traits$Plant_Area, DO_N10_Traits$Plant_Area,
        AP_N0_Traits$Plant_Area, AP_N10_Traits$Plant_Area,
        SM_N0_Traits$Plant_Area, SM_N10_Traits$Plant_Area,
        AG_N0_Traits$Plant_Area, AG_N10_Traits$Plant_Area,
        SN_N0_Traits$Plant_Area, SN_N10_Traits$Plant_Area,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species", 
        ylim=c(0, 98000),
        ylab="Plant Area (cm^3)",
        main= "Plant Area", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.002 *"))
text(locator(), labels = c("p=0.0003 *"))
text(locator(), labels = c("p=0.019 *"))
text(locator(), labels = c("p=0.592"))
text(locator(), labels = c("p=0.137"))






### E_Leaf_Stage  ### 
# E_Leaf_Stage stats

summary(aov(DO_Traits$E_Leaf_Stage ~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$E_Leaf_Stage ~DO_Traits$Treatment))
# p=0.004

summary(aov(AP_Traits$E_Leaf_Stage ~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$E_Leaf_Stage ~AP_Traits$Treatment))
# p=0.068

summary(aov(SM_Traits$E_Leaf_Stage~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$E_Leaf_Stage~SM_Traits$Treatment))
# p=0.985

summary(aov(AG_Traits$E_Leaf_Stage ~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$E_Leaf_Stage ~AG_Traits$Treatment))
# p=0.992

summary(aov(SN_Traits$E_Leaf_Stage ~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$E_Leaf_Stage ~SN_Traits$Treatment))
# p=0.871


## E_Leaf_Stage Figure ## 

boxplot(DO_N0_Traits$E_Leaf_Stage, DO_N10_Traits$E_Leaf_Stage,
        AP_N0_Traits$E_Leaf_Stage, AP_N10_Traits$E_Leaf_Stage,
        SM_N0_Traits$E_Leaf_Stage, SM_N10_Traits$E_Leaf_Stage,
        AG_N0_Traits$E_Leaf_Stage, AG_N10_Traits$E_Leaf_Stage,
        SN_N0_Traits$E_Leaf_Stage, SN_N10_Traits$E_Leaf_Stage,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Emerging Leaf Number",
        main= "Emerging Leaf Number", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.001 *"))
text(locator(), labels = c("p=0.019 *"))
text(locator(), labels = c("p=0.703"))
text(locator(), labels = c("p=0.586"))
text(locator(), labels = c("p=0.445"))



### S_Leaf_Stage  ### 
# S_Leaf_Stage stats
summary(aov(DO_Traits$S_Leaf_Stage ~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$S_Leaf_Stage ~DO_Traits$Treatment))
# p=0.221

summary(aov(AP_Traits$S_Leaf_Stage~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$S_Leaf_Stage~AP_Traits$Treatment))
# p=0.484

summary(aov(SM_Traits$S_Leaf_Stage~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$S_Leaf_Stage~SM_Traits$Treatment))
# p=0.881

summary(aov(AG_Traits$S_Leaf_Stage ~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$S_Leaf_Stage ~AG_Traits$Treatment))
# p=0.419

summary(aov(SN_Traits$S_Leaf_Stage~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$S_Leaf_Stage~SN_Traits$Treatment))
# p=0.992


## S_Leaf_Stage Figure ## 

boxplot(DO_N0_Traits$S_Leaf_Stage, DO_N10_Traits$S_Leaf_Stage,
        AP_N0_Traits$S_Leaf_Stage, AP_N10_Traits$S_Leaf_Stage,
        SM_N0_Traits$S_Leaf_Stage, SM_N10_Traits$S_Leaf_Stage,
        AG_N0_Traits$S_Leaf_Stage, AG_N10_Traits$S_Leaf_Stage,
        SN_N0_Traits$S_Leaf_Stage, SN_N10_Traits$S_Leaf_Stage,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylim=c(0,20),
        ylab="Senescing Leaf Number",
        main= "Senescing Leaf Number", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.078"))
text(locator(), labels = c("p=0.142"))
text(locator(), labels = c("p=0.426"))
text(locator(), labels = c("p=0.237"))
text(locator(), labels = c("p=0.790"))










### F_Leaf_Stage  ### 
# F_Leaf_Stage stats
summary(aov(DO_Traits$F_Leaf_Stage~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$F_Leaf_Stage~DO_Traits$Treatment))
# p=0.015

summary(aov(AP_Traits$F_Leaf_Stage~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$F_Leaf_Stage~AP_Traits$Treatment))
# p=0.621

summary(aov(SM_Traits$F_Leaf_Stage~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$F_Leaf_Stage~SM_Traits$Treatment))
# p=0.849

summary(aov(AG_Traits$F_Leaf_Stage~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$F_Leaf_Stage~AG_Traits$Treatment))
# p=1.000

summary(aov(SN_Traits$F_Leaf_Stage~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$F_Leaf_Stage~SN_Traits$Treatment))
# p=0.845


## F_Leaf_Stage Figure ## 

boxplot(DO_N0_Traits$F_Leaf_Stage, DO_N10_Traits$F_Leaf_Stage,
        AP_N0_Traits$F_Leaf_Stage, AP_N10_Traits$F_Leaf_Stage,
        SM_N0_Traits$F_Leaf_Stage, SM_N10_Traits$F_Leaf_Stage,
        AG_N0_Traits$F_Leaf_Stage, AG_N10_Traits$F_Leaf_Stage,
        SN_N0_Traits$F_Leaf_Stage, SN_N10_Traits$F_Leaf_Stage,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Fully Emerged Leaf Number",
        main= "Fully Emerged Leaf Number", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.006 *"))
text(locator(), labels = c("p=0.172"))
text(locator(), labels = c("p=0.442"))
text(locator(), labels = c("p=0.259"))
text(locator(), labels = c("p=0.464"))







###  Percent_N_AbG  ### 
#  Percent_N_AbG stats
summary(aov(DO_Traits$Percent_N_AbG~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Percent_N_AbG~DO_Traits$Treatment))
# p<2e-16

summary(aov(AP_Traits$Percent_N_AbG~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Percent_N_AbG~AP_Traits$Treatment))
# p=0.004

summary(aov(SM_Traits$Percent_N_AbG~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Percent_N_AbG~SM_Traits$Treatment))
# p=0.199

summary(aov(AG_Traits$Percent_N_AbG~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Percent_N_AbG~AG_Traits$Treatment))
# p=0.5448965

summary(aov(SN_Traits$Percent_N_AbG~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Percent_N_AbG~SN_Traits$Treatment))
# p=0.0006937


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Percent_N_AbG, DO_N10_Traits$Percent_N_AbG,
        AP_N0_Traits$Percent_N_AbG, AP_N10_Traits$Percent_N_AbG,
        SM_N0_Traits$Percent_N_AbG, SM_N10_Traits$Percent_N_AbG,
        AG_N0_Traits$Percent_N_AbG, AG_N10_Traits$Percent_N_AbG,
        SN_N0_Traits$Percent_N_AbG, SN_N10_Traits$Percent_N_AbG,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylim=c(0.5, 6),
        ylab="Leaf Percent N",
        main= "Leaf Percent N", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p<2e-16 *"))
text(locator(), labels = c("p=0.002 *"))
text(locator(), labels = c("p=0.078 *"))
text(locator(), labels = c("p=0.213"))
text(locator(), labels = c("p=0.0005 *"))




# Percent_C_AbG
###  Percent_C_AbG  ### 
#  Percent_c_AbG stats
summary(aov(DO_Traits$Percent_C_AbG~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Percent_C_AbG~DO_Traits$Treatment))
# p=0.0041023

summary(aov(AP_Traits$Percent_C_AbG~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Percent_C_AbG~AP_Traits$Treatment))
# p=0.4034214

summary(aov(SM_Traits$Percent_C_AbG~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Percent_C_AbG~SM_Traits$Treatment))
# p=0.0427744

summary(aov(AG_Traits$Percent_C_AbG~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Percent_C_AbG~AG_Traits$Treatment))
# p=0.8818922

summary(aov(SN_Traits$Percent_C_AbG~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Percent_C_AbG~SN_Traits$Treatment))
# p=0.432387


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Percent_C_AbG, DO_N10_Traits$Percent_C_AbG,
        AP_N0_Traits$Percent_C_AbG, AP_N10_Traits$Percent_C_AbG,
        SM_N0_Traits$Percent_C_AbG, SM_N10_Traits$Percent_C_AbG,
        AG_N0_Traits$Percent_C_AbG, AG_N10_Traits$Percent_C_AbG,
        SN_N0_Traits$Percent_C_AbG, SN_N10_Traits$Percent_C_AbG,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Leaf Percent C",
        main= "Leaf Percent C", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.002 *"))
text(locator(), labels = c("p=0.125"))
text(locator(), labels = c("p=0.014 *"))
text(locator(), labels = c("p=0.453"))
text(locator(), labels = c("p=0.145"))



###  SLA  ### 
#  SLA stats
summary(aov(DO_Traits$SLA~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$SLA~DO_Traits$Treatment))
# p=0.8040425

summary(aov(AP_Traits$SLA~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$SLA~AP_Traits$Treatment))
# p=0.9715708

summary(aov(SM_Traits$SLA~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$SLA~SM_Traits$Treatment))
# p=0.5087415

summary(aov(AG_Traits$SLA~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$SLA~AG_Traits$Treatment))
# p=0.0188641

summary(aov(SN_Traits$SLA~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$SLA~SN_Traits$Treatment))
# p=0.432387


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$SLA, DO_N10_Traits$SLA,
        AP_N0_Traits$SLA, AP_N10_Traits$SLA,
        SM_N0_Traits$SLA, SM_N10_Traits$SLA,
        AG_N0_Traits$SLA, AG_N10_Traits$SLA,
        SN_N0_Traits$SLA, SN_N10_Traits$SLA,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="SLA",
        main= "SLA (Specific Leaf Area)", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.473"))
text(locator(), labels = c("p=0.850"))
text(locator(), labels = c("p=0.176"))
text(locator(), labels = c("p=0.003 *"))
text(locator(), labels = c("p=0.363"))





###  SRL  ### 
#  SRL stats
summary(aov(DO_Traits$SRL~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$SRL~DO_Traits$Treatment))
# p=0.8387802

summary(aov(AP_Traits$SRL~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$SRL~AP_Traits$Treatment))
# p=0.6803643

summary(aov(SM_Traits$SRL~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$SRL~SM_Traits$Treatment))
# p=0.9549019

summary(aov(AG_Traits$SRL~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$SRL~AG_Traits$Treatment))
# p=0.8301265

summary(aov(SN_Traits$SRL~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$SRL~SN_Traits$Treatment))
# p=0.9393496


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$SRL, DO_N10_Traits$SRL,
        AP_N0_Traits$SRL, AP_N10_Traits$SRL,
        SM_N0_Traits$SRL, SM_N10_Traits$SRL,
        AG_N0_Traits$SRL, AG_N10_Traits$SRL,
        SN_N0_Traits$SRL, SN_N10_Traits$SRL,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="SRL",
        main= "SRL (Specific Root Length)", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.412"))
text(locator(), labels = c("p=0.241"))
text(locator(), labels = c("p=0.656"))
text(locator(), labels = c("p=0.365"))
text(locator(), labels = c("p=0.600"))




###  Leaf thickness  ### 
#  Leaf thickness stats
summary(aov(DO_Traits$Leaf_Thickness_cm~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Leaf_Thickness_cm~DO_Traits$Treatment))
# p=0.8382774

summary(aov(AP_Traits$Leaf_Thickness_cm~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Leaf_Thickness_cm~AP_Traits$Treatment))
# p=0.1220146

summary(aov(SM_Traits$Leaf_Thickness_cm~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Leaf_Thickness_cm~SM_Traits$Treatment))
# p=0.1856080

summary(aov(AG_Traits$Leaf_Thickness_cm~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Leaf_Thickness_cm~AG_Traits$Treatment))
# p=0.9760295

summary(aov(SN_Traits$Leaf_Thickness_cm~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Leaf_Thickness_cm~SN_Traits$Treatment))
# p=0.1272257


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Leaf_Thickness_cm, DO_N10_Traits$Leaf_Thickness_cm,
        AP_N0_Traits$Leaf_Thickness_cm, AP_N10_Traits$Leaf_Thickness_cm,
        SM_N0_Traits$Leaf_Thickness_cm, SM_N10_Traits$Leaf_Thickness_cm,
        AG_N0_Traits$Leaf_Thickness_cm, AG_N10_Traits$Leaf_Thickness_cm,
        SN_N0_Traits$Leaf_Thickness_cm, SN_N10_Traits$Leaf_Thickness_cm,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0.005, 0.07),
        xlab="Species",
        ylab="Leaf Thickness (cm)",
        main= "Leaf Thickness", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.412"))
text(locator(), labels = c("p=0.013 *"))
text(locator(), labels = c("p=0.067"))
text(locator(), labels = c("p=0.695"))
text(locator(), labels = c("p=0.052"))



###  Root_Diam_Max  ### 
#  Root_Diam_Max stats
summary(aov(DO_Traits$Root_Diam_Max~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Root_Diam_Max~DO_Traits$Treatment))
# p=0.7970638

summary(aov(AP_Traits$Root_Diam_Max~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Root_Diam_Max~AP_Traits$Treatment))
# p=0.999

summary(aov(SM_Traits$Root_Diam_Max~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Root_Diam_Max~SM_Traits$Treatment))
# p=0.998

summary(aov(AG_Traits$Root_Diam_Max~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Root_Diam_Max~AG_Traits$Treatment))
# p=0.469

summary(aov(SN_Traits$Root_Diam_Max~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Root_Diam_Max~SN_Traits$Treatment))
# p=0.1272257


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Root_Diam_Max, DO_N10_Traits$Root_Diam_Max,
        AP_N0_Traits$Root_Diam_Max, AP_N10_Traits$Root_Diam_Max,
        SM_N0_Traits$Root_Diam_Max, SM_N10_Traits$Root_Diam_Max,
        AG_N0_Traits$Root_Diam_Max, AG_N10_Traits$Root_Diam_Max,
        SN_N0_Traits$Root_Diam_Max, SN_N10_Traits$Root_Diam_Max,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Max Root Diamter (cm)",
        main= "Maximum Root Diamter", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.169"))
text(locator(), labels = c("p=0.993"))
text(locator(), labels = c("p=0.878"))
text(locator(), labels = c("p=0.170"))
text(locator(), labels = c("p=0.886"))


#### Root_Diam_Mean ####
#  Root_Diam_Mean stats
summary(aov(DO_Traits$Root_Diam_Mean~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Root_Diam_Mean~DO_Traits$Treatment))
# p=0.998

summary(aov(AP_Traits$Root_Diam_Mean~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Root_Diam_Mean~AP_Traits$Treatment))
# p=0.669

summary(aov(SM_Traits$Root_Diam_Mean~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Root_Diam_Mean~SM_Traits$Treatment))
# p=0.976

summary(aov(AG_Traits$Root_Diam_Mean~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Root_Diam_Mean~AG_Traits$Treatment))
# p=0.182

summary(aov(SN_Traits$Root_Diam_Mean~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Root_Diam_Mean~SN_Traits$Treatment))
# p=0.857


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Root_Diam_Mean, DO_N10_Traits$Root_Diam_Mean,
        AP_N0_Traits$Root_Diam_Mean, AP_N10_Traits$Root_Diam_Mean,
        SM_N0_Traits$Root_Diam_Mean, SM_N10_Traits$Root_Diam_Mean,
        AG_N0_Traits$Root_Diam_Mean, AG_N10_Traits$Root_Diam_Mean,
        SN_N0_Traits$Root_Diam_Mean, SN_N10_Traits$Root_Diam_Mean,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Mean Root Diamter (cm)",
        main= "Mean Root Diameter", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.886"))
text(locator(), labels = c("p=0.295"))
text(locator(), labels = c("p=0.698"))
text(locator(), labels = c("p=0.050"))
text(locator(), labels = c("p=0.483"))




#### Percent_C_BeG ####
#  Percent_C_BeG stats
summary(aov(DO_Traits$Percent_C_BeG~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Percent_C_BeG~DO_Traits$Treatment))
# p=0.995

summary(aov(AP_Traits$Percent_C_BeG~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Percent_C_BeG~AP_Traits$Treatment))
# p=0.448

summary(aov(SM_Traits$Percent_C_BeG~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Percent_C_BeG~SM_Traits$Treatment))
# p=0.998

summary(aov(AG_Traits$Percent_C_BeG~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Percent_C_BeG~AG_Traits$Treatment))
# p=0.589

summary(aov(SN_Traits$Percent_C_BeG~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Percent_C_BeG~SN_Traits$Treatment))
# p=0.999


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Percent_C_BeG, DO_N10_Traits$Percent_C_BeG,
        AP_N0_Traits$Percent_C_BeG, AP_N10_Traits$Percent_C_BeG,
        SM_N0_Traits$Percent_C_BeG, SM_N10_Traits$Percent_C_BeG,
        AG_N0_Traits$Percent_C_BeG, AG_N10_Traits$Percent_C_BeG,
        SN_N0_Traits$Percent_C_BeG, SN_N10_Traits$Percent_C_BeG,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        xlab="Species",
        ylab="Root Percent C",
        main= "Root Percent C", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p=0.826"))
text(locator(), labels = c("p=0.154"))
text(locator(), labels = c("p=0.889"))
text(locator(), labels = c("p=0.192"))
text(locator(), labels = c("p=0.891"))


#### Percent_N_BeG ####
#  Percent_N_BeG stats
summary(aov(DO_Traits$Percent_N_BeG~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Percent_N_BeG~DO_Traits$Treatment))
# p=0.008

summary(aov(AP_Traits$Percent_N_BeG~AP_Traits$Treatment))
TukeyHSD(aov(AP_Traits$Percent_N_BeG~AP_Traits$Treatment))
# p=0.000007

summary(aov(SM_Traits$Percent_N_BeG~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Percent_N_BeG~SM_Traits$Treatment))
# p=0.044

summary(aov(AG_Traits$Percent_N_BeG~AG_Traits$Treatment))
TukeyHSD(aov(AG_Traits$Percent_N_BeG~AG_Traits$Treatment))
# p=0.00002

summary(aov(SN_Traits$Percent_N_BeG~SN_Traits$Treatment))
TukeyHSD(aov(SN_Traits$Percent_N_BeG~SN_Traits$Treatment))
# p=0.000001


## Percent_N_AbG Figure ## 

boxplot(DO_N0_Traits$Percent_N_BeG, DO_N10_Traits$Percent_N_BeG,
        AP_N0_Traits$Percent_N_BeG, AP_N10_Traits$Percent_N_BeG,
        SM_N0_Traits$Percent_N_BeG, SM_N10_Traits$Percent_N_BeG,
        AG_N0_Traits$Percent_N_BeG, AG_N10_Traits$Percent_N_BeG,
        SN_N0_Traits$Percent_N_BeG, SN_N10_Traits$Percent_N_BeG,
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0, 2.5),
        xlab="Species",
        ylab="Root Percent N",
        main= "Root Percent N", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p<0.001 *"))
text(locator(), labels = c("p<0.001 *"))
text(locator(), labels = c("p=0.014 *"))
text(locator(), labels = c("p<0.001 *"))
text(locator(), labels = c("p<0.001 *"))










### Flower_Number ### 
# Flower_Number Stats
summary(aov(DO_Traits$Flower_Number~DO_Traits$Treatment))
TukeyHSD(aov(DO_Traits$Flower_Number~DO_Traits$Treatment))
# p=0.984

summary(aov(SM_Traits$Flower_Number~SM_Traits$Treatment))
TukeyHSD(aov(SM_Traits$Flower_Number~SM_Traits$Treatment))
# p=0.998

## Flower Number Figure ## 
View(DO_N0_Traits$Flower_Number)

boxplot(DO_N0_Traits$Flower_Number, DO_N10_Traits$Flower_Number,
        SM_N0_Traits$Flower_Number, SM_N10_Traits$Flower_Number,
        at = c(1.15, 2, 4.15, 5),
        names= c(" ", "DO", " ", "SM"),
        xlab="Species",
        ylim=c(0,4),
        ylab="Flower Number",
        main= "Flower Number in N Additions", 
        pch=20,
        col= pal2)
abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))








DO_N0 <- which(DO_Traits$Treatment == "Control")
DO_N10 <- which(DO_Traits$Treatment == "10")
AP_N0 <- which(AP_Traits$Treatment == "Control")
AP_N10 <- which(AP_Traits$Treatment == "10")
SM_N0 <- which(SM_Traits$Treatment == "Control")
SM_N10 <- which(SM_Traits$Treatment == "10")
AG_N0 <- which(AG_Traits$Treatment == "Control")
AG_N10 <- which(AG_Traits$Treatment == "10")
SN_N0 <- which(SN_Traits$Treatment == "Control")
SN_N10 <- which(SN_Traits$Treatment == "10")






# BoxPlots just for Change 
#sort out Change data by block by species 
#DO
DO_ChA_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="A")
DO_ChB_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="B")
DO_ChC_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="C")
DO_ChD_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="D")
DO_ChE_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="E")
DO_ChF_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="F")

#AP
AP_ChA_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="A")
AP_ChB_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="B")
AP_ChC_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="C")
AP_ChD_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="D")
AP_ChE_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="E")
AP_ChF_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="F")


#SM
SM_ChA_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="A")
SM_ChB_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="B")
SM_ChC_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="C")
SM_ChD_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="D")
SM_ChE_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="E")
SM_ChF_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="F")

#SN
SN_ChA_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="A")
SN_ChB_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="B")
SN_ChC_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="C")
SN_ChD_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="D")
SN_ChE_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="E")
SN_ChF_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="F")

#AG
AG_ChA_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="A")
AG_ChB_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="B")
AG_ChC_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="C")
AG_ChD_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="D")
AG_ChE_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="E")
AG_ChF_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Block=="F")




#sort out Treatments 


DO_A_N0 <- which(DO_Ch_Traits$Treatment == "Control") 




DO_N10 <- which(DO_Traits$Treatment == "10")
AP_N0 <- which(AP_Traits$Treatment == "Control")
AP_N10 <- which(AP_Traits$Treatment == "10")
SM_N0 <- which(SM_Traits$Treatment == "Control")
SM_N10 <- which(SM_Traits$Treatment == "10")
AG_N0 <- which(AG_Traits$Treatment == "Control")
AG_N10 <- which(AG_Traits$Treatment == "10")
SN_N0 <- which(SN_Traits$Treatment == "Control")
SN_N10 <- which(SN_Traits$Treatment == "10")


#Group by Block 

# plant height 

boxplot(DO_Traits$SRL[DO_N0], DO_Traits$SRL[DO_N10],
        AP_Traits$SRL[AP_N0], AP_Traits$SRL[AP_N10],
        SM_Traits$SRL[SM_N0], SM_Traits$SRL[SM_N10],
        AG_Traits$SRL[AG_N0], AG_Traits$SRL[AG_N10],
        SN_Traits$SRL[SN_N0], SN_Traits$SRL[SN_N10],
        at = c(1.15, 2, 4.15, 5, 7.15, 8, 10.15, 11, 13.15, 14),
        names= cat3,
        ylim=c(0,17),
        xlab="Species", 
        ylab="SRL", 
        main= "SRL by Species in N Additions vs Control", 
        pch=20,
        col= pal2)

abline(v = c(3, 6, 9, 12), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "Treatment, 10g N"), fill = pal2, bg = "white")







# try again 

# group by species 
  #show by block 





spnames <- c(" ", " ", "   DO", " ", " ", " ", " ", " ", "   AP", " ", " ", " ", " ", " ", "SM", " ", " ", " ", " ", " ", "   AG", " ", " ", " ", " ", " ", "   SN", " ", " ", " ")
blockcol <- c("#E69F00","#800080","#009E73","#F0E442","#0072B2","#CC79A7")




boxplot(DO_ChA_Traits$SRL, DO_ChB_Traits$SRL,DO_ChC_Traits$SRL, DO_ChD_Traits$SRL, DO_ChE_Traits$SRL, DO_ChF_Traits$SRL, 
        AP_ChA_Traits$SRL, AP_ChB_Traits$SRL,AP_ChC_Traits$SRL, AP_ChD_Traits$SRL, AP_ChE_Traits$SRL, AP_ChF_Traits$SRL,
        SM_ChA_Traits$SRL, SM_ChB_Traits$SRL,SM_ChC_Traits$SRL, SM_ChD_Traits$SRL, SM_ChE_Traits$SRL, SM_ChF_Traits$SRL,
        AG_ChA_Traits$SRL, AG_ChB_Traits$SRL,AG_ChC_Traits$SRL, AG_ChD_Traits$SRL, AG_ChE_Traits$SRL, AG_ChF_Traits$SRL, 
        SN_ChA_Traits$SRL, SN_ChB_Traits$SRL,SN_ChC_Traits$SRL, SN_ChD_Traits$SRL, SN_ChE_Traits$SRL, SN_ChF_Traits$SRL,
        names= spnames,
        col= blockcol,
        pch=20, 
        ylab="SRL", 
        xlab="Species",       
        main= "SRL by Species in Change Experiment")
abline(v = c(6.5, 12.5, 18.5, 24.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("A", "B", "C", "D", "E", "F"), fill = c("#E69F00","#800080","#009E73","#F0E442","#0072B2","#CC79A7"), bg = "white", title = "Block")


#         at = c(1.15, 6.8, 7.15, 13.8, 19.15, 26.8, 27.15, 33.8, 34.15,40.8),






# REDO to group by Treatment * in Change

###  FINAL BOXPLOT LOOK ###
# add asterisk and p-values

DO_Ch_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")
AP_Ch_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")
SM_Ch_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")
AG_Ch_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")
SN_Ch_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")


#DO- code for Control, 2.5, 10, 20 in Change
DO_Ch_N0_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="Control")
DO_Ch_N2_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="2.5")
DO_Ch_N10_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="10")
DO_Ch_N20_Traits <- All_Traits %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="20")

#AP- code for Control, 2.5, 10, 20 in Change
AP_Ch_N0_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="Control")
AP_Ch_N2_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="2.5")
AP_Ch_N10_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="10")
AP_Ch_N20_Traits <- All_Traits %>%
  filter(Species=="Ambrosia_psilostachya") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="20")

#SM- code for Control, 2.5, 10, 20 in Change
SM_Ch_N0_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="Control")
SM_Ch_N2_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="2.5")
SM_Ch_N10_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="10")
SM_Ch_N20_Traits <- All_Traits %>%
  filter(Species=="Solidago_missouriensis") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="20")

#AG- code for Control, 2.5, 10, 20 in Change
AG_Ch_N0_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="Control")
AG_Ch_N2_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="2.5")
AG_Ch_N10_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="10")
AG_Ch_N20_Traits <- All_Traits %>%
  filter(Species=="Andropogon_gerardii") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="20")

#SN- code for Control, 2.5, 10, 20 in Change
SN_Ch_N0_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="Control")
SN_Ch_N2_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="2.5")
SN_Ch_N10_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="10")
SN_Ch_N20_Traits <- All_Traits %>%
  filter(Species=="Sorghastrum_nutans") %>%
  filter(Experiment=="Change")%>%
  filter(Treatment=="20")

# Redwoods palette: 
# gray: #BDB2A7, Control
# purple: #6E687E 2.5N
# green: #769370, 10N
# yellow: #F1C646, 20N

trtcol <- c("#BDB2A7", "#6E687E", "#769370", "#F1C646")
spnames <- c(" ", "   DO", " ", " ", " ", "  AP", " ", " ", " ", "  SM", " ", " ", " ", "    AG", " ", " ", " ", "      SN", " ", " ")




### SLA Change, Statistics ###

summary(aov(DO_Ch_Traits$SLA~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$SLA~DO_Ch_Traits$Treatment))
# p= 0.222

summary(aov(AP_Ch_Traits$SLA~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$SLA~AP_Ch_Traits$Treatment))
# p= 0.518

summary(aov(SM_Ch_Traits$SLA~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$SLA~SM_Ch_Traits$Treatment))
# p= 0.357

summary(aov(AG_Ch_Traits$SLA~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$SLA~AG_Ch_Traits$Treatment))
# p= 0.169

summary(aov(SN_Ch_Traits$SLA~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$SLA~SN_Ch_Traits$Treatment))
# p= 0.203

### SLA Change, figure ###

boxplot(DO_Ch_N0_Traits$SLA, DO_Ch_N2_Traits$SLA,DO_Ch_N10_Traits$SLA, DO_Ch_N20_Traits$SLA,
        AP_Ch_N0_Traits$SLA, AP_Ch_N2_Traits$SLA,AP_Ch_N10_Traits$SLA, AP_Ch_N20_Traits$SLA,
        SM_Ch_N0_Traits$SLA, SM_Ch_N2_Traits$SLA,SM_Ch_N10_Traits$SLA, SM_Ch_N20_Traits$SLA,
        AG_Ch_N0_Traits$SLA, AG_Ch_N2_Traits$SLA,AG_Ch_N10_Traits$SLA, AG_Ch_N20_Traits$SLA,
        SN_Ch_N0_Traits$SLA, SN_Ch_N2_Traits$SLA,SN_Ch_N10_Traits$SLA, SN_Ch_N20_Traits$SLA,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="SLA", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.852"))
text(locator(), labels = c("p=0.518"))
text(locator(), labels = c("p=0.357"))
text(locator(), labels = c("p=0.169"))
text(locator(), labels = c("p=0.203"))





## SRL, Change ###
# SRL Change, Statistics # 

summary(aov(DO_Ch_Traits$SRL~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$SRL~DO_Ch_Traits$Treatment))
# p= 0.905

summary(aov(AP_Ch_Traits$SRL~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$SRL~AP_Ch_Traits$Treatment))
# p= 0.247

summary(aov(SM_Ch_Traits$SRL~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$SRL~SM_Ch_Traits$Treatment))
# p= 0.233

summary(aov(AG_Ch_Traits$SRL~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$SRL~AG_Ch_Traits$Treatment))
# p= 0.676

summary(aov(SN_Ch_Traits$SRL~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$SRL~SN_Ch_Traits$Treatment))
# p= 0.525

# SRL Change, Figure # 

boxplot(DO_Ch_N0_Traits$SRL, DO_Ch_N2_Traits$SRL,DO_Ch_N10_Traits$SRL, DO_Ch_N20_Traits$SRL,
        AP_Ch_N0_Traits$SRL, AP_Ch_N2_Traits$SRL,AP_Ch_N10_Traits$SRL, AP_Ch_N20_Traits$SRL,
        SM_Ch_N0_Traits$SRL, SM_Ch_N2_Traits$SRL,SM_Ch_N10_Traits$SRL, SM_Ch_N20_Traits$SRL,
        AG_Ch_N0_Traits$SRL, AG_Ch_N2_Traits$SRL,AG_Ch_N10_Traits$SRL, AG_Ch_N20_Traits$SRL,
        SN_Ch_N0_Traits$SRL, SN_Ch_N2_Traits$SRL,SN_Ch_N10_Traits$SRL, SN_Ch_N20_Traits$SRL,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="SRL", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.905"))
text(locator(), labels = c("p=0.247"))
text(locator(), labels = c("p=0.233"))
text(locator(), labels = c("p=0.676"))
text(locator(), labels = c("p=0.525"))


## Aboveground_Dry_Weight_g, Change ##
# Aboveground_Dry_Weight_g Change, Statistics # 

summary(aov(DO_Ch_Traits$Aboveground_Dry_Weight_g~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Aboveground_Dry_Weight_g~DO_Ch_Traits$Treatment))
# p= 0.163

summary(aov(AP_Ch_Traits$Aboveground_Dry_Weight_g~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Aboveground_Dry_Weight_g~AP_Ch_Traits$Treatment))
# p= 0.142

summary(aov(SM_Ch_Traits$Aboveground_Dry_Weight_g~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Aboveground_Dry_Weight_g~SM_Ch_Traits$Treatment))
# p= 0.436

summary(aov(AG_Ch_Traits$Aboveground_Dry_Weight_g~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Aboveground_Dry_Weight_g~AG_Ch_Traits$Treatment))
# p= 0.147

summary(aov(SN_Ch_Traits$Aboveground_Dry_Weight_g~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Aboveground_Dry_Weight_g~SN_Ch_Traits$Treatment))
# p= 0.167

# Aboveground_Dry_Weight_g Change, Figure # 

boxplot(DO_Ch_N0_Traits$Aboveground_Dry_Weight_g, DO_Ch_N2_Traits$Aboveground_Dry_Weight_g,DO_Ch_N10_Traits$Aboveground_Dry_Weight_g, DO_Ch_N20_Traits$Aboveground_Dry_Weight_g,
        AP_Ch_N0_Traits$Aboveground_Dry_Weight_g, AP_Ch_N2_Traits$Aboveground_Dry_Weight_g,AP_Ch_N10_Traits$Aboveground_Dry_Weight_g, AP_Ch_N20_Traits$Aboveground_Dry_Weight_g,
        SM_Ch_N0_Traits$Aboveground_Dry_Weight_g, SM_Ch_N2_Traits$Aboveground_Dry_Weight_g,SM_Ch_N10_Traits$Aboveground_Dry_Weight_g, SM_Ch_N20_Traits$Aboveground_Dry_Weight_g,
        AG_Ch_N0_Traits$Aboveground_Dry_Weight_g, AG_Ch_N2_Traits$Aboveground_Dry_Weight_g,AG_Ch_N10_Traits$Aboveground_Dry_Weight_g, AG_Ch_N20_Traits$Aboveground_Dry_Weight_g,
        SN_Ch_N0_Traits$Aboveground_Dry_Weight_g, SN_Ch_N2_Traits$Aboveground_Dry_Weight_g,SN_Ch_N10_Traits$Aboveground_Dry_Weight_g, SN_Ch_N20_Traits$Aboveground_Dry_Weight_g,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylim=c(-0.5,11.5),
        ylab="Aboveground Dry Mass (g)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.163"))
text(locator(), labels = c("p=0.142"))
text(locator(), labels = c("p=0.436"))
text(locator(), labels = c("p=0.147"))
text(locator(), labels = c("p=0.167"))



## Leaf_Number, Change ##
# Leaf_Number Change, Statistics # 
summary(aov(DO_Ch_Traits$Leaf_Number~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Leaf_Number~DO_Ch_Traits$Treatment))
# p= 0.19

summary(aov(AP_Ch_Traits$Leaf_Number~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Leaf_Number~AP_Ch_Traits$Treatment))
# p= 0.0002

summary(aov(SM_Ch_Traits$Leaf_Number~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Leaf_Number~SM_Ch_Traits$Treatment))
# p=0.637

summary(aov(AG_Ch_Traits$Leaf_Number~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Leaf_Number~AG_Ch_Traits$Treatment))
# p=0.0437

summary(aov(SN_Ch_Traits$Leaf_Number~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Leaf_Number~SN_Ch_Traits$Treatment))
# p= 0.718

## Leaf_Number Change, Figure ## 
boxplot(DO_Ch_N0_Traits$Leaf_Number, DO_Ch_N2_Traits$Leaf_Number,DO_Ch_N10_Traits$Leaf_Number, DO_Ch_N20_Traits$Leaf_Number,
        AP_Ch_N0_Traits$Leaf_Number, AP_Ch_N2_Traits$Leaf_Number,AP_Ch_N10_Traits$Leaf_Number, AP_Ch_N20_Traits$Leaf_Number,
        SM_Ch_N0_Traits$Leaf_Number, SM_Ch_N2_Traits$Leaf_Number,SM_Ch_N10_Traits$Leaf_Number, SM_Ch_N20_Traits$Leaf_Number,
        AG_Ch_N0_Traits$Leaf_Number, AG_Ch_N2_Traits$Leaf_Number,AG_Ch_N10_Traits$Leaf_Number, AG_Ch_N20_Traits$Leaf_Number,
        SN_Ch_N0_Traits$Leaf_Number, SN_Ch_N2_Traits$Leaf_Number,SN_Ch_N10_Traits$Leaf_Number, SN_Ch_N20_Traits$Leaf_Number,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        ylim=c(0, 60),
        pch=20,
        ylab="Leaf Number", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.190"))
text(locator(), labels = c("p=0.0002 *"))
text(locator(), labels = c("p=0.637"))
text(locator(), labels = c("p=0.043 *"))
text(locator(), labels = c("p=0.718"))



## S_Leaf_Stage, Change ##
# S_Leaf_Stage Change, Statistics # 
summary(aov(DO_Ch_Traits$S_Leaf_Stage~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$S_Leaf_Stage~DO_Ch_Traits$Treatment))
# p= 0.189

summary(aov(AP_Ch_Traits$S_Leaf_Stage~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$S_Leaf_Stage~AP_Ch_Traits$Treatment))
# p= 0.695

summary(aov(SM_Ch_Traits$S_Leaf_Stage~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$S_Leaf_Stage~SM_Ch_Traits$Treatment))
# p=0.892

summary(aov(AG_Ch_Traits$S_Leaf_Stage~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$S_Leaf_Stage~AG_Ch_Traits$Treatment))
# p=0.016

summary(aov(SN_Ch_Traits$S_Leaf_Stage~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$S_Leaf_Stage~SN_Ch_Traits$Treatment))
# p= 0.983

## S_Leaf_Stage Change, Figure ## 

boxplot(DO_Ch_N0_Traits$S_Leaf_Stage, DO_Ch_N2_Traits$S_Leaf_Stage,DO_Ch_N10_Traits$S_Leaf_Stage, DO_Ch_N20_Traits$S_Leaf_Stage,
        AP_Ch_N0_Traits$S_Leaf_Stage, AP_Ch_N2_Traits$S_Leaf_Stage,AP_Ch_N10_Traits$S_Leaf_Stage, AP_Ch_N20_Traits$S_Leaf_Stage,
        SM_Ch_N0_Traits$S_Leaf_Stage, SM_Ch_N2_Traits$S_Leaf_Stage,SM_Ch_N10_Traits$S_Leaf_Stage, SM_Ch_N20_Traits$S_Leaf_Stage,
        AG_Ch_N0_Traits$S_Leaf_Stage, AG_Ch_N2_Traits$S_Leaf_Stage,AG_Ch_N10_Traits$S_Leaf_Stage, AG_Ch_N20_Traits$S_Leaf_Stage,
        SN_Ch_N0_Traits$S_Leaf_Stage, SN_Ch_N2_Traits$S_Leaf_Stage,SN_Ch_N10_Traits$S_Leaf_Stage, SN_Ch_N20_Traits$S_Leaf_Stage,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylab="Senescing Leaf Number", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.189"))
text(locator(), labels = c("p=0.695"))
text(locator(), labels = c("p=0.892"))
text(locator(), labels = c("p=0.016 *"))
text(locator(), labels = c("p=0.983"))



## E_Leaf_Stage, Change ##
# E_Leaf_Stage Change, Statistics # 
summary(aov(DO_Ch_Traits$E_Leaf_Stage~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$E_Leaf_Stage~DO_Ch_Traits$Treatment))
# p= 0.1

summary(aov(AP_Ch_Traits$E_Leaf_Stage~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$E_Leaf_Stage~AP_Ch_Traits$Treatment))
# p= 0.0004

summary(aov(SM_Ch_Traits$E_Leaf_Stage~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$E_Leaf_Stage~SM_Ch_Traits$Treatment))
# p= 0.302

summary(aov(AG_Ch_Traits$E_Leaf_Stage~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$E_Leaf_Stage~AG_Ch_Traits$Treatment))
# p=0.0559

summary(aov(SN_Ch_Traits$E_Leaf_Stage~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$E_Leaf_Stage~SN_Ch_Traits$Treatment))
# p= 0.353

## E_Leaf_Stage Change, Figure ## 

boxplot(DO_Ch_N0_Traits$E_Leaf_Stage, DO_Ch_N2_Traits$E_Leaf_Stage,DO_Ch_N10_Traits$E_Leaf_Stage, DO_Ch_N20_Traits$E_Leaf_Stage,
        AP_Ch_N0_Traits$E_Leaf_Stage, AP_Ch_N2_Traits$E_Leaf_Stage,AP_Ch_N10_Traits$E_Leaf_Stage, AP_Ch_N20_Traits$E_Leaf_Stage,
        SM_Ch_N0_Traits$E_Leaf_Stage, SM_Ch_N2_Traits$E_Leaf_Stage,SM_Ch_N10_Traits$E_Leaf_Stage, SM_Ch_N20_Traits$E_Leaf_Stage,
        AG_Ch_N0_Traits$E_Leaf_Stage, AG_Ch_N2_Traits$E_Leaf_Stage,AG_Ch_N10_Traits$E_Leaf_Stage, AG_Ch_N20_Traits$E_Leaf_Stage,
        SN_Ch_N0_Traits$E_Leaf_Stage, SN_Ch_N2_Traits$E_Leaf_Stage,SN_Ch_N10_Traits$E_Leaf_Stage, SN_Ch_N20_Traits$E_Leaf_Stage,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylab="Emerging Leaf Number", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.100"))
text(locator(), labels = c("p=0.0004 *"))
text(locator(), labels = c("p=0.302"))
text(locator(), labels = c("p=0.0559"))
text(locator(), labels = c("p=0.353"))



## F_Leaf_Stage, Change ##
# F_Leaf_Stage Change, Statistics # 
summary(aov(DO_Ch_Traits$F_Leaf_Stage~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$F_Leaf_Stage~DO_Ch_Traits$Treatment))
# p= 0.372

summary(aov(AP_Ch_Traits$F_Leaf_Stage~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$F_Leaf_Stage~AP_Ch_Traits$Treatment))
# p= 0.003

summary(aov(SM_Ch_Traits$F_Leaf_Stage~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$F_Leaf_Stage~SM_Ch_Traits$Treatment))
# p= 0.937

summary(aov(AG_Ch_Traits$F_Leaf_Stage~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$F_Leaf_Stage~AG_Ch_Traits$Treatment))
# p=0.187

summary(aov(SN_Ch_Traits$F_Leaf_Stage~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$F_Leaf_Stage~SN_Ch_Traits$Treatment))
# p= 0.446

## F_Leaf_Stage Change, Figure ## 

boxplot(DO_Ch_N0_Traits$F_Leaf_Stage, DO_Ch_N2_Traits$F_Leaf_Stage,DO_Ch_N10_Traits$F_Leaf_Stage, DO_Ch_N20_Traits$F_Leaf_Stage,
        AP_Ch_N0_Traits$F_Leaf_Stage, AP_Ch_N2_Traits$F_Leaf_Stage,AP_Ch_N10_Traits$F_Leaf_Stage, AP_Ch_N20_Traits$F_Leaf_Stage,
        SM_Ch_N0_Traits$F_Leaf_Stage, SM_Ch_N2_Traits$F_Leaf_Stage,SM_Ch_N10_Traits$F_Leaf_Stage, SM_Ch_N20_Traits$F_Leaf_Stage,
        AG_Ch_N0_Traits$F_Leaf_Stage, AG_Ch_N2_Traits$F_Leaf_Stage,AG_Ch_N10_Traits$F_Leaf_Stage, AG_Ch_N20_Traits$F_Leaf_Stage,
        SN_Ch_N0_Traits$F_Leaf_Stage, SN_Ch_N2_Traits$F_Leaf_Stage,SN_Ch_N10_Traits$F_Leaf_Stage, SN_Ch_N20_Traits$F_Leaf_Stage,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylab="Fully Emerged Leaf Number", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.372"))
text(locator(), labels = c("p=0.003 *"))
text(locator(), labels = c("p=0.937"))
text(locator(), labels = c("p=0.187"))
text(locator(), labels = c("p=0.446"))



## Flower_Number, Change ##
# Flower_Number Change, Statistics # 
summary(aov(DO_Ch_Traits$Flower_Number~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Flower_Number~DO_Ch_Traits$Treatment))
# p= 0.914

summary(aov(SM_Ch_Traits$Flower_Number~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Flower_Number~SM_Ch_Traits$Treatment))
# p= 0.087

## Flower_Number Change, Figure ## 

boxplot(DO_Ch_N0_Traits$Flower_Number, DO_Ch_N2_Traits$Flower_Number,DO_Ch_N10_Traits$Flower_Number, DO_Ch_N20_Traits$Flower_Number,
        SM_Ch_N0_Traits$Flower_Number, SM_Ch_N2_Traits$Flower_Number,SM_Ch_N10_Traits$Flower_Number, SM_Ch_N20_Traits$Flower_Number,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5,7.5, 8.5, 9.5),
        col= trtcol,
        pch=20,
        ylab="Flower Number", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))
text(locator(), labels = c("p="))


## Plant_Area, Change ##
# Plant_Area Change, Statistics # 
summary(aov(DO_Ch_Traits$Plant_Area~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Plant_Area~DO_Ch_Traits$Treatment))
# p= 0.27

summary(aov(AP_Ch_Traits$Plant_Area~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Plant_Area~AP_Ch_Traits$Treatment))
# p= 0.0005

summary(aov(SM_Ch_Traits$Plant_Area~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Plant_Area~SM_Ch_Traits$Treatment))
# p= 0.663

summary(aov(AG_Ch_Traits$Plant_Area~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Plant_Area~AG_Ch_Traits$Treatment))
# p=0.703

summary(aov(SN_Ch_Traits$Plant_Area~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Plant_Area~SN_Ch_Traits$Treatment))
# p=0.596

## Plant_Area Change, Figure ## 

boxplot(DO_Ch_N0_Traits$Plant_Area, DO_Ch_N2_Traits$Plant_Area,DO_Ch_N10_Traits$Plant_Area, DO_Ch_N20_Traits$Plant_Area,
        AP_Ch_N0_Traits$Plant_Area, AP_Ch_N2_Traits$Plant_Area,AP_Ch_N10_Traits$Plant_Area, AP_Ch_N20_Traits$Plant_Area,
        SM_Ch_N0_Traits$Plant_Area, SM_Ch_N2_Traits$Plant_Area,SM_Ch_N10_Traits$Plant_Area, SM_Ch_N20_Traits$Plant_Area,
        AG_Ch_N0_Traits$Plant_Area, AG_Ch_N2_Traits$Plant_Area,AG_Ch_N10_Traits$Plant_Area, AG_Ch_N20_Traits$Plant_Area,
        SN_Ch_N0_Traits$Plant_Area, SN_Ch_N2_Traits$Plant_Area,SN_Ch_N10_Traits$Plant_Area, SN_Ch_N20_Traits$Plant_Area,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        ylim=c(0, 95000),
        pch=20,
        ylab="Plant Area (cm^2)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.270"))
text(locator(), labels = c("p=0.0005 *"))
text(locator(), labels = c("p=0.663"))
text(locator(), labels = c("p=0.703"))
text(locator(), labels = c("p=0.596"))


## Plant_Height_cm, Change ##
# Plant_Height_cm Change, Statistics # 
summary(aov(DO_Ch_Traits$Plant_Height_cm~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Plant_Height_cm~DO_Ch_Traits$Treatment))
# p= 0.337

summary(aov(AP_Ch_Traits$Plant_Height_cm~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Plant_Height_cm~AP_Ch_Traits$Treatment))
# p= 0.015

summary(aov(SM_Ch_Traits$Plant_Height_cm~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Plant_Height_cm~SM_Ch_Traits$Treatment))
# p= 0.464

summary(aov(AG_Ch_Traits$Plant_Height_cm~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Plant_Height_cm~AG_Ch_Traits$Treatment))
# p=0.429

summary(aov(SN_Ch_Traits$Plant_Height_cm~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Plant_Height_cm~SN_Ch_Traits$Treatment))
# p=0.137

## Plant_Height_cm Change, Figure ## 
boxplot(DO_Ch_N0_Traits$Plant_Height_cm, DO_Ch_N2_Traits$Plant_Height_cm,DO_Ch_N10_Traits$Plant_Height_cm, DO_Ch_N20_Traits$Plant_Height_cm,
        AP_Ch_N0_Traits$Plant_Height_cm, AP_Ch_N2_Traits$Plant_Height_cm,AP_Ch_N10_Traits$Plant_Height_cm, AP_Ch_N20_Traits$Plant_Height_cm,
        SM_Ch_N0_Traits$Plant_Height_cm, SM_Ch_N2_Traits$Plant_Height_cm,SM_Ch_N10_Traits$Plant_Height_cm, SM_Ch_N20_Traits$Plant_Height_cm,
        AG_Ch_N0_Traits$Plant_Height_cm, AG_Ch_N2_Traits$Plant_Height_cm,AG_Ch_N10_Traits$Plant_Height_cm, AG_Ch_N20_Traits$Plant_Height_cm,
        SN_Ch_N0_Traits$Plant_Height_cm, SN_Ch_N2_Traits$Plant_Height_cm,SN_Ch_N10_Traits$Plant_Height_cm, SN_Ch_N20_Traits$Plant_Height_cm,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylab="Plant Height (cm)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topleft", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.337"))
text(locator(), labels = c("p=0.015 *"))
text(locator(), labels = c("p=0.464"))
text(locator(), labels = c("p=0.429"))
text(locator(), labels = c("p=0.137"))


## Focal_Root_Weight_g, Change ##
# Focal_Root_Weight_g Change, Statistics # 
summary(aov(DO_Ch_Traits$Focal_Root_Weight_g~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Focal_Root_Weight_g~DO_Ch_Traits$Treatment))
# p=0.988

summary(aov(AP_Ch_Traits$Focal_Root_Weight_g~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Focal_Root_Weight_g~AP_Ch_Traits$Treatment))
# p= 0.044

summary(aov(SM_Ch_Traits$Focal_Root_Weight_g~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Focal_Root_Weight_g~SM_Ch_Traits$Treatment))
# p= 0.655

summary(aov(AG_Ch_Traits$Focal_Root_Weight_g~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Focal_Root_Weight_g~AG_Ch_Traits$Treatment))
# p=0.200

summary(aov(SN_Ch_Traits$Focal_Root_Weight_g~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Focal_Root_Weight_g~SN_Ch_Traits$Treatment))
# p=0.773

## Focal_Root_Weight_g Change, Figure ## 
boxplot(DO_Ch_N0_Traits$Focal_Root_Weight_g, DO_Ch_N2_Traits$Focal_Root_Weight_g,DO_Ch_N10_Traits$Focal_Root_Weight_g, DO_Ch_N20_Traits$Focal_Root_Weight_g,
        AP_Ch_N0_Traits$Focal_Root_Weight_g, AP_Ch_N2_Traits$Focal_Root_Weight_g,AP_Ch_N10_Traits$Focal_Root_Weight_g, AP_Ch_N20_Traits$Focal_Root_Weight_g,
        SM_Ch_N0_Traits$Focal_Root_Weight_g, SM_Ch_N2_Traits$Focal_Root_Weight_g,SM_Ch_N10_Traits$Focal_Root_Weight_g, SM_Ch_N20_Traits$Focal_Root_Weight_g,
        AG_Ch_N0_Traits$Focal_Root_Weight_g, AG_Ch_N2_Traits$Focal_Root_Weight_g,AG_Ch_N10_Traits$Focal_Root_Weight_g, AG_Ch_N20_Traits$Focal_Root_Weight_g,
        SN_Ch_N0_Traits$Focal_Root_Weight_g, SN_Ch_N2_Traits$Focal_Root_Weight_g,SN_Ch_N10_Traits$Focal_Root_Weight_g, SN_Ch_N20_Traits$Focal_Root_Weight_g,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylab="Root Mass (g)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.988"))
text(locator(), labels = c("p=0.044 *"))
text(locator(), labels = c("p=0.655"))
text(locator(), labels = c("p=0.2"))
text(locator(), labels = c("p=0.773"))



## Total_BeG_Weight_g, Change ##
# Total_BeG_Weight_g Change, Statistics # 
summary(aov(DO_Ch_Traits$Total_BeG_Weight_g~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$Total_BeG_Weight_g~DO_Ch_Traits$Treatment))
# p=0.576

summary(aov(AP_Ch_Traits$Total_BeG_Weight_g~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$Total_BeG_Weight_g~AP_Ch_Traits$Treatment))
# p= 0.691

summary(aov(SM_Ch_Traits$Total_BeG_Weight_g~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$Total_BeG_Weight_g~SM_Ch_Traits$Treatment))
# p= 0.395

summary(aov(AG_Ch_Traits$Total_BeG_Weight_g~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$Total_BeG_Weight_g~AG_Ch_Traits$Treatment))
# p=0.89

summary(aov(SN_Ch_Traits$Total_BeG_Weight_g~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$Total_BeG_Weight_g~SN_Ch_Traits$Treatment))
# p=0.574

## Total_BeG_Weight_g Change, Figure ## 
boxplot(DO_Ch_N0_Traits$Total_BeG_Weight_g, DO_Ch_N2_Traits$Total_BeG_Weight_g,DO_Ch_N10_Traits$Total_BeG_Weight_g, DO_Ch_N20_Traits$Total_BeG_Weight_g,
        AP_Ch_N0_Traits$Total_BeG_Weight_g, AP_Ch_N2_Traits$Total_BeG_Weight_g,AP_Ch_N10_Traits$Total_BeG_Weight_g, AP_Ch_N20_Traits$Total_BeG_Weight_g,
        SM_Ch_N0_Traits$Total_BeG_Weight_g, SM_Ch_N2_Traits$Total_BeG_Weight_g,SM_Ch_N10_Traits$Total_BeG_Weight_g, SM_Ch_N20_Traits$Total_BeG_Weight_g,
        AG_Ch_N0_Traits$Total_BeG_Weight_g, AG_Ch_N2_Traits$Total_BeG_Weight_g,AG_Ch_N10_Traits$Total_BeG_Weight_g, AG_Ch_N20_Traits$Total_BeG_Weight_g,
        SN_Ch_N0_Traits$Total_BeG_Weight_g, SN_Ch_N2_Traits$Total_BeG_Weight_g,SN_Ch_N10_Traits$Total_BeG_Weight_g, SN_Ch_N20_Traits$Total_BeG_Weight_g,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20,
        ylim=c(0,12),
        ylab="Total Belowground Mass (g)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.576"))
text(locator(), labels = c("p=0.691"))
text(locator(), labels = c("p=0.395"))
text(locator(), labels = c("p=0.89"))
text(locator(), labels = c("p=0.574"))

colnames(DO_Traits)

# Leaf_Thickness_cm 

### Leaf_Thickness_cm Change, Statistics ###

summary(aov(DO_Ch_Traits$SLA~DO_Ch_Traits$Treatment))
TukeyHSD(aov(DO_Ch_Traits$SLA~DO_Ch_Traits$Treatment))
# p= 0.222

summary(aov(AP_Ch_Traits$SLA~AP_Ch_Traits$Treatment))
TukeyHSD(aov(AP_Ch_Traits$SLA~AP_Ch_Traits$Treatment))
# p= 0.518

summary(aov(SM_Ch_Traits$SLA~SM_Ch_Traits$Treatment))
TukeyHSD(aov(SM_Ch_Traits$SLA~SM_Ch_Traits$Treatment))
# p= 0.357

summary(aov(AG_Ch_Traits$SLA~AG_Ch_Traits$Treatment))
TukeyHSD(aov(AG_Ch_Traits$SLA~AG_Ch_Traits$Treatment))
# p= 0.169

summary(aov(SN_Ch_Traits$SLA~SN_Ch_Traits$Treatment))
TukeyHSD(aov(SN_Ch_Traits$SLA~SN_Ch_Traits$Treatment))
# p= 0.203

### SLA Change, figure ###

boxplot(DO_Ch_N0_Traits$SLA, DO_Ch_N2_Traits$SLA,DO_Ch_N10_Traits$SLA, DO_Ch_N20_Traits$SLA,
        AP_Ch_N0_Traits$SLA, AP_Ch_N2_Traits$SLA,AP_Ch_N10_Traits$SLA, AP_Ch_N20_Traits$SLA,
        SM_Ch_N0_Traits$SLA, SM_Ch_N2_Traits$SLA,SM_Ch_N10_Traits$SLA, SM_Ch_N20_Traits$SLA,
        AG_Ch_N0_Traits$SLA, AG_Ch_N2_Traits$SLA,AG_Ch_N10_Traits$SLA, AG_Ch_N20_Traits$SLA,
        SN_Ch_N0_Traits$SLA, SN_Ch_N2_Traits$SLA,SN_Ch_N10_Traits$SLA, SN_Ch_N20_Traits$SLA,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="SLA", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.766"))
text(locator(), labels = c("p=0.518"))
text(locator(), labels = c("p=0.357"))
text(locator(), labels = c("p=0.169"))
text(locator(), labels = c("p=0.203"))



# Leaf_Thickness_cm 

### Leaf_Thickness_cm Change, Statistics ###

summary(aov(DO_Ch_Traits$Leaf_Thickness_cm~DO_Ch_Traits$Treatment))
# p= 0.564

summary(aov(AP_Ch_Traits$Leaf_Thickness_cm~AP_Ch_Traits$Treatment))
# p= 0.105

summary(aov(SM_Ch_Traits$Leaf_Thickness_cm~SM_Ch_Traits$Treatment))
# p= 0.379

summary(aov(AG_Ch_Traits$Leaf_Thickness_cm~AG_Ch_Traits$Treatment))
# p= 0.842

summary(aov(SN_Ch_Traits$Leaf_Thickness_cm~SN_Ch_Traits$Treatment))
# p=  0.26

### Leaf_Thickness_cm Change, figure ###

boxplot(DO_Ch_N0_Traits$Leaf_Thickness_cm, DO_Ch_N2_Traits$Leaf_Thickness_cm,DO_Ch_N10_Traits$Leaf_Thickness_cm, DO_Ch_N20_Traits$Leaf_Thickness_cm,
        AP_Ch_N0_Traits$Leaf_Thickness_cm, AP_Ch_N2_Traits$Leaf_Thickness_cm,AP_Ch_N10_Traits$Leaf_Thickness_cm, AP_Ch_N20_Traits$Leaf_Thickness_cm,
        SM_Ch_N0_Traits$Leaf_Thickness_cm, SM_Ch_N2_Traits$Leaf_Thickness_cm,SM_Ch_N10_Traits$Leaf_Thickness_cm, SM_Ch_N20_Traits$Leaf_Thickness_cm,
        AG_Ch_N0_Traits$Leaf_Thickness_cm, AG_Ch_N2_Traits$Leaf_Thickness_cm,AG_Ch_N10_Traits$Leaf_Thickness_cm, AG_Ch_N20_Traits$Leaf_Thickness_cm,
        SN_Ch_N0_Traits$Leaf_Thickness_cm, SN_Ch_N2_Traits$Leaf_Thickness_cm,SN_Ch_N10_Traits$Leaf_Thickness_cm, SN_Ch_N20_Traits$Leaf_Thickness_cm,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Leaf Thickness (cm)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.564"))
text(locator(), labels = c("p=0.105"))
text(locator(), labels = c("p=0.379"))
text(locator(), labels = c("p=0.842"))
text(locator(), labels = c("p=0.260"))




### SRL Change, Statistics ###

summary(aov(DO_Ch_Traits$SRL~DO_Ch_Traits$Treatment))
# p= 0.874

summary(aov(AP_Ch_Traits$SRL~AP_Ch_Traits$Treatment))
# p= 0.156

summary(aov(SM_Ch_Traits$SRL~SM_Ch_Traits$Treatment))
# p= 0.596

summary(aov(AG_Ch_Traits$SRL~AG_Ch_Traits$Treatment))
# p= 0.0679

summary(aov(SN_Ch_Traits$SRL~SN_Ch_Traits$Treatment))
# p=  0.328

### SRL Change, figure ###

boxplot(DO_Ch_N0_Traits$SRL, DO_Ch_N2_Traits$SRL,DO_Ch_N10_Traits$SRL, DO_Ch_N20_Traits$SRL,
        AP_Ch_N0_Traits$SRL, AP_Ch_N2_Traits$SRL,AP_Ch_N10_Traits$SRL, AP_Ch_N20_Traits$SRL,
        SM_Ch_N0_Traits$SRL, SM_Ch_N2_Traits$SRL,SM_Ch_N10_Traits$SRL, SM_Ch_N20_Traits$SRL,
        AG_Ch_N0_Traits$SRL, AG_Ch_N2_Traits$SRL,AG_Ch_N10_Traits$SRL, AG_Ch_N20_Traits$SRL,
        SN_Ch_N0_Traits$SRL, SN_Ch_N2_Traits$SRL,SN_Ch_N10_Traits$SRL, SN_Ch_N20_Traits$SRL,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="SRL", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.874"))
text(locator(), labels = c("p=0.156"))
text(locator(), labels = c("p=0.596"))
text(locator(), labels = c("p=0.068"))
text(locator(), labels = c("p=0.328"))



### Root_Diam_Max Change, Statistics ###

summary(aov(DO_Ch_Traits$Root_Diam_Max~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Root_Diam_Max~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Root_Diam_Max~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Root_Diam_Max~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Root_Diam_Max~SN_Ch_Traits$Treatment))

### Root_Diam_Max Change, figure ###

boxplot(DO_Ch_N0_Traits$Root_Diam_Max, DO_Ch_N2_Traits$Root_Diam_Max,DO_Ch_N10_Traits$Root_Diam_Max, DO_Ch_N20_Traits$Root_Diam_Max,
        AP_Ch_N0_Traits$Root_Diam_Max, AP_Ch_N2_Traits$Root_Diam_Max,AP_Ch_N10_Traits$Root_Diam_Max, AP_Ch_N20_Traits$Root_Diam_Max,
        SM_Ch_N0_Traits$Root_Diam_Max, SM_Ch_N2_Traits$Root_Diam_Max,SM_Ch_N10_Traits$Root_Diam_Max, SM_Ch_N20_Traits$Root_Diam_Max,
        AG_Ch_N0_Traits$Root_Diam_Max, AG_Ch_N2_Traits$Root_Diam_Max,AG_Ch_N10_Traits$Root_Diam_Max, AG_Ch_N20_Traits$Root_Diam_Max,
        SN_Ch_N0_Traits$Root_Diam_Max, SN_Ch_N2_Traits$Root_Diam_Max,SN_Ch_N10_Traits$Root_Diam_Max, SN_Ch_N20_Traits$Root_Diam_Max,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Maximum Root Diameter", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("bottomright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.264"))
text(locator(), labels = c("p=0.269"))
text(locator(), labels = c("p=0.780"))
text(locator(), labels = c("p=0.190"))
text(locator(), labels = c("p=0.501"))

#Root_Diam_Mean
### Root_Diam_Mean Change, Statistics ###

summary(aov(DO_Ch_Traits$Root_Diam_Mean~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Root_Diam_Mean~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Root_Diam_Mean~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Root_Diam_Mean~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Root_Diam_Mean~SN_Ch_Traits$Treatment))

### Root_Diam_Max Change, figure ###

boxplot(DO_Ch_N0_Traits$Root_Diam_Mean, DO_Ch_N2_Traits$Root_Diam_Mean,DO_Ch_N10_Traits$Root_Diam_Mean, DO_Ch_N20_Traits$Root_Diam_Mean,
        AP_Ch_N0_Traits$Root_Diam_Mean, AP_Ch_N2_Traits$Root_Diam_Mean,AP_Ch_N10_Traits$Root_Diam_Mean, AP_Ch_N20_Traits$Root_Diam_Mean,
        SM_Ch_N0_Traits$Root_Diam_Mean, SM_Ch_N2_Traits$Root_Diam_Mean,SM_Ch_N10_Traits$Root_Diam_Mean, SM_Ch_N20_Traits$Root_Diam_Mean,
        AG_Ch_N0_Traits$Root_Diam_Mean, AG_Ch_N2_Traits$Root_Diam_Mean,AG_Ch_N10_Traits$Root_Diam_Mean, AG_Ch_N20_Traits$Root_Diam_Mean,
        SN_Ch_N0_Traits$Root_Diam_Mean, SN_Ch_N2_Traits$Root_Diam_Mean,SN_Ch_N10_Traits$Root_Diam_Mean, SN_Ch_N20_Traits$Root_Diam_Mean,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Mean Root Diameter (cm)", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.255"))
text(locator(), labels = c("p=0.407"))
text(locator(), labels = c("p=0.507"))
text(locator(), labels = c("p=0.108"))
text(locator(), labels = c("p=0.881"))



#Percent_N_AbG
### Percent_N_AbG Change, Statistics ###

summary(aov(DO_Ch_Traits$Percent_N_AbG~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Percent_N_AbG~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Percent_N_AbG~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Percent_N_AbG~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Percent_N_AbG~SN_Ch_Traits$Treatment))

### Percent_N_AbG Change, figure ###

boxplot(DO_Ch_N0_Traits$Percent_N_AbG, DO_Ch_N2_Traits$Percent_N_AbG,DO_Ch_N10_Traits$Percent_N_AbG, DO_Ch_N20_Traits$Percent_N_AbG,
        AP_Ch_N0_Traits$Percent_N_AbG, AP_Ch_N2_Traits$Percent_N_AbG,AP_Ch_N10_Traits$Percent_N_AbG, AP_Ch_N20_Traits$Percent_N_AbG,
        SM_Ch_N0_Traits$Percent_N_AbG, SM_Ch_N2_Traits$Percent_N_AbG,SM_Ch_N10_Traits$Percent_N_AbG, SM_Ch_N20_Traits$Percent_N_AbG,
        AG_Ch_N0_Traits$Percent_N_AbG, AG_Ch_N2_Traits$Percent_N_AbG,AG_Ch_N10_Traits$Percent_N_AbG, AG_Ch_N20_Traits$Percent_N_AbG,
        SN_Ch_N0_Traits$Percent_N_AbG, SN_Ch_N2_Traits$Percent_N_AbG,SN_Ch_N10_Traits$Percent_N_AbG, SN_Ch_N20_Traits$Percent_N_AbG,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Leaf Percent N", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=7.4e-07 *"))
text(locator(), labels = c("p=9.6e-05 *"))
text(locator(), labels = c("p=0.085"))
text(locator(), labels = c("p=0.001 *"))
text(locator(), labels = c("p=1e-06 *"))


#Percent_C_AbG
### Percent_C_AbG Change, Statistics ###

summary(aov(DO_Ch_Traits$Percent_C_AbG~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Percent_C_AbG~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Percent_C_AbG~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Percent_C_AbG~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Percent_C_AbG~SN_Ch_Traits$Treatment))

### Percent_C_AbG Change, figure ###

boxplot(DO_Ch_N0_Traits$Percent_C_AbG, DO_Ch_N2_Traits$Percent_C_AbG,DO_Ch_N10_Traits$Percent_C_AbG, DO_Ch_N20_Traits$Percent_C_AbG,
        AP_Ch_N0_Traits$Percent_C_AbG, AP_Ch_N2_Traits$Percent_C_AbG,AP_Ch_N10_Traits$Percent_C_AbG, AP_Ch_N20_Traits$Percent_C_AbG,
        SM_Ch_N0_Traits$Percent_C_AbG, SM_Ch_N2_Traits$Percent_C_AbG,SM_Ch_N10_Traits$Percent_C_AbG, SM_Ch_N20_Traits$Percent_C_AbG,
        AG_Ch_N0_Traits$Percent_C_AbG, AG_Ch_N2_Traits$Percent_C_AbG,AG_Ch_N10_Traits$Percent_C_AbG, AG_Ch_N20_Traits$Percent_C_AbG,
        SN_Ch_N0_Traits$Percent_C_AbG, SN_Ch_N2_Traits$Percent_C_AbG,SN_Ch_N10_Traits$Percent_C_AbG, SN_Ch_N20_Traits$Percent_C_AbG,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Leaf Percent C", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("bottomright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.166"))
text(locator(), labels = c("p=0.108"))
text(locator(), labels = c("p=0.023 *"))
text(locator(), labels = c("p=0.017 *"))
text(locator(), labels = c("p=0.448"))



#Percent_N_BeG
### Percent_N_BeG Change, Statistics ###

summary(aov(DO_Ch_Traits$Percent_C_AbG~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Percent_C_AbG~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Percent_C_AbG~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Percent_C_AbG~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Percent_C_AbG~SN_Ch_Traits$Treatment))

### Percent_C_AbG Change, figure ###

boxplot(DO_Ch_N0_Traits$Percent_C_AbG, DO_Ch_N2_Traits$Percent_C_AbG,DO_Ch_N10_Traits$Percent_C_AbG, DO_Ch_N20_Traits$Percent_C_AbG,
        AP_Ch_N0_Traits$Percent_C_AbG, AP_Ch_N2_Traits$Percent_C_AbG,AP_Ch_N10_Traits$Percent_C_AbG, AP_Ch_N20_Traits$Percent_C_AbG,
        SM_Ch_N0_Traits$Percent_C_AbG, SM_Ch_N2_Traits$Percent_C_AbG,SM_Ch_N10_Traits$Percent_C_AbG, SM_Ch_N20_Traits$Percent_C_AbG,
        AG_Ch_N0_Traits$Percent_C_AbG, AG_Ch_N2_Traits$Percent_C_AbG,AG_Ch_N10_Traits$Percent_C_AbG, AG_Ch_N20_Traits$Percent_C_AbG,
        SN_Ch_N0_Traits$Percent_C_AbG, SN_Ch_N2_Traits$Percent_C_AbG,SN_Ch_N10_Traits$Percent_C_AbG, SN_Ch_N20_Traits$Percent_C_AbG,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Leaf Percent C", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("bottomright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.166"))
text(locator(), labels = c("p=0.108"))
text(locator(), labels = c("p=0.023 *"))
text(locator(), labels = c("p=0.017 *"))
text(locator(), labels = c("p=0.448")
 
     
         
# Percent_C_BeG
summary(aov(DO_Ch_Traits$Percent_C_BeG~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Percent_C_BeG~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Percent_C_BeG~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Percent_C_BeG~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Percent_C_BeG~SN_Ch_Traits$Treatment))

### Percent_C_BeG Change, figure ###

boxplot(DO_Ch_N0_Traits$Percent_C_BeG, DO_Ch_N2_Traits$Percent_C_BeG,DO_Ch_N10_Traits$Percent_C_BeG, DO_Ch_N20_Traits$Percent_C_BeG,
        AP_Ch_N0_Traits$Percent_C_BeG, AP_Ch_N2_Traits$Percent_C_BeG,AP_Ch_N10_Traits$Percent_C_BeG, AP_Ch_N20_Traits$Percent_C_BeG,
        SM_Ch_N0_Traits$Percent_C_BeG, SM_Ch_N2_Traits$Percent_C_BeG,SM_Ch_N10_Traits$Percent_C_BeG, SM_Ch_N20_Traits$Percent_C_BeG,
        AG_Ch_N0_Traits$Percent_C_BeG, AG_Ch_N2_Traits$Percent_C_BeG,AG_Ch_N10_Traits$Percent_C_BeG, AG_Ch_N20_Traits$Percent_C_BeG,
        SN_Ch_N0_Traits$Percent_C_BeG, SN_Ch_N2_Traits$Percent_C_BeG,SN_Ch_N10_Traits$Percent_C_BeG, SN_Ch_N20_Traits$Percent_C_BeG,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylab="Root Percent C", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.275"))
text(locator(), labels = c("p=0.297"))
text(locator(), labels = c("p=0.300"))
text(locator(), labels = c("p=0.139"))
text(locator(), labels = c("p=0.833"))
  
     
     

# Percent_N_BeG
summary(aov(DO_Ch_Traits$Percent_N_BeG~DO_Ch_Traits$Treatment))

summary(aov(AP_Ch_Traits$Percent_N_BeG~AP_Ch_Traits$Treatment))

summary(aov(SM_Ch_Traits$Percent_N_BeG~SM_Ch_Traits$Treatment))

summary(aov(AG_Ch_Traits$Percent_N_BeG~AG_Ch_Traits$Treatment))

summary(aov(SN_Ch_Traits$Percent_N_BeG~SN_Ch_Traits$Treatment))

### Percent_N_BeG Change, figure ###

boxplot(DO_Ch_N0_Traits$Percent_N_BeG, DO_Ch_N2_Traits$Percent_N_BeG,DO_Ch_N10_Traits$Percent_N_BeG, DO_Ch_N20_Traits$Percent_N_BeG,
        AP_Ch_N0_Traits$Percent_N_BeG, AP_Ch_N2_Traits$Percent_N_BeG,AP_Ch_N10_Traits$Percent_N_BeG, AP_Ch_N20_Traits$Percent_N_BeG,
        SM_Ch_N0_Traits$Percent_N_BeG, SM_Ch_N2_Traits$Percent_N_BeG,SM_Ch_N10_Traits$Percent_N_BeG, SM_Ch_N20_Traits$Percent_N_BeG,
        AG_Ch_N0_Traits$Percent_N_BeG, AG_Ch_N2_Traits$Percent_N_BeG,AG_Ch_N10_Traits$Percent_N_BeG, AG_Ch_N20_Traits$Percent_N_BeG,
        SN_Ch_N0_Traits$Percent_N_BeG, SN_Ch_N2_Traits$Percent_N_BeG,SN_Ch_N10_Traits$Percent_N_BeG, SN_Ch_N20_Traits$Percent_N_BeG,
        names= spnames,
        at = c(1.5, 2.5, 3.5, 4.5, 6.5, 7.5, 8.5, 9.5, 11.5, 12.5, 13.5, 14.5, 16.5, 17.5, 18.5, 19.5, 21.5, 22.5, 23.5, 24.5),
        col= trtcol,
        pch=20, 
        ylim=c(0.0, 2.7),
        ylab="Root Percent N", 
        xlab="Species",       
        main= " ")
abline(v = c(5.5, 10.5, 15.5, 20.5), lty = 1, lwd=1)
legend("topright", inset = 0.01, legend = c("Control", "2.5N", "10N", "20N"), fill = trtcol, bg = "white", title = "Treatment")
text(locator(), labels = c("p=0.0005 *"))
text(locator(), labels = c("p=0.001 *"))
text(locator(), labels = c("p=0.129"))
text(locator(), labels = c("p=1.2e-06 *"))
text(locator(), labels = c("p=1e-07 *"))
     