#### Thesis Results Analysis-Correlation Matrix ####
###  Gora_Thesis-Results-CorrelationMatrix.R
###  by Sarah Gora
###  Date created: Feb 22, 2022

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

#install packages
library(readr)
install.packages("vegan")
library(vegan)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
library(stats)
install.packages("corrplot")
library(corrplot)


# Ordination data with only a certain number of traits
# Filtered by species
# FOR N0N10
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

# FOR CHANGE
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


DO_Ch0_Traits_O <- All_Traits_O %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment2=="0")


########## CORRELATION MATRIX AND PLOTS FOR N0N10 TRAITS #####

setwd("~/Desktop/Thesis_Data/CorrelationMatrix/MatrixOutput")


#Correlation matrix- DO for N0N10
#matrix
cr_DO_N0N10 <- cor(DO_N0N10_Traits_O[10:26], method="spearman")
?cor()

#save Matrix Summary
capture.output(cr_DO_N0N10, file = "Gora_ThesisResults_CorMatrix_DO-N0N10.txt")

#plot all traits
plot(DO_N0N10_Traits_O[10:26], main= "Correlation Matrix Scatterplot for DO for N0N10")

# visualize with Corplot
corrplot(cr_DO_N0N10, method="number", title = "Correlation Matrix for DO for N0N10 Traits")
plot.new()

#Correlation matrix- AP for N0N10
#matrix
cr_AP_N0N10 <- cor(AP_N0N10_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_AP_N0N10, file = "Gora_ThesisResults_CorMatrix_AP-N0N10.txt")

#plot all traits
plot(AP_N0N10_Traits_O[10:26], main= "Correlation Matrix Scatterplot for AP for N0N10")

# visualize with Corplot
corrplot(cr_AP_N0N10, method= "number", title = "Correlation Matrix for AP for N0N10 Traits")




#Correlation matrix- SM for N0N10
#matrix
cr_SM_N0N10 <- cor(SM_N0N10_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_SM_N0N10, file = "Gora_ThesisResults_CorMatrix_SM-N0N10.txt")

#plot all traits
plot(SM_N0N10_Traits_O[10:26], main="Correlation Matrix Scatterplot for SM for N0N10")

# visualize with Corplot
corrplot(cr_SM_N0N10,method="number", title = "Correlation Matrix for SM for N0N10 Traits")



#Correlation matrix- SN for N0N10
#matrix
cr_SN_N0N10 <- cor(SN_N0N10_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_SN_N0N10, file = "Gora_ThesisResults_CorMatrix_SN-N0N10.txt")

#plot all traits
plot(SN_N0N10_Traits_O[10:26], main="Correlation Matrix Scatterplot for SN for N0N10")

# visualize with Corplot
corrplot(cr_SN_N0N10,method="number", title = "Correlation Matrix for SN for N0N10 Traits")



#Correlation matrix- AG for N0N10
#matrix
cr_AG_N0N10 <- cor(AG_N0N10_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_AG_N0N10, file = "Gora_ThesisResults_CorMatrix_AG-N0N10.txt")

#plot all traits
plot(AG_N0N10_Traits_O[10:26], main="Correlation Matrix Scatterplot for AG for N0N10")

# visualize with Corplot
corrplot(cr_AG_N0N10, method="number", title = "Correlation Matrix for AG for N0N10 Traits")





########## CORRELATION MATRIX AND PLOTS FOR CHANGE TRAITS #####

#Correlation matrix- DO for Change
#matrix
cr_DO0_Ch <- cor(DO_Ch0_Traits_O[10:27], method="spearman")

#save Matrix Summary
capture.output(cr_DO_Ch, file = "Gora_ThesisResults_CorMatrix_DO-Change.txt")

#plot all traits
plot(DO_Ch0_Traits_O[10:27], main="Correlation Matrix Scatterplot for DO for Change")

# visualize with Corplot
corrplot(cr_DO_Ch,method="number", title = "Correlation Matrix for DO for Control Traits")


ggcorr(DO_Ch0_Traits_O[10:27], method = c("everything", "pearson")) 





#Correlation matrix- AP for Change
#matrix
cr_AP_Ch <- cor(AP_Ch_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_AP_Ch, file = "Gora_ThesisResults_CorMatrix_AP-Change.txt")

#plot all traits
plot(AP_Ch_Traits_O[10:26], main="Correlation Matrix Scatterplot for AP for Change")

# visualize with Corplot
corrplot(cr_AP_Ch, method="number", title = "Correlation Matrix for AP for Change Traits")




#Correlation matrix- SM for Change
#matrix
cr_SM_Ch <- cor(SM_Ch_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_SM_Ch, file = "Gora_ThesisResults_CorMatrix_SM-Change.txt")

#plot all traits
plot(SM_Ch_Traits_O[10:26], main="Correlation Matrix Scatterplot for SM for Change")

# visualize with Corplot
corrplot(cr_SM_Ch, method="number", title = "Correlation Matrix for SM for Change Traits")



#Correlation matrix- SN for Change
#matrix
cr_SN_Ch <- cor(SN_Ch_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_SN_Ch, file = "Gora_ThesisResults_CorMatrix_SN-Change.txt")

#plot all traits
plot(SN_Ch_Traits_O[10:26], main="Correlation Matrix Scatterplot for SN for Change")

# visualize with Corplot
corrplot(cr_SN_Ch,method="number", title = "Correlation Matrix for SN for Change Traits")



#Correlation matrix- AG for Ch
#matrix
cr_AG_Ch <- cor(AG_Ch_Traits_O[10:26], method="spearman")

#save Matrix Summary
capture.output(cr_AG_Ch, file = "Gora_ThesisResults_CorMatrix_AG-Ch.txt")

#plot all traits
plot(AG_Ch_Traits_O[10:26], main="Correlation Matrix Scatterplot for AG for Change")

# visualize with Corplot
corrplot(cr_AG_Ch, method="number", title = "Correlation Matrix for AG for Change Traits")








########################## Chart w P-value included #########################

cr_DO_N0N10

install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")


# DO N0N10
cr_DO_N0N10
chart.Correlation(cr_DO_N0N10, histogram=TRUE, method="spearman", pch=19, title="Correlation Matrix for DO N0N10")

?chart.Correlation()

