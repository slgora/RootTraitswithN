#### RootTraitswithN Results Analysis-Correlation Matrix, Redo with N0N10 ####
###  Gora_RootTraitswithN-Results-CorrelationMatrix_Redo.R
###  by Sarah Gora
###  Date created: February 17, 2023

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

# load packages
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

#check if ggplot2 was installed
system.file(package='ggplot2')
# yes ggplot2 installed

## Error in ggally package install
# install ggally from github
install.packages("devtools")
library(devtools)
install_github("ggobi/ggally")



# loda data as matrices?

# Make Pearson's Corr plot
# ggpairs function in the Ggally package for plotting 
# calculated the correlation and significance between each two based on Pearson’s correlation analysis

# make Correlograms by Species (5 Correlograms total)
# group by Nitrogen treatment within the correlogram and color by Treatment

# Notes: had to run dev.off() 
# bc an error came up 
# Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
# invalid graphics state


# all traits 2 is the latest version - use this: may 2, 2022
All_Traits_O <- read.csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_Datasheet_O2.csv")




### Example Code to build from ###

#### Correlogram for Dican using ALL data ###
DO_Traits_O <- All_Traits_O %>%
  filter(Species=="Dichanthelium_oligosanthes")

# remove NAs

# Correlogram
# use all the Dican data 
# color by N treatment in the correlogram

p <- ggpairs(DO_Traits_O,          
             columns = 10:26,
             aes(color = Treatment, alpha = 1), 
             title="Correlogram for DO with ggpairs()",
             upper = list(continuous = wrap("cor", size = 1.5)))

# change colors manually 
cols4 <- c("#769370", "#6E687E","#F1C646","#BDB2A7")
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=cols4) +
      scale_color_manual(values=cols4)  
  }
}

p





###  use the traits drop model 


library(readr)
All_Traits_O_Drop <- read_csv("~/Desktop/All_Traits_O_DroppedTraits.csv")

# subset DO 
DO_TraitsDrop_O <- All_Traits_O_Drop %>%
  filter(Species=="Dichanthelium_oligosanthes")

# tidyverse/filter not working? 
# fix documented below
DO_TraitsDrop_O <- All_Traits_O_Drop %>%
  subset(Species=="Dichanthelium_oligosanthes")

# Correlogram
# use all the Dican data 
# color by N treatment in the correlogram

p2 <- ggpairs(DO_TraitsDrop_O,          
              columns = 10:20,
              aes(color = Treatment, alpha=0.99),
              columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
              title="Correlogram for DO with ggpairs()",
              upper = list(continuous = wrap("cor", size = 1.5)))

# change colors manually 
cols4 <- c("#769370", "#6E687E","#F1C646","#BDB2A7")
for(i in 1:p2$nrow) {
  for(j in 1:p2$ncol){
    p2[i,j] <- p2[i,j] + 
      scale_fill_manual(values=cols4) +
      scale_color_manual(values=cols4)  
  }
}

p2 
p2+ theme(columnLabels = element_text(face = 'bold', size = 10))
# didnt work in rerun bc i need to set/make the columnLabels dataframe




####### CorPlots using only N0N10 data  #######

# use traits drop data
# missing individuals dropped if didnt have data for all 11 traits
library(readr)
All_Traits_O_Drop <- read_csv("~/Desktop/All_Traits_O_DroppedTraits.csv")

### Subset out N0N10 data by species
# Ordination data with only a certain number of traits
DO_N0N10_Traits_Cor <- All_Traits_O_Drop %>%
  filter(Species=="Dichanthelium_oligosanthes") %>%
  filter(Treatment=="Control"| Treatment=="10")
AP_N0N10_Traits_Cor <- All_Traits_O_Drop %>%
  filter(Species=="Ambrosia_psilostachya")%>%
  filter(Treatment=="Control"| Treatment=="10")
SM_N0N10_Traits_Cor <- All_Traits_O_Drop %>%
  filter(Species=="Solidago_missouriensis")%>%
  filter(Treatment=="Control"| Treatment=="10")
SN_N0N10_Traits_Cor <- All_Traits_O_Drop %>%
  filter(Species=="Sorghastrum_nutans")%>%
  filter(Treatment=="Control"| Treatment=="10")
AG_N0N10_Traits_Cor <- All_Traits_O_Drop %>%
  filter(Species=="Andropogon_gerardii")%>%
  filter(Treatment=="Control"| Treatment=="10")

# error , object Species not found 
# filter is in both dplyr and stats,
# to fix this: detach from dyplr and stats and reload packages again 
detach("package:dplyr")  # Unload dplyr
detach("package:stats") # Unload stats
library(stats)
library(dplyr)
# filter worked again to find object, Species


### Correlogram, DO N0N10 data 
# color by N treatment (10g) in correlogram

p_do_n0n10 <- ggpairs(DO_N0N10_Traits_Cor,          
              columns = 10:20, 
              aes(color = Treatment, alpha=0.99),
              columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
              title="Correlogram for D. oligosanthes in 10g N with ggpairs()",
              upper = list(continuous = wrap("cor", size = 2.5)))

p_do_n0n10
# corrplot worked 

# change colors manually 
# 10g N= green #769370
# Control= tan #BDB2A7
cols2 <- c("#769370","#BDB2A7")
for(i in 1:p_do_n0n10$nrow) {
  for(j in 1:p_do_n0n10$ncol){
    p_do_n0n10[i,j] <- p_do_n0n10[i,j] + 
      scale_fill_manual(values=cols2) +
      scale_color_manual(values=cols2)  
  }
}

p_do_n0n10

# p_do_n0n10+ theme(columnLabels = element_text(face = 'bold', size = 10))
# make labels bold, didnt get to work



### Correlogram, AP N0N10 
p_ap_n0n10 <- ggpairs(AP_N0N10_Traits_Cor,          
                      columns = 10:20, 
                      aes(color = Treatment, alpha=0.99),
                      columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
                      title="Correlogram for A. psilostachya in 10g N with ggpairs()",
                      upper = list(continuous = wrap("cor", size = 2.5)))

p_ap_n0n10

# change colors manually 
# 10g N= green #769370
# Control= tan #BDB2A7
# cols2 <- c("#769370","#BDB2A7")
for(i in 1:p_ap_n0n10$nrow) {
  for(j in 1:p_ap_n0n10$ncol){
    p_ap_n0n10[i,j] <- p_ap_n0n10[i,j] + 
      scale_fill_manual(values=cols2) +
      scale_color_manual(values=cols2)  
  }
}
# view plot
p_ap_n0n10



### Correlogram, SM N0N10 
p_sm_n0n10 <- ggpairs(SM_N0N10_Traits_Cor,          
                      columns = 10:20, 
                      aes(color = Treatment, alpha=0.99),
                      columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
                      title="Correlogram for S. missouriensis in 10g N with ggpairs()",
                      upper = list(continuous = wrap("cor", size = 2.5)))

# change colors manually 
# 10g N= green #769370
# Control= tan #BDB2A7
# cols2 <- c("#769370","#BDB2A7")
for(i in 1:p_sm_n0n10$nrow) {
  for(j in 1:p_sm_n0n10$ncol){
    p_sm_n0n10[i,j] <- p_sm_n0n10[i,j] + 
      scale_fill_manual(values=cols2) +
      scale_color_manual(values=cols2)  
  }
}
# view plot
p_sm_n0n10



### Correlogram, AG N0N10 
p_ag_n0n10 <- ggpairs(AG_N0N10_Traits_Cor,          
                      columns = 10:20, 
                      aes(color = Treatment, alpha=0.99),
                      columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
                      title="Correlogram for A. gerardii in 10g N with ggpairs()",
                      upper = list(continuous = wrap("cor", size = 2.5)))

# change colors manually 
# 10g N= green #769370
# Control= tan #BDB2A7
# cols2 <- c("#769370","#BDB2A7")
for(i in 1:p_ag_n0n10$nrow) {
  for(j in 1:p_ag_n0n10$ncol){
    p_ag_n0n10[i,j] <- p_ag_n0n10[i,j] + 
      scale_fill_manual(values=cols2) +
      scale_color_manual(values=cols2)  
  }
}
# view plot
p_ag_n0n10



### Correlogram, SN N0N10 
p_sn_n0n10 <- ggpairs(SN_N0N10_Traits_Cor,          
                      columns = 10:20, 
                      aes(color = Treatment, alpha=0.99),
                      columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
                      title="Correlogram for S. nutans in 10g N with ggpairs()",
                      upper = list(continuous = wrap("cor", size = 2.5)))

# change colors manually 
# 10g N= green #769370
# Control= tan #BDB2A7
# cols2 <- c("#769370","#BDB2A7")
for(i in 1:p_sn_n0n10$nrow) {
  for(j in 1:p_sn_n0n10$ncol){
    p_sn_n0n10[i,j] <- p_sn_n0n10[i,j] + 
      scale_fill_manual(values=cols2) +
      scale_color_manual(values=cols2)  
  }
}
# view plot
p_sn_n0n10


