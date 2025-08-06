#### RootTraitswithN Results Analysis-Correlogram ####
###  Gora_RootTraitswithN-Results-Analysis_Correlogram.R
###  by Sarah Gora
###  Date created: April 4, 2022

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")

# load packages
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)
install.packages("GGally")
library(GGally)

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
All_Traits_O <- read_csv("~/Desktop/Thesis_Data/Ordinations/All_Traits_Datasheet_O2.csv")


#### Correlogram for Dican

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




# try 1 to select only the plots I want... 
plots = list()
for (i in 1:6){
  plots <- c(plots, lapply(1:5, function(j) getPlot(p, i = i, j = j)))
}  
ggmatrix(plots, 
         nrow = 6, 
         ncol = 5,
         xAxisLabels = p$xAxisLabels[1:5], 
         yAxisLabels = p$yAxisLabels, 
         title="Nice plot with only the panels I care about")


# try 2 to select only the plots I want... 
plots = list()
{plots <- c(plots, lapply(1:20, function(j) getPlot(p, i = i, j = j)))
}  
ggmatrix(plots, 
         nrow = 10,  # keep rows 
         ncol= 7)








# is there any way to get these AbG vs BeG

### subset out the plots you want 
### yes there is a way 

## I also want to change the color of the column headers 





# subset AP
AP_TraitsDrop_O <- All_Traits_O_Drop %>%
  filter(Species=="Ambrosia_psilostachya")

# Correlogram
p <- ggpairs(AP_TraitsDrop_O,          
             columns = 10:20,
             columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
             aes(color = Treatment, alpha=0.99), 
             title="Correlogram for AP with ggpairs()",
             upper = list(continuous = wrap("cor", size = 1.5)))
p

#columnLabels = c("SLA","Leaf Thickness", "AbG Biomass","Root Biomass", "SRL", "Mean Root Diam", "Fully Dev Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C") 

### what alpha value do i want???


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


####### still working on code to subset out only the plots I want with 10 ABG vs 7 BeG






# subset SM
SM_TraitsDrop_O <- All_Traits_O_Drop %>%
  filter(Species=="Solidago_missouriensis")

# Correlogram
p <- ggpairs(SM_TraitsDrop_O,          
             columns = 10:20,
             columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
             aes(color = Treatment, alpha=0.99), 
             title="Correlogram for SM with ggpairs()",
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


####### still working on code to subset out only the plots I want with 10 ABG vs 7 BeG





# subset AG
AG_TraitsDrop_O <- All_Traits_O_Drop %>%
  filter(Species=="Andropogon_gerardii")

# Correlogram
p <- ggpairs(AG_TraitsDrop_O,          
             columns = 10:20,
             aes(color = Treatment, alpha=0.99), 
             columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
             title="Correlogram for AG with ggpairs()",
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


####### still working on code to subset out only the plots I want with 10 ABG vs 7 BeG







# subset SN
SN_TraitsDrop_O <- All_Traits_O_Drop %>%
  filter(Species=="Sorghastrum_nutans")

# Correlogram
p <- ggpairs(SN_TraitsDrop_O,          
             columns = 10:20,
             aes(color = Treatment, alpha=0.99),
             columnLabels= c("SLA","Leaf Th", "AbG Bio","Rt Bio", "SRL", "Mean RtDiam", "Em Leaves", "Leaf %N", "Leaf %C", "Root %N", "Root %C"), 
             title="Correlogram for SN with ggpairs()",
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


####### still working on code to subset out only the plots I want with 10 ABG vs 7 BeG







