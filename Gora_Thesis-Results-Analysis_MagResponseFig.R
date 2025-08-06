#### Thesis Results Analysis-Magnitude Response Figure ####
###  #### Thesis Results Analysis ####
###  Gora_Thesis-Results-Analysis_MagResponseFig.R
###  by Sarah Gora
###  Date created: Nov 17, 2023

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
install.packages("ggplot2")
library(ggplot2)
install.packages("ggforce")
library(ggforce)


install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("ggbeeswarm")
library(ggbeeswarm)
install.packages("ggforce")
library(ggforce)
install.packages("ggtext")
library(ggtext)

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


### Load data
# what data do I use? 
# N0N10 trait data used for calculating the ANOVAs
## RR Data
library(readxl)
RR_Data <- read_excel("~/Desktop/Thesis_Data/RR_Data.xlsx")
View(RR_Data)

### July 29 edits, SG
## rename SLA and SRL
library(readxl)
RR_Data3 <- read_excel("~/Desktop/Thesis_Data/RR_Data3.xlsx")
View(RR_Data3)


## Effect Size Figure ###
## Keep barchart from the first round of analyses
# Panel A. Trait Response Figure- Barchart 
# Panel B. Response Ratio graph

# Koerner 2018, NEE paper 

#### PLOT (single panel plot) ####
# All traits, all species
# Response Ratio, RR = (Trt-control)/control) 
# RR for each trait 
# 1 dot per trait, per species
# x= Magnitude (RR value)
# y= Traits

# Colors: 
# 5 species = 5 colors
# choose different colors than what I did before
# DO - 
# AP- 
# SM- 
# AG- 
# SN-
col5 <- c("#fb8500", "#ffb703", "#023047", "#219ebc", "#c1121f")


# Circles:
# closed/filled circle = Significant
# open circle = Not significant


# Plot 
p3 <- ggplot(data = RR_Data3) +
  aes(x = RR, y = reorder(Traits, Plane, decreasing=T), color=Species) +
  xlab("Effect Size (RR = Trt-Ctrl/Ctrl)") +
  ylab("Traits") +
  scale_color_manual(values = col5)+
  xlim(-1,4.7)+
  geom_vline(xintercept = 0) +
  geom_beeswarm(cex = 1.8) 

p3

p4 <- p3 + theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line.x = element_line(color = "black")
)

p4

# shape by significance, color by species
# Plot 
p5 <- ggplot(data = RR_Data3) +
  aes(x = RR, y = reorder(Traits, Plane, decreasing=T), 
      color=Species,
      shape=Significant) +
  xlab("Effect Size (RR = Trt-Ctrl/Ctrl)") +
  ylab("Traits") +
  scale_color_manual(values = c("#fb8500", "#ffb703", "#023047", "#219ebc", "#c1121f")) +
  scale_shape_manual(values = c(1, 16)) +
  xlim(-1,4.7)+
  geom_vline(xintercept = 0) +
  geom_beeswarm(cex = 1.8) 

p5

p6 <- p5 + theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line.x = element_line(color = "black")
)

p6

# final edits, July 29, 2024
# SG







p5 <- ggplot(data = RR_Data) +
  aes(x = RR, y = Traits, shape=Species, color=Significant) +
  xlab("Effect Size (RR = Trt-Ctrl/Ctrl)") +
  scale_color_manual(values = c("#3b3b3b", "#940000"))+
  xlim(-1,4.7)+
  geom_vline(xintercept = 0) +
  # geom_jitter() +  
  geom_beeswarm(cex = 1.8)

p5




p6 <- ggplot(data = RR_Data) +
  aes(x = RR, y = Traits, 
      color=Species, 
      shape=Significant) +
  xlab("Effect Size (RR = Trt-Ctrl/Ctrl)") +
  scale_color_manual(values = c("#fb8500", "#ffb703", "#023047", "#219ebc", "#c1121f")) +
  scale_shape_manual(values = c(1, 16)) +
  xlim(-1,4.7)+
  geom_vline(xintercept = 0) +
  geom_beeswarm(cex = 1.8)

p6





# dont use not good
# p7 <- p3 + theme(legend.text = element_text(colour="blue", size = 10, face = "italic"), legend.text = element_text(colour="black))
            


p6 <- p3 + theme(
  plot.background = element_rect(fill = "white"),
  panel.background = element_rect(fill = "white"),
  axis.line.x = element_line(color = "black")
)

p6

# To Do:
# dot fill color= significant
# dot no-fill/ border colored= non-significant
# make legend in powerpoint
 


# cut
scale_fill_discrete(name= "Species",
                    labels = c("*A. gerardii*",
                               "*A. psilostachya*",
                               "*D. oligosanthes*",
                               "*S. missouriensis*",
                               "*S. nutans*"))+
  theme(legend.text = element_markdown())

       
       
       