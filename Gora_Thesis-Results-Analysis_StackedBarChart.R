#### Thesis Results Analysis- Stacked Bar Chart ####
###  Gora_Thesis-Results-Analysis_StackedBarChart.R
###  by Sarah Gora
###  Date created: March 21, 2022

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


# install packages
library(ggplot2)
library(dplyr)
library(tidyr)

# create N0N10 dataframe

# DO: 8/11 AbG, 1/7 BeG 72.73, -14.28
# AP: 5/11 AbG, 1/7 BeG 45.45, -14.28
# SM: 1/11 AbG, 1/7 BeG 9.10, -14.28
# AG: 1/11 AbG, 1/7 BeG 9.10, -14.28
# SN: 1/11 AbG, 1/7 BeG 9.10, -14.28


#plot 
df_barchart_N0N10 <- data.frame(Species = rep(c("DO", "AP", "SM", "AG", "SN")),
                                Traits = rep(c("Aboveground","Belowground"),times=5),
                                Percent_Responsive_to_N = c(
                                  72.73, -14.28, 
                                  9.10, -14.28, 
                                  9.10, -14.28, 
                                  45.5, -14.28,
                                  9.10, -14.28))
df_barchart_N0N10_1 <- df_barchart_N0N10   # Replicate original data
df_barchart_N0N10_1$Species <- factor(df_barchart_N0N10_1$Species,                                    # Change ordering manually
                  levels = c("DO", "AP", "SM", "AG", "SN"))

my_plot <- ggplot(df_barchart_N0N10_1, aes(x=Species, y=Percent_Responsive_to_N, fill=Traits, group=Traits,group=Species)) + ylim(-20, 100)+
  geom_bar(position="stack", stat="identity", colour = "black")
 
myplot_theme <- my_plot + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
a <- myplot_theme + scale_fill_manual(values=c("darkolivegreen4", "bisque4")) + geom_hline(yintercept=0)+ ggtitle("Trait response to 10g N")+labs(y= "Traits Responsive to N")

a

# add % labels to N0N10plot
bar_labels_a <- c("72.7%","14.3%","9.1%","14.3%","9.1%","14.3%","45.5%","14.3%","9.1%","14.3%")

a_labled <- a + geom_text(aes(label = bar_labels_a),size = 4, fontface = "bold", vjust = 1.5,color="white") + 
        geom_text(aes(label = bar_labels_a),size = 4, fontface = "bold", vjust = -1.5,color="white")
  
a_labled



#########  Change Barchart ######## 
# based on regressions
# grouped + stacked, barchart 

# create Change dataframe

# DO: 6/11 AbG, 1/7 BeG   54.5, -14.28
# AP: 9/11 AbG, 2/7 BeG   81.8, -28.6
# SM: 2/11 AbG, 0/7 BeG   18.2, 0
# AG: 5/11 AbG, 1/7 BeG   45.5, -14.28
# SN: 3/11 AbG, 1/7 BeG   27.3, -14.28

# * 7 traits BeG also in focal 


df_barchart_Ch <- data.frame(Species = rep(c("DO","DO", "AP","AP", "SM", "SM","AG", "AG", "SN", "SN")),
                                Traits = rep(c("Aboveground","Belowground"),times=5),
                             Percent_Responsive_to_N = c(
                                  54.5, -14.28, #
                                  81.8, -28.6, #4
                                  18.2, 0,    #1 
                                  45.5, -14.28, 
                                  27.3, -14.28))

df_barchart_Ch_1 <- df_barchart_Ch   # Replicate original data
df_barchart_Ch_1$Species <- factor(df_barchart_Ch_1$Species,                                    # Change ordering manually
                                      levels = c("DO", "AP", "SM", "AG", "SN"))

my_plot_Ch <- ggplot(df_barchart_Ch_1, aes(x=Species, y=Percent_Responsive_to_N, fill=Traits, group=Traits,group=Species)) + ylim(-35, 100) + 
  geom_bar(position="stack", stat="identity", colour = "black")

myplot_Ch_theme <- my_plot_Ch + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
b <- myplot_Ch_theme + scale_fill_manual(values=c("darkolivegreen4", "bisque4")) + geom_hline(yintercept=0) + ggtitle("Trait response to N gradients")+labs(y= "Traits Responsive to N")

b

# add labels 
bar_labels_b <- c("54.5%","14.3%","81.8%","28.6%","18.2%"," ","45.5%","14.3%","27.3%","14.3%")

# add % labels to Change plot
b_labled <- b + geom_text(aes(label = bar_labels_b),size = 4, fontface = "bold", vjust = 1.5,color="white") + geom_text(aes(label = bar_labels_b),size = 4, fontface = "bold", vjust = -1.5,color="white")
b_labled







## COMBINE
#remove legend from N0N10 plot
a_lab_nolegend <- a_labled + theme(legend.position="none")

#remove legend from N0N10 plot
b_lab_nolegend <- b_labled + theme(legend.position="none")


# Package for Combining Plots
install.packages("ggpubr")
library(ggpubr)

ggarrange(a_lab_nolegend, b_lab_nolegend)



