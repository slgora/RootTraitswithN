#### Thesis Results Analysis-Traits Bar Graphs ####
###  Gora_Thesis-Results-Analysis_Bargraph.R
###  by Sarah Gora
###  Date created: April 2, 2022

# set WD 
setwd("~/Desktop/Thesis_Data/R Code")


# load data 
FRic_N0N10 <- read_csv("~/Desktop/Thesis_Data/FRic/FRic_N0N10.csv")
FRic_Change <- read_csv("~/Desktop/Thesis_Data/FRic/FRic_Change.csv")


#########  Fdis -N0N10 Barchart ######## 

par(mfrow = c(1,2))

# Modify data for Base R barplot
FRic_N0N10 <- read_csv("~/Desktop/Thesis_Data/FRic/FRic_N0N10.csv")
FRic_N0N10 <- data.frame(FRic_N0N10, row.names = 1)
FRic_N0N10_matrix <- as.matrix(FRic_N0N10)
FRic_N0N10_matrix    

catlables <-  c( "          DO" , "","          AP" , "","         SM", "", "          AG" , "", "          SN" , " " )
pal2 <- c("#BDB2A7" ,"#769370")

  
abc <- barplot(height = FRic_N0N10_matrix,                     
        beside = TRUE, 
        ylim = c(0,25),
        names=catlables,
        col= c("#BDB2A7" ,"#769370"),
        main=" ",
        xlab="Species",
        ylab="Functional Dispersion (FDis)")
abline(h = c(0), lty = 1, lwd=3)
legend("topleft", inset = 0.001, legend = c("0g", "10g"), title = "N Treatment", fill = pal2, bg = "white", box.lty=0)          
# text(abc, 0, round(FRic_N0N10_matrix, 1),cex=0.8,pos=3)
#do
text(locator(), labels = c("9.6"), cex=0.8)
text(locator(), labels = c("15.4"), cex=0.8)
#ap
text(locator(), labels = c("18.1"), cex=0.8)
text(locator(), labels = c("16.0"), cex=0.8)
#sm
text(locator(), labels = c("6.5"), cex=0.8)
text(locator(), labels = c("7.8"), cex=0.8)
#ag
text(locator(), labels = c("5.5"), cex=0.8)
text(locator(), labels = c("22.1"), cex=0.8)
#sn
text(locator(), labels = c("7.8"), cex=0.8)
text(locator(), labels = c("6.1"), cex=0.8)
     

#########  Fdis -Change Barchart ######## 

# Modify data for Base R barplot
FRic_Change <- read_csv("~/Desktop/Thesis_Data/FRic/FRic_Change.csv")
FRic_Change <- data.frame(FRic_Change, row.names = 1)
FRic_Change_matrix <- as.matrix(FRic_Change)
FRic_Change_matrix 

#Labels
catlables2 <-  c( "","DO","","","","AP","","","","SM","","","","AG","","","","SN","","")
trtcol <- c("#BDB2A7", "#6E687E" ,"#769370" ,"#F1C646")

abcd <- barplot(height = FRic_Change_matrix,                     
        beside = TRUE, 
        ylim = c(0,25),
        names=catlables2,
        col= trtcol,
        main=" ",
        xlab="Species",
        ylab="Functional Dispersion (FDis)")
abline(h = c(0), lty = 1, lwd=3)
legend("topright", inset = 0.001, legend = c("0g", "2.5g", "10g", "20g"), fill = trtcol, bg = "white", title = "N Treatment", box.lty=0)
#text(abcd, 0, round(FRic_Change_matrix, 1),cex=0.8,pos=3)
#do
text(locator(), labels = c("6.5"), cex=0.7)
text(locator(), labels = c("8.9"), cex=0.7)
text(locator(), labels = c("12.4"), cex=0.7)
text(locator(), labels = c("7.0"), cex=0.7)

#ap
text(locator(), labels = c("7.0"), cex=0.7)
text(locator(), labels = c("11.8"), cex=0.7)
text(locator(), labels = c("21.3"), cex=0.7)
text(locator(), labels = c("6.6"), cex=0.7)

#sm
text(locator(), labels = c("5.0"), cex=0.7)
text(locator(), labels = c("4.4"), cex=0.7)
text(locator(), labels = c("3.7"), cex=0.7)
text(locator(), labels = c("4.2"), cex=0.7)

#ag
text(locator(), labels = c("3.3"), cex=0.7)
text(locator(), labels = c("7.1"), cex=0.7)
text(locator(), labels = c("3.9"), cex=0.7)
text(locator(), labels = c("5.9"), cex=0.7)

#sn
text(locator(), labels = c("10.4"), cex=0.7)
text(locator(), labels = c("5.5"), cex=0.7)
text(locator(), labels = c("4.6"), cex=0.7)
text(locator(), labels = c("6.3"), cex=0.7)



