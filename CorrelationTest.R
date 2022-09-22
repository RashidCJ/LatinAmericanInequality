rm(list = ls())

# Initial setup -----------------------------------------------------------

library(readr)
library(tidyverse)
if(osVersion=="Windows 10 x64 (build 19044)") {
  setwd("C:/Users/Rashid/OneDrive - Indiana University/General/Doctoral Studies/Research LatAm Inequality/For Publication/Latin American Inequality final/ThinDrive")
  print("Working directory set in accordance to Windows settings. Directorio seteado en config. Windows.")
} else {
  setwd("~/Documents/OneDrive - Indiana University/General/Doctoral Studies/Research LatAm Inequality/For Publication/Latin American Inequality final/ThinDrive")
  print("Working directory set in accordance to Mac/Linux settings. Directorio seteado en config. Mac.")
}

# Load file for analysis --------------------------------------

library(strucchange)
library(readxl)
LatAm2014 <- read_excel("LatAm2014.xlsx", col_types = c("numeric", "text", "numeric", 
                                                        "numeric", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "text", "text", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "text", "text", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "text", "text"))





#LatAm2014$year<-format(LatAm2014$year, format="%Y")
attach(LatAm2014)
#detach(LatAm2014)
LatAm2014$gini100<-gini_equivalized*100

set.seed(42)

# Simple pearson test for all pairs of used variables ---------------------

giniminwage<-cor.test(LatAm2014$gini100,LatAm2014$logminwage,method="pearson")
giniminwage$estimate
giniminwage$p.value
cor.test(LatAm2014$gini100,LatAm2014$PresidentPartyLeftRightIndex_mean,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$PresidentPartyLeftRightIndex_meanlag,method="pearson")
?cor.test
cor.test(LatAm2014$gini100,LatAm2014$educexpgdp,method="spearman")
cor.test(LatAm2014$gini100,LatAm2014$socexpgdp,method="pearson")

cor.test(LatAm2014$gini100,LatAm2014$highlyeducatedlabor,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$employment,method="pearson")

cor.test(LatAm2014$gini100,LatAm2014$HeterogeneityIndex,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$SimpsonDiversity,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$EthnicFractionalizationTS,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$EthnicFractLag1,method="pearson")

cor.test(LatAm2014$gini100,LatAm2014$Fractionalization,method="pearson")


cor.test(LatAm2014$gini100,LatAm2014$Blacks,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$Mulattoes,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$Amerindians,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$White,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$Asians,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$Mestizo,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$CreolesEtGafurinas,method="pearson")
cor.test(LatAm2014$gini100,LatAm2014$wage.arrangement,method="pearson")
