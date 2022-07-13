#### PhD: P2 Fitting CV ####
## Function: Fits Estimated WTP
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 13/07/2022
## TODO: setup RENV


#------------------------------
# Replication Information: ####
# Selected output of 'sessionInfo()'
#------------------------------

# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252  LC_CTYPE=English_United Kingdom.1252   
# [1] MASS_7.3-56    compiler_4.1.3 tools_4.1.3    renv_0.15.4  

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggridges)
library(tidyr)
library(DCchoice)



#------------------------------
# Section 1: Import Data ####
#------------------------------



FullSurvey2 <- data.frame(read.csv("FullSurvey2.csv")) 

Q1_WTP <- data.frame(readRDS("Q1_SBDCModel_Covariates_FittedWTP.rds"))
Q2_WTP <- data.frame(readRDS("Q2_SBDCModel_Covariates_FittedWTP.rds"))

Q1_WTP_Lower <- data.frame(readRDS("Q6_SBDCModel_WTP_Lower.rds"))
Q2_WTP_Lower <- data.frame(readRDS("Q7_SBDCModel_WTP_Lower.rds"))

Q1_WTP_Upper <- data.frame(readRDS("Q6_SBDCModel_WTP_Upper.rds"))
Q2_WTP_Upper <- data.frame(readRDS("Q7_SBDCModel_WTP_Upper.rds"))

Q1_LV <- data.frame(readRDS("Q6ICLV_SDMixed_2022_06_01_ConWTP.rds"))
Q2_LV <- data.frame(readRDS("Q7ICLV_SDMixed_2022_06_01_ConWTP.rds"))

#-------------------------------
#### Merging: ####
#-------------------------------

FullSurvey2 <- cbind(
  FullSurvey2,
  "Q1WTP"=Q1_WTP$readRDS..Q1_SBDCModel_Covariates_FittedWTP.rds..,
  "Q2WTP"=Q2_WTP$readRDS..Q2_SBDCModel_Covariates_FittedWTP.rds..,
  
  "Q1WTPLower"=Q1_WTP_Lower$Q1WTP,
  "Q2WTPLower"=Q2_WTP_Lower$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  
  "Q1WTPUpper"=Q1_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q6_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  "Q2WTPUpper"=Q2_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  
  "Q1LV"=Q1_LV$post..mean,
  "Q2LV"=Q2_LV$post..mean 
)



#-------------------------------
#### Boxplot of CV vs LV: ####
#-------------------------------




# Replicating Abate et al (2020) box and whiskers:
FullSurvey2$LVQuantilesQ1 <- (ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
FullSurvey2$LVQuantilesQ2 <- (ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
# quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))


## This part is Q1
Q1Box <- ggplot(FullSurvey2, aes(LVQuantilesQ1, Q1WTP)) +   
  geom_boxplot(coef=5,
               aes(group = LVQuantilesQ1),
               width = 0.1)+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,150),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Q1 WTP by percentile of latent variable estimated in Q1 Hybrid Choice Model.")+
  geom_text(x = 1.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2))),color="red")+
  geom_text(x = 2.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2))),color="red")+
  geom_text(x = 3.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2))),color="red")+
  geom_text(x = 4.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2))),color="red")+
  geom_text(x = 5.25, y =round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2))),color="red")


## This is Q2
Q2Box <- ggplot(FullSurvey2, aes(LVQuantilesQ2, Q2WTP)) +   
  geom_boxplot(coef=5,
               aes(group = LVQuantilesQ2),
               width = 0.1)+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,150),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Q2 WTP by percentile of latent variable estimated in Q2 Hybrid Choice Model.")+
  geom_text(x = 1.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2))),color="blue")+
  geom_text(x = 2.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2))),color="blue")+
  geom_text(x = 3.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2))),color="blue")+
  geom_text(x = 4.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2))),color="blue")+
  geom_text(x = 5.25, y =round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2) , label = paste0("Mean:\n£",sprintf("%.2f",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2))),color="blue")


#---------------------------------------
# Export Plots ####
#---------------------------------------


ggsave(grid.arrange(Q1Box,Q2Box), device = "jpeg",
       filename = "CVBoxplots_2022_07_13.jpeg",
       width=20,height=15,units = "cm",dpi=1000)


# End Of Script ---------------------------------------------------