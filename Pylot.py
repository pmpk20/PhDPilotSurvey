# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
#Step 1: Import necessary packages
import os
os.chdir('H:\PhDPilotSurvey')   
import pandas as pd
import numpy as np
import matplotlib.pyplot as mp
import statsmodels.formula.api as smf
import matplotlib.patches as mpatches
import matplotlib.pyplot as plt
import matplotlib as mpl
from cycler import cycler
mpl.rcParams['axes.prop_cycle'] = cycler(color='bgcrmyk')

#Step 2: Import data
Survey = pd.read_csv("\\\\myfiles\pmpk20\dos\PhD\Data Collection\Pilot\PhD Survey_ Sample A.csv")

### To translate to R
#Pilot <- Pilot[ -c(1.2,8,27)]
##Section Two. Pre-processing.
### To do. Convert to factors
###        Change column names
#colnames(Pilot) <- c("Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Q7CE1", "Q8CE2","Q9CE3","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey")
#Pilot2 <- Pilot
#Pilot2 <- data.frame(Pilot2)
#
#for (i in colnames(Pilot)){
#  if (is.factor(Pilot[[i]]) == TRUE){
#    Pilot2[[i]] <- as.numeric(Pilot[[i]])-1
#  }
#}
#
#
#Pilot2$Q3Distance[Pilot2$Q3Distance == 1] <- 4
#Pilot2$Q3Distance[Pilot2$Q3Distance == 0] <- 1
#Pilot2$Q7CE1[Pilot2$Q7CE1 == 0] <-2 #The CE sets Status Quo as 1 and Alternative to 0 so this switches it around
#Pilot2$Q7CE1[Pilot2$Q7CE1 == 1] <-0
#Pilot2$Q7CE1[Pilot2$Q7CE1 == 2] <-1 
#Pilot2$Q8CE2[Pilot2$Q8CE2 == 0] <-2 
#Pilot2$Q8CE2[Pilot2$Q8CE2 == 1] <-0
#Pilot2$Q8CE2[Pilot2$Q8CE2 == 2] <-1 
#Pilot2$Q9CE3[Pilot2$Q9CE3 == 0] <-2 
#Pilot2$Q9CE3[Pilot2$Q9CE3 == 1] <-0
#Pilot2$Q9CE3[Pilot2$Q9CE3 == 2] <-1 
#Pilot2$Q17Understanding[Pilot2$Q17Understanding == 0] <-3 # Understanding question should be weak, average, strong not 1,2,0
#Pilot2$Q20Education[Pilot2$Q20Education == 2] <-3 #Just shifting every value up one for education
#Pilot2$Q20Education[Pilot2$Q20Education == 1] <-2
#Pilot2$Q20Education[Pilot2$Q20Education == 0] <-1
#Pilot2$Q21Employment[Pilot2$Q21Employment == 0] <-5 #Switch full to 5 for now
#Pilot2$Q21Employment[Pilot2$Q21Employment == 1] <-0 #Change NEET to zero
#Pilot2$Q21Employment[Pilot2$Q21Employment == 2] <-1 #Change Part to 1 
#Pilot2$Q21Employment[Pilot2$Q21Employment == 4] <-2 #Self stays same so student changes to 2
#Pilot2$Q21Employment[Pilot2$Q21Employment == 5] <-4 #Put full back as highest value
#Pilot2$Q22Income[Pilot2$Q22Income == 7] <-0.5 #The only wrong assignment of Employment was the 500-1000 level which it put last?
#Pilot2$Q22Income[Pilot2$Q22Income == 6] <-7
#Pilot2$Q22Income[Pilot2$Q22Income == 5] <-6
#Pilot2$Q22Income[Pilot2$Q22Income == 4] <-5
#Pilot2$Q22Income[Pilot2$Q22Income == 3] <-4
#Pilot2$Q22Income[Pilot2$Q22Income == 2] <-3
#Pilot2$Q22Income[Pilot2$Q22Income == 1] <-2
#Pilot2$Q22Income[Pilot2$Q22Income == 0.5] <-1
#
#
#Choices <- data.frame("Effectiveness.SQ" =c(0,0,0,0,0,0,0,0,0,0), 
#                      "Effectiveness.ALT" =c(100,0,0,0,10,10,10,50,50,50), 
#                      "Env.SQ" =c(0,0,0,0,0,0,0,0,0,0),
#                      "Env.ALT" =c(100,90,40,90,0,40,90,0,40,90),
#                      "Price.SQ" =c(0,0,0,0,0,0,0,0,0,0),
#                      "Price.ALT" =c(0,0,1,5,1,5,0,5,0,1),
#                      "Health.SQ" =c(0,0,0,0,0,0,0,0,0,0),
#                      "Health.ALT" =c(0,0.1, 0.1,0.6,0.6,1,0.1,0.1,0.6,1))
#
#SpecificChoices <- data.frame("Effectiveness.SQ" =c(0,0,0), 
#                      "Effectiveness.ALT" =c(100,0,0), 
#                      "Env.SQ" =c(0,0,0),
#                      "Env.ALT" =c(100,90,40),
#                      "Price.SQ" =c(0,0,0),
#                      "Price.ALT" =c(0,0,1),
#                      "Health.SQ" =c(0,0,0),
#                      "Health.ALT" =c(0,0.1, 0.1))
#
#Chosen <- data.frame(Pilot2$Q7CE1,Pilot2$Q8CE2,Pilot2$Q9CE3)
#Chosen$Block <- rep(1,nrow(Chosen))
#Chosen$ID <- seq.int(nrow(Chosen))
#Chosen <- data.frame(Chosen$ID, Chosen$Block, Chosen$Pilot2.Q7CE1,Chosen$Pilot2.Q8CE2, Chosen$Pilot2.Q9CE3)
#colnames(Chosen) <- c("ID","BLOCK","Q7","Q8","Q9")