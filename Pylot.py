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
Pilot = pd.read_csv("\\\\myfiles\pmpk20\dos\PhD\Data Collection\Pilot\PhD Survey_ Sample A.csv")


#
#####################################################################################
################ Section 2: Data pre-processing  ##########################
#####################################################################################
#
#
#Pilot <- Pilot[ -c(1,2,8,27)] #Drop rows of no importance to the quantitative analysis, namely text responses.
#
#colnames(Pilot) <- c("Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Q7CE1", "Q8CE2","Q9CE3","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey") #Renames columns for ease of analysis
#
#Pilot2 <- Pilot # Create a backup of the Pilot data
#
#Pilot2 <- data.frame(Pilot2) # Force to the data.frame format
#
#SpecificChoices <- data.frame("Effectiveness.ALT" =c(100,0,0), 
#                              "Env.ALT" =c(100,90,40),
#                              "Price.ALT" =c(0,0,1),
#                              "Health.ALT" =c(0,0.1, 0.1))
### Aim here is to make a dataframe with the choice set data. Extremely important for later.
#
#Effectiveness <- factor(SpecificChoices$Effectiveness.ALT) #Forces an attribute of the DCE to be a factor for ease of analysis
#
#Accumulation <- factor(SpecificChoices$Env.ALT) #Forces an attribute of the DCE to be a factor for ease of analysis
#
#Price <- factor(SpecificChoices$Price.ALT) #Forces an attribute of the DCE to be a factor for ease of analysis
#
#Health <- factor(SpecificChoices$Health.ALT) #Forces an attribute of the DCE to be a factor for ease of analysis
#
#contrasts(Effectiveness) <- contr.sum(unique(SpecificChoices$Effectiveness.ALT)) # Ensures a DCE attribute is appropriately stored as a factor
#
#contrasts(Accumulation) <- contr.sum(unique(SpecificChoices$Env.ALT))# Ensures a DCE attribute is appropriately stored as a factor
#
#contrasts(Price) <- contr.sum(unique(SpecificChoices$Price.ALT))# Ensures a DCE attribute is appropriately stored as a factor
#
#contrasts(Health) <- contr.sum(unique(SpecificChoices$Health.ALT))# Ensures a DCE attribute is appropriately stored as a factor
#
#SpecificChoices <- data.frame(Effectiveness, Accumulation, Price, Health) # Creates a dataframe with all the DCE attributes in levels
#
#Pilot2$ID <- seq.int(nrow(Pilot2)) # Adds an ID column to the Pilot survey. This is anonymous and bears no relation to actual respondents.
#
#for (i in colnames(Pilot2)){
#  if (is.factor(Pilot[[i]]) == TRUE){
#    contrasts(Pilot2[,i]) <- contr.sum(unique(Pilot2[,i]))
#  }
#}
### Aim of the function is to express all variables in the Pilot data as factors
#
#library(dplyr) # Important for data manipulation
#
#Test <- cbind(slice(.data = Pilot2,rep(1:n(), each = nrow(SpecificChoices))),slice(.data = SpecificChoices,rep(1:n(), times = nrow(Pilot2))))
## So TEST is a dataframe that transforms the PILOT data into an appropriate format for the estimation.
## The code repeats each row of the PILOT data for each choice the respondent made. Therefore, each respondent now has three rows one for Q7, Q8, Q9.
#
#Choices <- data.frame(a = c(t(data.frame(rep(Pilot2[,7:9], times=1)))[,]))
## Forces the responses to Q7, Q8, Q9 from PILOT to be in one long column. Each respondent has three rows.
#
#Test <- data.frame(Test$ID,Test[1:6],Choices,Test[10:23],Test[25:28])
## Combines and reorders the TEST dataframe. Use View(Test) to see that this dataframe combines the choice sets, responses, and respondent data.
#
#colnames(Test) <- c("ID","Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Choice","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey","Effectiveness","Accumulation","Price","Health")
## Adds and updates column names.
#
#
###########################################################################
################ Section 3: Estimation of models ##########################
###########################################################################
#
#
#library(nnet)
#summary(multinom(Test$Choice ~ Test$Q1Gender +Test$Q2Age +Test$Q3Distance+Test$Q4Trips+Test$Q10Action+Test$Q11Self + Test$Q12Others + Test$Q13Marine + Test$Q14BP + Test$Q15Responsibility + Test$Q16Charity + Test$Q17Understanding + Test$Q18Consequentiality + Test$Q19Experts + Test$Q20Education + Test$Q21Employment + Test$Q22Income + Test$Q23Survey, data=Test))
