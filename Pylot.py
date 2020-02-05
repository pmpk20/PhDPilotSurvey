# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
#####################################################################################
################ Section 1: Importing necessary packages ##########################
#####################################################################################
import os
os.chdir('H:\PhDPilotSurvey')   
import pandas as pd
import numpy as np
import matplotlib as mpl
import itertools
import statsmodels
from statsmodels.formula.api import ols
from patsy.contrasts import Sum
from cycler import cycler
mpl.rcParams['axes.prop_cycle'] = cycler(color='bgcrmyk')

#Step 2: Import data
Pilot = pd.read_csv("\\\\myfiles\pmpk20\dos\PhD\Data Collection\Pilot\PhD Survey_ Sample A.csv")


#
#####################################################################################
################ Section 2: Data pre-processing  ##########################
#####################################################################################

Pilot = Pilot.drop([Pilot.columns[0],Pilot.columns[1],Pilot.columns[7],Pilot.columns[26]],axis='columns')
Pilot = Pilot.reset_index()

Pilot.columns = ["ID","Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Q7CE1", "Q8CE2","Q9CE3","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey"]

Pilot2 = Pilot

SpecificChoices = pd.DataFrame({'Effectiveness.ALT':[0.0,0.0,0.0],'Env.ALT':[90.0,40.0,40.0],'Price.ALT':[0.0,1.0,1.0],'Health.ALT':[0.0,0.1,0.6] })

SQChoices = pd.DataFrame({'Effectiveness.SQ':[0.0,0.0,0.0],'Env.SQ':[0.0,0.0,0.0],'Price.SQ':[0.0,0.0,0.0],'Health.SQ':[1.0,1.0,1.0] })

Pilot2 = Pilot2.loc[Pilot2.index.repeat(SpecificChoices.shape[0])]
SpecificChoices = SpecificChoices.loc[SpecificChoices.index.repeat(Pilot.shape[0])]
SQChoices= SQChoices.loc[SQChoices.index.repeat(Pilot.shape[0])]
SpecificChoices.index = Pilot2.index
SQChoices.index = Pilot2.index
Test = pd.concat([Pilot,SpecificChoices,SQChoices],axis=1)

Choices = pd.concat([Pilot.iloc[:,7], Pilot.iloc[:,8], Pilot.iloc[:,9]],axis=1).transpose()
Choices = Choices.unstack().reset_index(drop=True)
Test['Q7CE1'] = Choices
Test = Test.drop([Test.columns[8],Test.columns[9]],axis='columns')
Task = pd.DataFrame(itertools.chain.from_iterable(itertools.repeat(range(3), Pilot.shape[0])))
Task.index = Test.index
Test.insert(1,"Task",Task,True)
Task = pd.DataFrame(itertools.chain.from_iterable(itertools.repeat(range(3), Pilot.shape[0])))

Test.columns = ["ID","Task","Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Choice","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey","Effectiveness.ALT","Env.ALT","Price.ALT","Health.ALT","Effectiveness.SQ","Env.SQ","Price.SQ","Health.SQ"]

Test.Choice[Test.Choice != "Alternative"] = 0
Test.Choice[Test.Choice == "Alternative"] = 1

#####################################################################################
################ Section 2B: Effects coding #########################################
#####################################################################################


contrast = Sum().code_without_intercept([np.count_nonzero(Test.Q1Gender.unique())])
print(contrast.matrix)
mod = ols("Q23Survey ~ C(Q1Gender)+ Q2Age", data=Test)
res = mod.fit()
print(res.summary())

Model1 = statsmodels.discrete.discrete_model.Logit(np.asfarray(Test.Choice),np.asfarray(Test.Q10Action))
# https://www.statsmodels.org/dev/generated/statsmodels.discrete.discrete_model.Logit.html


#####################################################################################
################ R Translation ######################################################
#####################################################################################


#for (i in colnames(Pilot)){
#  if (is.factor(Pilot[[i]]) == TRUE){
#    Pilot2[[i]] <- as.numeric(Pilot[[i]])-1
#  }
#}
#
#Pilot2$Q3Distance[Pilot2$Q3Distance == 1] <- 4
#Pilot2$Q3Distance[Pilot2$Q3Distance == 0] <- 1
#Pilot2$Q6QOV <- t(t(1-Pilot2$Q6QOV)) # Change the QOV coding so 1 is precaution
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

#summary(glm(Test$Q5CVM1 ~ Test$Q1Gender + Test$Q2Age + Test$Q3Distance + Test$Q4Trips + Test$Q10Action + Test$Q22Income + Test$Q21Employment + Test$Q20Education + Test$Q11Self + Test$Q12Others + Test$Q13Marine + Test$Q14BP + Test$Q15Responsibility + Test$Q16Charity + Test$Q17Understanding, data=Test))
#
## Here I've made a function that plots which variables are the most significant
#Significance <- function(Model) {
#  PV <- data.frame(summary(Model)$coefficients[,4],summary(Model)$coefficients[,1] )
#  colnames(PV) <- c("PV","Effect")
#  PV <- subset(PV,PV <=0.05)
#  PV <- data.frame(row.names(PV), PV$PV, PV$Effect)
#  colnames(PV) <- c("Variables","PV","Effect")
#  return(barplot(PV$Effect, names.arg = PV$Variables,xlab = "Variables",ylab = "Effect",ylim = c(-2,5),axes = TRUE))
#}