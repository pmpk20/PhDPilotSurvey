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
from sklearn.preprocessing import LabelEncoder

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

LE = LabelEncoder()
for i in range(Pilot2.shape[1]):
    Pilot2.iloc[:,i] = LE.fit_transform(y=Pilot2.iloc[:,i])


Pilot2.Q3Distance[Pilot2.Q3Distance == 1] = 4.0
Pilot2.Q3Distance[Pilot2.Q3Distance == 0] = 1.0
#Pilot2.Q6QOV = t(t(1-Pilot2.Q6QOV)) # Change the QOV coding so 1 is precaution
Pilot2.Q7CE1[Pilot2.Q7CE1 == 0] =2.0 #The CE sets Status Quo as 1 and Alternative to 0 so this switches it around
Pilot2.Q7CE1[Pilot2.Q7CE1 == 1] =0.0
Pilot2.Q7CE1[Pilot2.Q7CE1 == 2] =1.0
Pilot2.Q8CE2[Pilot2.Q8CE2 == 0] =2.0
Pilot2.Q8CE2[Pilot2.Q8CE2 == 1] =0.0
Pilot2.Q8CE2[Pilot2.Q8CE2 == 2] =1.0
Pilot2.Q9CE3[Pilot2.Q9CE3 == 0] =2.0
Pilot2.Q9CE3[Pilot2.Q9CE3 == 1] =0.0
Pilot2.Q9CE3[Pilot2.Q9CE3 == 2] =1.0
Pilot2.Q17Understanding[Pilot2.Q17Understanding == 0] =3.0 # Understanding question should be weak, average, strong not 1,2,0
Pilot2.Q20Education[Pilot2.Q20Education == 2] = 3.0 #Just shifting every value up one for education
Pilot2.Q20Education[Pilot2.Q20Education == 1] = 2.0
Pilot2.Q20Education[Pilot2.Q20Education == 0] = 1.0
Pilot2.Q21Employment[Pilot2.Q21Employment == 0] = 5.0 #Switch full to 5 for now
Pilot2.Q21Employment[Pilot2.Q21Employment == 1] = 0.0 #Change NEET to zero
Pilot2.Q21Employment[Pilot2.Q21Employment == 2] = 1.0 #Change Part to 1
Pilot2.Q21Employment[Pilot2.Q21Employment == 4] = 2.0 #Self stays same so student changes to 2
Pilot2.Q21Employment[Pilot2.Q21Employment == 5] = 4.0 #Put full back as highest value
Pilot2.Q22Income[Pilot2.Q22Income == 7] = 0.5 #The only wrong assignment of Employment was the 500-1000 level which it put last?
Pilot2.Q22Income[Pilot2.Q22Income == 6] = 7.0
Pilot2.Q22Income[Pilot2.Q22Income == 5] = 6.0
Pilot2.Q22Income[Pilot2.Q22Income == 4] = 5.0
Pilot2.Q22Income[Pilot2.Q22Income == 3] = 4.0
Pilot2.Q22Income[Pilot2.Q22Income == 2] = 3.0
Pilot2.Q22Income[Pilot2.Q22Income == 1] = 2.0
Pilot2.Q22Income[Pilot2.Q22Income == 0.5] = 1.0


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


Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q1Gender , Test.Q2Age , Test.Q3Distance , Test.Q4Trips , Test.Q6QOV, Test.Q10Action ,  Test.Q22Income , Test.Q21Employment , Test.Q20Education , Test.Q11Self , Test.Q12Others , Test.Q13Marine , Test.Q14BP + Test.Q15Responsibility , Test.Q16Charity , Test.Q17Understanding, Test.Q18Consequentiality, Test.Q19Experts],axis=1))
Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q1Gender , Test.Q2Age , Test.Q3Distance , Test.Q4Trips , Test.Q6QOV, Test.Q10Action ,  Test.Q11Self , Test.Q12Others , Test.Q13Marine , Test.Q14BP + Test.Q15Responsibility , Test.Q16Charity , Test.Q17Understanding, Test.Q18Consequentiality , Test.Q20Education, Test.Q21Employment ,  Test.Q22Income ,Test.Q23Survey],axis=1))


Test = statsmodels.tools.tools.add_constant(Test, prepend=True, has_constant='add')

##########################################################################
############### Section 3: Estimation of DCE models #####################
##########################################################################


# SKLEARN MNL approach: works but no summary, great.

# PYLOGIT approach:

from collections import OrderedDict    # For recording the model specification 

import pandas as pd                    # For file input/output
import numpy as np                     # For vectorized math operations
import statsmodels.tools.numdiff as numdiff       # For numeric hessian
import scipy.linalg                    # For matrix inversion

import pylogit as pl                   # For choice model estimation
from pylogit import nested_logit as nl # For nested logit convenience funcs






# STATSMODELS approach: hates 13 (never works),14, 16
Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q1Gender , Test.Q2Age , Test.Q3Distance , Test.Q4Trips , Test.Q6QOV, Test.Q10Action ,Test.Q11Self , Test.Q12Others,Test.Q15Responsibility,Test.Q17Understanding, Test.Q18Consequentiality, Test.Q19Experts  , Test.Q20Education, Test.Q21Employment ,  Test.Q22Income, Test.Q23Survey ],axis=1))
Model1 = statsmodels.discrete.discrete_model.MNLogit(np.asfarray(Test.Choice),Dependents.to_numpy()).fit()
Model1.summary(alpha=0.05,yname='DCE Choice',xname=Dependents.columns.to_list())
Model1.get_margeff().summary()
PV = pd.DataFrame(pd.read_html(Model1.get_margeff().summary().tables[1].as_html(), header=0, index_col=0)[0])
PV = PV[:PV.index.get_loc('y=1')]
PV = pd.DataFrame([PV['dy/dx'],PV['P>|z|']])
PV = PV.transpose()
PV.index = ["Test.Q1Gender", "Test.Q2Age" , "Test.Q3Distance" , "Test.Q4Trips" , "Test.Q6QOV", "Test.Q10Action" ,  "Test.Q11Self" , "Test.Q12Others" , "Test.Q15Responsibility" , "Test.Q17Understanding", "Test.Q18Consequentiality" ,"Test.Q19Experts", "Test.Q20Education", "Test.Q21Employment" ,  "Test.Q22Income", "Test.Q23Survey"] 
PV.columns = ["Marginal Effect","PValue"]
PV['PValue'] = PV['PValue'].astype('float')
PV = PV.PValue[PV.PValue < 0.05]
print(PV)

Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q10Action ,Test.Q11Self , Test.Q12Others,Test.Q15Responsibility,Test.Q17Understanding, Test.Q20Education ,  Test.Q22Income ],axis=1))
Model1 = statsmodels.discrete.discrete_model.MNLogit(np.asfarray(Test.Choice),Dependents.to_numpy()).fit()
Model1.summary(alpha=0.05,yname='DCE Choice',xname=Dependents.columns.to_list())
print(Model1.get_margeff().summary())
PV = pd.DataFrame(pd.read_html(Model1.get_margeff().summary().tables[1].as_html(), header=0, index_col=0)[0])
PV = PV[:PV.index.get_loc('y=1')]
PV = pd.DataFrame([PV['dy/dx'],PV['P>|z|']])
PV = PV.transpose()
PV.index = [ "Test.Q10Action" ,  "Test.Q11Self" , "Test.Q12Others" , "Test.Q15Responsibility" , "Test.Q17Understanding", "Test.Q20Education",  "Test.Q22Income"] 
PV.columns = ["Marginal Effect","PValue"]
PV['PValue'] = PV['PValue'].astype('float')
PV = PV.PValue[PV.PValue < 0.05]
print(PV)





#Significance <- function(Model) {
#  PV <- data.frame(summary(Model).coefficients[,4],summary(Model).coefficients[,1] )
#  colnames(PV) <- c("PV","Effect")
#  PV <- subset(PV,PV <=0.05)
#  PV <- data.frame(row.names(PV), PV.PV, PV.Effect)
#  colnames(PV) <- c("Variables","PV","Effect")
#  return(barplot(PV.Effect, names.arg = PV.Variables,xlab = "Variables",ylab = "Effect",ylim = c(-2,5),axes = TRUE))
#}


# https://www.statsmodels.org/dev/generated/statsmodels.discrete.discrete_model.Logit.html



##########################################################################
############### Section 3B: Estimation of CVM models #####################
##########################################################################
#
#
#summary(glm(Test.Q5CVM1 ~ Test.Q1Gender + Test.Q2Age + Test.Q3Distance + Test.Q4Trips + Test.Q10Action + Test.Q22Income + Test.Q21Employment + Test.Q20Education + Test.Q11Self + Test.Q12Others + Test.Q13Marine + Test.Q14BP + Test.Q15Responsibility + Test.Q16Charity + Test.Q17Understanding, data=Test))
#
## Here I've made a function that plots which variables are the most significant
#Significance <- function(Model) {
#  PV <- data.frame(summary(Model).coefficients[,4],summary(Model).coefficients[,1] )
#  colnames(PV) <- c("PV","Effect")
#  PV <- subset(PV,PV <=0.05)
#  PV <- data.frame(row.names(PV), PV.PV, PV.Effect)
#  colnames(PV) <- c("Variables","PV","Effect")
#  return(barplot(PV.Effect, names.arg = PV.Variables,xlab = "Variables",ylab = "Effect",ylim = c(-2,5),axes = TRUE))
#}

##########################################################################
############### Section 3C: Estimation of QOV models #####################
##########################################################################


#require(ggplot2)
#ModelQOV <- (glm(Test.Q6QOV ~ Test.Q1Gender + Test.Q2Age + Test.Q3Distance + Test.Q4Trips + Test.Q10Action + Test.Q22Income + Test.Q21Employment + Test.Q20Education + Test.Q11Self + Test.Q12Others + Test.Q13Marine + Test.Q14BP + Test.Q15Responsibility + Test.Q16Charity + Test.Q17Understanding, data=Test))
#Significance(ModelQOV)
#ggplot(Test, aes(x = Q6QOV, y = Q3Distance, colour = Health_SQ)) + geom_point() + facet_wrap(~Q1Gender)


##########################################################################
############### Section 4: Estimation of other models #####################
##########################################################################

#
## Testing determinants of belief in experts
#Model1 <- lm(Test.Q19Experts ~ Test.Q1Gender + Test.Q2Age + Test.Q3Distance + Test.Q4Trips + Test.Q10Action + Test.Q22Income + Test.Q21Employment + Test.Q20Education + Test.Q11Self + Test.Q12Others + Test.Q13Marine + Test.Q14BP + Test.Q15Responsibility + Test.Q16Charity + Test.Q17Understanding)
#Significance(Model1)
