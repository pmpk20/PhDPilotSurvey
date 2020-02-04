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
import itertools
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

#Tests <- data.frame(Test[,1:2],Test$Choice,Test[,24:31])
## Takes the core elements of the TEST data frame
#
#
#levels(Tests$Test.Choice)[1] <- "ALT"
#levels(Tests$Test.Choice)[2] <- "SQ"
