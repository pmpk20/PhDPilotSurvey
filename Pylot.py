# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
##########################################################################
##########################################################################
###############                                             ##############
###############     Section 1: Setup                        ############## 
###############                                             ##############
##########################################################################
##########################################################################




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




##########################################################################
##########################################################################
###############                                             ##############
###############     Section 2: Data pre-processing          ############## 
###############                                             ##############
##########################################################################
##########################################################################




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
Test = pd.concat([Pilot2,SpecificChoices,SQChoices],axis=1)

Choices = pd.concat([Pilot.iloc[:,7], Pilot.iloc[:,8], Pilot.iloc[:,9]],axis=1).transpose()
Choices = Choices.unstack().reset_index(drop=True)
Test['Q7CE1'] = Choices
Test = Test.drop([Test.columns[8],Test.columns[9]],axis='columns')
Task = pd.DataFrame(itertools.chain.from_iterable(itertools.repeat(range(3), Pilot.shape[0])))
Task.index = Test.index
Test.insert(1,"Task",Task,True)


Test.columns = ["ID","Task","Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Choice","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey","Effectiveness.ALT","Env.ALT","Price.ALT","Health.ALT","Effectiveness.SQ","Env.SQ","Price.SQ","Health.SQ"]
Test = statsmodels.tools.tools.add_constant(Test, prepend=True, has_constant='add')
Test[['const', 'ID', 'Task', 'Q1Gender', 'Q2Age', 'Q3Distance', 'Q4Trips',
      'Q10Action', 'Q11Self', 'Q12Others',
       'Q13Marine', 'Q14BP', 'Q15Responsibility', 'Q16Charity',
       'Q17Understanding', 'Q18Consequentiality', 'Q19Experts', 'Q20Education',
       'Q21Employment', 'Q22Income', 'Q23Survey',  'Q5CVM1', 'Q6QOV', 'Choice','Effectiveness.ALT',
       'Env.ALT', 'Price.ALT', 'Health.ALT', 'Effectiveness.SQ', 'Env.SQ',
       'Price.SQ', 'Health.SQ']]


Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q1Gender , Test.Q2Age , Test.Q3Distance , Test.Q4Trips , Test.Q5CVM1 ,Test.Q6QOV, Test.Q10Action ,  Test.Q11Self , Test.Q12Others , Test.Q13Marine , Test.Q14BP, Test.Q15Responsibility , Test.Q16Charity , Test.Q17Understanding, Test.Q18Consequentiality , Test.Q19Experts, Test.Q20Education, Test.Q21Employment ,  Test.Q22Income ,Test.Q23Survey],axis=1))


Test['ALT_AV'] = np.ones(Test.shape[0],dtype=int)
Test['SQ_AV'] = np.ones(Test.shape[0],dtype=int)

##########################################################################
##########################################################################
###############                                             ##############
###############     Section 3: Estimation of DCE models     ############## 
###############                                             ##############
##########################################################################
##########################################################################


##########################################################################
############### DCE method: PYLOGIT
############### Link: https://github.com/timothyb0912/pylogit/tree/master/examples/.ipynb_checkpoint 
############### Comment: Will learn but the package hasn't been updated since 2017 so maybe avoid
##########################################################################

!pip install pylogit
from collections import OrderedDict    # For recording the model specification 
import statsmodels.tools.numdiff as numdiff       # For numeric hessian
import scipy.linalg                    # For matrix inversion
import pylogit as pl                   # For choice model estimation
from pylogit import nested_logit as nl # For nested logit convenience funcs

## Here I cheat and import the working dataset I made in R
Test_Long = pd.read_csv("H:/PhDPilotSurvey/Test_Long.csv")
Test_Long.alt[Test_Long.alt == "SQ"] = 0
Test_Long.alt[Test_Long.alt == "ALT"] = 1
## I tried coercing the Python dataframe using PYLOGIT but it would not work.

B_specification = OrderedDict() ## To create the specification in PYLOGIT you need these dictionaries? Don't understand why but I understand how to work it
B_names = OrderedDict() ## A similar one for names

## Here I added the DCE attributes and an ASC intercept to the utility functions
B_specification["intercept"] = [1] ## 0,1 specifies what indirect utility function it equals. There may be J-1 ASCs so only one here.
B_names["intercept"] = ['ALT:(intercept)'] # Can pick any name but went with the generated one in R

B_specification["Price"] = [[0, 1]]
B_names["Price"] = ['Price']

B_specification["Health"] = [[0, 1]]
B_names["Health"] = ['Health']

#B_specification["Accumulation"] = [[0, 1]]
#B_names["Accumulation"] = ['Accumulation']

#B_specification["Effectiveness"] = [[0, 1]]
#B_names["Effectiveness"] = ['Effectiveness']
## You can add effectiveness or accumulation but not both and they ruin the values of the others. 
## R just straight up refused to estimate with them


## Here I add all the regressors which are from the dataframe.
B_specification["Q1Gender"] = [1]
B_names["Q1Gender"] = ['Q1Gender']
B_specification["Q2Age"] = [1]
B_names["Q2Age"] = ['Q2Age']
B_specification["Q3Distance"] = [1]
B_names["Q3Distance"] = ['Q3Distance']
B_specification["Q4Trips"] = [1]
B_names["Q4Trips"] = ['Q4Trips']
B_specification["Q6QOV"] = [1]
B_names["Q6QOV"] = ['Q6QOV']
B_specification["Q10Action"] = [1]
B_names["Q10Action"] = ['Q10Action']
B_specification["Q11Self"] = [1]
B_names["Q11Self"] = ['Q11Self']
B_specification["Q12Others"] = [1]
B_names["Q12Others"] = ['Q12Others']
B_specification["Q13Marine"] = [1]
B_names["Q13Marine"] = ['Q13Marine']
B_specification["Q14BP"] = [1]
B_names["Q14BP"] = ['Q14BP']
B_specification["Q15Responsibility"] = [1]
B_names["Q15Responsibility"] = ['Q15Responsibility']
B_specification["Q16Charity"] = [1]
B_names["Q16Charity"] = ['Q16Charity']
B_specification["Q17Understanding"] = [1]
B_names["Q17Understanding"] = ['Q17Understanding']
B_specification["Q18Consequentiality"] = [1]
B_names["Q18Consequentiality"] = ['Q18Consequentiality']
B_specification["Q19Experts"] = [1]
B_names["Q19Experts"] = ['Q19Experts']
B_specification["Q20Education"] = [1]
B_names["Q20Education"] = ['Q20Education']
B_specification["Q21Employment"] = [1]
B_names["Q21Employment"] = ['Q21Employment']
B_specification["Q22Income"] = [1]
B_names["Q22Income"] = ['Q22Income']
B_specification["Q23Survey"] = [1]
B_names["Q23Survey"] = ['Q23Survey']


Pilot_MNL = pl.create_choice_model(data=Test_Long,alt_id_col='alt',
                       obs_id_col='chid',choice_col='Choice',
                       model_type="MNL",specification=B_specification,
                       names=B_names)
## The create_choice_model function is similar to MLOGIT in R and produces the same output
## It requires specifying certain columns as noted above
Pilot_MNL.fit_mle(np.zeros(len(B_specification))) ## Initialises it with zeroes for starting values
Pilot_MNL.get_statsmodels_summary()
Pilot_MNL.print_summaries() ## Summarises the model

Pilot_MNL.fit_summary ## Summarises model fit 
np.round(Pilot_MNL.summary, 3) #  Rounds the above to 3 decimal places for ease of comparison.



##########################################################################
############### DCE method: STATSMODELS
############### Link: https://www.statsmodels.org/stable/generated/statsmodels.discrete.discrete_model.MultinomialResults.html
############### Comment: Not sure on other logits but widely used.
##########################################################################

# This formula throws a fit when using questions 13 (never works),14, 16
# Aim here is to estimate a fully general MNL model of Choice ~ Dependents and report the significant variables
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
PV.drop(PV[PV['PValue'] > 0.05].index , inplace=True)
print(PV)

## Aim of this block below is to estimate above with only the significant variables
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
PV.drop(PV[PV['PValue'] > 0.05].index , inplace=True)
print(PV)





##########################################################################
###############                                             ##############
###############     Section 3B: Estimation of CVM models    ############## 
###############                                             ##############
##########################################################################

#
# Q5CVM ~ Dependents using probit

##########################################################################
###############                                             ##############
###############     Section 3C: Estimation of QOV models    ############## 
###############                                             ##############
##########################################################################

#
# Q6QOV ~ Dependents using probit

##########################################################################
###############                                             ##############
###############     Section 4: Estimation of other models   ############## 
###############                                             ##############
##########################################################################

#
# Experts ~ Dependents using OLS

##########################################################################
###############                                             ##############
###############     Section 5: Hybrid Choice models?        ############## 
###############                                             ##############
##########################################################################

#
# Read this first: https://transp-or.epfl.ch/documents/talks/IVT10.pdf