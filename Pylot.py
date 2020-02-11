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


Dependents = pd.DataFrame(pd.concat([Test.const, Test.Q1Gender , Test.Q2Age , Test.Q3Distance , Test.Q4Trips , Test.Q6QOV, Test.Q10Action ,  Test.Q11Self , Test.Q12Others , Test.Q13Marine , Test.Q14BP, Test.Q15Responsibility , Test.Q16Charity , Test.Q17Understanding, Test.Q18Consequentiality , Test.Q20Education, Test.Q21Employment ,  Test.Q22Income ,Test.Q23Survey],axis=1))

## Need to add ALT and SQ AV columns
## Need to add a 0,1 for whether choice is alt/sq

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

Test.head().T
DD = Dependents.columns.tolist()[1:18] ##


AV_variables = {u'Effectiveness': dict([(0, 'Effectiveness.SQ'),
                                               (1, 'Effectiveness.ALT')]),
                          u'Env': dict([(0, 'Env.SQ'),
                                                (1, 'Env.ALT')]),
                          u'Price': dict([(0, 'Price.SQ'),
                                            (1, 'Price.ALT')]),
                          u'Health': dict([(0, 'Health.SQ'),
                                            (1, 'Health.ALT')])}

Availability_variables = {0: 'SQ_AV',1:'ALT_AV'}

Custom_ALT_ID = "modes_ID"

OBS_ID_COLUMN = "custom_id"
Test[OBS_ID_COLUMN] = np.arange(Test.shape[0],
                                            dtype=int) + 1
Choice_Column = "Choice"

long_test = pl.convert_wide_to_long(Test, DD,  
                                AV_variables, Availability_variables, 
                                OBS_ID_COLUMN, Choice_Column, 
                                new_alt_id_name=Custom_ALT_ID)

####### Fine to here

B_specification = OrderedDict()
B_names = OrderedDict()

B_specification["intercept"] = [1]
B_names["intercept"] = ['ASC ALT']

B_specification["Effectiveness"] = [0, 1]
B_names["Effectiveness"] = ['Effectiveness_SQ','Effectiveness_ALT']

B_specification["Price"] = [0, 1]
B_names["Price"] = ['Price_SQ','Price_ALT']

B_specification["Health"] = [0, 1]
B_names["Health"] = ['Health_SQ','Health_ALT']

B_specification["Env"] = [0, 1]
B_names["Env"] = ['Env_SQ','Env_ALT']


long_test = long_test.drop([long_test.columns[11],long_test.columns[12]],axis='columns')

MNLT = pl.create_choice_model(data=long_test,alt_id_col=Custom_ALT_ID,obs_id_col=OBS_ID_COLUMN,choice_col=Choice_Column,specification=B_specification,model_type="MNL")
MNLT.fit_mle(np.zeros(9))
# Singular matrix produced arghhhhhhhhhhhh

MNLT.get_statsmodels_summary()

MNLT.print_summaries()

MNLT.fit_summary

np.round(MNLT.summary, 3)


## To Do: basic_specification downwards

#############################################################################
#############################################################################
wide_swiss_metro = pd.read_csv("H:\PhDPilotSurvey\swissmetro.dat",sep='\t')
include_criteria = (wide_swiss_metro.PURPOSE.isin([1, 3]) &
                    (wide_swiss_metro.CHOICE != 0))
wide_swiss_metro = wide_swiss_metro.loc[include_criteria].copy()
wide_swiss_metro.head().T
ind_variables = wide_swiss_metro.columns.tolist()[:15] ##

alt_varying_variables = {u'travel_time': dict([(1, 'TRAIN_TT'),
                                               (2, 'SM_TT'),
                                               (3, 'CAR_TT')]),
                          u'travel_cost': dict([(1, 'TRAIN_CO'),
                                                (2, 'SM_CO'),
                                                (3, 'CAR_CO')]),
                          u'headway': dict([(1, 'TRAIN_HE'),
                                            (2, 'SM_HE')]),
                          u'seat_configuration': dict([(2, "SM_SEATS")])}

availability_variables = {1: 'TRAIN_AV',
                          2: 'SM_AV', 
                          3: 'CAR_AV'}

custom_alt_id = "mode_id"

obs_id_column = "custom_id"
wide_swiss_metro[obs_id_column] = np.arange(wide_swiss_metro.shape[0],
                                            dtype=int) + 1

choice_column = "CHOICE"
long_swiss_metro = pl.convert_wide_to_long(wide_swiss_metro, 
                                           ind_variables, 
                                           alt_varying_variables, 
                                           availability_variables, 
                                           obs_id_column, 
                                           choice_column,
                                           new_alt_id_name=custom_alt_id)

long_swiss_metro.head(10).T

long_swiss_metro["travel_time_hrs"] = long_swiss_metro["travel_time"] / 60.0

long_swiss_metro["headway_hrs"] = long_swiss_metro["headway"] / 60.0

long_swiss_metro["free_ticket"] = (((long_swiss_metro["GA"] == 1) |
                                    (long_swiss_metro["WHO"] == 2)) &
                                   long_swiss_metro[custom_alt_id].isin([1,2])).astype(int)

long_swiss_metro["travel_cost_hundreth"] = (long_swiss_metro["travel_cost"] *
                                            (long_swiss_metro["free_ticket"] == 0) /
                                            100.0)

long_swiss_metro["single_luggage_piece"] = (long_swiss_metro["LUGGAGE"] == 1).astype(int)

long_swiss_metro["multiple_luggage_pieces"] = (long_swiss_metro["LUGGAGE"] == 3).astype(int)

long_swiss_metro["regular_class"] = 1 - long_swiss_metro["FIRST"]

long_swiss_metro["train_survey"] = 1 - long_swiss_metro["SURVEY"]


basic_specification = OrderedDict()
basic_names = OrderedDict()
## Edits up to here
basic_specification["intercept"] = [1, 2]
basic_names["intercept"] = ['ASC Train',
                            'ASC Swissmetro']

basic_specification["travel_time_hrs"] = [[1, 2,], 3]
basic_names["travel_time_hrs"] = ['Travel Time, units:hrs (Train and Swissmetro)',
                                  'Travel Time, units:hrs (Car)']

basic_specification["travel_cost_hundreth"] = [1, 2, 3]
basic_names["travel_cost_hundreth"] = ['Travel Cost * (Annual Pass == 0), units: 0.01 CHF (Train)',
                                       'Travel Cost * (Annual Pass == 0), units: 0.01 CHF (Swissmetro)',
                                       'Travel Cost, units: 0.01 CHF (Car)']

basic_specification["headway_hrs"] = [1, 2]
basic_names["headway_hrs"] = ["Headway, units:hrs, (Train)",
                              "Headway, units:hrs, (Swissmetro)"]

basic_specification["seat_configuration"] = [2]
basic_names["seat_configuration"] = ['Airline Seat Configuration, base=No (Swissmetro)']

basic_specification["train_survey"] = [[1, 2]]
basic_names["train_survey"] = ["Surveyed on a Train, base=No, (Train and Swissmetro)"]

basic_specification["regular_class"] = [1]
basic_names["regular_class"] = ["First Class == False, (Swissmetro)"]

basic_specification["single_luggage_piece"] = [3]
basic_names["single_luggage_piece"] = ["Number of Luggage Pieces == 1, (Car)"]

basic_specification["multiple_luggage_pieces"] = [3]
basic_names["multiple_luggage_pieces"] = ["Number of Luggage Pieces > 1, (Car)"]


swissmetro_mnl = pl.create_choice_model(data=long_swiss_metro,
                                        alt_id_col=custom_alt_id,
                                        obs_id_col=obs_id_column,
                                        choice_col=choice_column,
                                        specification=basic_specification,
                                        model_type="MNL")

swissmetro_mnl.fit_mle(np.zeros(14))

swissmetro_mnl.get_statsmodels_summary()

swissmetro_mnl.print_summaries()

swissmetro_mnl.fit_summary

np.round(swissmetro_mnl.summary, 3)

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