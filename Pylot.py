# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
##########################################################################
############### Current Issues:                             ##############
############### -- All samples work except PILOT_CONS           ########## 
############### -- Need to find MWTP, bootstrapping, LCM and HCM methods##
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
###############                                             ##############
###############     Trimming the sample                     ############## 
###############                                             ##############
##########################################################################


Test_Long = pd.read_csv("H:/PhDPilotSurvey/Test_Long.csv")
## Here I cheat and import the working dataset I made in R
Test_Long.alt[Test_Long.alt == "SQ"] = 0
Test_Long.alt[Test_Long.alt == "ALT"] = 1
Test_Long["Choice"] = Test_Long["Choice"].astype(int)


Pilot_Dominated = Test_Long
for i in range(len(list(Test_Long.ID[(Test_Long.Task == 1) & (Test_Long.Choice ==0) & (Test_Long["Unnamed: 0"].str.contains("SQ") == False)]))):
    Pilot_Dominated = Pilot_Dominated.drop(Pilot_Dominated[Pilot_Dominated.ID == list(Pilot_Dominated.ID[(Pilot_Dominated.Task == 1) & (Pilot_Dominated.Choice ==0) & (Pilot_Dominated["Unnamed: 0"].str.contains("SQ") == False)].unique())[0] ].index)

for i in range(len(list(Pilot_Dominated.ID[Pilot_Dominated.Q23Survey <= 5].unique()))):
    Pilot_Dominated = Pilot_Dominated.drop(Pilot_Dominated[Pilot_Dominated.ID ==  list(Pilot_Dominated.ID[Pilot_Dominated.Q23Survey <= 5].unique())[0] ].index,axis=0)
Pilot_Understanding = Pilot_Dominated  

Pilot_Cons = Pilot_Understanding.drop(Pilot_Understanding[Pilot_Understanding.Q18Consequentiality == 0].index,axis=0)
Pilot_Cons = Pilot_Cons.drop(columns="Unnamed: 0")
Test_Long = Pilot_Cons
Test_Long.index = range(len(Test_Long))
    

!pip install pylogit
from collections import OrderedDict    # For recording the model specification 
import statsmodels.tools.numdiff as numdiff       # For numeric hessian
import scipy.linalg                    # For matrix inversion
import pylogit as pl                   # For choice model estimation
from pylogit import nested_logit as nl # For nested logit convenience funcs


##########################################################################
############### MULTINOMIAL LOGIT 
############### ISSUES:
## Marginal Effects
### No idea how to calculate from PYLOGIT
#
## P values
### Can be done just haven't
#
## WTP and MRS calculations:
### Again, no idea about marginal effects so idk WTP
#
##########################################################################


## Basic Multinomial logit first
specification_dict = OrderedDict()
name_dict = OrderedDict()

specification_dict["intercept"] = [1]
name_dict["intercept"] = ["ASC ALT"]
specification_dict["Price"] = [[0,1]]
name_dict["Price"] = ["Price"]
specification_dict["Health"] = [[0,1]]
name_dict["Health"] = ["Health"]

Base_MNL = pl.create_choice_model(data=Test_Long,
                                   alt_id_col="alt",
                                   obs_id_col="chid",
                                   choice_col="Choice",
                                   specification=specification_dict,
                                   model_type="MNL",
                                   names=name_dict)

initial_values = np.zeros(len(specification_dict))
Base_MNL.fit_mle(initial_values)
Base_MNL.get_statsmodels_summary()


## Extended Multinomial logit second
B_specification = OrderedDict() ## To create the specification in PYLOGIT you need these dictionaries? Don't understand why but I understand how to work it
B_names = OrderedDict() ## A similar one for names
## Here I added the DCE attributes and an ASC intercept to the utility functions
B_specification["intercept"] = [1] ## 0,1 specifies what indirect utility function it equals. There may be J-1 ASCs so only one here.
B_names["intercept"] = ['ALT:(intercept)'] # Can pick any name but went with the generated one in R
B_specification["Price"] = [[0, 1]]
B_names["Price"] = ['Price']
B_specification["Health"] = [[0, 1]]
B_names["Health"] = ['Health']
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
B_specification["Q14BP"] = [1]
B_names["Q14BP"] = ['Q14BP']
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

Pilot_MNL.fit_mle(np.zeros(len(B_specification)))  ## Initialises it with zeroes for starting values
Pilot_MNL.get_statsmodels_summary()
Pilot_MNL.print_summaries() ## Summarises the model
Pilot_MNL.fit_summary ## Summarises model fit 
np.round(Pilot_MNL.summary, 3) #  Rounds the above to 3 decimal places for ease of comparison.

## Likelihood ratio test for models
from scipy.stats.distributions import chi2
def likelihood_ratio(llmin, llmax):
    return(2*(llmax-llmin))
LR = likelihood_ratio(Base_MNL.log_likelihood,Pilot_MNL.log_likelihood)
p = chi2.sf(LR, 1) # L2 has 1 DoF more than L1
print('p: %.30f' % p) 


#Pilot_MNLTL = pl.create_choice_model(data=Pilot_Cons,alt_id_col='alt',
#                       obs_id_col='chid',choice_col='Choice',
#                       model_type="MNL",specification=B_specification,
#                       names=B_names)
#
#Pilot_MNLTL.fit_mle(np.zeros(len(B_specification)))
#Pilot_MNLTL.get_statsmodels_summary()
#Pilot_MNLTL.coefs[1]


# Values per sample used:
#PD: 26.655270580934108
#PU: 26.655270580934108
#PC:
#TL: 27.281266847596676


##########################################################################
############### MIXED LOGIT
##########################################################################


Test_Long["Choice"] = Test_Long["Choice"].astype(int)

index_var_names = ["Price", "Accumulation"]
for col in index_var_names:
    Test_Long[col] = Test_Long[col].astype(float)
example_specification = OrderedDict()
example_names = OrderedDict()

for col in index_var_names:
    example_specification[col] = [[0,1]]
    example_names[col] = [col]

MIXLuncorrelated = pl.create_choice_model(data=Test_Long,
                                       alt_id_col="alt",
                                       obs_id_col="chid",
                                       choice_col="Choice",
                                       specification=example_specification,
                                       model_type="Mixed Logit",
                                       names=example_names,
                                       mixing_id_col="ID",
                                       mixing_vars=index_var_names)

MIXLuncorrelated.fit_mle(init_vals=np.zeros(2 * len(index_var_names)),
                      num_draws=600,
                      seed=123)

MIXLuncorrelated.get_statsmodels_summary()

####################################

index_var_names = ["Price", "Accumulation",'Q1Gender', 
                   'Q2Age', 'Q3Distance', 'Q4Trips',
      'Q10Action', 'Q11Self', 'Q12Others',
       'Q13Marine', 'Q14BP', 'Q16Charity',
       'Q17Understanding', 'Q18Consequentiality', 'Q19Experts', 'Q20Education',
       'Q21Employment', 'Q22Income', 'Q23Survey']
for col in index_var_names:
    Test_Long[col] = Test_Long[col].astype(float)
example_specification = OrderedDict()
example_names = OrderedDict()

for col in index_var_names:
    example_specification[col] = [[0,1]]
    example_names[col] = [col]

MXLFull = pl.create_choice_model(data=Test_Long,
                                       alt_id_col="alt",
                                       obs_id_col="chid",
                                       choice_col="Choice",
                                       specification=example_specification,
                                       model_type="Mixed Logit",
                                       names=example_names,
                                       mixing_id_col="ID",
                                       mixing_vars=index_var_names)

MXLFull.fit_mle(init_vals=np.zeros(2 * len(index_var_names)),
                      num_draws=600,
                      seed=123)

MXLFull.get_statsmodels_summary()
## Note some slight differences to MLOGIT due to randomness


##########################################################################
###############                                             ##############
###############     Section 3B: Estimation of CVM models    ############## 
###############                                             ##############
##########################################################################


from statsmodels.discrete.discrete_model import Probit

#Test_Long = Pilot_Cons

def PVadjusted(DD, Sig):
    global Model, Model2, ME, PV
    Dependents = pd.DataFrame(pd.concat([Test_Long.Q1Gender , Test_Long.Q2Age, 
                                     Test_Long.Q3Distance , Test_Long.Q4Trips , 
                                     Test_Long.Q5CVM1,Test_Long.Q6QOV, Test_Long.Q14BP, 
                                     Test_Long.Q16Charity , 
                                     Test_Long.Q17Understanding, Test_Long.Q18Consequentiality , 
                                     Test_Long.Q19Experts, Test_Long.Q20Education, 
                                     Test_Long.Q21Employment , Test_Long.Q22Income],axis=1))
    Dependents = Dependents.drop(columns=DD)
    Y = Test_Long[DD]
    X = Dependents
    X = statsmodels.tools.tools.add_constant(X)
    Model = Probit(Y, X.astype(float)).fit()
    print(Model.get_margeff().summary())
    PV = Model.pvalues[Model.pvalues < Sig]
    a = []
    for i in range(PV.shape[0]):
        if PV.index[i] == "const":
            i
        else:
            Data = pd.DataFrame([Test_Long[str(PV.index[i])]]).transpose()
            a.append(Data)
    a=pd.concat(a,axis=1)
    Y = Test_Long[DD]
    X = a
    X = statsmodels.tools.tools.add_constant(X)
    Model2= Probit(Y, X.astype(float)).fit()
    print(Model2.get_margeff().summary())
    ME = Model2.get_margeff().summary()
    PV = Model2.pvalues[Model2.pvalues <Sig]

PVadjusted("Q5CVM1",0.05)
CVMProbit2 = Model2


##########################################################################
###############                                             ##############
###############     Section 3C: Estimation of QOV models    ############## 
###############                                             ##############
##########################################################################


PVadjusted("Q6QOV",0.05)
QOVProbit = Model2

##########################################################################
###############                                             ##############
###############     Section 4: Estimation of other models   ############## 
###############     Blue Planet                             ##############
##########################################################################

PVadjusted("Q14BP",0.05)
BPProbit = Model2

##########################################################################
###############                                             ##############
###############     Section 5: Hybrid Choice models?        ############## 
###############                                             ##############
##########################################################################

#
# Read this first: https://transp-or.epfl.ch/documents/talks/IVT10.pdf

##########################################################################
###############                                             ##############
###############     Discounting and NPV                     ############## 
###############                                             ##############
##########################################################################


import pandas as pd
import numpy as np
!pip install numpy_financial
import numpy_financial as npf 
    
def NPV(X):
    Base = pd.DataFrame([X])
    Total = pd.concat([Base],axis=1)
    Total.columns = ["Base"]
    Total.index = ["Totals"]
    BaseCosts = pd.DataFrame([Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"]])
    BaseCosts = BaseCosts.transpose()
    BaseCosts.columns = range(10)
    return (round(npf.npv(0.035,BaseCosts.iloc[0,:]),2))
    
    
Households =27800000
Products =23000000

ResearchPointCost =20
ResearchLowerCost =5
ResearchUpperCost =100
ResearchPointBenefit =1480
ResearchLowerBenefit =1390
ResearchUpperBenefit =1570


WWTPPointCost =1370
WWTPLowerCost =1000
WWTPUpperCost =5000
WWTPPointBenefit =2050
WWTPLowerBenefit =1970
WWTPUpperBenefit =2120

CosmeticsPointCost =1010
CosmeticsLowerCost =213
CosmeticsUpperCost =2500
CosmeticsPointBenefit =82.8
CosmeticsLowerBenefit =78.2
CosmeticsUpperBenefit =89.7

#### NPV
pd.concat([pd.DataFrame({'-20%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.8)) )/1000,2)]}),
pd.DataFrame({'-10%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.9)) )/1000,2)]}),
pd.DataFrame({'-5%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.95)) )/1000,2)]}),
pd.DataFrame({'0%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1)) )/1000,2)]}),
pd.DataFrame({'+5%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.05)) )/1000,2)]}),
pd.DataFrame({'+10%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.1)) )/1000,2)]}),
pd.DataFrame({'+20%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.2)) )/1000,2)]})],axis=1)

#### B:C Ratio
pd.concat([pd.DataFrame({'-20%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.8),2)]}),
pd.DataFrame({'-10%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.9),2)]}),
pd.DataFrame({'-5%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.95),2)]}),
pd.DataFrame({'0%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1),2)]}),
pd.DataFrame({'+5%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.05),2)]}),
pd.DataFrame({'+10%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.1),2)]}),
pd.DataFrame({'+20%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.2),2)]})],axis=1)




Households = 27800000
Products = 23000000
ResearchPoint = 20000000/27800000
ResearchLower = 5
ResearchUpper = 100

WWTPPoint = 1370
WWTPLower = 1000
WWTPUpper = 5000
WWTPPoint = 1370000000/27800000

CosmeticsPoint = 1010
CosmeticsLower = 2130
CosmeticsUpper = 2500
CosmeticsPoint = 1010000000/23000000

Q6 = 53.25
Q7 = 73.71
SampleY = 2192
Em = 0.038
Perf = 0.045


Explicits = pd.concat([pd.concat([pd.DataFrame([np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)),
np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)),
np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)),
np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)),
np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)),
np.mean(Q6*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))]).transpose(),
pd.DataFrame([round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)))/1000,2),
              round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)))/1000,2),
              round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)))/1000,2),
              round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)))/1000,2),
              round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)))/1000,2),
round(NPV((Q6*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))-np.mean(ResearchPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5)))/1000,2)]).transpose()],axis=0),
pd.concat([pd.DataFrame([np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)),
np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)),
np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)),
np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)),
np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)),
np.mean(Q7*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))]).transpose(),
pd.DataFrame([round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)))/1000,2),
              round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)))/1000,2),
              round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)))/1000,2),
              round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)))/1000,2),
              round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)))/1000,2),
              round(NPV((Q7*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))-np.mean(WWTPPoint*Households*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5)))/1000,2)]).transpose()],axis=0),
pd.concat([pd.DataFrame([np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)),
np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)),
np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)),
np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)),
np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)),
np.mean(Em*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))]).transpose(),
pd.DataFrame([round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0)))/1000,2),
              round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.5)))/1000,2),
              round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),0.75)))/1000,2),
              round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1)))/1000,2),
              round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.25)))/1000,2),
              round(NPV((Em*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5))-np.mean(CosmeticsPoint*Products*np.power(np.mean(SampleY/FullSurvey2.Q24AIncome),1.5)))/1000,2)]).transpose()],axis=0)])

Explicits.columns = ["e0","e0.5","e0.75","e1","e1.25","e1.5"]
Explicits.index=["Q6 WTP","Q6 NPV","Q7 WTP","Q7 NPV","Emissions MWTP","Restriction NPV"]
Explicits = round(Explicits,2)
Explicits.to_latex()




