# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
##########################################################################
############### Current Issues:                             ##############
############### -- Translate APOLLO to BIOGEME              ##########          ########## 
##########################################################################


##########################################################################
###############     Section 0: Setup                        ############## 

%reset -f
import os #
os.chdir('H:\PhDPilotSurvey')   ## Set directory as the github repo
import pandas as pd
import numpy as np
import matplotlib as mpl ## For graphing - like ggplot2 for R
import itertools
import statsmodels
from patsy.contrasts import Sum
from cycler import cycler 
mpl.rcParams['axes.prop_cycle'] = cycler(color='bgcrmyk') ## Change graph colour scheme
from sklearn.preprocessing import LabelEncoder ## Helpful to recode data if starting from the data straight from the survey company


## Installing pylogit for the CE modelling
!pip install pylogit
from collections import OrderedDict    # For recording the model specification 
import statsmodels.tools.numdiff as numdiff       # For numeric hessian
import scipy.linalg                    # For matrix inversion
import pylogit as pl                   # For choice model estimation
from pylogit import nested_logit as nl # For nested logit convenience funcs


## Install BIOGEME to translate from APOLLO
!pip install biogeme
import biogeme.database as db
import biogeme.biogeme as bio
import biogeme.models as models
import biogeme.messaging as msg
from biogeme.expressions import Beta,DefineVariable, exp, log, MonteCarlo, bioDraws


##########################################################################
###############     Section 1: Data Import ############## 


## Here I start from the Full_Long data to avoid reshaping it but can start from Full_Final if need
Full_Long = pd.read_csv("H:/PhDPilotSurvey/Full_Long.csv")

## Recode the alternative variable in [0,1] for ease of analysis
Full_Long.alt[Full_Long.alt == "A"] = 0
Full_Long.alt[Full_Long.alt == "B"] = 1
Full_Long["Choice"] = Full_Long["Choice"].astype(int) ## Again make numeric for analysis



##########################################################################
###############     Section 2: Sample Truncation ############## 


Full_Truncated = Full_Long
Protestors = [24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028]
Full_Truncated = Full_Truncated[~Full_Truncated.ID.isin(Protestors)] ## Drop protest votes
Full_Truncated = Full_Truncated.drop(Full_Truncated[Full_Truncated.Q25Understanding<7].index) ## Drop those who did not understand the survey that well
Full_Truncated = Full_Truncated.drop(Full_Truncated[Full_Truncated.Q8DominatedTest==1].index) ## Drop respondents failing the dominated scenario test
Full_Truncated = Full_Truncated.drop(Full_Truncated[Full_Truncated.Q12CECertainty==0].index) ## Drop uncertain respondents
Full_Truncated = Full_Truncated.drop(Full_Truncated[Full_Truncated.Q20Consequentiality==0].index) ## Drop those who view participation as inconsequential
Full_Truncated = Full_Truncated.drop(columns="Unnamed: 0")
Full_Truncated.index = range(len(Full_Truncated)) ## Reset index to avoid issues
len(Full_Truncated)/8 ## Just for reporting sake to make sure it is the correct number



##########################################################################
############### PYLOGIT MODELS
##########################################################################


## Basic Conditional logit first
specification_dict = OrderedDict()
name_dict = OrderedDict()

specification_dict["intercept"] = [1]
name_dict["intercept"] = ["ASC_B"]
specification_dict["Price"] = [[0,1]]
name_dict["Price"] = ["Price"]
specification_dict["Performance"] = [[0,1]]
name_dict["Performance"] = ["Performance"]
specification_dict["Emission"] = [[0,1]]
name_dict["Emission"] = ["Emission"]

Base_MNL = pl.create_choice_model(data=Full_Truncated,
                                   alt_id_col="alt",
                                   obs_id_col="chid",
                                   choice_col="Choice",
                                   specification=specification_dict,
                                   model_type="MNL",
                                   names=name_dict)

initial_values = np.zeros(len(specification_dict))
Base_MNL.fit_mle(initial_values)
Base_MNL.get_statsmodels_summary()


## Basic Multinomial Logit
B_specification = OrderedDict() ## To create the specification in PYLOGIT you need these dictionaries? Don't understand why but I understand how to work it
B_names = OrderedDict() ## A similar one for names
## Here I added the DCE attributes and an ASC intercept to the utility functions
B_specification["intercept"] = [1] ## 0,1 specifies what indirect utility function it equals. There may be J-1 ASCs so only one here.
B_names["intercept"] = ['ALT:(B)'] # Can pick any name but went with the generated one in R
B_specification["Price"] = [[0, 1]]
B_names["Price"] = ['Price']
B_specification["Performance"] = [[0, 1]]
B_names["Performance"] = ['Performance']
B_specification["Emission"] = [[0, 1]]
B_names["Emission"] = ['Emission']
B_specification["Q1Gender"] = [1]
B_names["Q1Gender"] = ['Q1Gender']
B_specification["Q2Age"] = [1]
B_names["Q2Age"] = ['Q2Age']
B_specification["Q3Distance"] = [1]
B_names["Q3Distance"] = ['Q3Distance']
B_specification["Q4Trips"] = [1]
B_names["Q4Trips"] = ['Q4Trips']
B_specification["Q16BP"] = [1]
B_names["Q16BP"] = ['Q16BP']
B_specification["Q18Charity"] = [1]
B_names["Q18Charity"] = ['Q18Charity']
B_specification["Q20Consequentiality"] = [1]
B_names["Q20Consequentiality"] = ['Q20Consequentiality']
B_specification["Q21Experts"] = [1]
B_names["Q21Experts"] = ['Q21Experts']
B_specification["Q22Education"] = [1]
B_names["Q22Education"] = ['Q22Education']
B_specification["Q23Employment"] = [1]
B_names["Q23Employment"] = ['Q23Employment']
B_specification["Q24AIncome"] = [1]
B_names["Q24AIncome"] = ['Q24AIncome']
B_specification["Q25Understanding"] = [1]
B_names["Q25Understanding"] = ['Q25Understanding']
B_specification["Order"] = [1]
B_names["Order"] = ['Order']
B_specification["Task"] = [1]
B_names["Task"] = ['Task']
B_specification["Q12CECertainty"] = [1]
B_names["Q12CECertainty"] = ['Q12CECertainty']


Pilot_MNL = pl.create_choice_model(data=Full_Truncated,alt_id_col='alt',
                       obs_id_col='chid',choice_col='Choice',
                       model_type="MNL",specification=B_specification,
                       names=B_names)

Pilot_MNL.fit_mle(np.zeros(len(B_specification)))  ## Initialises it with zeroes for starting values
Pilot_MNL.get_statsmodels_summary()
Pilot_MNL.print_summaries() ## Summarises the model
Pilot_MNL.fit_summary ## Summarises model fit 
np.round(Pilot_MNL.summary, 3) #  Rounds the above to 3 decimal places for ease of comparison.




## Basic MXL
B_specification = OrderedDict() ## To create the specification in PYLOGIT you need these dictionaries? Don't understand why but I understand how to work it
B_names = OrderedDict() ## A similar one for names
## Here I added the DCE attributes and an ASC intercept to the utility functions
B_specification["intercept"] = [1] ## 0,1 specifies what indirect utility function it equals. There may be J-1 ASCs so only one here.
B_names["intercept"] = ['ALT:(B)'] # Can pick any name but went with the generated one in R
B_specification["Price"] = [[0, 1]]
B_names["Price"] = ['Price']
B_specification["Performance"] = [[0, 1]]
B_names["Performance"] = ['Performance']
B_specification["Emission"] = [[0, 1]]
B_names["Emission"] = ['Emission']


index_var_names = ["Price", "Performance", "Emission"]
example_mixed = pl.create_choice_model(data=Full_Truncated,
                                       alt_id_col="alt",
                                       obs_id_col="chid",
                                       choice_col="Choice",
                                       specification=B_specification,
                                       model_type="Mixed Logit",
                                       names=B_names,
                                       mixing_id_col="ID",
                                       mixing_vars=index_var_names)

example_mixed.fit_mle(init_vals=np.zeros(2 * len(index_var_names)+1),
                      num_draws=600,seed=123)
example_mixed.get_statsmodels_summary()


##########################################################################
############### BIOGEME MODELS
##########################################################################


##########################################################################
############### Conditional Logit

# Read the data
df = pd.read_csv('Test_Truncated.csv')
database = db.Database('Test_Apollo', df)
globals().update(database.variables)


# Parameters to be estimated
ASC_B = Beta('ASC_B', 0, None, None, 0)
ASC_A = Beta('ASC_A', 0, None, None, 1)
B_Price = Beta('B_Price', 0, None, None, 0)
B_Performance = Beta('B_Performance', 0, None, None, 0)
B_Emission = Beta('B_Emission', 0, None, None, 0)


# Definition of the utility functions
V1 = ASC_A + \
     B_Price * Price_A + \
     B_Performance * Performance_A + \
     B_Emission * Emission_A
V2 = ASC_B + \
     B_Price * Price_B + \
     B_Performance * Performance_B + \
     B_Emission * Emission_B


# Associate utility functions with the numbering of alternatives
V = {1: V1,
     2: V2}

# Associate the availability conditions with the alternatives
av = {1: av_A,
      2: av_B}

# Definition of the model. This is the contribution of each
# observation to the log likelihood function.
logprob = models.loglogit(V, av, Choice)

# Create the Biogeme object
biogeme = bio.BIOGEME(database, logprob)
biogeme.modelName = 'ConditionalLogit'

# Estimate the parameters
results = biogeme.estimate()

# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)


##########################################################################
############### Multinomial Logit


# Parameters to be estimated
ASC_B = Beta('ASC_B', 0, None, None, 0)
ASC_A = Beta('ASC_A', 0, None, None, 1)
B_Price = Beta('B_Price', 0, None, None, 0)
B_Performance = Beta('B_Performance', 0, None, None, 0)
B_Emission = Beta('B_Emission', 0, None, None, 0)
B_Q1Gender = Beta('B_Q1Gender', 0, None, None, 0)
B_Q2Age = Beta('B_Q2Age', 0, None, None, 0)
B_Q3Distance = Beta('B_Q3Distance', 0, None, None, 0)
B_Q4Trips = Beta('B_Emission', 0, None, None, 0)
B_Q16BP = Beta('B_Q16BP', 0, None, None, 0)
B_Q18Charity = Beta('B_Q18Charity', 0, None, None, 0)
B_Education = Beta('B_Education', 0, None, None, 0)
B_Employment = Beta('B_Employment', 0, None, None, 0)
B_Income = Beta('B_Income', 0, None, None, 0)
B_Order = Beta('B_Order', 0, None, None, 0)
B_Task = Beta('B_Task', 0, None, None, 0)
B_Consequentiality = Beta('B_Consequentiality', 0, None, None, 0)
B_Experts = Beta('B_Experts', 0, None, None, 0)
B_Survey = Beta('B_Survey', 0, None, None, 0)
B_Q12CECertainty = Beta('B_Q12CECertainty', 0, None, None, 0)


# Definition of the utility functions
V1 = ASC_A + \
     B_Price * Price_A + \
     B_Performance * Performance_A + \
     B_Emission * Emission_A
V2 = ASC_B + B_Q1Gender*Q1Gender + B_Q2Age*Age +\
    B_Q3Distance * Distance + \
    B_Q4Trips * Trips +\
    B_Q16BP * BP +\
    B_Q18Charity * Charity + \
    B_Education * Education +\
    B_Employment * Employment + \
    B_Income * Income +\
    B_Order * Order +      \
    B_Task * Task +\
    B_Consequentiality * Consequentiality + \
    B_Experts * Experts+\
    B_Survey*Survey +\
    B_Q12CECertainty*Q12CECertainty +\
     B_Price * Price_B + \
     B_Performance * Performance_B + \
     B_Emission * Emission_B


# Associate utility functions with the numbering of alternatives
V = {1: V1,
     2: V2}

# Associate the availability conditions with the alternatives
av = {1: av_A,
      2: av_B}

# Definition of the model. This is the contribution of each
# observation to the log likelihood function.
logprob = models.loglogit(V, av, Choice)

# Create the Biogeme object
biogeme = bio.BIOGEME(database, logprob)
biogeme.modelName = 'MNL'

# Estimate the parameters
results = biogeme.estimate()

# Get the results in a pandas table
pandasResults = results.getEstimatedParameters()
print(pandasResults)


##########################################################################
############### Mixed Logit

# Parameters to be estimated
ASC_B = Beta('ASC_B', 0, None, None, 0)
ASC_A = Beta('ASC_A', 0, None, None, 1)

Mu_Price = Beta('Mu_Price', -3, None, None, 0)
Sig_Price = Beta('Sig_Price', 1, None, None, 0)
B_Price = -exp(Mu_Price + Sig_Price * bioDraws('B_Price', 'NORMAL'))

Mu_Performance = Beta('Mu_Performance', -3, None, None, 0)
Sig_Performance = Beta('Sig_Performance', 1, None, None, 0)
B_Performance = -exp(Mu_Performance + Sig_Performance * bioDraws('B_Performance', 'NORMAL'))

Mu_Emission = Beta('Mu_Emission', -3, None, None, 0)
Sig_Emission = Beta('Sig_Emission', 1, None, None, 0)
B_Emission = -exp(Mu_Emission + Sig_Emission * bioDraws('B_Emission', 'NORMAL'))

# Definition of the utility functions
V1 = ASC_A + \
     B_Price * Price_A + \
     B_Performance * Performance_A + \
     B_Emission * Emission_A
V2 = ASC_B + \
     B_Price * Price_B + \
     B_Performance * Performance_B + \
     B_Emission * Emission_B

V = {1: V1,
     2: V2}
av = {1: av_A,
      2: av_B}


# Conditional to B_TIME_RND, we have a logit model (called the kernel)
prob = models.logit(V, av, Choice)

# We integrate over B_TIME_RND using Monte-Carlo
logprob = log(MonteCarlo(prob))

# Define level of verbosity
logger = msg.bioMessage()
#logger.setSilent()
#logger.setWarning()
logger.setGeneral()
#logger.setDetailed()

# Create the Biogeme object
biogeme = bio.BIOGEME(database, logprob, numberOfDraws=10000)
biogeme.modelName = 'MXL19'

# Estimate the parameters
results = biogeme.estimate()
pandasResults = results.getEstimatedParameters()
print(pandasResults)


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




##########################################################################
############### PILOT CE MODELS: OLD BAD CODE DO NOT USE
##########################################################################


##########################################################################
##########################################################################
###############                                             ##############
###############     Section 2: Data pre-processing          ############## 
###############                                             ##############
##########################################################################
##########################################################################


#Import data
Pilot = pd.read_csv("\\\\myfiles\pmpk20\dos\PhD\Data Collection\Pilot\PhD Survey_ Sample A.csv")


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


###############     Section 3B: Estimation of CVM models    ############## 


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


###############     Section 3C: Estimation of QOV models    ############## 


PVadjusted("Q6QOV",0.05)
QOVProbit = Model2

###############     Section 4: Estimation of other models   ############## 

PVadjusted("Q14BP",0.05)
BPProbit = Model2
