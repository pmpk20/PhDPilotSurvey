# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 18:19:06 2020

@author: pmpk20
"""

#Script to replicate Pilot Data Analysis in Python
#Step 1: Import necessary packages
import os
os.chdir('H:/dos/PhD/Data Collection/Pilot')   
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
os.chdir('H:/dos/PhD/Data Collection/Pilot')    
!git clone https://github.com/pmpk20/PhDPilotSurvey.git
!git remote set-url origin https://github.com/pmpk20/PhDPilotSurvey.git
