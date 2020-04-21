#Peter King
####################################################################################
############### Introduction: First 100 Data Analysis Script  ##########################
####################################################################################

####################################################################################
############### Section 1: Import Data  ##########################
####################################################################################

install.packages("mlogit")
install.packages("gmnl")
install.packages("stargazer")
rm(list = ls())
############ Importing data:

setwd("H:/PhDPilotSurvey") # Sets working directory. This is where my Github repo is cloned to.

Pilot <- data.frame(read.csv("PhD Survey_ Sample A.csv")) # Imports the pilot survey data as a data.frame

