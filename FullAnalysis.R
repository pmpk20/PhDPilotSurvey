#### Survey data analysis script  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.


#### Section -1: Replication Information ####


# R version 4.0.2 (2020-06-22)
# RStudio Version 1.3.959
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Package information:
# stats     graphics  grDevices utils     datasets  methods   base     
# Rcpp_1.0.5          BiocManager_1.30.10 compiler_4.0.2      RSGHB_1.2.2        
# prettyunits_1.1.1   miscTools_0.6-26    tools_4.0.2         digest_0.6.25      
# pkgbuild_1.0.8      lattice_0.20-41     Matrix_1.2-18       cli_2.0.2          
# rstudioapi_0.11     maxLik_1.4-4        mvtnorm_1.1-1       SparseM_1.78       
# xfun_0.15           coda_0.19-4         MatrixModels_0.4-1  grid_4.0.2         
# glue_1.4.1          R6_2.4.1            randtoolbox_1.30.1  processx_3.4.2     
# fansi_0.4.1         callr_3.4.3         ps_1.3.3            mcmc_0.9-7         
# MASS_7.3-51.6       assertthat_0.2.1    mnormt_2.0.2        xtable_1.8-4       
# numDeriv_2016.8-1.1 Deriv_4.1.0         quantreg_5.55       sandwich_2.5-1     
# tinytex_0.24        MCMCpack_1.4-9      rngWELL_0.10-6      tmvnsim_1.0-2      
# crayon_1.3.4        zoo_1.8-8           apollo_0.1.0   


#### Section 0: Package installation ####


pkgbuild::has_build_tools()
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Rtools\\bin", sep = ";"))
pkgbuild::find_rtools(debug = TRUE)
install.packages("Rcpp") ## Necessary dependency for rngWELL
install.packages("rngWELL") ## Can sometimes fix APOLLO issues
install.packages("randtoolbox") ## Necessary for APOLLO random draws
install.packages("apollo") ## Most complex and powerful library for discrete choice
install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
install.packages("stargazer") ## To export to LaTeX code.
install.packages("dplyr") ## For data manipulation
install.packages("Hmisc") ## For random imputation
library(Hmisc)
library(dplyr)
library(ggplot2)
library(gridExtra)
options(scipen=50)
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
Full_Final <- data.frame(read.csv("FinalData.csv")) 


#### Section 1: Data importing and manipulating ####
#### To skip to estimation: import FinalData


FullSurvey <- data.frame(read.csv("FullSurvey.csv")) ## Imports from the excel file straight from the survey companies website.
# Full_Long <- data.frame(read.csv("Full_Long.csv"))
# Full_Final <- data.frame(read.csv("FinalData.csv")) ## Imports from the excel file straight from the survey companies website.
# FullSurvey <- FullSurvey[ !(FullSurvey$ï..Respondent %in% c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)), ] ## Drop protest rows


FullSurvey <- FullSurvey[ -c(4,14,15,16,23,24,26,27,58,68,69)] ## Drop columns of no importance to the quantitative analysis, namely text responses.


## Renaming the survey from original names to made up ones to link to the survey better.
colnames(FullSurvey) <- c("ID","Timing","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
                          "Q5Knowledge","Q6Bid","Q7Bid","Q6ResearchResponse",
                          "Q6ResearchCertainty","Q7TreatmentResponse",
                          "Q7TreatmentCertainty","Q7Bid2Upper","Q7Bid2Lower",
                          "Q7TreatmentUpperResponse","Q7TreatmentLowerResponse","Q8DominatedTest",
                          "Q9Block","Q9Performance","Q9Emission","Q9Price",
                          "Q10Block","Q10Performance","Q10Emission","Q10Price",
                          "Q11Block","Q11Performance","Q11Emission","Q11Price",
                          "Q12Block","Q12Performance","Q12Emission","Q12Price",
                          "Q9Choice","Q10Choice","Q11Choice","Q12Choice","Q12CECertainty",
                          "Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment",
                          "Q16BP","Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other",
                          "Q18Charity","Q19Knowledge","Q20Consequentiality",
                          "Q21Experts","Q22Education","Q23Employment",
                          "Q24RonaImpact","Q24AIncome","Q25Understanding")  


FullSurvey2 <- FullSurvey ## Create a backup of the FullSurvey data

# for (i in colnames(FullSurvey)){
#   if (is.factor(FullSurvey[[i]]) == TRUE){
#     FullSurvey2[[i]] <- as.numeric(FullSurvey[[i]])-1
#   }
# } ## Here convert all questions into numeric format for ease of analysis

for (i in colnames(FullSurvey)){
  if (is.factor(FullSurvey[[i]]) != TRUE){
    FullSurvey2[[i]] <- as.numeric(as.factor(as.character(FullSurvey[[i]])))-1
  }
} ## Use this if the previous one didn't work.

# FullSurvey2$Order[FullSurvey2$Order == 2] <-0 ## The order dummy should be 0 for Q6 > Q7 and 1 for Q7 > Q6


## Here I update the age categories to take the midpoint of the brackets.
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 0] <- 21.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 1] <- 32.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 2] <- 47.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 3] <- 63
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 4] <- 71
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 5.0] <- NA
FullSurvey2$Q2Age <- with(FullSurvey2, impute(FullSurvey2$Q2Age, 'random')) ## I replace the missing with a random imputed value
FullSurvey2$Q2Age <- as.numeric(FullSurvey2$Q2Age)


## The loop got the distances ordered incorrectly and also didn't use midpoints.
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 7
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 6
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 2
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 7] <- 3
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 6] <- 1
FullSurvey2$Q3Distance <- FullSurvey2$Q3Distance + 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 6.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 15.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 4] <- 35
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 5] <- 50
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 6] <- NA
FullSurvey2$Q3Distance <- with(FullSurvey2, impute(FullSurvey2$Q3Distance, 'random')) ## I replace the missing with a random imputed value
FullSurvey2$Q3Distance <- as.numeric(FullSurvey2$Q3Distance)


## Reordering the knowledge categories to reflect higher knowledge = higher value
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 4] <- 5
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 1] <- 6
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 6] <- 4
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 3] <- 1
FullSurvey2$Q5Knowledge[FullSurvey2$Q5Knowledge == 0] <- 3


## Changing to be unsure > quite sure > very sure
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 1] <-3
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 0] <- 1
FullSurvey2$Q6ResearchCertainty[FullSurvey2$Q6ResearchCertainty == 3] <- 0


## Same here, the coder was confused over the ordering.
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 1] <-3
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 0] <- 1
FullSurvey2$Q7TreatmentCertainty[FullSurvey2$Q7TreatmentCertainty == 3] <- 0


## More reordering here
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 1] <- 3
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 2] <- 1
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 3] <- 2


## More reordering here
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 1] <- 3
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 2] <- 1
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 3] <- 2


## Previously skipped or missed questions were set = 2 now they're NAs for ease of merging the two columns.
FullSurvey2$Q7TreatmentUpperResponse[FullSurvey2$Q7TreatmentUpperResponse == 2] <- NA
FullSurvey2$Q7TreatmentLowerResponse[FullSurvey2$Q7TreatmentLowerResponse == 2] <- NA


## As respondents did EITHER the upper or lower question there should only be one column. This requires using mutate and coalesce to merge the lower and upper responses.
FullSurvey <- mutate(Q7Bid2 = coalesce(FullSurvey$Q7Bid2Lower,FullSurvey$Q7Bid2Upper),.data = FullSurvey)
FullSurvey <- mutate(Q7Response2 = coalesce(FullSurvey$Q7TreatmentUpperResponse,FullSurvey$Q7TreatmentLowerResponse),.data = FullSurvey)

FullSurvey2 <- mutate(Q7Bid2 = coalesce(FullSurvey2$Q7Bid2Lower,FullSurvey2$Q7Bid2Upper),.data = FullSurvey2)
FullSurvey2 <- mutate(Q7Response2 = coalesce(FullSurvey2$Q7TreatmentUpperResponse,FullSurvey2$Q7TreatmentLowerResponse),.data = FullSurvey2)

FullSurvey2$Q6Bid <- FullSurvey$Q6Bid
FullSurvey2$Q7Bid <- FullSurvey$Q7Bid
FullSurvey2$Q7Bid2 <- FullSurvey$Q7Bid2


## The following section codes all the attributes as their actual values.
FullSurvey2$Q9Performance <- FullSurvey2$Q9Performance +1
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 2] <- 0.05
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 3] <- 0.5
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 1] <- 0.1
FullSurvey2$Q9Emission <- FullSurvey2$Q9Emission +1
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 2] <- 0.4
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 3] <- 0.9
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 1] <- 0.1
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 1] <- 2.5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 2] <- 5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 0] <- 1


## Converting automatic assignment into exact levels
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 1] <- 0.05
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 0] <- 0.1
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 0] <- 0.1
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 1] <- 0.4
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 2] <- 0.4
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 2] <- 1
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 1] <- 2.5
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 0] <- 0.5


## Converting automatic assignment into exact levels
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 0] <- 0.1
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 1] <- 0.05
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 0] <- 0.1
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 1] <- 0.9
FullSurvey2$Q11Price <- FullSurvey2$Q11Price+1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 1] <- 0.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 2] <- 1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 3] <- 2.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 4] <- 5


## Converting automatic assignment into exact levels
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 1] <- 0.05
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 2] <- 0.5
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 0] <- 0.1
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 1] <- 0.4
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 0] <- 0.1
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 1] <- 1
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 2] <- 2.5
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 0] <- 0.5
FullSurvey2$Q12Price[FullSurvey2$Q12Price == 3] <- 5


## Again certainty was confused so reordering here for higher number = more certain.
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 1] <- 3
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 0] <- 1
FullSurvey2$Q12CECertainty[FullSurvey2$Q12CECertainty == 3] <- 0


## The coder used zeros so changing that here by moving each value up one.
FullSurvey2$Q13CurrentThreatToSelf <- FullSurvey2$Q13CurrentThreatToSelf + 1
FullSurvey2$Q14FutureThreatToSelf <- FullSurvey2$Q14FutureThreatToSelf + 1
FullSurvey2$Q15ThreatToEnvironment <- FullSurvey2$Q15ThreatToEnvironment + 1


## More reordering of none > some > all
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 2] <- 3
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 0] <- 2
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 1] <- 0
FullSurvey2$Q16BP[FullSurvey2$Q16BP == 3] <- 1


## Changing it to be 1 = Chosen, 0 = Not Chosen:
FullSurvey2$Q17_Firms <- 1-FullSurvey2$Q17_Firms
FullSurvey2$Q17_Cons <- 1-FullSurvey2$Q17_Cons
FullSurvey2$Q17_Gov <- 1-FullSurvey2$Q17_Gov
FullSurvey2$Q17_LA <- 1-FullSurvey2$Q17_LA
FullSurvey2$Q17_Other <- 1-FullSurvey2$Q17_Other


## More reordering here
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 2] <- 3
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 1] <- 2
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 3] <- 1


## Same problem with Q5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 4] <- 5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 1] <- 4
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 0] <- 1
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 3] <- 0
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 1] <- 3
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 0] <- 1


## Reordering the consequentiality beliefs
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 0] <- 3
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 1] <- 0
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 2] <- 1
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 3] <- 2
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==2] <- 3
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==1] <- 2
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality==3] <- 1

## Belief in experts used a zero so just moving up one
FullSurvey2$Q21Experts <- FullSurvey2$Q21Experts +1


## Have to reorder education to GCSE > A level > Bachelor > Postgrad
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 4] <- 5
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 3] <- 4
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 1] <- 3
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 2] <- 1
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 0] <- 2
FullSurvey2$Q22Education[FullSurvey2$Q22Education == 5] <- 0


## New order: NEET > Retired > Student > Part > Self > Full
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 0] <- 7
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 3] <- 0
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 6] <- 3
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 7] <- 6
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 2] <- 7
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 4] <- 2
FullSurvey2$Q23Employment[FullSurvey2$Q23Employment == 7] <- 4


## Should be a dummy here with 2 for prefer not to say
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 1] <- 3
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 2] <- 1
FullSurvey2$Q24RonaImpact[FullSurvey2$Q24RonaImpact == 3] <- 2


## Changing the income to the midpoint of the brackets
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 8] <- 750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 4] <- 2750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 5] <- 3500.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 2] <- 1750.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 1] <- 1250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 3] <- 2250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 6] <- 4500.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 7] <- 5000
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 0] <- 250.00
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 9] <- NA 
FullSurvey2$Q24AIncome <- with(FullSurvey2, impute(FullSurvey2$Q24AIncome, 'random')) ## Using random imputation for missing values
FullSurvey2$Q24AIncome <- as.numeric(FullSurvey2$Q24AIncome)
FullSurvey2$Q24RonaImpact <- as.numeric(FullSurvey2$Q24RonaImpact)


## Updating the final survey question
FullSurvey2$Q25Understanding[FullSurvey2$Q25Understanding == 1] <- 10
FullSurvey2$Q25Understanding[FullSurvey2$Q25Understanding == 0] <- 1

## Adding an ID column which replaces the respondent category in the original dataset.
FullSurvey2$ID <- seq.int(nrow(FullSurvey2))

## Correcting the survey timing column:
FullSurvey2$Timing <- FullSurvey$Timing


## Aim of the function is to express all variables in the FullSurvey data as factors
for (i in colnames(FullSurvey2)){
  if (is.factor(FullSurvey[[i]]) == TRUE){
    contrasts(FullSurvey2[,i]) <- contr.sum(nlevels(FullSurvey2[,i]))
  }
} ## Using dummy coding


## Making a dataframe with the levels for Option A of the CE:
OptionA <- data.frame("Performance" =c(0,0,0,0), 
                      "Emission" =c(0,0,0,0),
                      "Price" =c(0,0,0,0))


library(dplyr) ## Essential library for data manipulation


## Full is a dataframe that transforms the FullSurvey data into an appropriate format for the estimation.
### The code repeats each row of the FullSurvey data for each choice the respondent made. Therefore, each respondent now has four rows one for Q9, Q10, Q11, Q12
Full <- cbind(slice(.data = FullSurvey2,rep(1:n(), each = 4)),slice(.data = OptionA,rep(1:n(), times = nrow(FullSurvey2))))

##  Creating a dataframe with all the levels that the Price attribute took for alternative B in the CE. 
DBPrice_B <- data.frame(Price_B = 
                          c(t(data.frame(rep(data.frame(FullSurvey2["Q9Price"],
                                                        FullSurvey2["Q10Price"],
                                                        FullSurvey2["Q11Price"],
                                                        FullSurvey2["Q12Price"]),
                                             times=1)))[,]))

##  Creating a dataframe with all the levels that the Performance attribute took for alternative B in the CE. 
DBPerformance_B <- data.frame(Performance_B = 
                                c(t(data.frame(rep(data.frame(FullSurvey2["Q9Performance"],
                                                              FullSurvey2["Q10Performance"],
                                                              FullSurvey2["Q11Performance"],
                                                              FullSurvey2["Q12Performance"]),
                                                   times=1)))[,]))

##  Creating a dataframe with all the levels that the Emission attribute took for alternative B in the CE. 
DBEmission_B <- data.frame(Emission_B = 
                             c(t(data.frame(rep(data.frame(FullSurvey2["Q9Emission"],
                                                           FullSurvey2["Q10Emission"],
                                                           FullSurvey2["Q11Emission"],
                                                           FullSurvey2["Q12Emission"]),
                                                times=1)))[,]))

##  Creating a single column that contains all respondents CE choices. 
Choices <- data.frame(Choice = c(t(
  data.frame(rep(FullSurvey2[,36:39], times=1)))[,]))

##  Chopping and reorganising the columns of the Full dataframe into a new order which includes the attributes and their levels alongside all the choices in a single vector.
### The final argument creates a variable called TASK
Full <- data.frame(Full[,1:14],Full[,19],DBPrice_B, DBPerformance_B, DBEmission_B,
                    Full[,61:63],Choices, Full[,20],Full[,24],Full[,28],
                    Full[,32],Full[,40:60],
                    rep(1:4,times=nrow(FullSurvey2)))

##  Assigning column names for ease of analysis.
colnames(Full) <- c("ID","Timing","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
                     "Q5Knowledge","Q6Bid","Q7Bid","Q6ResearchResponse",
                     "Q6ResearchCertainty","Q7TreatmentResponse",
                     "Q7TreatmentCertainty","Q8DominatedTest",
                     "Price_B","Performance_B","Emission_B", 
                     "Performance_A","Emission_A","Price_A","Choice",
                     "Q9Block","Q10Block","Q11Block","Q12Block","Q12CECertainty",
                     "Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment",
                     "Q16BP","Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other",
                    "Q18Charity","Q19Knowledge","Q20Consequentiality",
                     "Q21Experts","Q22Education","Q23Employment",
                     "Q24RonaImpact","Q24AIncome","Q25Understanding","Q7Bid2","Q7Response2",
                     "Task")  

Responsibility <- data.frame(cbind(Full$Q17_Firms,Full$Q17_Cons,Full$Q17_Gov,Full$Q17_LA,Full$Q17_Other))
colnames(Responsibility) <- c("Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other")
Full <- cbind(Full,"Responsibility" =rowSums(Responsibility))

Fulls <- Full # Later I use the Full dataframe but just without choies as A or B
Full$av_A <- rep(1,nrow(Full)) # Add a vector of ones to show that the alternative choice is always available to respondents.
Full$av_B <- rep(1,nrow(Full)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
Full$Choice[Full$Choice == 0] <- "A"  ## Necessary here to change numeric to string
Full$Choice[Full$Choice == 1] <- "B" ## The MFORMULA looks for _B or _A so choice must be SQ or ALT

# The data manipulation now moves into the CE specific manipulation.
library(mlogit) 

## Here the dataframe Full is reshaped from wide to long format for use in the MLOGIT estimations.
Full_Long <- mlogit.data(Full, shape = "wide", choice = "Choice",
                          varying = 16:21, sep = "_", id.var = "ID")




#### Section 2: Sample truncation ####
#### Notes: Protest IDs determined from eyeballing Excel responses.



### Truncation Rule One:
AllCriteria <- data.frame("IDs" = unique(Full_Long$ID[ (Full_Long$Q25Understanding >=7) &
                                                         (Full_Long$Timing/60 >= (median(Full_Long$Timing)/60)/100*48) &
                                                         (Full_Long$Q8DominatedTest == 0) &
                                                         (Full_Long$Q12CECertainty == 2) &
                                                         (Full_Long$Q20Consequentiality == 2) ])) 
AllCriteria1 <- AllCriteria[ !(AllCriteria$IDs %in% c()),]
Full_Full1 <- Full_Final[ (Full_Final$ID) %in% c(AllCriteria1),]
nrow(Full_Full1)/8


### Truncation Rule Two:
AllCriteria <- data.frame("IDs" = unique(Full_Long$ID[ (Full_Long$Q25Understanding >=7) &
                                  (Full_Long$Timing/60 >= (median(Full_Long$Timing)/60)/100*48) &
                                  (Full_Long$Q8DominatedTest == 0) &
                                  (Full_Long$Q12CECertainty >= 1) &
                                  (Full_Long$Q20Consequentiality >= 1) ])) 

## Here checking if any of the remaining respondents were on the protest list: 
AllCriteria <- AllCriteria[ !(AllCriteria$IDs %in% c()),]
Full_Full <- Full_Final[ (Full_Final$ID) %in% c(AllCriteria),] ## Fully truncated:
# Full_Full <- Full_Long[ (Full_Long$ID) %in% c(AllCriteria),] ## Can truncate the Full_Long dataframe too if needing to estimate in MLOGIT
Full_Excluded <- Full_Final[ !(Full_Final$ID) %in% c(AllCriteria),] ## The excluded responses
nrow(Full_Full)/8


length(Full_Long$ID[(Full_Long$Timing/60 >= (median(Full_Long$Timing)/60)/100*48)])/8
length(Full_Long$ID[(Full_Long$Timing/60 >= (median(Full_Long$Timing)/60)/100*50)])/8

## Can eliminate protestors here:
AllCriteria <- AllCriteria[ !(AllCriteria$IDs %in% c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)),]
# Note: Protests examined by eyeballing the text responses.

#### Section 2B: Sample Characteristics: ####

SummaryTable <- cbind("Q1Gender"=summary(FullSurvey2$Q1Gender),
      "Q2Age"=summary(FullSurvey2$Q2Age),
      "Q3Distance"=summary(FullSurvey2$Q3Distance),
      "Q4Trips"=summary(FullSurvey2$Q4Trips),
      "Q5Knowledge"=summary(FullSurvey2$Q5Knowledge),
      "Q6ResearchResponse"=summary(FullSurvey2$Q6ResearchResponse),
      "Q6ResearchCertainty"=summary(FullSurvey2$Q6ResearchCertainty),
      "Q7TreatmentResponse"=summary(FullSurvey2$Q7TreatmentResponse),
      "Q7TreatmentCertainty"=summary(FullSurvey2$Q7TreatmentCertainty),
      "Q7TreatmentUpperResponse"=summary(FullSurvey2$Q7TreatmentUpperResponse),
      "Q7TreatmentLowerResponse"=summary(FullSurvey2$Q7TreatmentLowerResponse),
      "Q8DominatedTest"=summary(FullSurvey2$Q8DominatedTest),
      "Q12CECertainty"=summary(FullSurvey2$Q12CECertainty),
      "Q13CurrentThreatToSelf"=summary(FullSurvey2$Q13CurrentThreatToSelf),
      "Q14FutureThreatToSelf"=summary(FullSurvey2$Q14FutureThreatToSelf),
      "Q15ThreatToEnvironment"=summary(FullSurvey2$Q15ThreatToEnvironment),
      "Q16BP"=summary(FullSurvey2$Q16BP),
      "Q18Charity"=summary(FullSurvey2$Q18Charity),
      "Q19Knowledge"=summary(FullSurvey2$Q19Knowledge),
      "Q20Consequentiality"=summary(FullSurvey2$Q20Consequentiality),
      "Q21Experts"=summary(FullSurvey2$Q21Experts),
      "Q22Education"=summary(FullSurvey2$Q22Education),
      "Q23Employment"=summary(FullSurvey2$Q23Employment),
      "Q24RonaImpact"=summary(FullSurvey2$Q24RonaImpact),
      "Q24AIncome"=summary(FullSurvey2$Q24AIncome),
      "Q25Understanding"=summary(FullSurvey2$Q25Understanding),
      "Timing"=summary(FullSurvey2$Timing),
      "Order"=summary(FullSurvey2$Order))
xtable::xtable(SummaryTable,digits=3)

# Gender proportion:
100/670*table(FullSurvey$Q1Gender)

# Mean age:
mean(FullSurvey2$Q2Age)

# Mean number of annual trips:
mean(FullSurvey2$Q4Trips)

# Sample education levels:
100/670*table(FullSurvey$Q22Education)

# Sample employment types:
100/670*table(FullSurvey$Q23Employment)





#### Section 3: Choice Experiment ####
############ Notes: Long and estimates a lot of models, initially in MLOGIT but later in GMNL.


library(mlogit) #Already have package installed
Full_Long <- mlogit.data(Full, shape = "wide", choice = "Choice",
                          varying = 16:21, sep = "_", id.var = "ID")


## Coding attribute levels in percentage points
Full_Long$Performance[Full_Long$Performance == 0.05] <- 5
Full_Long$Performance[Full_Long$Performance == 0.10] <- 10
Full_Long$Performance[Full_Long$Performance == 0.50] <- 50
Full_Long$Emission[Full_Long$Emission == 0.1] <- 10
Full_Long$Emission[Full_Long$Emission == 0.9] <- 90 
Full_Long$Emission[Full_Long$Emission == 0.4] <- 40 


## Sample truncation: 
Full_Dominated <- Full_Long[Full_Long$Q8DominatedTest == 0,]
# Full_Understanding <- Full_Dominated[Full_Dominated$Q25Understanding >= 5]
Full_Certain <- Full_Dominated[Full_Dominated$Q12CECertainty >= 2,]
Full_Cons <- Full_Certain[Full_Certain$Q20Consequentiality >= 1,]


###### Aim is here to estimate a range of possible specifications 

## Model 1: Attributes only MNL
MNL_1 <- mlogit(Choice ~  Price + Performance + Emission, 
                   Full_Long,
                   alt.subset = c("A","B"),reflevel = "A") 
summary(MNL_1) ## Estimates a simplistic mlogit model before adding in individual-specifics
MNL_1_WTP <- c(-1*coef(MNL_1)["Emission"]/coef(MNL_1)["Price"],-1*coef(MNL_1)["Performance"]/coef(MNL_1)["Price"])
MNL_1_WTP

## Model 2: Attributes only with quadratic terms: 
MNL_2 <- mlogit(Choice ~  Price + I(Performance^2) + I(Emission^2), 
                   Full_Long,
                   alt.subset = c("A","B"),reflevel = "A") 
summary(MNL_2) ## Estimates a simplistic mlogit model before adding in individual-specifics
MNL_2_WTP <- c(-1*coef(MNL_2)["I(Emission^2)"]/coef(MNL_2)["Price"],-1*coef(MNL_2)["I(Performance^2)"]/coef(MNL_2)["Price"])


## Model 3: MNL with all sociodemographics:
MNL_3 <- mlogit(Choice ~ Price + Performance + Emission | 
                      Order + Task + Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q12CECertainty + Q16BP + Q18Charity 
                    + Q20Consequentiality
                    + Q21Experts +Q22Education+ Q23Employment
                    +  Q24AIncome + Timing + Q25Understanding,  
                    Full_Long, alt.subset = c("A", "B"), 
                    reflevel = "A",method="bfgs") 
summary(MNL_3) ## Summarises the MNL output
MNL_3_WTP <- c(-1*coef(MNL_3)["Emission"]/coef(MNL_3)["Price"],-1*coef(MNL_3)["Performance"]/coef(MNL_3)["Price"])


## Model 3B: MNL with all sociodemographics with levels:
MNL_4 <- mlogit(Choice ~  Price + Performance.f5 + Emission, 
                Full_Long,
                alt.subset = c("A","B"),reflevel = "A") 
summary(MNL_4) ## Estimates a simplistic mlogit model before adding in individual-specifics
MNL_4_WTP <- c(-1*coef(MNL_4)["Emission"]/coef(MNL_4)["Price"],-1*coef(MNL_4)["Performance"]/coef(MNL_4)["Price"])



## Model 4: Now MIXED LOGIT - Attributes Only
MXL_1 <- mlogit(Choice ~ Price + Performance + Emission ,
  Full_Long, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=13)
summary(MXL_1)
MXL_1_WTP <- c(-1*coef(MXL_1)["Emission"]/coef(MXL_1)["Price"],-1*coef(MXL_1)["Performance"]/coef(MXL_1)["Price"])


## Model 5: MXL quadratic attributes only
MXL_2 <- mlogit(
  Choice ~ Price + I(Performance^2) + I(Emission^2) ,
  Full_Long, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_2)
MXL_2_WTP <- c(-1*coef(MXL_2)["I(Emission^2)"]/coef(MXL_2)["Price"],-1*coef(MXL_2)["I(Performance^2)"]/coef(MXL_2)["Price"])


## Model 6: MXL all sociodemographics, utility-space
MXL_3 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q12CECertainty + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n",Performance="n",Emission="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_3)
MXL_3_WTP <- c(-1*coef(MXL_3)["Emission"]/coef(MXL_3)["Price"],-1*coef(MXL_3)["Performance"]/coef(MXL_3)["Price"])


## Model 7: MXL all sociodemographics, WTP-space
MXL_4 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q12CECertainty + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing + Q25Understanding,
  Full_Long, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=13)
summary(MXL_4)
round(summary(MXL_4)$CoefTable,3)
MXL_4_WTP <- c(-1*coef(MXL_4)["Emission"]/coef(MXL_4)["Price"],-1*coef(MXL_4)["Performance"]/coef(MXL_4)["Price"])
MXL_4_WTP

data.frame(round(coef(MXL_4),3))
data.frame(round(stdEr(MXL_4),3))
data.frame(round(summary(MXL_4)$CoefTable[,3],3))
data.frame(round(summary(MXL_4)$CoefTable[,4],3))

## Model 7B: MXL all sociodemographics, WTP-space, attributes in levels
# MXL_4B <- mlogit(
#   Choice ~ Price + Performance.Level + Emission.Level | 
#     Order + Task + Q1Gender + Q2Age + Q3Distance
#   + Q4Trips + Q16BP + Q18Charity 
#   + Q20Consequentiality
#   + Q21Experts +Q22Education+ Q23Employment
#   +  Q24AIncome + Timing,
#   Full_Long, rpar=c(Price="n"),
#   R=1000,correlation = FALSE,
#   reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
# summary(MXL_4B)
# MXL_4_WTP <- c(-1*coef(MXL_4)["Emission"]/coef(MXL_4)["Price"],-1*coef(MXL_4)["Performance"]/coef(MXL_4)["Price"])



# Can truncate sample by protest votes:
# Full_Cons <- Full_Cons[ !(Full_Cons$ï..Respondent %in% c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)), ] ## Drop protest rows


## Model 8: MXL all sociodemographics, WTP-space, truncated sample
MXL_5 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q12CECertainty + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing + Q25Understanding,
  Full_Full, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5)
MXL_5_WTP <- c(-1*coef(MXL_5)["Emission"]/coef(MXL_5)["Price"],-1*coef(MXL_5)["Performance"]/coef(MXL_5)["Price"])

# Full_Long$Choice[Full_Long$Q12CECertainty == 0] <- 0
# Full_Full <- Full_Long[ (Full_Long$ID) %in% c(AllCriteria),]

#### Section 3B: Post-estimation analysis ####
############ Notes: LRtests, AIC, LLik, prediction accuracy, sensitivity analysis


## Testing whether the MXL is preferred to the MNL:   
lrtest(MNL_3,MXL_4)
lrtest(MXL_3,MXL_4)
lrtest(MXL_4,MXL_5)



############# Model goodness-of-fit:



## Storing all models AICs as an indicator of goodness-of-fit
Models_AIC <- round(t(data.frame("Model 1: MNL - Attributes only" = AIC(MNL_1),
                                 "Model 2: MNL - Quadratic attributes:"=AIC(MNL_2),
                                 "Model 3: MNL - All sociodemographics:"=AIC(MNL_3),
                                 "Model 4: MXL - Attributes Only"=AIC(MXL_1),
                                 "Model 5: MXL - Quadratic attributes"=AIC(MXL_2),
                                 "Model 6: MXL - SDs, utility-space"=AIC(MXL_3),
                                 "Model 7: MXL - SDs, WTP-space"=AIC(MXL_4),
                                 "Model 8: MXL - SDs, WTP-space, truncated sample"=AIC(MXL_5))),6) 

## Storing all models loglikelihoods as an indicator of goodness-of-fit 
Models_LogLik <- round(t(data.frame("Model 1: MNL - Attributes only" = logLik(MNL_1)[1],
                                    "Model 2: MNL - Quadratic attributes:"=logLik(MNL_2)[1],
                                    "Model 3: MNL - All sociodemographics:"=logLik(MNL_3)[1],
                                    "Model 4: MXL - Attributes Only"=logLik(MXL_1)[1],
                                    "Model 5: MXL - Quadratic attributes"=logLik(MXL_2)[1],
                                    "Model 6: MXL - SDs, utility-space"=logLik(MXL_3)[1],
                                    "Model 7: MXL - SDs, WTP-space"=logLik(MXL_4)[1],
                                    "Model 8: MXL - SDs, WTP-space, truncated sample"=logLik(MXL_5)[1])),6) 

Models_Evaluation <- cbind(AllWTPs,Models_AIC,Models_LogLik)
xtable::xtable(Models_Evaluation,digits=3)


############# Model prediction accuracy:


## MN_1 prediction accuracy
MNL_1_Predictions <- MNL_1$probabilities ## Getting probabilities of choosing each option from the model
colnames(MNL_1_Predictions) <- c(0,1) ## Name columns as each option 
MNL1Options <- data.frame("MNL1Options" = as.integer(colnames(MNL_1_Predictions)[apply(round(MNL_1_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MNL1Options)
MNL_1_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MNL1Options[ (Fulls$Choice == "A") & (Fulls$MNL1Options == 0)  ] ) + 
                     length(Fulls$MNL1Options[ (Fulls$Choice == "B") & (Fulls$MNL1Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MNL1Options[ (Fulls$Choice == "A") & (Fulls$MNL1Options == 1)  ] ) + 
                               length(Fulls$MNL1Options[ (Fulls$Choice == "B") & (Fulls$MNL1Options == 0)  ])))),3)
MNL_1_Accuracy


## MN_2 prediction accuracy
MNL_2_Predictions <- MNL_2$probabilities ## Getting probabilities of choosing each option from the model
colnames(MNL_2_Predictions) <- c(0,1) ## Name columns as each option 
MNL2Options <- data.frame("MNL2Options" = as.integer(colnames(MNL_2_Predictions)[apply(round(MNL_2_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MNL2Options)
MNL_2_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MNL2Options[ (Fulls$Choice == "A") & (Fulls$MNL2Options == 0)  ] ) + 
                     length(Fulls$MNL2Options[ (Fulls$Choice == "B") & (Fulls$MNL2Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MNL2Options[ (Fulls$Choice == "A") & (Fulls$MNL2Options == 1)  ] ) + 
                               length(Fulls$MNL2Options[ (Fulls$Choice == "B") & (Fulls$MNL2Options == 0)  ])))),3)
MNL_2_Accuracy


## MN_3 prediction accuracy
MNL_3_Predictions <- MNL_3$probabilities ## Getting probabilities of choosing each option from the model
colnames(MNL_3_Predictions) <- c(0,1) ## Name columns as each option 
MNL3Options <- data.frame("MNL3Options" = as.integer(colnames(MNL_3_Predictions)[apply(round(MNL_3_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MNL3Options)
MNL_3_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MNL3Options[ (Fulls$Choice == "A") & (Fulls$MNL3Options == 0)  ] ) + 
                     length(Fulls$MNL3Options[ (Fulls$Choice == "B") & (Fulls$MNL3Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MNL3Options[ (Fulls$Choice == "A") & (Fulls$MNL3Options == 1)  ] ) + 
                               length(Fulls$MNL3Options[ (Fulls$Choice == "B") & (Fulls$MNL3Options == 0)  ])))),3)
MNL_3_Accuracy


## MXL_1 prediction accuracy
MXL_1_Predictions <- MXL_1$probabilities ## Getting probabilities of choosing each option from the model
colnames(MXL_1_Predictions) <- c(0,1) ## Name columns as each option 
MXL1Options <- data.frame("MXL1Options" = as.integer(colnames(MXL_1_Predictions)[apply(round(MXL_1_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MXL1Options)
MXL_1_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MXL1Options[ (Fulls$Choice == "A") & (Fulls$MXL1Options == 0)  ] ) + 
                     length(Fulls$MXL1Options[ (Fulls$Choice == "B") & (Fulls$MXL1Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MXL1Options[ (Fulls$Choice == "A") & (Fulls$MXL1Options == 1)  ] ) + 
                               length(Fulls$MXL1Options[ (Fulls$Choice == "B") & (Fulls$MXL1Options == 0)  ])))),3)
MXL_1_Accuracy


## MXL_2 prediction accuracy
MXL_2_Predictions <- MXL_2$probabilities ## Getting probabilities of choosing each option from the model
colnames(MXL_2_Predictions) <- c(0,1) ## Name columns as each option 
MXL2Options <- data.frame("MXL2Options" = as.integer(colnames(MXL_2_Predictions)[apply(round(MXL_2_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MXL2Options)
MXL_2_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MXL2Options[ (Fulls$Choice == "A") & (Fulls$MXL2Options == 0)  ] ) + 
                     length(Fulls$MXL2Options[ (Fulls$Choice == "B") & (Fulls$MXL2Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MXL2Options[ (Fulls$Choice == "A") & (Fulls$MXL2Options == 1)  ] ) + 
                               length(Fulls$MXL2Options[ (Fulls$Choice == "B") & (Fulls$MXL2Options == 0)  ])))),3)
MXL_2_Accuracy


## MXL_3 prediction accuracy
MXL_3_Predictions <- MXL_3$probabilities ## Getting probabilities of choosing each option from the model
colnames(MXL_3_Predictions) <- c(0,1) ## Name columns as each option 
MXL3Options <- data.frame("MXL3Options" = as.integer(colnames(MXL_3_Predictions)[apply(round(MXL_3_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MXL3Options)
MXL_3_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MXL3Options[ (Fulls$Choice == "A") & (Fulls$MXL3Options == 0)  ] ) + 
                     length(Fulls$MXL3Options[ (Fulls$Choice == "B") & (Fulls$MXL3Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MXL3Options[ (Fulls$Choice == "A") & (Fulls$MXL3Options == 1)  ] ) + 
                               length(Fulls$MXL3Options[ (Fulls$Choice == "B") & (Fulls$MXL3Options == 0)  ])))),3)
MXL_3_Accuracy


## MXL_4 prediction accuracy
MXL_4_Predictions <- MXL_4$probabilities ## Getting probabilities of choosing each option from the model
colnames(MXL_4_Predictions) <- c(0,1) ## Name columns as each option 
MXL4Options <- data.frame("MXL4Options" = as.integer(colnames(MXL_4_Predictions)[apply(round(MXL_4_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
Fulls <- cbind(Fulls,MXL4Options)
MXL_4_Accuracy <- round(data.frame("Right"=c(
  100/nrow(Fulls)*(length(Fulls$MXL4Options[ (Fulls$Choice == "A") & (Fulls$MXL4Options == 0)  ] ) + 
                     length(Fulls$MXL4Options[ (Fulls$Choice == "B") & (Fulls$MXL4Options == 1)  ] ))),
  "Wrong"=c(100/nrow(Fulls)*(length(Fulls$MXL4Options[ (Fulls$Choice == "A") & (Fulls$MXL4Options == 1)  ] ) + 
                               length(Fulls$MXL4Options[ (Fulls$Choice == "B") & (Fulls$MXL4Options == 0)  ])))),3)
MXL_4_Accuracy

## MXL_5 prediction accuracy
MXL_5_Predictions <- MXL_5$probabilities ## Getting probabilities of choosing each option from the model
colnames(MXL_5_Predictions) <- c(0,1) ## Name columns as each option 
MXL5Options <- data.frame("MXL5Options" = as.integer(colnames(MXL_5_Predictions)[apply(round(MXL_5_Predictions,4),1,which.max)])) ## This picks the class that is most likely for each individual
MXL5Full <- MXL5Full[ (MXL5Full$ID) %in% c(Full_Cons$ID) , ]
MXL5Full <- cbind(MXL5Full,MXL5Options)
MXL_5_Accuracy <- round(data.frame("Right"=c(
  100/nrow(MXL5Full)*(length(MXL5Full$MXL5Options[ (MXL5Full$Choice == "A") & (MXL5Full$MXL5Options == 0)  ] ) + 
                        length(MXL5Full$MXL5Options[ (MXL5Full$Choice == "B") & (MXL5Full$MXL5Options == 1)  ] ))),
  "Wrong"=c(100/nrow(MXL5Full)*(length(MXL5Full$MXL5Options[ (MXL5Full$Choice == "A") & (MXL5Full$MXL5Options == 1)  ] ) + 
                                  length(MXL5Full$MXL5Options[ (MXL5Full$Choice == "B") & (MXL5Full$MXL5Options == 0)  ])))),3)
MXL_5_Accuracy

## Data frame of all model's prediction accuracy: 
Models_Predictions <- rbind("Model 1: MNL - Attributes only" = c(MNL_1_Accuracy),
                            "Model 2: MNL - Quadratic attributes:"=c(MNL_2_Accuracy),
                            "Model 3: MNL - All sociodemographics:"=c(MNL_3_Accuracy),
                            "Model 4: MXL - Attributes Only"=c(MXL_1_Accuracy),
                            "Model 5: MXL - Quadratic attributes"=c(MXL_2_Accuracy),
                            "Model 6: MXL - SDs, utility-space"=c(MXL_3_Accuracy),
                            "Model 7: MXL - SDs, WTP-space"=c(MXL_4_Accuracy),
                            "Model 8: MXL - SDs, WTP-space, truncated sample"=c(MXL_5_Accuracy))


############# Model WTP:



############# Model-specific WTP:

AllWTPs <- round(t(data.frame("Model 1: MNL - Attributes only" = c(MNL_1_WTP),
                              "Model 2: MNL - Quadratic attributes:"=c(MNL_2_WTP),
                              "Model 3: MNL - All sociodemographics:"=c(MNL_3_WTP),
                              "Model 4: MXL - Attributes Only"=c(MXL_1_WTP),
                              "Model 5: MXL - Quadratic attributes"=c(MXL_2_WTP),
                              "Model 6: MXL - SDs, utility-space"=c(MXL_3_WTP),
                              "Model 7: MXL - SDs, WTP-space"=c(MXL_4_WTP),
                              "Model 8: MXL - SDs, WTP-space, truncated sample"=c(MXL_5_WTP))),6)
AllWTPs <- data.frame(AllWTPs)
AllWTP <- rownames(AllWTPs)
colnames(AllWTPs) <- c("Emission","Performance")
AllWTPs <- data.frame(cbind(AllWTPs$Emission,AllWTPs$Emission*100,AllWTPs$Performance,AllWTPs$Performance*100))
colnames(AllWTPs) <- c("Emission MWTP","Emission Total","Performance MWTP", "Performance Total")
rownames(AllWTPs) <- AllWTP
## The aim above is to create a data.frame which stores all the MWTP and total WTP by model specification


############# Sample-specific WTP:

FullWTPs <- data.frame("Full sample" = 
                     c(-1*coef(MXL_4)["Emission"]/coef(MXL_4)["Price"],
                       -1*coef(MXL_4)["Performance"]/coef(MXL_4)["Price"])
                   ,"x100%" = 
                     c(-1*(coef(MXL_4)["Emission"]/coef(MXL_4)["Price"] * 100),
                       -1*(coef(MXL_4)["Performance"]/coef(MXL_4)["Price"] *100)),
                   "Truncated" = 
                     c(-1*coef(MXL_5)["Emission"]/coef(MXL_5)["Price"],
                       -1*coef(MXL_5)["Performance"]/coef(MXL_5)["Price"]),
                   " x100%" = 
                     c(-1*(coef(MXL_5)["Emission"]/coef(MXL_5)["Price"] * 100),
                       -1*(coef(MXL_5)["Performance"]/coef(MXL_5)["Price"] *100)))
FullWTPs


############# Plotted WTP:

## Plot conditional distribution of MWTP. 
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(rpar(MXL_4,"Price"), main="MXL: Full sample")
plot(rpar(MXL_5,"Price"), main="MXL: Truncation")
AIC(MXLFullTruncated)


############# Bootstrapped WTP:

## Bootstrapped clustered individual standard errors: 
library(clusterSEs)
CBSM <- cluster.bs.mlogit(MXL_4, Full_Long, ~ ID, boot.reps=100,seed = 123)

WTPbs <- data.frame("Emission" = c(CBSM$ci[4,1]/CBSM$ci[2,1],CBSM$ci[4,2]/CBSM$ci[2,2]),
                    "Performance"=c(CBSM$ci[3,1]/CBSM$ci[2,1],CBSM$ci[3,2]/CBSM$ci[2,2]))
WTPbs <- t(WTPbs)
WTPbs <- -1* WTPbs
WTPbs <- cbind(WTPbs[,1],FullWTPs[,2],FullWTPs[,2])
colnames(WTPbs) <- c("Lower","Mean","Upper")
round(WTPbs,3)


############# Fitting respondent-specific MWTP:


Fulls <- cbind(Fulls,
               "PriceParam"=fitted(MXL_4,type = "parameters"),
               "EmissionWTP"=coef(MXL_4)["Emission"],
               "PerformanceWTP"=coef(MXL_4)["Performance"])

Fulls$EmissionWTP <- -1*Fulls$EmissionWTP/Fulls$Price
Fulls$PerformanceWTP <- -1*Fulls$PerformanceWTP/Fulls$Price


Full_Long <- cbind(Full_Long,
                   "PriceCoef"=slice(.data = data.frame(fitted(MXL_4,type = "parameters")),rep(1:n(), each = 2)),
                   "EmissionCoef"=coef(MXL_4)["Emission"],
                   "PerformanceCoef"=coef(MXL_4)["Performance"])
names(Full_Long)[45] <- "PriceCoef"
Full_Long$EmissionCoef <- -1*Full_Long$EmissionCoef/Full_Long$PriceCoef
Full_Long$PerformanceCoef <- -1*Full_Long$PerformanceCoef/Full_Long$PriceCoef


ChoiceData <- data.frame("cs" = slice(.data = Choices,rep(1:n(), each = 2)))
colnames(ChoiceData) <- c("cs")
Full_Final <- cbind(Full_Final, "cs"=ChoiceData)


#### Section 3C: Sensitivity Analysis ####


############## Testing random distribution: 


## MXL1 with Lognormal distribution for the price parameter
MXL_1_Lognormal <- mlogit(
  Choice ~ Price + Performance + Emission ,
  Full_Long, rpar=c(Price="ln"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=13)
summary(MXL_1_Lognormal)
MXL_1_Lognormal_WTP <- c(-1*coef(MXL_1_Lognormal)["Emission"]/coef(MXL_1_Lognormal)["Price"],-1*coef(MXL_1_Lognormal)["Performance"]/coef(MXL_1_Lognormal)["Price"])


## MXL1 with uniform distribution for the price parameter
MXL_1_Uniform <- mlogit(
  Choice ~ Price + Performance + Emission ,
  Full_Long, rpar=c(Price="u"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=13)
summary(MXL_1_Uniform)
MXL_1_Uniform_WTP <- c(-1*coef(MXL_1_Uniform)["Emission"]/coef(MXL_1_Uniform)["Price"],-1*coef(MXL_1_Uniform)["Performance"]/coef(MXL_1_Uniform)["Price"])

## MXL1 with triangular distribution for the price parameter
MXL_1_Tri <- mlogit(
  Choice ~ Price + Performance + Emission ,
  Full_Long, rpar=c(Price="t"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=13)
summary(MXL_1_Tri)
MXL_1_Tri_WTP <- c(-1*coef(MXL_1_Tri)["Emission"]/coef(MXL_1_Tri)["Price"],-1*coef(MXL_1_Tri)["Performance"]/coef(MXL_1_Tri)["Price"])


round(cbind("Normal"=MXL_1_WTP,"Uniform"=MXL_1_Uniform_WTP,"Triangular"=MXL_1_Tri_WTP),5)
plot(rpar(MXL_1,"Price"))
plot(rpar(MXL_1_Tri,"Price"))
plot(rpar(MXL_1_Uniform,"Price"))

############## Testing number of draws: 

## Repeating MXL_4 but experimenting with number of random draws, here 100
MXL_5_Draws1 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n"),
  R=100,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5_Draws1)
MXL_5_Draws1_WTP <- c(-1*coef(MXL_5_Draws1)["Emission"]/coef(MXL_5_Draws1)["Price"],-1*coef(MXL_5_Draws1)["Performance"]/coef(MXL_5_Draws1)["Price"])


## Repeating MXL_4 but experimenting with number of random draws, here 500
MXL_5_Draws2 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n"),
  R=500,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5_Draws2)
MXL_5_Draws2_WTP <- c(-1*coef(MXL_5_Draws2)["Emission"]/coef(MXL_5_Draws2)["Price"],-1*coef(MXL_5_Draws2)["Performance"]/coef(MXL_5_Draws2)["Price"])


## Repeating MXL_4 but experimenting with number of random draws, here 5000
MXL_5_Draws3 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n"),
  R=5000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5_Draws3)
MXL_5_Draws3_WTP <- c(-1*coef(MXL_5_Draws3)["Emission"]/coef(MXL_5_Draws3)["Price"],-1*coef(MXL_5_Draws3)["Performance"]/coef(MXL_5_Draws3)["Price"])


## Repeating MXL_4 but experimenting with number of random draws, here 10,000
MXL_5_Draws4 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n"),
  R=10000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5_Draws4)
MXL_5_Draws4_WTP <- c(-1*coef(MXL_5_Draws4)["Emission"]/coef(MXL_5_Draws4)["Price"],-1*coef(MXL_5_Draws4)["Performance"]/coef(MXL_5_Draws4)["Price"])


## Repeating MXL_4 but experimenting with number of random draws, here 10,000
MXL_5_Draws5 <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome + Timing,
  Full_Long, rpar=c(Price="n"),
  R=100000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=FALSE,seed=123)
summary(MXL_5_Draws5)
MXL_5_Draws5_WTP <- c(-1*coef(MXL_5_Draws5)["Emission"]/coef(MXL_5_Draws5)["Price"],-1*coef(MXL_5_Draws5)["Performance"]/coef(MXL_5_Draws5)["Price"])


## Reporting MWTP with draws:  
round(cbind("Draws: 100"=MXL_5_Draws1_WTP,"Draws: 500"=MXL_5_Draws2_WTP,"Draws: 1,000"=MXL_4_WTP,"Draws: 5,000"=MXL_5_Draws3_WTP,"Draws: 10,000"=MXL_5_Draws4_WTP,"Draws: 100,000"=MXL_5_Draws5_WTP),5)


#### Section 3D: Choice Experiment with GMNL, an alternative to MLOGIT ####
############ NOTES: LCM currently not working with latest GMNL version.


library(gmnl)


## Replicating the MNL
MNL_GM <- gmnl(  Choice ~ Price + Performance + Emission | 
                   Order + Task + Q1Gender + Q2Age + Q3Distance
                 + Q4Trips + Q16BP + Q18Charity 
                 + Q20Consequentiality
                 + Q21Experts +Q22Education+ Q23Employment
                 +  Q24AIncome + Timing,
                 data = Full_Long,
                 model = "mnl",alt.subset = c("A","B"),reflevel = "A")
summary(MNL_GM)


## GMNL has an inbuilt WTP function:
wtp.gmnl(MNL_GM,"Price",3)


## Replicating the MXL_4
GMNL_MXL3 <- gmnl(Choice ~ Price + Performance + Emission | 1 | 0|
                    Price, data = Full_Long,
                  model = "mixl",
                  ranp = c( Price = "n"),
                  mvar = list(Price = c("Price")),
                  R = 1000,
                  haltons = NA
                  ,seed = 13,reflevel = "A",correlation=FALSE,panel=TRUE)
summary(GMNL_MXL3)
wtp.gmnl(GMNL_MXL3,"Price",3)
coef(GMNL_MXL3)["Performance"]/coef(GMNL_MXL3)["Price"]
coef(GMNL_MXL3)["Emission"]/coef(GMNL_MXL3)["Price"]

## Replicating the MXL_4
GMNL_MXL4 <- gmnl(Choice ~ Price + Performance + Emission | 1 | 0|
                          Order + Task + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity 
                        + Q20Consequentiality
                        + Q21Experts +Q22Education+ Q23Employment
                        +  Q24AIncome + Timing, data = Full_Long,
                        model = "mixl",
                        ranp = c( Price = "n"),
                        mvar = list(Price = c("Q18Charity")),
                        R = 1000,
                        haltons = NA
                        ,seed = 13,reflevel = "A",correlation=FALSE,panel=TRUE)
summary(GMNL_MXL4)
wtp.gmnl(GMNL_MXL4,"Price",3)
coef(GMNL_MXL4)["Performance"]/coef(GMNL_MXL4)["Price"]
coef(GMNL_MXL4)["Emission"]/coef(GMNL_MXL4)["Price"]


## GMNL has a plot function for the conditional distribution of the random parameters:
plot(GMNL_MXLDefault, par = "Price",type = "density", col = "grey",wrt="Price")



#### Section 3Da: Latent-Class Models: ####


install.packages("versions")
library(versions)
install.versions(c('gmnl'), c('1.1-3.1')) ## May need to revert version for the LCM to work


## Two class model:
LC_GM <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
              + Q4Trips + Q16BP + Q18Charity
              + Q21Experts +Q22Education+ Q23Employment
              +  Q24AIncome,
              data = Full_Long,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) ## 1749.514
BIC(LC_GM) ## 1839.79


## Three class model:
LC_GM3 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                 0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
               + Q4Trips + Q16BP + Q18Charity
               + Q21Experts +Q22Education+ Q23Employment
               +  Q24AIncome,
               data = Full_Long,
               model = 'lc',
               panel = TRUE,
               Q = 3)
summary(LC_GM3)
AIC(LC_GM3) # 1691.97
BIC(LC_GM3) # 1856.605


## Four-class model: (NOTE: Does not compute) 
LC_GM4 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                 0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
               + Q4Trips + Q16BP + Q18Charity
               + Q21Experts +Q22Education+ Q23Employment
               +  Q24AIncome,
               data = Full_Long,
               model = 'lc',
               panel = TRUE,
               Q = 4)
summary(LC_GM4)
AIC(LC_GM4) # 1680.126
BIC(LC_GM4) # 1919.10


## The following two Functions are from https://rpubs.com/msarrias1986/335556 which calculates the share of the sample in each class
exp(coef(LC_GM3)["(class)2"]) / (exp(0) + exp(coef(LC_GM3)["(class)2"]))
shares <- function(obj){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  Q <- length(cons_class)
  shares <- exp(cons_class) / sum(exp(cons_class))
  names(shares) <- paste("share q", 1:Q, sep = "=")  
  return(shares)
}


## Plot confidence-intervals for the LCM: 
plot_ci_lc <- function(obj, var = NULL, mar = c(2, 5, 2, 2),
                       cex.pts = 0.9, cex.var = 0.8, 
                       var.las = 2, pch.pts = 20, col.pts = 1, ...){
  if (!inherits(obj, "gmnl")) stop("The model was not estimated using gmnl")
  if (obj$model != "lc") stop("The model is not a LC-MNL")
  bhat <- coef(obj)
  se   <- sqrt(diag(vcov(obj)))
  cons_class <- c(0, bhat[grep("(class)", names(bhat), fixed = TRUE)])
  name.x <- if (is.null(var)) names(obj$mf)[-1] else var
  Q <- length(cons_class)
  lc.names <- c()
  for (i in 1:length(name.x)) {
    lc.names <- c(lc.names, paste("class", 1:Q, name.x[i], 
                                  sep = "."))
  }
  bhat <- bhat[lc.names]
  se   <- se[lc.names]
  
  u <-  bhat + 1.96 * se
  l <-  bhat - 1.96 * se
  n.c <- length(bhat)
  idx <- seq(1, n.c)
  k <- 1 / n.c
  
  par(mar = mar)
  plot(c(l, u), c(idx + k, idx - k), 
       type = "n", axes = F, main = "" , xlab = "", 
       ylab = "", ...)
  axis(3)
  axis(2, n.c:1, names(bhat)[n.c:1], las = var.las, 
       tck = FALSE, lty = 0, cex.axis = cex.var)
  abline(v = 0, lty = 2)
  points(bhat, idx, pch = pch.pts, cex = cex.pts, 
         col = col.pts)
  segments(l, idx, u, idx, lwd = 2, 
           col = "red")
}


## Reports class-shares:
shares(LC_GM)
shares(LC_GM3)


## Assigning classes to individuals:
ClassProbs <- LC_GM3$Qir ## Thankfully GMNL has an inbuilt method of calculating individual likelihood of class-memberships
colnames(ClassProbs) <- c(1,2,3) ## Name columns as one of the classes. Here I'm using the 2-class model but this can easily be augmented if the 2+ models fit better. 
Classes <- data.frame("Classes" = as.integer(colnames(ClassProbs)[apply(round(ClassProbs,4),1,which.max)])) ## This picks the class that is most likely for each individual
Full_Long <- cbind(Full_Long,slice(.data = Classes,rep(1:n(), each = 8)))
Full_Long$Classes <- as.double(Full_Long$Classes)


## Plotting the confidence intervals of coefficients:
plot_ci_lc(LC_GM,var = c("Price"))


## LCM class-specific WTP:
-1* (coef(LC_GM)["class.1.Performance"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.1.Emission"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.2.Performance"]/coef(LC_GM)["class.2.Price"])
-1* (coef(LC_GM)["class.2.Emission"]/coef(LC_GM)["class.2.Price"])


## Sample average WTP 
wtp_bar <- data.frame("Emissions" = (-coef(LC_GM)["class.1.Emission"] / coef(LC_GM)["class.1.Price"]) * shares(LC_GM)[1] + 
  (-coef(LC_GM)["class.2.Emission"] / coef(LC_GM)["class.2.Price"]) * shares(LC_GM)[2],
"Performance" = (-coef(LC_GM)["class.1.Performance"] / coef(LC_GM)["class.1.Price"]) * shares(LC_GM)[1] + 
  (-coef(LC_GM)["class.2.Performance"] / coef(LC_GM)["class.2.Price"]) * shares(LC_GM)[2])
wtp_bar
wtp_bar*100


#### Section 4: Contingent Valuation ####
############ NOTES: Split into estimation (1367:1705) and Marginal Effects (1705:1745) sections


#### Section 4A: Setup packages, data manipulation and graphing ####
## Have to do some R magic here to install a package not on CRAN
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")
install.packages("interval")
install.packages("DCchoice")
library(DCchoice)


## Creating new dataframes depending on ordering or consequentiality. 
Full_NormalOrder <-Full_Long[Full_Long$Order == 0,]
Full_OtherOrder <-Full_Long[Full_Long$Order == 1,]
Full_Consequential <-Full_Long[Full_Long$Q20Consequentiality == 1,]
Full_Inconsequential <-Full_Long[Full_Long$Q20Consequentiality != 1,]


## I also split the other dataframe for ease of fitting WTP 
FullSurvey2 <- data.frame(FullSurvey2)
Full_Order1 <- FullSurvey2[ (FullSurvey2$Order ==0) ,]
Full_Order2 <- FullSurvey2[ (FullSurvey2$Order ==1) ,]


## Here I construct dataframes which calculate acceptance rates for each CVM question by ordering 
Q6 <- t(data.frame("Normal" = c(length(FullSurvey2$Q6ResearchResponse[(FullSurvey2$Order ==0) & (FullSurvey2$Q6ResearchResponse ==0)]),length(FullSurvey2$Q6ResearchResponse[(FullSurvey2$Order ==0) & (FullSurvey2$Q6ResearchResponse ==1)])),
                   "Alternate" = c(length(FullSurvey2$Q6ResearchResponse[(FullSurvey2$Order ==1) & (FullSurvey2$Q6ResearchResponse ==0)]),length(FullSurvey2$Q6ResearchResponse[(FullSurvey2$Order ==1) & (FullSurvey2$Q6ResearchResponse ==1)]))))
Q6 <- data.frame(cbind(Q6,Q6[,2]/sum(Q6[2,]),c(1,2)))
colnames(Q6) <- c("Reject","Accept","Percentage","Order")

Q7 <- t(data.frame("Normal" = c(length(FullSurvey2$Q7TreatmentResponse[(FullSurvey2$Order ==0) & (FullSurvey2$Q7TreatmentResponse ==0)]),length(FullSurvey2$Q7TreatmentResponse[(FullSurvey2$Order ==0) & (FullSurvey2$Q7TreatmentResponse ==1)])),
                   "Alternate" = c(length(FullSurvey2$Q7TreatmentResponse[(FullSurvey2$Order ==1) & (FullSurvey2$Q7TreatmentResponse ==0)]),length(FullSurvey2$Q7TreatmentResponse[(FullSurvey2$Order ==1) & (FullSurvey2$Q7TreatmentResponse ==1)]))))
Q7 <- data.frame(cbind(Q7,Q7[,2]/sum(Q7[2,]),c(1,2)))
colnames(Q7) <- c("Reject","Accept","Percentage","Order")

Q7b <- t(data.frame("Normal" = c(length(FullSurvey2$Q7Response2[(FullSurvey2$Order ==0) & (FullSurvey2$Q7Response2 ==0)]),length(FullSurvey2$Q7Response2[(FullSurvey2$Order ==0) & (FullSurvey2$Q7Response2 ==1)])),
                    "Alternate" = c(length(FullSurvey2$Q7Response2[(FullSurvey2$Order ==1) & (FullSurvey2$Q7Response2 ==0)]),length(FullSurvey2$Q7Response2[(FullSurvey2$Order ==1) & (FullSurvey2$Q7Response2 ==1)]))))
Q7b <- data.frame(cbind(Q7b,Q7b[,2]/sum(Q7b[2,]),c(1,2)))
colnames(Q7b) <- c("Reject","Accept","Percentage","Order")

## Combines all CVM qestion acceptance rates
CVM <- cbind(rbind(Q6,Q7,Q7b),c("Q6","Q6","Q7","Q7","Q7b","Q7b"))
colnames(CVM) <- c("Reject","Accept","Percentage","Order","Question")
CVM <- data.frame(CVM)


## Plotting order effects on bid acceptance by question. 
ggplot(aes(x=Order,y=Percentage),data=CVM)+ 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=F) +
  facet_wrap(~Question,ncol = 1) + 
  scale_x_continuous(name="Question",breaks = 1:2, 
                     labels=c("Q6 then Q7","Q7 then  Q6"))+
  scale_y_continuous(name="Percent choosing Option A",
                     breaks=waiver())+
  ggtitle("Percentage accepting or rejecting the bid level.")

## Here I am trying to construct a dataframe to show that ordering may effect whether respondents accept or reject the third valuation exercise. 
Ordering <- data.frame(rbind("Normal order"=data.frame("Accepting higher bid"=c(length(unique(Full_Long$ID[ (Full_Long$Q7Response2 == 0) & (Full_Long$Q7Bid > Full_Long$Q7Bid2) & (Full_Long$Order == 0) ]))),
                                                       "Rejecting higher bid" = c(length(unique(Full_Long$ID[ (Full_Long$Q7Response2 == 1) & (Full_Long$Q7Bid > Full_Long$Q7Bid2) & (Full_Long$Order == 0) ])))),
                             "Reversed"=data.frame("Accepting higher bid"=c(length(unique(Full_Long$ID[ (Full_Long$Q7Response2 == 0) & (Full_Long$Q7Bid > Full_Long$Q7Bid2) & (Full_Long$Order == 1) ]))),
                                                   "Rejecting higher bid" = c(length(unique(Full_Long$ID[ (Full_Long$Q7Response2 == 1) & (Full_Long$Q7Bid > Full_Long$Q7Bid2) & (Full_Long$Order == 1) ]))))))


#### Section 4B: CV WTP Distributions with KMT survival function ####


## This section deals with Q6 and Q7 respectively but uses a non-parametric Kaplan-Meier-Turnbull survival function:
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = Full_Order1)
summary(ResearchKMT)
plot(ResearchKMT)


## Reporting the KMT for Q7.
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = FullSurvey2)
summary(TreatmentKMT)
plot(TreatmentKMT)

TreatmentKMTSB <- turnbull.sb(formula = Q7TreatmentResponse  ~  Q7Bid ,data = FullSurvey2)
summary(TreatmentKMT)
plot(TreatmentKMTSB,main="Q7 Single-Bound KMT survival function")


## Plot both KMT functions together in one plot. 
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(ResearchKMT, main="Q6 Kaplan-Meier-Turnbull survival function.")
plot(TreatmentKMT, main="Q7 Kaplan-Meier-Turnbull survival function.")


#### Section 4C: CV Estimation: ####
############ NOTES: Q6 then Q7 and exploring ordering and consequentiality


## Q6 WTP:
# Full_Long$Q6ResearchResponse[Full_Long$Q6ResearchCertainty==0] <- 0
## Use the above if testing for certainty-recoding. 

## The Q6 model with constant and bid only:
Research_BidOnly <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Full_Long,dist="logistic")
summary(Research_BidOnly) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.

Full_Long$Q24AIncome <- Full_Long$Q24AIncome*12
## The Q6 model with all covariates:
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips +Q6ResearchCertainty + Q16BP + Q18Charity + Q20Consequentiality
                        + Q21Experts + Q22Education + Q23Employment
                        +  Q24AIncome + Q25Understanding + Timing | Q6Bid, data = Full_Long,dist="logistic")
summary(Research_SB) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_SB)
bootCI(Research_SB)


## Q6 on truncated sample:
Research_Truncated <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity
                        + Q21Experts + Q22Education + Q23Employment
                        +  Q24AIncome + Timing | Q6Bid, data = Full_Full,dist="logistic")
summary(Research_Truncated)
krCI(Research_Truncated)


####### Testing ordering effects: 
Research_Order1 <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity 
                            + Q20Consequentiality
                            + Q21Experts +Q22Education+ Q23Employment
                            +  Q24AIncome + Timing| Q6Bid, data = Full_NormalOrder,dist="logistic",seed=123)
summary(Research_Order1) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Research_Order2 <- sbchoice(Q6ResearchResponse ~  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity 
                            + Q20Consequentiality
                            + Q21Experts +Q22Education+ Q23Employment
                            +  Q24AIncome + Timing | Q6Bid, data = Full_OtherOrder,dist="logistic",seed=123)
summary(Research_Order2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_Order1)
krCI(Research_Order2)


## Testing the effect of consequentiality beliefs
Research_Consequential <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                                   + Q4Trips + Q16BP + Q18Charity 
                                   + Q21Experts +Q22Education+ Q23Employment
                                   +  Q24AIncome | Q6Bid, data = Full_Consequential,dist="logistic")
summary(Research_Consequential) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Research_Inconsequential <- sbchoice(Q6ResearchResponse ~  Q1Gender + Q2Age + Q3Distance
                                     + Q4Trips + Q16BP + Q18Charity
                                     + Q21Experts +Q22Education+ Q23Employment
                                     +  Q24AIncome | Q6Bid, data = Full_Inconsequential,dist="logistic")
summary(Research_Inconsequential) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_Consequential)
krCI(Research_Inconsequential)


## Q7 WTP:
# Full_Long$Q7TreatmentResponse[Full_Long$Q7TreatmentCertainty != 2] <- 0

Treatment_BidOnly <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Full_Long,dist="logistic")
summary(Treatment_BidOnly) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Treatment_BidOnly2 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1 |Q7Bid + Q7Bid2, data = Full_Long,dist="logistic")
summary(Treatment_BidOnly2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


## Repeating the same as above but for Q7 the DBDC question:
Treatment_DB <- dbchoice(Q7TreatmentResponse + Q7Response2 ~  Order + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q7TreatmentCertainty +Q16BP + Q18Charity + Q20Consequentiality
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome + Q25Understanding + Timing  | Q7Bid + Q7Bid2,data = Full_Long,dist="logistic")
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)


## Q7 on truncated sample: 
Treatment_Truncated <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome + Timing | Q7Bid + Q7Bid2,data = Full_Full,dist="logistic")
summary(Treatment_Truncated)
krCI(Treatment_Truncated)
bootCI(Treatment_Truncated)


## Analysing only the firt bound if anchoring is an issue
Treatment1_SB <- sbchoice(Q7TreatmentResponse ~  Order + Q1Gender + Q2Age + Q3Distance
                          + Q4Trips + Q7TreatmentCertainty + Q16BP + Q18Charity + Q20Consequentiality
                          + Q21Experts + Q22Education + Q23Employment
                          +  Q24AIncome + Q25Understanding + Timing  | Q7Bid ,data = Full_Long,dist="logistic")
summary(Treatment1_SB)
bootCI(Treatment1_SB)


## Estimating Q7 by both orders using DBDC:
Treatment_DBOrder1 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome + Timing | Q7Bid + Q7Bid2,data = Full_NormalOrder,dist="logistic")
summary(Treatment_DBOrder1)
Treatment_DBOrder2 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome + Timing  | Q7Bid + Q7Bid2,data = Full_OtherOrder,dist="logistic")
summary(Treatment_DBOrder2)
krCI(Treatment_DBOrder1)
krCI(Treatment_DBOrder2)


###### Still testing ordering effects using SBDC:
Treatment_SBOrder1 <- sbchoice(Q7TreatmentResponse  ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid,data = Full_NormalOrder,dist="logistic")
summary(Treatment_SBOrder1)
Treatment_SBOrder2 <- sbchoice(Q7TreatmentResponse  ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid,data = Full_OtherOrder,dist="logistic")
summary(Treatment_SBOrder2)
krCI(Treatment_SBOrder1)
krCI(Treatment_SBOrder2)


## Splitting CVM by consequentiality beliefs:
Treatment_Consequential <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                                    + Q4Trips + Q16BP + Q18Charity 
                                    + Q21Experts +Q22Education+ Q23Employment
                                    +  Q24AIncome | Q7Bid + Q7Bid2,data = Full_Consequential,dist="logistic")
summary(Treatment_Consequential)
Treatment_Inconsequential <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                                      + Q4Trips + Q16BP + Q18Charity
                                      + Q21Experts +Q22Education+ Q23Employment
                                      +  Q24AIncome | Q7Bid + Q7Bid2,data = Full_Inconsequential,dist="logistic")
summary(Treatment_Inconsequential)
krCI(Treatment_Consequential)
krCI(Treatment_Inconsequential)


Treatment_ConsequentialKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~ Q7Bid + Q7Bid2,data = Full_Consequential)
Treatment_InconsequentialKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~ Q7Bid + Q7Bid2,data = Full_Inconsequential)
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(Treatment_ConsequentialKMT)
plot(Treatment_InconsequentialKMT)


#### Section 4D: Fitting CV WTP ####


## In this section I directly compare the Full-bound Full-round Q6 and Q7 WTP valuations
Research <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                     + Q4Trips + Q16BP + Q18Charity
                     + Q21Experts + Q22Education + Q23Employment
                     +  Q24AIncome + Timing| Q6Bid, data = Full_Order1,dist="logistic")
Treatment <- sbchoice(Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance
                      + Q4Trips + Q16BP + Q18Charity
                      + Q21Experts + Q22Education + Q23Employment
                      +  Q24AIncome + Timing| Q7Bid, data = Full_Order2,dist="logistic")
summary(Research)
summary(Treatment)
Full_Order1 <- cbind(Full_Order1,
                      apply(Full_Order1, 
                            1, 
                            function(i) c(krCI(Research,individual = data.frame(Q1Gender = Full_Order1$Q1Gender[i], Q2Age = Full_Order1$Q2Age[i], Q3Distance = Full_Order1$Q3Distance[i],Q4Trips = Full_Order1$Q4Trips[i], Q16BP = Full_Order1$Q16BP[i],Q18Charity = Full_Order1$Q18Charity[i],Q21Experts = Full_Order1$Q21Experts[i],Q22Education = Full_Order1$Q22Education[i], Q23Employment = Full_Order1$Q23Employment[i], Q24AIncome = Full_Order1$Q24AIncome[i],Timing=Full_Order1$Timing[i]))$out[4,1])))
colnames(Full_Order1)[56] <- "Q6WTP"
Full_Order2 <- cbind(Full_Order2,
                      apply(Full_Order2, 
                            1, 
                            function(i) c(krCI(Treatment,individual = data.frame(Q1Gender = Full_Order2$Q1Gender[i], Q2Age = Full_Order2$Q2Age[i], Q3Distance = Full_Order2$Q3Distance[i],Q4Trips = Full_Order2$Q4Trips[i], Q16BP = Full_Order2$Q16BP[i],Q18Charity = Full_Order2$Q18Charity[i],Q21Experts = Full_Order2$Q21Experts[i],Q22Education = Full_Order2$Q22Education[i], Q23Employment = Full_Order2$Q23Employment[i], Q24AIncome = Full_Order2$Q24AIncome[i],Timing=Full_Order1$Timing[i]))$out[4,1])))
colnames(Full_Order2)[56] <- "Q7WTP"


######### Fitting respondent precautionary-premia:
### This differs from above which elicits sample QOV by taking best-case sample WTP


## In this experimental code I fit WTP to an average respondent and then examine the difference in median WTP by ordering effects only. 
Research_WTP <- sbchoice(Q6ResearchResponse ~ Order +  Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome + Timing | Q6Bid, data = FullSurvey2,dist="logistic")
O1 <- krCI(Research_WTP,individual = data.frame(Order=0, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome),Timing = mean(FullSurvey2$Timing)))
O2 <- krCI(Research_WTP,individual = data.frame(Order=1, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome),Timing = mean(FullSurvey2$Timing)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))
Treatment_DBWTP <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order +  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity
                            + Q21Experts + Q22Education + Q23Employment
                            +  Q24AIncome + Timing | Q7Bid + Q7Bid2,data = FullSurvey2,dist="logistic")
O1 <- krCI(Treatment_DBWTP,individual = data.frame(Order=0, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome),Timing = mean(FullSurvey2$Timing)))
O2 <- krCI(Treatment_DBWTP,individual = data.frame(Order=1, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome),Timing = mean(FullSurvey2$Timing)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))
i=1


## With this function I append bootstrapped individual WTP to the original dataframe 
FullSurvey2 <- cbind(FullSurvey2,
                      apply(FullSurvey2, 
                            1, 
                            function(i) c(krCI(Research_WTP,individual = data.frame(Order= FullSurvey2$Order[abs(i)], Q1Gender = FullSurvey2$Q1Gender[abs(i)], Q2Age = FullSurvey2$Q2Age[abs(i)], Q3Distance = FullSurvey2$Q3Distance[abs(i)],Q4Trips = FullSurvey2$Q4Trips[abs(i)], Q16BP = FullSurvey2$Q16BP[abs(i)],Q18Charity = FullSurvey2$Q18Charity[abs(i)],Q21Experts = FullSurvey2$Q21Experts[abs(i)],Q22Education = FullSurvey2$Q22Education[abs(i)], Q23Employment = FullSurvey2$Q23Employment[abs(i)], Q24AIncome = FullSurvey2$Q24AIncome[abs(i)],Timing = FullSurvey2$Timing[abs(i)]))$out[4,1])))
colnames(FullSurvey2)[56] <- "Q6WTP"
i=0
FullSurvey2 <- cbind(FullSurvey2,
                      apply(FullSurvey2, 
                            1, 
                            function(i) c(krCI(Treatment_DBWTP,individual = data.frame(Order= FullSurvey2$Order[abs(i)], Q1Gender = FullSurvey2$Q1Gender[abs(i)], Q2Age = FullSurvey2$Q2Age[abs(i)], Q3Distance = FullSurvey2$Q3Distance[abs(i)],Q4Trips = FullSurvey2$Q4Trips[abs(i)], Q16BP = FullSurvey2$Q16BP[abs(i)],Q18Charity = FullSurvey2$Q18Charity[abs(i)],Q21Experts = FullSurvey2$Q21Experts[abs(i)],Q22Education = FullSurvey2$Q22Education[abs(i)], Q23Employment = FullSurvey2$Q23Employment[abs(i)], Q24AIncome = FullSurvey2$Q24AIncome[abs(i)],Timing=FullSurvey2$Timing[abs(i)]))$out[4,1])))
colnames(FullSurvey2)[57] <- "Q7WTP"

FullSurvey2 <- cbind(FullSurvey2,(FullSurvey2$Q7WTP - FullSurvey2$Q6WTP ))
colnames(FullSurvey2)[58] <- "Precaution"


Full_Final <- cbind(Full_Long,slice(.data = FullSurvey2[,56:58],rep(1:n(), each = 8)))
Full_Final <- (cbind(Full_Final,slice(.data = Responsibility,rep(1:n(), each = 2))))
write.csv(Full_Final,file = "FinalData.csv")
# FullSurvey2 <- FullSurvey2[ (FullSurvey2$Q1Gender == 0) | (FullSurvey2$Q1Gender == 1),]


summary(FullSurvey2$Precaution) ## Individual QOV within the sample:


R1O1 <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Full_Order1,dist="logistic")
summary(R1O1) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
R1O1WTP <- krCI(R1O1)

R1O2 <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Full_Order2,dist="logistic")
summary(R1O2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
R1O2WTP <- krCI(R1O2)

R1O3 <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = FullSurvey2,dist="logistic")
summary(R1O2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
R1O3WTP <- krCI(R1O3)


T1O1 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Full_Order1,dist="logistic")
summary(T1O1) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
T1O1WTP <- krCI(T1O1)

T1O2 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Full_Order2,dist="logistic")
summary(T1O2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
T1O2WTP <- krCI(T1O2)

T1O3 <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = FullSurvey2,dist="logistic")
summary(T1O2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
T1O3WTP <- krCI(T1O3)


## Calculating 95% confidence intervals
a <- mean(Full_Final$Precaution)
s <- sd(Full_Final$Precaution)
n <- 670
error <- qnorm(0.975)*s/sqrt(n)
left <- a-error 
right <- a+error


## Making all possible WTP calculations into a dataframe
PrecautionaryPremium <- round(rbind("Q6Order1"=(R1O1WTP$out[4,]),
      "Q6Order2"=(R1O2WTP$out[4,]),
      "Q6FullSample"=(R1O3WTP$out[4,]),
      "Q7Order1"=(T1O1WTP$out[4,]),
      "Q7Order2"=(T1O2WTP$out[4,]),
      "Q7FullSample"=(T1O3WTP$out[4,]),
      "PrecautionaryPremium"=data.frame(cbind("Mean"=mean(Full_Final$Precaution), "LB"=left,  "UB"=right))),2)
PrecautionaryPremium <- cbind(PrecautionaryPremium$Mean,"%Income"=round(data.frame("%Income"=100/(mean(Full_Final$Q24AIncome*12))*PrecautionaryPremium[,1]),2),PrecautionaryPremium$LB,PrecautionaryPremium$UB)
colnames(PrecautionaryPremium) <- c("Mean","Percent of annual income","Lower bound", "Upper bound")
rownames(PrecautionaryPremium) <- c("Q6Order1","Q6Order2","Q6FulLSample","Q7Order1","Q7Order2","Q7FullSample","PrecautionaryPremium")
PrecautionaryPremium
Full_Final$PrecautionProportion <- 100/(Full_Final$Q24AIncome*12)*Full_Final$Precaution


## Fitting Q7 WTP using the individual level SB format
Treatment_SBWTP <- sbchoice(Q7TreatmentResponse ~ Order +  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity
                            + Q21Experts + Q22Education + Q23Employment
                            +  Q24AIncome + Timing | Q7Bid ,data = FullSurvey2,dist="logistic")
FullSurvey2 <- cbind(FullSurvey2,
                     apply(FullSurvey2, 
                           1, 
                           function(i) c(krCI(Treatment_SBWTP,individual = data.frame(Order= FullSurvey2$Order[abs(i)], Q1Gender = FullSurvey2$Q1Gender[abs(i)], Q2Age = FullSurvey2$Q2Age[abs(i)], Q3Distance = FullSurvey2$Q3Distance[abs(i)],Q4Trips = FullSurvey2$Q4Trips[abs(i)], Q16BP = FullSurvey2$Q16BP[abs(i)],Q18Charity = FullSurvey2$Q18Charity[abs(i)],Q21Experts = FullSurvey2$Q21Experts[abs(i)],Q22Education = FullSurvey2$Q22Education[abs(i)], Q23Employment = FullSurvey2$Q23Employment[abs(i)], Q24AIncome = FullSurvey2$Q24AIncome[abs(i)],Timing=FullSurvey2$Timing[abs(i)]))$out[4,1])))
colnames(FullSurvey2)[61] <- "Q7WTPSB"


######### Replicating Cameron (2005):


## Here I make dataset which has the same variables as they mention in text: 
FL <- cbind(Full_Long,"ET"=1-Full_Long$Q14FutureThreatToSelf,
            "IncomeBid" = ((Full_Long$Q24AIncome*12)-Full_Long$Q7Bid)/((Full_Long$Q24AIncome*12)),
            "VarT" = rep((-var(Full_Long$Q13CurrentThreatToSelf)),times=nrow(Full_Long)))

## Fitting the basic model here:
#### NOTE: Including VarT is highly correlated and won't estimate so is left out. 
Cameron <- sbchoice(Q7TreatmentResponse ~ FL$ET | FL$IncomeBid, data = FL,dist="logistic")
summary(Cameron)
krCI(Cameron)

CameronME <- probitmfx(formula = Q7TreatmentResponse ~ FL$ET | FL$IncomeBid,data = FL,robust = TRUE)
summary(CameronME$fit)

## If the Cameron model estimates my plan was to fit individual level OP.
#### NOTE: Code not currently fixed.
# cbind(FL,
#       apply(FL, 
#             1, 
#             function(i) c(krCI(Cameron,individual = data.frame(ET=FL$ET[i]))$out[4,1])))


#### Section 4F: CV Marginal Effects ####
############ Lines: 1705:1749


## Install required packages to estimate Probit and reqport the marginal effects 
install.packages("aod")
install.packages("mfx")
require(aod)
library(mfx)


## Model for Q6:
CVMProbit <-glm(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                + Q4Trips + Q6ResearchCertainty + Q16BP + Q18Charity + Q20Consequentiality
                + Q21Experts + Q22Education + Q23Employment
                +  Q24AIncome + Q25Understanding + Timing + Q6Bid, family = binomial(link = "probit"),data = FullSurvey2)
summary(CVMProbit) ## Report the model
confint(CVMProbit) ## Estimate confidence interval -default 95%
# wald.test(b = coef(CVMProbit), Sigma = vcov(CVMProbit), Terms=2) ## Attempt a Wald-test
stargazer(CVMProbit, title = "CVMProbit", align = TRUE,report="p*") ## Export results to LaTeX code


CVMME <- probitmfx(formula = Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                   + Q4Trips + Q6ResearchCertainty + Q16BP + Q18Charity
                   + Q21Experts + Q22Education + Q23Employment
                   +  Q24AIncome + Timing + Q6Bid,data = FullSurvey2,robust = TRUE)
CVMME ## Report Marginal Effects


## Model for Q7:
QOVProbit <-glm(Q7TreatmentResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                + Q4Trips + Q16BP + Q18Charity + Q20Consequentiality
                + Q21Experts + Q22Education + Q23Employment
                +  Q24AIncome + Q25Understanding + Timing  + Q7Bid, 
                family = binomial(link = "probit"),data = FullSurvey2)
summary(QOVProbit)
QOVME <- probitmfx(formula = Q7Response2 ~ Order + Q1Gender + Q2Age + Q3Distance
                   + Q4Trips + Q7TreatmentCertainty +Q16BP + Q18Charity + Q20Consequentiality
                   + Q21Experts + Q22Education + Q23Employment
                   +  Q24AIncome + Q25Understanding + Timing+ Q7Bid2,data = FullSurvey2,robust = TRUE)
confint(QOVProbit)
wald.test(b = coef(QOVProbit), Sigma = vcov(QOVProbit), Terms=2)
## All the same commands as the Q6 models

data.frame(round(QOVME$mfxest[,1],3))
data.frame(round(coef(Treatment1_SB),3))
data.frame(round(stdEr(Treatment1_SB),3))


#### Section 4G: WTP Determinants ####


summary(lm(Q6WTP~Order + Q1Gender + Q2Age + Q3Distance
+ Q4Trips + Q5Knowledge + Q6ResearchCertainty +
  Q13CurrentThreatToSelf + Q14FutureThreatToSelf + Q15ThreatToEnvironment+Q16BP + Q17_Firms + Q17_Cons + Q17_Gov + Full_Final$Q17_LA+ Q18Charity + Q19Knowledge + Q20Consequentiality
+ Q21Experts + Q22Education + Q23Employment + Q24RonaImpact
+  Q24AIncome + Q25Understanding + Timing,Full_Final))

summary(lm(Q7WTP~Order + Q1Gender + Q2Age + Q3Distance
           + Q4Trips + Q5Knowledge + Q7TreatmentCertainty +
             Q13CurrentThreatToSelf + Q14FutureThreatToSelf + Q15ThreatToEnvironment+Q16BP + Q17_Firms + Q17_Cons + Q17_Gov + Full_Final$Q17_LA+ Q18Charity + Q19Knowledge + Q20Consequentiality
           + Q21Experts + Q22Education + Q23Employment + Q24RonaImpact
           +  Q24AIncome + Q25Understanding + Timing,Full_Final))

summary(lm(PerformanceCoef~Order + Q1Gender + Q2Age + Q3Distance
           + Q4Trips + Q5Knowledge + Q12CECertainty + Q12Block+
             Q13CurrentThreatToSelf + Q14FutureThreatToSelf + Q15ThreatToEnvironment+Q16BP + Q17_Firms + Q17_Cons + Q17_Gov + Full_Final$Q17_LA+ Q18Charity + Q19Knowledge + Q20Consequentiality
           + Q21Experts + Q22Education + Q23Employment + Q24RonaImpact
           +  Q24AIncome + Q25Understanding + Timing+ Price + Performance +Emission
           ,Full_Final))

summary(lm(EmissionCoef~Order + Q1Gender + Q2Age + Q3Distance
           + Q4Trips + Q5Knowledge + Q12CECertainty + Q12Block+
             Q13CurrentThreatToSelf + Q14FutureThreatToSelf + Q15ThreatToEnvironment+Q16BP + Q17_Firms + Q17_Cons + Q17_Gov + Full_Final$Q17_LA+ Q18Charity + Q19Knowledge + Q20Consequentiality
           + Q21Experts + Q22Education + Q23Employment + Q24RonaImpact
           +  Q24AIncome + Q25Understanding + Timing+ Price + Performance +Emission
           ,Full_Final))

PrecautionModel <- (lm(Precaution~Order + Q1Gender + Q2Age + Q3Distance
           + Q4Trips + Q519 + Q6ResearchCertainty +Q7TreatmentCertainty+
             Q13CurrentThreatToSelf + Q14FutureThreatToSelf + Q15ThreatToEnvironment+
             Q16BP + Q17_Firms + Q17_Cons + Q17_Gov + Full_Final$Q17_LA+ Q18Charity +
             Q20Consequentiality+
            Q21Experts + Q22Education + Q23Employment + Q24RonaImpact+
           Q24AIncome + Q25Understanding + Timing
           ,Full_Final))
round(summary(PrecautionModel)$coefficients,3)
summary(PrecautionModel)


#### Section 5: Descriptive Statistics and calculations ####
############ Notes: Very long and verbose. Skip if uninterested.


## Truncation Strategy One:
round(mean(Full_Full$Q6WTP),2) ## 24.89
round(mean(Full_Full$Q7WTP),2) ## 45.25
round(mean(Full_Full$PerformanceCoef),3) ## -0.041
round(mean(Full_Full$EmissionCoef),3) ## 0.035


## Truncation Strategy Two:
round(mean(Full_Full$Q6WTP),2) ## 23.72
round(mean(Full_Full$Q7WTP),2) ## 45.33
round(mean(Full_Full$PerformanceCoef),3) ## -0.053
round(mean(Full_Full$EmissionCoef),3) ## 0.045


## Gender: 
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[FullSurvey$Q1Gender=="Male"]) ## Proportion of the sample that is male. Actual: 46, Target: 49
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[FullSurvey$Q1Gender=="Female"]) ## Proportion of the sample that is female. Actual: 53, Target: 51 
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[(FullSurvey$Q1Gender !="Female")  & (FullSurvey$Q1Gender != "Male")]) ## Estimating the percentage who reported otherwise


## Age: 
mean(FullSurvey2$Q2Age) ## Estimating average age


## Trips: 
mean(FullSurvey2$Q4Trips) ## Estimating average annual trips


## Charity: 
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="No"])
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="Yes"])
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="Prefer not to say"])


## Education: 
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==0]) ## 0 = Prefer not to say 2.23%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==1]) ## 1 = GCSE 21.94%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==2]) ## 2 = A level 26.56%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==3]) ## 3 = Bachelors 31.64%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==4]) ## 4 = PG 17.61%
# Less than University: 50.74%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==0]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==1]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==2])
# Bachelors or more: 49.25%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==3]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==4])


## Employment: 
100/670*table(FullSurvey2$Q23Employment)

## Income: 
mean(FullSurvey2$Q24AIncome) ## Estimating sample income


## Timing (faster than 48% time)
(median(Full_Long$Timing)/60)/100*48
library(ggplot2)
ggplot(FullSurvey2, aes(x=Timing/60)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  geom_vline(aes(xintercept=(median(FullSurvey2$Timing)/60)/100*48),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name="Completion length in minutes")+
  geom_vline(xintercept=((median(FullSurvey2$Timing)/60)/100*48), colour="grey") +
  geom_text(aes(x=((median(FullSurvey2$Timing)/60)/100*48)-2, label="Speeders", y=100), colour="red", angle=0) +
  geom_text(aes(x=((median(FullSurvey2$Timing)/60)/100*48)+2, label="Included", y=100), colour="blue", angle=0)+
  ggtitle("Distribution of survey completion timings.")


### Plotting survey understanding
ggplot(Full_Final, aes(x=Q25Understanding)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  geom_vline(aes(xintercept=7),color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name="Self-reported survey undertanding in 1-10")+
  geom_vline(xintercept=(7), colour="grey") +
  geom_text(aes(x=5, label="Excluded", y=400), colour="red", angle=0) +
  geom_text(aes(x=8, label="Included", y=400), colour="blue", angle=0)+
  ggtitle("Distribution of survey understanding")


## Protest analysis:
FullSurvey3 <- data.frame(read.csv("FullSurvey.csv")) 
Protestors <- c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)
Protestors <- ifelse(FullSurvey3$ï..Respondent %in%  Protestors== TRUE, 1,0)
Full_Final <- cbind(Full_Final,slice(.data = data.frame(Protestors),rep(1:n(), each = 8)))

### Plotting protest effects:
ProtestGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Protestors, labeller = as_labeller(c(
    `0` = "Protest\n (N = 561)",
    `1` = "Included\n (N = 109)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by protest votes") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Protesting",y="WTP")


grid.arrange(ggplot(Full_Final[Full_Final$Protestors==0,], aes(x=Full_Final$Q6WTP[Full_Final$Protestors ==0])) + 
               geom_histogram(color="black", fill="white",bins = 50)+
               scale_x_continuous(breaks=waiver(),limits = c(0,50),
                                  n.breaks = 10)+
               ggtitle("Histogram of CV Q6 WTP for protestors."),
             ggplot(Full_Final[Full_Final$Protestors==1,], aes(x=Full_Final$Q6WTP[Full_Final$Protestors ==1])) + 
               geom_histogram(color="black", fill="white",bins = 50)+
               scale_x_continuous(breaks=waiver(),limits = c(0,50),
                                  n.breaks = 10)+
               ggtitle("Histogram of CV Q6 WTP for non-protestors."))

### Plotting protest effects a different way:
ProtestGraph <- ggplot(Full_Final, aes(x=as.numeric(Protestors))) +
  geom_col(aes(y=Q6WTP)) +
  ggtitle("WTP by protesting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Protests",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Protested","Did not"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


## Reporting WTP by responsibility:
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Firms == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Cons == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Gov == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_LA == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Other == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Firms == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Cons == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Gov == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_LA == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Other == 0]),2)


## Responsibility beliefs:
Full_Final$Q17_Firms[ (Full_Final$Q17_Cons == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Other == 0) ] <- 2
Full_Final$Q17_Cons[ (Full_Final$Q17_Firms == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Other == 0) ] <- 2
Full_Final$Q17_Cons[Full_Final$Q17_Cons != 2] <- 0
Full_Final$Q17_Cons[Full_Final$Q17_Cons == 2] <- 1
Full_Final$Q17_Other[ (Full_Final$Q17_Cons == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Firms == 0) ] <- 2



ggplot(Full_Final, aes(x=as.numeric(Q13CurrentThreatToSelf))) + 
  facet_grid( ~ Order, labeller = as_labeller(c(
    `0` = paste("Order 1 (N =",nrow(Full_Final[Full_Final$Order==0,])/8,")"),
    `1` = paste("Order 2 (N =",nrow(Full_Final[Full_Final$Order==1,])/8,")"))))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and precaution given ordering") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  labs(x = "Environmental Attitudes",y="WTP")


## Correlation heatmap:
Full_Final <- data.frame(read.csv("FinalData.csv")) ## Imports from the excel file straight from the survey companies website.
Full_Final <- Full_Final[ -c(2,grep("av_A", colnames(Full_Final)),grep("av_B", colnames(Full_Final)))] ## Drop columns of no importance to the quantitative analysis, namely text responses.
Full_Final$Choice[Full_Final$Choice==FALSE] <- 0
Full_Final$Choice[Full_Final$Choice==TRUE] <- 1
Full_Final$alt <- as.numeric(Full_Final$alt)
library(ggplot2)
library(reshape2)
cormat <- round(cor(Full_Final),2) ## store correlations in a matrix
melted_cormat <- melt(cormat) ## Simplify corr matrix
## Get lower triangle
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
##  Get upper triangle
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
## Use the melt function to simplify the matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

## Rename matrix columns
colnames(melted_cormat) <- c("Variable1", "Variable2","Correlation")


## Correlation heatmap
ggheatmap <- ggplot(melted_cormat, aes(Variable2, Variable1, fill = Correlation))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 6, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()


#### Section 5B: Test of means for the truncation strategies ####


## Rule 1) Timing testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$Q6WTP[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$Q7WTP[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$PerformanceCoef[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$EmissionCoef[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/= 


## Rule 2) Understanding testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$Q6WTP[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: NULL
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$Q7WTP[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$PerformanceCoef[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$EmissionCoef[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/= 


## Rule 3) Protest vote testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Protestors==0],y=Full_Final$Q6WTP[Full_Final$Protestors==1]) ## Outcome: NULL
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Protestors==0],y=Full_Final$Q7WTP[Full_Final$Protestors==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Protestors==0],y=Full_Final$PerformanceCoef[Full_Final$Protestors==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Protestors==0],y=Full_Final$EmissionCoef[Full_Final$Protestors==1]) ## Outcome: Alternative of =/= 


## Rule 4) Dominated scenario testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q8DominatedTest==0],y=Full_Final$Q6WTP[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q8DominatedTest==0],y=Full_Final$Q7WTP[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==0],y=Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==0],y=Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/= 


## Rule 5) Consequentiality testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.2353
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.007
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.007
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000




## Rule 6) Certainty testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]) ## Outcome: Alternative of =/=

wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]) ## Outcome: Alternative of =/= 

wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1]) ## Outcome: NULL
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL

wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1]) ## Outcome: NULL 
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL 
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL 

## Testing difference between full and truncated samples
wilcox.test(x=Full_Final$Q6WTP,y=Full_Full$Q6WTP) ## P=0.049
wilcox.test(x=Full_Final$Q6WTP,y=Full_Full1$Q6WTP) ## P=0.041 
wilcox.test(x=Full_Full$Q6WTP,y=Full_Full1$Q6WTP) ## P=0.16

wilcox.test(x=Full_Final$Q7WTP,y=Full_Full$Q7WTP) ## P=0.049
wilcox.test(x=Full_Final$Q7WTP,y=Full_Full1$Q7WTP) ## P=0.041 
wilcox.test(x=Full_Full$Q7WTP,y=Full_Full1$Q7WTP) ## P=0.16

wilcox.test(x=Full_Final$PerformanceCoef,y=Full_Full$PerformanceCoef) ## P=0.049
wilcox.test(x=Full_Final$PerformanceCoef,y=Full_Full1$PerformanceCoef) ## P=0.041 
wilcox.test(x=Full_Full$PerformanceCoef,y=Full_Full1$PerformanceCoef) ## P=0.16

wilcox.test(x=Full_Final$EmissionCoef,y=Full_Full$EmissionCoef) ## P=0.049
wilcox.test(x=Full_Final$EmissionCoef,y=Full_Full1$EmissionCoef) ## P=0.041 
wilcox.test(x=Full_Full$EmissionCoef,y=Full_Full1$EmissionCoef) ## P=0.16



#### Section 5C: Descriptive Graphics ####
############ Notes: Long 


# PART ONE: Acceptance Rates
library(ggplot2)


Acceptance <- data.frame("Full_Long" =c(length(Full_Long$ID[(((Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)))]),length(Full_Long$ID[(((Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)))])), 
                         "Full_Cons" =c(length(Full_Cons$ID[(((Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)))]),length(Full_Cons$ID[(((Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)))])),
                         "Full_Dominated" =c(length(Full_Dominated$ID[(((Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)))]),length(Full_Dominated$ID[(((Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)))])),
                         "Full_Certain" =c(length(Full_Certain$ID[(((Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)))]),length(Full_Certain$ID[(((Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)))])))
Acceptance <- t(Acceptance) ## Transposed is easier to work with
Acceptance <- cbind(Acceptance,rowSums(Acceptance),deparse.level = 2) ## Add total responses
Acceptance <- cbind(Acceptance,100/Acceptance[,3]*Acceptance[,1],100/Acceptance[,3]*Acceptance[,2],c(1,2,3,4),deparse.level = 2) ## Add percentage accepting and rejecting A and number the questions.
colnames(Acceptance) <- c("A","B","Total","Acceptance","Rejected","Dataset")
Acceptance <- data.frame(Acceptance)


## AcceptanceRate1 is acceptance of each option in the Full sample
AcceptanceRate1 <- rbind(t(data.frame("Q9"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 1) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 1) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 2) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 2) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 3) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 3) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 4) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 4) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))))
AcceptanceRate1 <- cbind(AcceptanceRate1,c(1,2,3,4),round(AcceptanceRate1[,1:2]/sum(AcceptanceRate1[1,1:2])*100,3))
colnames(AcceptanceRate1) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate1 <- data.frame(AcceptanceRate1)


## AcceptanceRate2 is acceptance of each option in the sample eliminating those failing the dominance test
AcceptanceRate2 <- rbind(t(data.frame("Q9"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 1) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 1) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 2) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 2) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 3) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 3) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 4) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 4) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))))
AcceptanceRate2 <- cbind(AcceptanceRate2,c(1,2,3,4),round(AcceptanceRate2[,1:2]/sum(AcceptanceRate2[1,1:2])*100,3))
colnames(AcceptanceRate2) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate2 <- data.frame(AcceptanceRate2)
AcceptanceRate2


## AcceptanceRate3 is acceptance of each option in the sample relying on certainty
AcceptanceRate3 <- rbind(t(data.frame("Q9"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 1) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 1) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 2) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 2) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 3) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 3) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 4) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 4) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))))
AcceptanceRate3 <- cbind(AcceptanceRate3,c(1,2,3,4),round(AcceptanceRate3[,1:2]/sum(AcceptanceRate3[1,1:2])*100,3))
colnames(AcceptanceRate3) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate3 <- data.frame(AcceptanceRate3)
AcceptanceRate3


## AcceptanceRate4 is acceptance of each option in the consequential sample
AcceptanceRate4 <- rbind(t(data.frame("Q9"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 1) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 1) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 2) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 2) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 3) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 3) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 4) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 4) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))))
AcceptanceRate4 <- cbind(AcceptanceRate4,c(1,2,3,4),round(AcceptanceRate4[,1:2]/sum(AcceptanceRate4[1,1:2])*100,3))
colnames(AcceptanceRate4) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate4 <- data.frame(AcceptanceRate4)
AcceptanceRate4


## Combining line plots of acceptance rates over questions by dataset
### Note no major differences by question or data. 
ggplot()+
  geom_line(aes(x=Question,y=APercent,color="red"),data=AcceptanceRate1)+
  geom_line(aes(x=Question,y=APercent,color="blue"),data=AcceptanceRate2)+
  geom_line(aes(x=Question,y=APercent,color="green"),data=AcceptanceRate3)+
  geom_line(aes(x=Question,y=APercent,color="orange"),data=AcceptanceRate4)+
  scale_x_continuous(name="Question",breaks = 1:4, 
                     labels=c(rownames(AcceptanceRate1)))+
  scale_y_continuous(name="Percent choosing Option A",
                     breaks=waiver(),limits = c(25,75),
                     n.breaks = 10, labels = function(x) paste0(x, "%"))+
  scale_color_discrete(name = "Lines", 
                       labels = c("Full sample", "Dominated","Certainty","Consequentiality"))+
  ggtitle("Percentage choosing the status quo by question and truncation strategy.")


# PART TWO: Attitudes
Full_Cons <- data.frame(Full_Cons) ## Change into dataframe format 
Full_Long <- data.frame(Full_Long) ## Same here - ggplot2 throws a fit without it.

## Here the Blue-Planet question is manipulated to be either of two values
Full_Cons$Q16BP[Full_Cons$Q16BP == 0] <- "Not watched" 
Full_Cons$Q16BP[Full_Cons$Q16BP == 1] <- "Watched"
Full_Cons$Q16BP[Full_Cons$Q16BP == 2] <- "Watched"

## Categorising charity involvement.
Full_Cons$Q18Charity[Full_Cons$Q18Charity == 0] <- "No involvement"
Full_Cons$Q18Charity[Full_Cons$Q18Charity == 1] <- "Donated or joined"

## Rephrasing consequentiality into strings for use in graphics. 
### The strings used here and above are changed back later for econometric analysis. 
Full_Long$Q20Consequentiality[Full_Long$Q20Consequentiality == 0] <- "Inconsequential"
Full_Long$Q20Consequentiality[Full_Long$Q20Consequentiality == 1] <- "Consequential"


## Using these two packages for graphics.
library(scales)
library(ggplot2) 

## Plot 1 (of 2):
P1 <- ggplot(Full_Long, aes(Q3Distance,Q4Trips)) + 
  geom_point(shape = 1) +
  facet_grid(~Q18Charity) + 
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between distance and trips by charity involvement.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Distance",y="Trips")+
  scale_y_continuous(limits = c(0,1))

## Plot 2 (of 2):
P2 <- ggplot(Full_Long, aes(Q3Distance,Q4Trips)) + 
  geom_point(shape = 1) +
  facet_grid(~Q16BP) + 
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between distance and trips by Blue-Planet II") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Distance",y="Trips")+
  scale_y_continuous(limits = c(0,1))

## Plots both Plot1 and Plot2 together in the same view.
library(gridExtra)
grid.arrange(P1, P2 )


# PART THREE: Demand Curves


## Demand Curve: Plotting choice versus price levels 
PriceCurve <- ggplot(Fulls, aes(Choice,Price_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Demand curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))+
  scale_y_continuous(breaks = 0:3, labels=c(0.5,1.0,2.5,5.0))


## Emission Curve: Plotting choice versus emission levels 
EmissionsCurve <- ggplot(Fulls, aes(Choice,Emission_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Emission curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))

## Performance Curve: Plotting choice versus performance levels 
PerformanceCurve <- ggplot(Fulls, aes(Choice,Performance_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Performance curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1,labels=c(0,1))

grid.arrange(PriceCurve, EmissionsCurve, PerformanceCurve)


#### Section 5D: Plotting covariates ####
############ NOTES: Very long and possibly repetitive. Will update with Figure numbers.


library(ggplot2)
library(gridExtra)


#### Section 5Da: CE Section: ####


## Plotting a histogram of individual attribute-specific WTP 
EmissionDistribution <- ggplot(Full_Final, aes(x=EmissionCoef)) + 
  geom_histogram(color="black", fill="white",bins = 50)+
  scale_x_continuous(breaks=waiver(),limits = c(-0.5,0.5),
                     n.breaks = 10)+
  ggtitle("Histogram of emission WTP.")


## Plotting a histogram of individual attribute-specific WTP
PerformanceDistribution <- ggplot(Full_Final, aes(x=PerformanceCoef)) + 
  geom_histogram(color="black", fill="white",bins = 50)+
  scale_x_continuous(breaks=waiver(),limits = c(-0.5,0.5),
                     n.breaks = 10)+
  ggtitle("Histogram of performance WTP.")


### Plotting Q8 Dominance test on MWTP:
Q8PerformanceGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), abs(PerformanceCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="| Performance MWTP |",
                   breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==0]),3))), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==1]),3))), color="white")+
  ggtitle("Absolute performance MWTP by dominated test.")
  
Q8EmissionGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",abs(round(mean(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==0]),3))), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",abs(round(mean(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==1]),3))), color="white")+
  ggtitle("Emissions MWTP by dominated test.")
  
grid.arrange(Q8PerformanceGraph, Q8EmissionGraph)

### Plotting Q8 Dominance test
PerformanceWTP <- ggplot(Full_Final, aes(y= PerformanceCoef,x=as.numeric(Q24AIncome))) + 
  geom_point(shape = 1) +
  facet_grid( ~ Q8DominatedTest, labeller = as_labeller(c(
    `0` = "Pass",
    `1` = "Fail")))+
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between income and MWTP by Q8") +
  scale_y_continuous(name="Performance MWTP",
                     breaks=waiver(),limits = c(-1,1),
                     n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Performance MWTP")


### Plotting MWTP for emissions attribute
EmissionWTP <- ggplot(Full_Final, aes(y= EmissionCoef,x=as.numeric(Q24AIncome))) + 
  geom_point(shape = 1) +
  facet_grid( ~ Q8DominatedTest, labeller = as_labeller(c(
    `0` = "Pass",
    `1` = "Fail")))+
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between income and WTP by Q8") +
  scale_y_continuous(name="Emission MWTP",
                     breaks=waiver(),limits = c(-1,1),
                     n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Emission MWTP")

Dominated <- rbind(cbind(round(median(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest ==0]),4), round(median(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest ==1]),4)),
      (cbind(round(median(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest ==1]),4))),
      cbind(round(median(Full_Final$Q6WTP[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$Q6WTP[Full_Final$Q8DominatedTest ==1]),4)),
      cbind(round(median(Full_Final$Q7WTP[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$Q7WTP[Full_Final$Q8DominatedTest ==1]),4)))
rownames(Dominated) <- c("Performance MWTP","Emission MWTP","Q6 WTP","Q7 WTP")
colnames(Dominated) <- c("Passed","Failed")
Dominated


### Plotting Consequentiality effects
ggplot(data=Fulls, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q20Consequentiality, labeller = as_labeller(c(
    `0` = "Inconsequential",
    `1` = "Consequential",
    `2` = "Don't Know")))+
  geom_smooth(aes(y=Performance,color="red"),method="lm",se=F) +
  geom_smooth(aes(y=Emission,color="blue"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  ggtitle("Relationship between income and WTP by consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="MWTP",breaks = waiver(),n.breaks = 10, 
                     limits=c(-5,5),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="MWTP")


#### Section 5Db: CV Section: ####


## Here I make a dataframe which has Q6, Q7 WTP and the precautionary premium and is truncated according to all criteria 
FS <- Full_Final[ (Full_Final$ID) %in% c(Full_Full$ID),  ]


## Plotting precaution  
ggplot() + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male",
    `2` = "Other")))+
  geom_smooth(aes(x=Q24AIncome,y=Q6WTP,color="red"),data=FS,method="lm",se=F) +
  geom_smooth(aes(x=Q24AIncome,y=Q7WTP,color="blue"),data=FS,method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for Research", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Difference between Q6 and Q7 WTP")


Full_Final <- subset(Full_Final,Q1Gender !=2)
## Plotting the effect of income on precautionary premia, faceted by gender.
Precaution <- ggplot(Full_Final) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female\n (N = 356)",
    `1` = "Male\n (N = 311)")))+
  geom_smooth(aes(x=Q24AIncome,y=Precaution,color="red"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Precautionary premium", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,40),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Income",y="WTP")

Precaution





## Plotting a histogram for the precautionary premia
PPHist <- ggplot(FullSurvey2, aes(x=Precaution)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  scale_x_continuous(breaks=waiver(),limits = c(0,40),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  ggtitle("Histogram of respondent precautionary premia.")


### Plotting Consequentiality effects
Q20Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q20Consequentiality, labeller = as_labeller(c(
    `0` = "No\n (N = 110)",
    `1` = "Yes\n (N = 358)",
    `2` = "Don't Know\n (N = 202)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=10,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


Full_Final <- cbind(Full_Final,slice(.data = data.frame(FullSurvey2$Q7WTPSB),rep(1:n(), each = 8)))
FF <- subset(Full_Final,subset = Full_Final$Q20Consequentiality != 2)

### Plotting Consequentiality effects on CV fitted WTP
Q20GraphB <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and consequentiality (Q7 using DB)") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")


ConsBarEmissions <- ggplot(Full_Final, aes((Q20Consequentiality), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                 labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0]),3)), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1]),3)), color="white")+
  geom_text(x = 2, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]),3)), color="white")+ 
  ggtitle("Mean Emission MWTP by consequentiality beliefs.")

ConsBarPerformance <- ggplot(Full_Final, aes((Q20Consequentiality), PerformanceCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Performance MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0]),3))), color="white")+
  geom_text(x = 1, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1]),3))), color="white")+
  geom_text(x = 2, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]),3))), color="white")+
  ggtitle("Mean Performance MWTP by consequentiality beliefs.")

ConsBarQ6 <- ggplot(Full_Final, aes((Q20Consequentiality), Q6WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Q6WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1]),2)), color="white")+
  geom_text(x = 2, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]),2)), color="white")+ 
  ggtitle("Mean Q6 WTP by consequentiality beliefs.")

ConsBarQ7 <- ggplot(Full_Final, aes((Q20Consequentiality), Q7WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Q7WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1]),2)), color="white")+
  geom_text(x = 2, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]),2)), color="white")+
  ggtitle("Mean Q7 WTP by consequentiality beliefs.")

grid.arrange(ConsBarEmissions, ConsBarPerformance)
grid.arrange(ConsBarQ6, ConsBarQ7)

  
  
rbind(cbind("Q6o0C0"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==0)])/8,"Q6o1C0"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==0)])/8),
cbind("Q6o0C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==1)])/8,"Q6o1C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==1)])/8),
cbind("Q6o0C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==2)])/8,"Q6o1C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==2)])/8))

rbind(cbind("Q7o0C0"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==0)])/8,"Q7o1C0"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==0)])/8),
      cbind("Q7o0C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==1)])/8,"Q7o1C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==1)])/8),
      cbind("Q7o0C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==2)])/8,"Q7o1C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==2)])/8))

### Plotting Consequentiality effects on CV fitted WTP
Q20GraphSB <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=FullSurvey2.Q7WTPSB,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and consequentiality (Q7 using SB)") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")

### Plotting Consequentiality effects on CE MWTP
Q20GraphC <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between MWTP and consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="CE absolute MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")


### Plotting Certainty effects
Q12Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q12CECertainty, labeller = as_labeller(c(
    `0` = "Unsure\n (N = 40)",
    `1` = "Quite Sure\n (N = 328)",
    `2` = "Very Sure\n (N = 302)")))+
  geom_smooth(aes(y=PerformanceCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by certainty") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=10,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")

Q12GraphB <- ggplot(Full_Final, aes(x=Q12CECertainty)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
    ggtitle("Relationship between CE MWTP and certainty.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Self-reported certainty",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("Unsure","Quite Sure","Very Sure"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

CertaintyGraph <- ggplot(Full_Final, aes(x=Q6ResearchCertainty)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(inherit.aes = FALSE,aes(y=Q7WTP,x=Q7TreatmentCertainty,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by certainty") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Self-reported certainty",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("Unsure","Quite Sure","Very Sure"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

Q20P <- ggplot(Full_Final, aes(as.numeric(Q6ResearchCertainty), abs(PerformanceCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q12CECertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 40)","1 (Quite Sure)\n (N = 328)","2 (Very Sure)\n (N=302)"))+
  scale_y_continuous(name="| PerformanceCoef |",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0]),3))), color="white")+
  geom_text(x = 1, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1]),3))), color="white")+
  geom_text(x = 2, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]),3))), color="white")+
  ggtitle("Performance MWTP by certainty beliefs.")

Q20E <- ggplot(Full_Final, aes(as.numeric(Q12CECertainty), abs(EmissionCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q12CECertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 40)","1 (Quite Sure)\n (N = 328)","2 (Very Sure)\n (N=302)"))+
  scale_y_continuous(name="Emission MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0]),3)), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1]),3)), color="white")+
  geom_text(x = 2, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]),3)), color="white")+ 
  ggtitle("Emissions MWTP by certainty beliefs.")

grid.arrange(Q20P,Q20E)


Q20Q6 <- ggplot(Full_Final, aes(as.numeric(Q6ResearchCertainty), abs(Q6WTP))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q6ResearchCertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 70)","1 (Quite Sure)\n (N = 277)","2 (Very Sure)\n (N=323)"))+
  scale_y_continuous(name="Q6WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0]),2)), color="white")+
  geom_text(x = 1, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1]),2)), color="white")+
  geom_text(x = 2, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]),2)), color="white")+ 
  ggtitle("Mean Q6 WTP by certainty beliefs.")

Q20Q7 <- ggplot(Full_Final, aes(as.numeric(Q7TreatmentCertainty), abs(Q7WTP))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q7ResearchCertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 54)","1 (Quite Sure)\n (N = 269)","2 (Very Sure)\n (N=347)"))+
  scale_y_continuous(name="Q7WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0]),2)), color="white")+
  geom_text(x = 1, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1]),2)), color="white")+
  geom_text(x = 2, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]),2)), color="white")+ 
  ggtitle("Mean Q7 WTP by certainty beliefs.")

grid.arrange(Q20Q6,Q20Q7)




Q8EmissionGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))
grid.arrange(Q8PerformanceGraph, Q8EmissionGraph)


grid.arrange(Q12GraphB,CertaintyGraph)


AgeGraphCV <- ggplot(Full_Final, aes(x=Q2Age)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by age") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),
                     n.breaks = 10,limits=c(15,80))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

AgeGraphCE <- ggplot(Full_Final, aes(x=Q2Age)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between CE MWTP and age") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),
                     n.breaks = 10,limits=c(15,80))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

GenderGraphCV <- ggplot(Full_Final, aes(x=Q1Gender)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by gender") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Gender",breaks = waiver(),
                     n.breaks = 2, labels = c("Female","Male"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

GenderGraphCE <- ggplot(Full_Final, aes(x=Q1Gender)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between CE MWTP and gender") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Gender",breaks = waiver(),
                     n.breaks = 2, labels = c("Female","Male"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

grid.arrange(GenderGraphCE,GenderGraphCV,AgeGraphCE,AgeGraphCV)


rbind("Q6"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$Q6WTP[Full_Final$Q1Gender== 0]),3)),
                                             data.frame("Male"=round(median(Full_Final$Q6WTP[Full_Final$Q1Gender== 1]),3)),
                                             data.frame("18-25"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                             data.frame("26-39"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                             data.frame("40-55"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                             data.frame("56-70"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                             data.frame("71+"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Q7"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$Q7WTP[Full_Final$Q1Gender== 0]),3)),
                                             data.frame("Male"=round(median(Full_Final$Q7WTP[Full_Final$Q1Gender== 1]),3)),
                                             data.frame("18-25"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                             data.frame("26-39"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                             data.frame("40-55"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                             data.frame("56-70"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                             data.frame("71+"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Performance"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$PerformanceCoef[Full_Final$Q1Gender== 0]),3)),
                                                      data.frame("Male"=round(median(Full_Final$PerformanceCoef[Full_Final$Q1Gender== 1]),3)),
                                                      data.frame("18-25"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                      data.frame("26-39"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                      data.frame("40-55"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                      data.frame("56-70"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                      data.frame("71+"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
  "Emission"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$EmissionCoef[Full_Final$Q1Gender== 0]),3)),
                                                   data.frame("Male"=round(median(Full_Final$EmissionCoef[Full_Final$Q1Gender== 1]),3)),
                                                   data.frame("18-25"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                   data.frame("26-39"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                   data.frame("40-55"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                   data.frame("56-70"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                   data.frame("71+"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
  "Income"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(mean(Full_Final$Q24AIncome[Full_Final$Q1Gender== 0]),3)),
                                                 data.frame("Male"=round(mean(Full_Final$Q24AIncome[Full_Final$Q1Gender== 1]),3)),
                                                 data.frame("18-25"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                 data.frame("26-39"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                 data.frame("40-55"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                 data.frame("56-70"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                 data.frame("71+"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Concern"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q1Gender== 0]),3)),
                                                  data.frame("Male"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q1Gender== 1]),3)),
                                                  data.frame("18-25"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                  data.frame("26-39"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                  data.frame("40-55"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                  data.frame("56-70"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                  data.frame("71+"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))))


## Plotting the effect of distance from the coast on WTP
Q3Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q3Distance,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q3Distance,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q3: Distance") +
  scale_x_continuous(name="Distance",breaks=waiver(),limits=c(0,50),
                     n.breaks=5)+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10))


### Plotting the effect of distance:
Q3GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q3Distance))) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between income and distance from the coast") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
 scale_y_continuous(name="Gross monthly income",breaks = waiver(), n.breaks=10,
                     limits=c(0,5000),labels = function(x) paste0("£", x))+
  scale_x_continuous(name="Distance",breaks=waiver(),limits=c(0,50),
                     n.breaks=5)+ labs(x = "Distance",y="Income")
  
  
## Plotting the effect of number of trips to the coast on WTP
Q4Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q4Trips,y=Q7WTP,color="blue"),method="lm",se=T) +
  geom_smooth(aes(x=Q4Trips,y=Q6WTP,color="red"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for treatment", "WTP for research"))+
  ggtitle("WTP by Q4: Trips") +
  scale_x_continuous(name="Annual trips to the coast.",breaks = waiver(),limits = c(0,3),
                     n.breaks = 3,
                     labels=c("0\n (N = 76)","1-2\n (N = 267)","3-5\n (N = 153)","6+\n (N = 174)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10))


## Plotting WTP by pre-survey microplastic knowledge
Q4GraphB <- ggplot(Full_Final, aes(x=Q4Trips)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)*12),method="lm",se=F)+
  ggtitle("Relationship between income and trips.") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Annual trips to the coast.",breaks = waiver(),limits = c(0,3),
                     n.breaks = 3,
                     labels=c("0\n (N = 76)","1-2\n (N = 267)","3-5\n (N = 153)","6+\n (N = 174)"))+
  scale_y_continuous(name="Gross annual income",breaks = waiver(), n.breaks=10,
                     limits=c(0,60000),labels = function(x) paste0("£", round(x,2)/1000,",000"))+
  labs(x = "Trips",y="Income")


## Plotting WTP by pre-survey microplastic knowledge
Q5Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q5Knowledge,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q5Knowledge,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q5: Knowledge") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c("1\n(N = 71)","2\n(N = 187)","3\n(N = 252)","4\n(N = 113)","5\n(N = 47)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of Q19 knowledge about microplastics post survey
Q19Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q19Knowledge,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q19Knowledge,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q19: Knowledge") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c("1\n(N = 43)","2\n(N = 158)","3\n(N = 267)","4\n(N = 159)","5\n(N = 43)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of health concern on WTP
Q13Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q13CurrentThreatToSelf,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q13CurrentThreatToSelf,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q13: Current Threat To Self") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of future concerns on WTP
Q14Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q14FutureThreatToSelf,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q14FutureThreatToSelf,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q14: Future Threat To Self") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP in £",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of environmental concern on WTP
Q15Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q15ThreatToEnvironment,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q15ThreatToEnvironment,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q15: Threat To Environment") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP in £",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting CV WTP by COVID-19 
COVID_Graph1 <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q24RonaImpact, labeller = as_labeller(c(
    `0` = "No\n (N = 332)",
    `1` = "Yes\n (N = 319)",
    `2` = "Prefer not to say\n (N = 19)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by effect of COVID-19 on income.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=6,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


## Plotting CE MWPT by COVID-19  
COVID_Graph2 <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q24RonaImpact, labeller = as_labeller(c(
    `0` = "No\n (N = 332)",
    `1` = "Yes\n (N = 319)",
    `2` = "Prefer not to say\n (N = 19)")))+
  geom_smooth(aes(y=EmissionCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=PerformanceCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and Marginal WTP by effect of COVID-19 on income.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Emission MWTP", "Performance MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), 
                     limits=c(-0.5,0.5),n.breaks=10,labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


## Plotting CV WTP by perceived responsibility Q17_1:  
Q17_FirmsGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q17_Firms, labeller = as_labeller(c(
    `0` = "No",
    `1` = "Yes")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by whether firms are responsible.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), 
                     limits=c(0,75),n.breaks=15,labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Plotting CV WTP by perceived responsibility Q17_2:  
Q17_ConsGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q17_Cons, labeller = as_labeller(c(
    `0` = "No\n (N = 286)",
    `1` = "Yes\n (N = 384)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by whether consumers only are responsible.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Plotting CV WTP by charity involvement:  
Q18Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q18Charity, labeller = as_labeller(c(
    `0` = "No\n (N = 425)",
    `1` = "Yes\n (N = 219)",
    `2` = "Prefer not to say\n (N = 26)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by charity involvement.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Charity vs WTP
Q18GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q18Charity))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and charity involvement.") +
  scale_color_discrete(name = "Lines", 
    labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Charity involvement",breaks = waiver(),limits = c(0,1),
    n.breaks = 2,labels=c("No involvement\n (N = 451)", "Donated or member\n (N = 219)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
    limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Charity",y="WTP")


### Another method of plotting charity on WTP
Q18GraphC <- ggplot(Full_Final, aes(x=as.numeric(Q18Charity))) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and charity involvement.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Charity involvement",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2,labels=c("No involvement\n (N = 451)", "Donated or member\n (N = 219)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0.00,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Charity",y="WTP")


## Plotting CV WTP by belief in experts: 
Q21Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q21Experts, labeller = as_labeller(c(
    `1` = "1: Unconfident\n (N = 16)",
    `2` = "2\n (N = 46)",
    `3` = "3\n (N = 251)",
    `4` = "4\n (N = 237)",
    `5` = "5: Confident\n (N = 120)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by level of confidence in experts.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


### Plotting belief in experts
Q21GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between confidence in experts and WTP (lm fitting).") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Experts",y="WTP")


### Plotting the relationship between confidence in experts and WTP (lm fitting).
Q21GraphC <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=Q5Knowledge,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q19Knowledge,color="red"),method="lm",se=F) +
  geom_smooth(aes(y=Q13CurrentThreatToSelf,color="darkorange1"),method="lm",se=F,linetype="dotdash") +
  geom_smooth(aes(y=Q14FutureThreatToSelf,color="darkorange2"),method="lm",se=F,linetype="dashed") +
  geom_smooth(aes(y=Q15ThreatToEnvironment,color="darkorange3"),method="lm",se=F,linetype="dotted") +
  ggtitle("Relationship between confidence in experts and WTP (lm fitting).") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q5 Knowledge", "Q19Knowledge","Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="Likert scale",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c(1,2,3,4,5))+
  labs(x = "Experts",y="Likert scale")


### Another experts plot but on MWTP
Q21GraphD <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=PerformanceCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between confidence in experts and MWTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(-0.5,0.5),labels = function(x) paste0("£",x))+
  labs(x = "Experts",y="WTP")


## Plotting CV WTP by employment type:
Q22Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q22Education, labeller = as_labeller(c(
    `0` = "Prefer not to say\n (N = 15)",
    `1` = "GCSE\n (N = 147)",
    `2` = "A-level\n (N = 178)",
    `3` = "Bachelors\n (N = 212)",
    `4` = "Postgraduate\n (N = 118)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by education level.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 3, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


### Relationship between education levels and CV WTP
Q22GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q22Education))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between education and WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="WTP")


### Relationship between education levels and CE WTP
Q22GraphBB <- ggplot(Full_Final, aes(x=as.numeric(Q22Education))) + 
  geom_smooth(aes(y=PerformanceCoef*-1,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between education and CE WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=21,
                     limits=c(0,0.2),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="WTP")

### Alternative to plotting education/wtp
Q22GraphC <- ggplot(Full_Final, aes(x=Q22Education)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between education and income") +
  theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="Income",breaks = waiver(), n.breaks=5,
                     limits=c(0,5000),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="Income")


## Plotting CV WTP by employment type:
Q23Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q23Employment, labeller = as_labeller(c(
    `0` = "Prefer not to say\n (N = 18)",
    `1` = "NEET\n (N = 76)",
    `2` = "Retired\n (N = 52)",
    `3` = "Student\n (N = 30)",
    `4` = "Part-time\n (N = 100)",
    `5` = "Self-employed\n (N = 46)",
    `6` = "Full-time\n (N = 348)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by employment type.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 3, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


ggplot(Full_Final, aes(x=as.numeric(Q23Employment))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between employment and WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                               "Retired\n (N = 52)",
                                               "Student\n (N = 30)",
                                               "Part-time\n (N = 100)",
                                               "Self-employed\n (N = 46)",
                                               "Full-time\n (N = 348)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="WTP")


### Relationship between employment levels and CE WTP
Q23GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q23Employment))) + 
  geom_smooth(aes(y=PerformanceCoef*-1,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between employment and CE WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                           "Retired\n (N = 52)",
                                           "Student\n (N = 30)",
                                           "Part-time\n (N = 100)",
                                           "Self-employed\n (N = 46)",
                                           "Full-time\n (N = 348)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="WTP")


Q23GraphC <- ggplot(Full_Final, aes(x=Q23Employment)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between employment and income") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                           "Retired\n (N = 52)",
                                           "Student\n (N = 30)",
                                           "Part-time\n (N = 100)",
                                           "Self-employed\n (N = 46)",
                                           "Full-time\n (N = 348)"))+
  scale_y_continuous(name="Income",breaks = waiver(), n.breaks=5,
                     limits=c(0,5000),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="Income")

###### Went a bit crazy here plotting points for Q16:


## First create subsets for each response:
BP0 <- subset(Full_Final,Full_Final$Q16BP==0)
BP1 <- subset(Full_Final,Full_Final$Q16BP==1)
BP2 <- subset(Full_Final,Full_Final$Q16BP==2)
BP <- data.frame("Q6WTPBP0I0"=round(mean(BP0$Q6WTP[BP0$Q24AIncome <= unique(BP0$Q24AIncome)[4]]),2),
                 "Q7WTPBP0I0"=round(mean(BP0$Q7WTP[BP0$Q24AIncome <= unique(BP0$Q24AIncome)[4]]),2),
                 "Q6WTPBP0I1"=round(mean(BP0$Q6WTP[BP0$Q24AIncome > unique(BP0$Q24AIncome)[5]]),2),
                 "Q7WTPBP0I1"=round(mean(BP0$Q7WTP[BP0$Q24AIncome > unique(BP0$Q24AIncome)[5]]),2),
                 "Q6WTPBP1I0"=round(mean(BP1$Q6WTP[BP1$Q24AIncome <= unique(BP1$Q24AIncome)[4]]),2),
                 "Q7WTPBP1I0"=round(mean(BP1$Q7WTP[BP1$Q24AIncome <= unique(BP1$Q24AIncome)[4]]),2),
                 "Q6WTPBP1I1"=round(mean(BP1$Q6WTP[BP1$Q24AIncome > unique(BP1$Q24AIncome)[5]]),2),
                 "Q7WTPBP1I1"=round(mean(BP1$Q7WTP[BP1$Q24AIncome > unique(BP1$Q24AIncome)[5]]),2),
                 "Q6WTPBP2I0"=round(mean(BP2$Q6WTP[BP2$Q24AIncome <= unique(BP2$Q24AIncome)[4]]),2),
                 "Q7WTPBP2I0"=round(mean(BP2$Q7WTP[BP2$Q24AIncome <= unique(BP2$Q24AIncome)[4]]),2),
                 "Q6WTPBP2I1"=round(mean(BP2$Q6WTP[BP2$Q24AIncome > unique(BP2$Q24AIncome)[5]]),2),
                 "Q7WTPBP2I1"=round(mean(BP2$Q7WTP[BP2$Q24AIncome > unique(BP2$Q24AIncome)[5]]),2))

## Plotting Q16 responses for CV WTP with points added:
Q16Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q16BP, labeller = as_labeller(c(
    `0` = "None\n (N = 209)",
    `1` = "Some\n (N = 342)",
    `2` = "All\n (N = 119)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  
  geom_point(data=BP0,aes(y=BP[1,1], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,1]>0,paste0("£", round(BP[1,1],2)),""),y=BP[1,1]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP0,aes(y=BP[1,2], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,2]>0,paste0("£", round(BP[1,2],2)),""),y=BP[1,2]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP0,aes(y=BP[1,3], x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,3]>0,paste0("£", round(BP[1,3],2)),""),y=BP[1,3]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP0,aes(y=BP[1,4], x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,4]>0,paste0("£", round(BP[1,4],2)),""),y=BP[1,4]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  
  geom_point(data=BP1,aes(y=BP[1,5], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,5]>0,paste0("£", round(BP[1,5],2)),""),y=BP[1,5]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP1,aes(y=BP[1,6], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,6]>0,paste0("£", round(BP[1,6],2)),""),y=BP[1,6]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP1,aes(y=BP[1,7]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,7]>0,paste0("£", round(BP[1,7],2)),""),y=BP[1,7]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP1,aes(y=BP[1,8]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,8]>0,paste0("£", round(BP[1,8],2)),""),y=BP[1,8]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  
  geom_point(data=BP2,aes(y=BP[1,9], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,9]>0,paste0("£", round(BP[1,9],2)),""),y=BP[1,9]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP2,aes(y=BP[1,10]+1, x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,10]>0,paste0("£", round(BP[1,10],2)),""),y=BP[1,10]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP2,aes(y=BP[1,11], x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,11]>0,paste0("£", round(BP[1,11],2)),""),y=BP[1,11]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP2,aes(y=BP[1,12]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,12]>0,paste0("£", round(BP[1,12],2)),""),y=BP[1,12]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  ggtitle("Relationship between income and WTP faceted by Blue-Planet II viewership") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Plotting Q16 simply versus CV WTP:
Q16GraphB <- ggplot(Full_Final, aes(x=Q16BP)) + 
  geom_smooth(aes(y=as.numeric(Q6WTP),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=as.numeric(Q7WTP),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("Relationship between BP viewing and WTP") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Amount of BPII viewed.",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("None","Some","All"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

## Plotting Q16 simply versus CE WTP:
Q16GraphC <- ggplot(Full_Final, aes(x=Q16BP)) + 
  geom_smooth(aes(y=as.numeric(abs(PerformanceCoef)),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=as.numeric(EmissionCoef),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance|", "Emission"))+
  ggtitle("Relationship between BP viewing and CE WTP") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Amount of BPII viewed.",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("None","Some","All"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

## Plotting Q5 knowledge vs concern about microplastics
KnowledgeGraphA <- ggplot(Full_Final, aes(x=Q5Knowledge)) + 
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between knowledge (Q5) and concern (Q13,14,15)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Q5Knowledge",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("No\nknowledge\n(N = 71)","Little\n(N = 187)","Average\n(N = 252)","Good\n(N = 113)","Strong\nknowledge\n(N = 47)"))+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Income",y="WTP")


## Plotting Q19 knowledge and concern, allowing respondents to update knowledge:
KnowledgeGraphB <- ggplot(Full_Final, aes(x=Q19Knowledge)) + 
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between knowledge (Q19) and concern (Q13,14,15)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Q19Knowledge",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("No\nknowledge\n(N = 43)","Little\n(N = 158)","Average\n(N = 267)","Good\n(N = 159)","Strong\nknowledge\n(N = 43)"))+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Income",y="WTP")


### Plotting age, gender, and concern
KnowledgeGraphC <- ggplot(Full_Final, aes(x=Q2Age)) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male")))+
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between age, gender and concern") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),limits = c(20.0,70.0),
                     n.breaks = 5)+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Age",y="Concern")


### Age, gender and knowledge
KnowledgeGraphD <- ggplot(Full_Final, aes(x=Q2Age)) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male")))+
  geom_smooth(aes(y=(Q5Knowledge),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q19Knowledge),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q5", "Q19"))+
  ggtitle("Relationship between age, gender and concern") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),limits = c(20.0,70.0),
                     n.breaks = 5)+
  scale_y_continuous(name="Likert scale of knowledge",breaks = waiver(), n.breaks=5,
                     limits=c(1,5))+
  labs(x = "Age",y="Concern")


#### Section 5Dc: Spatial CV WTP: #### 
library(rnaturalearth)
library(sf)
library(raster)
library(tidyverse)
library(RColorBrewer)

## Importing and transforming a UK outline
UK <- ne_countries(scale = 50, country = "united kingdom", returnclass = "sf")
UK <- st_transform(UK, 27700)
grid <- st_make_grid(UK, cellsize = 1000, what = "centers")
grid <- st_intersection(grid, UK)  
UK <- st_cast(UK, "MULTILINESTRING")
dist <- st_distance(UK, grid)
df <- data.frame(dist = as.vector(dist),st_coordinates(grid))

## The approach below is to classify the distances from the coast according to the survey categories
ClassifiedQ6 <- data.frame(ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[1], 0,
       ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[2], 1,
              ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[3], 2,
                     ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[4], 3,
                            ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[5], 4,4))))))

ClassifiedQ7 <- ClassifiedQ6
Q6D1 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[1]]),3)
Q6D2 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[2]]),3)
Q6D3 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[3]]),3)
Q6D4 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[4]]),3)
Q6D5 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[5]]),3)

ClassifiedQ6 <- ifelse(ClassifiedQ6==0, Q6D1,
       ifelse(ClassifiedQ6==1, Q6D2,
              ifelse(ClassifiedQ6==2, Q6D3,
                     ifelse(ClassifiedQ6==3, Q6D4,
                            ifelse(ClassifiedQ6==4, Q6D5,Q6D5)))))
colnames(ClassifiedQ6) <- "ClassifiedQ6"

Q7D1 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[1]]),3)
Q7D2 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[2]]),3)
Q7D3 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[3]]),3)
Q7D4 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[4]]),3)
Q7D5 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[5]]),3)

ClassifiedQ7 <- ifelse(ClassifiedQ7==0, Q7D1,
                       ifelse(ClassifiedQ7==1, Q7D2,
                              ifelse(ClassifiedQ7==2, Q7D3,
                                     ifelse(ClassifiedQ7==3, Q7D4,
                                            ifelse(ClassifiedQ7==4, Q7D5,Q7D5)))))
colnames(ClassifiedQ7) <- "ClassifiedQ7"
df2 <- (data.frame(dist = as.vector(dist),st_coordinates(grid),ClassifiedQ6=ClassifiedQ6,ClassifiedQ7=ClassifiedQ7))


## Plotting the two CV WTP maps:
CQ6 <- ggplot(df2, aes(X, Y, fill = ClassifiedQ6))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(brewer.pal(5, "RdGy")))+ #colors for plotting the distance
  labs(fill = "WTP (Q6 in per person per year)")+ #legend name
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=8,face="bold"),
        legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid"),
        legend.key.size = grid::unit(2, "lines"))+ 
  coord_fixed(1) +
  ggtitle("Spatial distribution of Q6 WTP")

CQ7 <- ggplot(df2, aes(X, Y, fill = ClassifiedQ7))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(brewer.pal(10, "BrBG")))+ #colors for plotting the distance
  labs(fill = "WTP (Q7 in per person per year)")+ #legend name
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=8,face="bold"),
        legend.background = element_rect(fill="lightblue",size=0.25, linetype="solid"),
        legend.key.size = grid::unit(1, "lines"))+ 
  coord_fixed(1) +
  ggtitle("Spatial distribution of Q7 WTP")



## Reporting distance-decay WTP:
DistanceDecay <- rbind("Q6"=data.frame("LowIncomeLowDistance"=mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "LowIncomeHighDistance"= mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeLowDistance"=mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeHighDistance"= mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Q7"=data.frame("LowIncomeLowDistance"=mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "LowIncomeHighDistance"= mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeLowDistance"=mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeHighDistance"= mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Emissions"=data.frame("LowIncomeLowDistance"=mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                              "LowIncomeHighDistance"= mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                              "HighIncomeLowDistance"=mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                              "HighIncomeHighDistance"= mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Performance"=data.frame("LowIncomeLowDistance"=mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                                "LowIncomeHighDistance"= mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                                "HighIncomeLowDistance"=mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                                "HighIncomeHighDistance"= mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])))


## Histograms of Q5 and Q19 understanding:
Q5GraphA <- ggplot(Full_Final, aes(x=Q5Knowledge)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q5Knowledge), sd = sd(Q5Knowledge), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 71)","2\n (N = 187)","3\n (N = 252)","4\n (N = 113)","5\n (N = 47)"))+
  ggtitle("Histogram of Q5 Knowledge responses.")


### Plotting knowledge histograms
Q19GraphA <- ggplot(Full_Final, aes(x=Q19Knowledge)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q19Knowledge), sd = sd(Q19Knowledge), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 43)","2\n (N = 158)","3\n (N = 267)","4\n (N = 159)","5\n (N = 43)"))+
  ggtitle("Histogram of Q19 Knowledge responses.")
grid.arrange(Q5GraphA, Q19GraphA)


## Correlations between attitudes and awareness:
# Q5/Q13: 0.309, p=0 
# Q5/Q14: 0.238, p=0 
# Q5/Q15: 0.273, p=0 
# Q19/Q13: 0.321, p=0 
# Q19/Q14: 0.359, p=0 
# Q19/Q15: 0.364, p=0 


## Histograms of Q13, 14, 15 concern:
Q13GraphA <- ggplot(Full_Final, aes(x=Q13CurrentThreatToSelf)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q13CurrentThreatToSelf), sd = sd(Q13CurrentThreatToSelf), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 32)","2\n (N = 55)","3\n (N = 290)","4\n (N = 184)","5\n (N = 109)"))+
  ggtitle("Histogram of Q13 Current Threat to Self responses.")


### Q14 histogram
Q14GraphA <- ggplot(Full_Final, aes(x=Q14FutureThreatToSelf)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q14FutureThreatToSelf), sd = sd(Q14FutureThreatToSelf), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 20)","2\n (N = 37)","3\n (N = 204)","4\n (N = 243)","5\n (N = 166)"))+
  ggtitle("Histogram of Q14FutureThreatToSelf responses.")


### Q15 Histogram
Q15GraphA <- ggplot(Full_Final, aes(x=Q15ThreatToEnvironment)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q15ThreatToEnvironment), sd = sd(Q15ThreatToEnvironment), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 12)","2\n (N = 30)","3\n (N = 154)","4\n (N = 216)","5\n (N = 258)"))+
  ggtitle("Histogram of Q15ThreatToEnvironment responses.")
grid.arrange(Q13GraphA, Q14GraphA,Q15GraphA)


### Q21 Histogram
Q21Hist <- ggplot(Full_Final, aes(x=Q21Experts)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(
    fun = function(x, mean, sd, n){
      n * dnorm(x = x, mean = mean, sd = sd)
    }, 
    args = with(Full_Final, c(mean = mean(Q21Experts), sd = sd(Q21Experts), n
                              = 6)))+
  scale_x_continuous(breaks=waiver(),
                     name="Experts",
                     labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  ggtitle("Histogram of Q21Experts")


### Plotting notable plots:
Q3Graph
Q4Graph
Q12Graph
Q13Graph
Q14Graph
Q15Graph
Q16Graph
Q17_ConsGraph
Q17_FirmsGraph
Q18Graph
Q20Graph
Q21Graph


### Combining multiple plots:
grid.arrange(Q3Graph, Q4Graph)
grid.arrange(Q5Graph, Q19Graph)
grid.arrange(Q18GraphB, Q18GraphC)
grid.arrange(KnowledgeGraphA, KnowledgeGraphB)
grid.arrange(KnowledgeGraphD, KnowledgeGraphC)
grid.arrange(PerformanceWTP, EmissionWTP )
grid.arrange(PerformanceDistribution, EmissionDistribution )


#### Section 6: Experimental Code: ####


#### Section 6A: MNL Linear ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

### Load Apollo library
library(apollo)
apollo_initialise()

Test_Apollo <- data.frame(Fulls$ID,Fulls$Task, Fulls$Q1Gender,
                          Fulls$Q2Age,as.numeric(Fulls$Q3Distance),Fulls$Q4Trips,
                          Fulls$Q13CurrentThreatToSelf,Fulls$Q14FutureThreatToSelf,
                          Fulls$Q15ThreatToEnvironment,
                          Fulls$Q16BP,Fulls$Q18Charity,
                          Fulls$Q20Consequentiality,Fulls$Q21Experts,
                          Fulls$Q22Education,Fulls$Q23Employment,
                          Fulls$Q25Understanding,Fulls[,16:21],
                          Fulls$Choice,as.numeric(Fulls$Q24AIncome),
                          Fulls$Order, as.numeric(Fulls$Task), Fulls$Q20Consequentiality,
                          Fulls$Q21Experts,
                          Fulls$Q6ResearchResponse,Fulls$Q6Bid,
                          Fulls$Q7Bid,Fulls$Q7Bid2,
                          Fulls$Q7TreatmentResponse,Fulls$Q7Response2,Fulls$Timing,
                          Fulls$Q12CECertainty,Fulls$Q6ResearchCertainty,Fulls$Q7TreatmentCertainty)
colnames(Test_Apollo) <- c("ID","Task","Q1Gender","Age","Distance",
                           "Trips","Q13CurrentThreatToSelf","Q14FutureThreatToSelf",
                           "Q15ThreatToEnvironment","BP","Charity",
                           "Consequentiality",
                           "Experts","Education","Employment","Survey",
                           "Price_B","Performance_B","Emission_B",
                           "Performance_A","Emission_A","Price_A"
                           ,"Choice","Income", "Order","Task",
                           "Consequentiality","Experts",
                           "Q6ResearchResponse","Q6Bid",
                           "Q7Bid","Q7Bid2",
                           "Q7TreatmentResponse","Q7Response2","Timing",
                           "Q12CECertainty","Q6ResearchCertainty","Q7TreatmentCertainty")

# Tests_Dominated <- Test_Apollo[!Test_Apollo$ID %in% c(Test_Apollo$ID[ ((Test_Apollo$Task == 1) & (Test_Apollo$Choice ==1) & (grepl("SQ",rownames(Test_Apollo),fixed = TRUE) == FALSE)) ]),]
# Tests_Understanding <- Tests_Dominated[!Tests_Dominated$ID %in% c( unique(Tests_Dominated$ID[Tests_Dominated$Survey <= 5])),]
# Test_Apollo <- Tests_Understanding

Test_Apollo$av_A <- rep(1,nrow(Full)) 
Test_Apollo$av_B <- rep(1,nrow(Full)) 

Test_Apollo$Q6ResearchResponse <- Test_Apollo$Q6ResearchResponse +1
Test_Apollo$Q7TreatmentResponse <- Test_Apollo$Q7TreatmentResponse +1
Test_Apollo$Bid_Alt <- rep(0,nrow(Test_Apollo))

# Test_Apollo$Choice[Test_Apollo$Choice == 1] <- 2
# Test_Apollo$Choice[Test_Apollo$Choice == 0] <- 1
database = Test_Apollo
write.csv(Test_Apollo,file = "Test_Apollo.csv")

apollo_control = list(
  modelName  ="Replicating Full_MNL",
  indivID    ="ID"
)


## Set parameters and their initial values here 
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              b_Price    = 0,
              b_Performance   = 0,
              b_Emission      = 0,
              b_Gender = 0,
              b_Age      = 0,
              b_Distance = 0,
              b_Trips    = 0,
              b_BP       = 0,
              b_Charity  = 0,
              b_Education  = 0,
              b_Employment = 0,
              b_Income     = 0,
              b_Order      = 0,
              b_Task       = 0,
              b_Cons       = 0,
              b_Experts    = 0,
              b_Timing     = 0)

## Set one of the ASCs as zero using the utility-difference approach: 
apollo_fixed = c("asc_A")

## Check model is good so far 
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  ## Must specify SDs against the ASC directly
  asc_B = asc_B + b_Gender*Q1Gender + b_Age*Age +
    b_Distance * Distance + 
    b_Trips * Trips +
    b_BP * BP +
    b_Charity * Charity + 
    b_Education * Education +
    b_Employment * Employment + 
    b_Income * Income +
    b_Order * Order +      
    b_Task * Task +       
    b_Cons * Consequentiality +       
    b_Experts * Experts+
    b_Timing * Timing
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']]  = asc_A        + b_Performance  * Performance_A + b_Emission * Emission_A + b_Price * Price_A
  V[['B']]  = asc_B  + b_Performance  * Performance_B  + b_Emission * Emission_B + b_Price * Price_B

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(model,modelOutput_settings = list(printPVal=TRUE))

## WTP calculations: 

apollo_deltaMethod(model, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(model, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))


#### Section 6A: MNL Quadratic ####

apollo_control = list(
  modelName  ="Replicating Full_MNL",
  indivID    ="ID"
)


## Set parameters and their initial values here 
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              b_Price    = 0,
              b_Performance   = 0,
              b_Emission      = 0,
              b_Performance_SQ   = 0,
              b_Emission_SQ      = 0)

## Set one of the ASCs as zero using the utility-difference approach: 
apollo_fixed = c("asc_A")

## Check model is good so far 
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']]  = asc_A        + b_Performance  * Performance_A + b_Emission * Emission_A + b_Price * Price_A
  V[['B']]  = asc_B  + b_Performance  * Performance_B  + b_Emission * Emission_B + b_Price * Price_B + b_Performance_SQ*(Performance_B^2) + b_Emission_SQ*(Emission_B^2) 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

Quadratic = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(bootstrapSE=10))

apollo_modelOutput(Quadratic,modelOutput_settings = list(printPVal=TRUE))

## Model prediction accuracy
Quadratic_Predictions <- data.frame(Quadratic$avgCP) ## Getting probabilities of choosing each option from the model
Quadratic_Predictions[Quadratic_Predictions$Quadratic.avgCP < 0.5,] <- 0
Quadratic_Predictions[Quadratic_Predictions$Quadratic.avgCP >= 0.5,] <- 1
Quadratic_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(Quadratic_Predictions$Quadratic.avgCP),rep(1:n(), each = 4)))
Quadratic_Predictions$Match <- Quadratic_Predictions$Actual==Quadratic_Predictions$Quadratic_Predictions.Quadratic.avgCP
Quadratic_Predictions$Match[Quadratic_Predictions$Match==TRUE] <- 1
Quadratic_Predictions$Match[Quadratic_Predictions$Match==FALSE] <- 0
round(100/length(Quadratic_Predictions$Match)*length(Quadratic_Predictions$Match[Quadratic_Predictions$Match==0]),3)
# 23.507
round(100/length(Quadratic_Predictions$Match)*length(Quadratic_Predictions$Match[Quadratic_Predictions$Match==1]),3)
# 76.493


## WTP calculations: 
apollo_deltaMethod(Quadratic, deltaMethod_settings=list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(Quadratic, deltaMethod_settings=list(operation="ratio", parName1="b_Emission", parName2="b_Price"))


#### Section 6A: MNL Piecewise ####


### Load Apollo library
apollo_initialise()

apollo_control = list(
  modelName  ="Replicating Full_MNL",
  indivID    ="ID")

## Set parameters and their initial values here 
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              b_Price    = 0,
              b_Emission_Low     = 0, 
              b_Emission_Medium  = 0, 
              b_Emission_High    = 0, 
              b_Performance_Low       = 0, 
              b_Performance_Middle       = 0, 
              b_Performance_High      = 0)

## Set one of the ASCs as zero using the utility-difference approach: 
apollo_fixed = c("asc_A","b_Emission_Medium","b_Performance_Middle")

## Check model is good so far 
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()

  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + (b_Emission_Low*(Emission_A==0) + b_Performance_Low*(Performance_A==0)  + b_Price*Price_A)
  
  V[['B']] = asc_B + ( b_Emission_Low*(Emission_B==0.1) + b_Emission_Medium*(Emission_B==0.4) + b_Emission_High*(Emission_B==0.9) 
                       + b_Performance_Low*(Performance_B==0.05) + b_Performance_Middle*(Performance_B==0.10) + b_Performance_High*(Performance_B==0.50) 
                       + b_Price*Price_B )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

Piecewise_model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(Piecewise_model,modelOutput_settings = list(printPVal=TRUE))

## Model prediction accuracy
Piecewise_Predictions <- data.frame(Piecewise_model$avgCP) ## Getting probabilities of choosing each option from the model
Piecewise_Predictions[Piecewise_Predictions$Piecewise_model.avgCP < 0.5,] <- 0
Piecewise_Predictions[Piecewise_Predictions$Piecewise_model.avgCP >= 0.5,] <- 1
Piecewise_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(Piecewise_Predictions$Piecewise_model.avgCP),rep(1:n(), each = 4)))
Piecewise_Predictions$Match <- Piecewise_Predictions$Actual==Piecewise_Predictions$Piecewise_Predictions.Piecewise_model.avgCP
Piecewise_Predictions$Match[Piecewise_Predictions$Match==TRUE] <- 1
Piecewise_Predictions$Match[Piecewise_Predictions$Match==FALSE] <- 0
round(100/length(Piecewise_Predictions$Match)*length(Piecewise_Predictions$Match[Piecewise_Predictions$Match==0]),3)
# 23.51
round(100/length(Piecewise_Predictions$Match)*length(Piecewise_Predictions$Match[Piecewise_Predictions$Match==1]),3)
# 76.49





## WTP calculations: 
apollo_deltaMethod(Piecewise_model, deltaMethod_settings=list(operation="ratio", parName1="b_Performance_Low", parName2="b_Price"))
WTP_Performance_Low <- -0.0459

apollo_deltaMethod(Piecewise_model, deltaMethod_settings=list(operation="ratio", parName1="b_Performance_High", parName2="b_Price"))
WTP_Performance_High <- -1.6853

apollo_deltaMethod(Piecewise_model, deltaMethod_settings=list(operation="ratio", parName1="b_Emission_Low", parName2="b_Price"))
WTP_Emissions_Low <- 1.8243

apollo_deltaMethod(Piecewise_model, deltaMethod_settings=list(operation="ratio", parName1="b_Emission_High", parName2="b_Price"))
WTP_Emissions_High <- 1.2419

## Scope for Emissions:
round((WTP_Emissions_High - WTP_Emissions_Low)/((WTP_Emissions_High + WTP_Emissions_Low)/2) / ((0.1-0.9)/((0.9+0.1)/2)),2)

## Scope for Performance:
round(((WTP_Performance_High - WTP_Performance_Low)/((WTP_Performance_High + WTP_Performance_Low)/2)) / ((0.50-0.05)/((0.50+0.05)/2)),2)

forecast <- apollo_prediction(Piecewise_model,apollo_probabilities, apollo_inputs)


fitsTest_settings = list()
fitsTest_settings[["subsamples"]] = list()
fitsTest_settings$subsamples[["0"]] = database$Consequentiality==0
fitsTest_settings$subsamples[["1"]] = database$Consequentiality==1
fitsTest_settings$subsamples[["2"]] = database$Consequentiality==2

apollo_fitsTest(Piecewise_model, apollo_probabilities,apollo_inputs,fitsTest_settings)


#### Section 6B: MXL  [Halton draws - currently does not replicate MLOGIT] ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

apollo_initialise()

## Set core controls
apollo_control = list(
  modelName ="MXL",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 5
)


## Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Performance   = 0,
                b_Emission      = 0,
                mu_log_b_Price    =0,
                sigma_log_b_Price = 0)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Price_inter"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_Price"]] = -exp( mu_log_b_Price + sigma_log_b_Price * draws_Price_inter )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Function initialisation: do not change the following three commands
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ## List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

MXLmodel = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs, 
                           estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(MXLmodel,modelOutput_settings = list(printPVal=TRUE))

deltaMethod_settings=list(operation="ratio", parName1="b_Emission", parName2="mu_log_b_Price")
apollo_deltaMethod(MXLmodel, deltaMethod_settings)


#### Section 6B: MXL [Pseudo-Monte-Carlo Draws for the appendix] ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

apollo_initialise()

## Set core controls
apollo_control = list(
  modelName ="MXL",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 1
)


## Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Performance   = 0,
                b_Emission      = 0,
                mu_log_b_Price    =0,
                sigma_log_b_Price = 0)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "pmc",
  interNDraws    = 100,
  interUnifDraws = c("draws_Price_inter"),
  interNormDraws = c(),
  intraDrawsType = "pmc",
  intraNDraws    = 100,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_Price"]] = -exp( mu_log_b_Price + sigma_log_b_Price * draws_Price_inter )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Function initialisation: do not change the following three commands
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ## List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

MXLmodel_PMC = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs, 
                           estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(MXLmodel_PMC,modelOutput_settings = list(printPVal=TRUE))


#### Section 6B: MXL [MLHS Draws for the appendix] ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

apollo_initialise()

## Set core controls
apollo_control = list(
  modelName ="MXL",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 5
)


## Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Performance   = 0,
                b_Emission      = 0,
                mu_log_b_Price    =0,
                sigma_log_b_Price = 0)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 100,
  interUnifDraws = c("draws_Price_inter"),
  interNormDraws = c(),
  intraDrawsType = "mlhs",
  intraNDraws    = 100,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_Price"]] = -exp( mu_log_b_Price + sigma_log_b_Price * draws_Price_inter )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Function initialisation: do not change the following three commands
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ## List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

MXLmodel_MLHS = apollo_estimate(apollo_beta, apollo_fixed,
                               apollo_probabilities, apollo_inputs, 
                               estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(MXLmodel_MLHS,modelOutput_settings = list(printPVal=TRUE))


#### Section 6B: MXL [Sobol Draws for the appendix] ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

apollo_initialise()

## Set core controls
apollo_control = list(
  modelName ="MXL",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 5
)


## Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Performance   = 0,
                b_Emission      = 0,
                mu_log_b_Price    =0,
                sigma_log_b_Price = 0)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 100,
  interUnifDraws = c("draws_Price_inter"),
  interNormDraws = c(),
  intraDrawsType = "sobol",
  intraNDraws    = 100,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_Price"]] = -exp( mu_log_b_Price + sigma_log_b_Price * draws_Price_inter )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Function initialisation: do not change the following three commands
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ## List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

MXLmodel_Sobol = apollo_estimate(apollo_beta, apollo_fixed,
                                apollo_probabilities, apollo_inputs, 
                                estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(MXLmodel_Sobol,modelOutput_settings = list(printPVal=TRUE))


#### Section 6B: MXL [Piecewise indirect utility for the appendix] ####


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

apollo_initialise()

## Set core controls
apollo_control = list(
  modelName ="MXL",
  indivID   ="ID",  
  mixing    = TRUE, 
  nCores    = 5
)


## Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_log_b_Price    =0,
                sigma_log_b_Price = 0,
                b_Emission_Low     = 0, 
                b_Emission_Medium  = 0,  
                b_Emission_High    = 0,  
                b_Performance_Low       = 0, 
                b_Performance_Middle       = 0, 
                b_Performance_High      = 0)

## Set one of the ASCs as zero using the utility-difference approach: 
apollo_fixed = c("asc_A","b_Emission_Low","b_Performance_Low")


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interUnifDraws = c("draws_Price_inter"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 100,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_Price"]] = -exp( mu_log_b_Price + sigma_log_b_Price * draws_Price_inter )
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Function initialisation: do not change the following three commands
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  
  ## List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Emission_Low*(Emission_A==0) + b_Performance_Low*(Performance_A==0) )
  
  V[['B']] = asc_B + b_Price*(Price_B+ b_Emission_Low*(Emission_B==0.1) + b_Emission_Medium*(Emission_B==0.4) + b_Emission_High*(Emission_B==0.9) 
                       + b_Performance_Low*(Performance_B==0.05) + b_Performance_Middle*(Performance_B==0.10) + b_Performance_High*(Performance_B==0.50))
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V
  )
  
  ## Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ## Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ## Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ## Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Optional speedTest
#speedTest_settings=list(
#   nDrawsTry = c(50, 75, 100),
#   nCoresTry = 1:3,
#   nRep      = 10
#)

#apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

MXLmodel_Piecewise = apollo_estimate(apollo_beta, apollo_fixed,
                                 apollo_probabilities, apollo_inputs, 
                                 estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(MXLmodel_Piecewise,modelOutput_settings = list(printPVal=TRUE))


## Section 6C: LCM 2-class [Note does not currently replicate GMNL] ####


apollo_initialise()

apollo_control = list(
  modelName  ="2-class LCM",
  modelDescr ="2-class LCM",
  indivID    ="ID",
  nCores     = 3
)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                delta_a         = 0,
                delta_b = 0,
                b_Gender_a = 0,
                b_Gender_b = 0,
                b_Age_a      = 0,
                b_Age_b      = 0,
                b_Distance_a =0,
                b_Distance_b=0)


### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2","delta_b","b_Gender_a","b_Age_a","b_Distance_a")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b)
  
  V=list()
  V[["class_a"]] = delta_a + b_Gender_a*Q1Gender + b_Age_a*Age +b_Distance_a*Distance
  V[["class_b"]] = delta_b + b_Gender_b*Q1Gender + b_Age_b*Age + b_Distance_b*Distance 
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice
  )
  
  ### Loop over classes
  s=1
  while(s<=2){
    
    ### Compute class-specific utilities
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodel = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs,
                          estimate_settings=list(writeIter=FALSE,bootstrapSE=10))

            ### Show output in screen
apollo_modelOutput(LCmodel,modelOutput_settings = list(printPVal=TRUE))

### Reporting WTP:
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Performance_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Performance_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_b", parName2="beta_Price_b"))

## Model prediction accuracy
LC2_Predictions <- data.frame(LCmodel$avgCP) ## Getting probabilities of choosing each option from the model
LC2_Predictions[LC2_Predictions$LCmodel.avgCP < 0.5,] <- 0
LC2_Predictions[LC2_Predictions$LCmodel.avgCP >= 0.5,] <- 1
LC2_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(LC2_Predictions$LCmodel.avgCP),rep(1:n(), each = 4)))
LC2_Predictions$Match <- LC2_Predictions$Actual==LC2_Predictions$LC2_Predictions.LCmodel.avgCP
LC2_Predictions$Match[LC2_Predictions$Match==TRUE] <- 1
LC2_Predictions$Match[LC2_Predictions$Match==FALSE] <- 0
round(100/length(LC2_Predictions$Match)*length(LC2_Predictions$Match[LC2_Predictions$Match==0]),3)
# 44.524
round(100/length(LC2_Predictions$Match)*length(LC2_Predictions$Match[LC2_Predictions$Match==1]),3)
# 55.746

## Model individual level class-allocation
LC2_Class <- data.frame(apollo_lcConditionals(model = LCmodel,apollo_probabilities,apollo_inputs)) ## Getting probabilities of choosing each option from the model
colnames(LC2_Class) <- c(1,2) ## Name columns as each option 
LC2_Class <- data.frame("Class" = as.integer(colnames(LC2_Class)[apply(round(LC2_Class,4),1,which.max)])) ## This picks the class that is most likely for each individual
colnames(LC2_Class) <- "LCM2Class"
Full_Final <- cbind(Full_Final,"LCM2Class"=slice(data.frame(LC2_Class$LCM2Class),rep(1:n(), each = 8)))
colnames(Full_Final)[69] <- "LCM2Class"


#### Section 6C: LCM 3-class [Note does not currently replicate GMNL] ####


apollo_initialise()

apollo_control = list(
  modelName  ="3-class LCM",
  modelDescr ="3-class LCM",
  indivID    ="ID",
  nCores     = 3
)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                delta_a         = 0,
                delta_b = 0,
                delta_c =0,
                b_Gender_a = 0,
                b_Gender_b = 0,
                b_Gender_c = 0,
                b_Age_a      = 0,
                b_Age_b      = 0,
                b_Age_c      = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2","delta_b","b_Gender_b","b_Age_b")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  V=list()
  V[["class_a"]] = delta_a + b_Gender_a*Q1Gender + b_Age_a*Age
  V[["class_b"]] = delta_b + b_Gender_b*Q1Gender + b_Age_b*Age
  V[["class_c"]] = delta_c + b_Gender_c*Q1Gender + b_Age_c*Age
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice
  )
  
  ### Loop over classes
  s=1
  while(s<=3){
    
    ### Compute class-specific utilities
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodel_3 = apollo_estimate(apollo_beta, apollo_fixed, 
                            apollo_probabilities, apollo_inputs,
                            estimate_settings=list(writeIter=FALSE,bootstrapSE=10))

apollo_modelOutput(LCmodel_3,modelOutput_settings = list(printPVal=TRUE))


## Model prediction accuracy
LC3_Predictions <- data.frame(LCmodel_3$avgCP) ## Getting probabilities of choosing each option from the model
LC3_Predictions[LC3_Predictions$LCmodel_3.avgCP < 0.5,] <- 0
LC3_Predictions[LC3_Predictions$LCmodel_3.avgCP >= 0.5,] <- 1
LC3_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(LC3_Predictions$LCmodel_3.avgCP),rep(1:n(), each = 4)))
LC3_Predictions$Match <- LC3_Predictions$Actual==LC3_Predictions$LC3_Predictions.LCmodel_3.avgCP
LC3_Predictions$Match[LC3_Predictions$Match==TRUE] <- 1
LC3_Predictions$Match[LC3_Predictions$Match==FALSE] <- 0
round(100/length(LC3_Predictions$Match)*length(LC3_Predictions$Match[LC3_Predictions$Match==0]),3)
# 41.791
round(100/length(LC3_Predictions$Match)*length(LC3_Predictions$Match[LC3_Predictions$Match==1]),3)
# 58.209

## Model individual level class-allocation
LC3_Class <- data.frame(apollo_lcConditionals(model = LCmodel,apollo_probabilities,apollo_inputs)) ## Getting probabilities of choosing each option from the model
colnames(LC3_Class) <- c(1,2,3) ## Name columns as each option 
LC3_Class <- data.frame("Class" = as.integer(colnames(LC3_Class)[apply(round(LC3_Class,4),1,which.max)])) ## This picks the class that is most likely for each individual
colnames(LC3_Class) <- "LCM3Class"
Full_Final <- cbind(Full_Final,"LCM3Class"=slice(data.frame(LC3_Class$LCM3Class),rep(1:n(), each = 8)))
colnames(Full_Final)[70] <- "LCM3Class"


### WTP:
apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Performance_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Performance_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Performance_c", parName2="beta_Price_c"))

apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Emission_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Emission_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel_3, list(operation="ratio", parName1="beta_Emission_c", parName2="beta_Price_c"))


#### Section 6C: LCM 4-class [Note does not currently replicate GMNL] ####


apollo_initialise()

apollo_control = list(
  modelName  ="4-class LCM",
  modelDescr ="4-class LCM",
  indivID    ="ID",
  nCores     = 3
)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Price_d       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Performance_d       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                beta_Emission_d       =0,
                delta_a         = 0,
                delta_b = 0,
                delta_c =0,
                delta_d =0,
                b_Gender_a = 0,
                b_Gender_b = 0,
                b_Gender_c = 0,
                b_Gender_d = 0,
                b_Age_a      = 0,
                b_Age_b      = 0,
                b_Age_c      = 0,
                b_Age_d      = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_2","delta_b","b_Gender_b","b_Age_b")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c,beta_Price_d)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c,beta_Performance_d)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c,beta_Emission_d)
  
  V=list()
  V[["class_a"]] = delta_a + b_Gender_a*Q1Gender + b_Age_a*Age
  V[["class_b"]] = delta_b + b_Gender_b*Q1Gender + b_Age_b*Age
  V[["class_c"]] = delta_c + b_Gender_c*Q1Gender + b_Age_c*Age
  V[["class_d"]] = delta_d + b_Gender_d*Q1Gender + b_Age_d*Age
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3,class_d=4), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice
  )
  
  ### Loop over classes
  s=1
  while(s<=4){
    
    ### Compute class-specific utilities
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodel_4 = apollo_estimate(apollo_beta, apollo_fixed, 
                            apollo_probabilities, apollo_inputs,
                            estimate_settings=list(writeIter=FALSE,bootstrapSE=1,bootstrapSeed = 24))

### Show output in screen
apollo_modelOutput(LCmodel_4,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(LCmodel_4, list(operation="ratio", parName1="beta_Performance_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel_4, list(operation="ratio", parName1="beta_Performance_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel_4, list(operation="ratio", parName1="beta_Performance_c", parName2="beta_Price_c"))
apollo_deltaMethod(LCmodel_4, list(operation="ratio", parName1="beta_Performance_d", parName2="beta_Price_d"))

apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_c", parName2="beta_Price_c"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_d", parName2="beta_Price_d"))


#### Section 6Ci: LCM with RRM and RUM classes ####
## https://www.advancedrrmmodels.com/latent-class-models


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

library(apollo)
apollo_initialise()
apollo_control = list(
  modelName  ="LC_RUM_PRRM_2classes",
  modelDescr ="Latent class with 2 classes: RUM and PRRM",
  indivID    ="ID",
  nCores     = 2 
)
## Parameters to be estimated and their starting values
## Price and Health attributes used only
apollo_beta = c(# Class 1
  B_Price_1 = 0,
  B_Performance_1 = 0,
  B_Emission_1 = 0,
  # Class 2
  B_Price_2 = 0,
  B_Performance_2  = -0.2,
  B_Emission_2 = 0,
  # Class membership parameters
  s_1     = 0,
  s_2     = 0)

## Define one class as fixed as the utility-differences approach 
apollo_fixed = c("s_1")


Test_Apollo$Price_A[Test_Apollo$Price_A == 0] <-1
Test_Apollo$Performance_A[Test_Apollo$Performance_A == 0] <-1
Test_Apollo$Emission_A[Test_Apollo$Emission_A == 0] <- 1
Test_Apollo$Price_B <- Test_Apollo$Price_B +1
Test_Apollo$Emission_B <- Test_Apollo$Emission_B +1
Test_Apollo$Performance_B <- Test_Apollo$Performance_B +1
database = Test_Apollo

## Grouping latent class parameters
apollo_lcPars = function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["B_Price"]] = list(B_Price_1, B_Price_2)
  lcpars[["B_Performance"]] = list(B_Performance_1, B_Performance_2)
  lcpars[["B_Emission"]] = list(B_Emission_1, B_Emission_2)  
  
  V=list()
  V[["class_1"]] = s_1
  V[["class_2"]] = s_2
  
  ###settings for class membership probabilities 
  mnl_settings = list(
    alternatives = c(class_1=1, class_2=2), 
    avail        = 1, 
    choiceVar    = NA, ###No choice variable as only the formula of MNL is used
    V            = V
  )
  ###Class membership probabilities
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  return(lcpars)
}

## Search the user work space for all necessary input 
apollo_inputs = apollo_validateInputs()

## Probability function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Attaches parameters and data so that variables can be referred by name
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Pairwise comparison and scale of attributes
  
  ## Define PRRM variables
  Price1_sc =( 1 / 1000 ) * Price_A
  Price2_sc =( 1 / 1000 ) * Price_B
  Performance1_sc =( 1 / 1000 ) * Performance_A
  Performance2_sc =( 1 / 1000 ) * Performance_B
  Emission1_sc =( 1 / 1000 ) * Emission_A
  Emission2_sc =( 1 / 1000 ) * Emission_B
  
  
  # Compute P-RRM Atrribute levels
  X_Price1 = pmax( 0 , Price2_sc - Price1_sc ) 
  X_Price2 = pmax( 0 , Price1_sc - Price2_sc ) 
  
  X_Performance1 = pmax( 0 , Performance2_sc - Performance1_sc ) 
  X_Performance2 = pmax( 0 , Performance1_sc - Performance2_sc ) 
  
  X_Emission1 = pmax( 0 , Emission2_sc - Emission1_sc ) 
  X_Emission2 = pmax( 0 , Emission1_sc - Emission2_sc ) 
  
  ###Create list for probabilities
  P = list()
  
  ###Input for calculating MNL probabilities
  mnl_settings = list(
    alternatives  = c(Alt1=1, Alt2=2), 
    avail         = list(Alt1=1, Alt2=1),
    choiceVar     = Choice
  )
  
  ### Compute class-specific utilities
  V=list()
  V[['Alt1']]  = B_Price_1 * Price1_sc + B_Performance_1 * Performance1_sc + B_Emission_1 * Emission1_sc
  V[['Alt2']]  = B_Price_1 * Price2_sc + B_Performance_1 * Performance2_sc + B_Emission_1 * Emission2_sc 
  
  ###Calculating probabilities based on MNL function for class 1
  mnl_settings$V = V
  P[[1]] = apollo_mnl(mnl_settings, functionality)
  P[[1]] = apollo_panelProd(P[[1]], apollo_inputs ,functionality)
  
  ### Compute class-specific regrets
  R=list()
  R[['Alt1']]  = B_Price_2 * X_Price1 + B_Performance_2 * X_Performance1 + B_Emission_2 * X_Emission1
  R[['Alt2']]  = B_Price_2 * X_Price2 + B_Performance_2 * X_Performance2 + B_Emission_2 * X_Emission2
  
  ###Calculating probabilities based on MNL function for class 2
  mnl_settings$V = lapply(R, "*", -1) ###the regrets must be negative (and used in MNL-settings as V)
  P[[2]] = apollo_mnl(mnl_settings, functionality)
  P[[2]] = apollo_panelProd(P[[2]], apollo_inputs ,functionality)
  
  ###Calculating choice probabilities using class membership and conditional probabilities 
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Starting value search:
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=20))

RRmodel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="numDeriv",bootstrapSE=1,bootstrapSeed = 24))
apollo_modelOutput(RRmodel,modelOutput_settings = list(printPVal=TRUE))

## RUM Performance MWTP
deltaMethod_settings=list(operation="ratio", parName1="B_Performance_1", parName2="B_Price_1")
apollo_deltaMethod(RRmodel, deltaMethod_settings)

## RUM Emission MWTP
deltaMethod_settings=list(operation="ratio", parName1="B_Emission_1", parName2="B_Price_1")
apollo_deltaMethod(RRmodel, deltaMethod_settings)

## RRM Performance MWTP
deltaMethod_settings=list(operation="ratio", parName1="B_Performance_2", parName2="B_Price_2")
apollo_deltaMethod(RRmodel, deltaMethod_settings)

## RRM Emission MWTP
deltaMethod_settings=list(operation="ratio", parName1="B_Emission_2", parName2="B_Price_2")
apollo_deltaMethod(RRmodel, deltaMethod_settings)

## Investigating class-allocation
RRMProbs <- slice(data.frame(RRmodel$avgCP),rep(1:n(), each = 8))
Full_Final <- cbind(Full_Final,"RRMProbs" = RRMProbs)
Full_Final[ (Full_Final$ID) %in% c(AllCriteria),  ]
Full_Final$RRmodel.avgCP[Full_Final$RRmodel.avgCP < 0.5] <- 0
Full_Final$RRmodel.avgCP[Full_Final$RRmodel.avgCP > 0.5] <- 1

## Plotting the class-allocation - no effect of sample truncation
hist(Full_Final$RRmodel.avgCP[ (Full_Final$ID) %in% c(AllCriteria) ])
hist(Full_Final$RRmodel.avgCP)


#### Section 6D: ICLV model: CE ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "ICLV model: CE",
  modelDescr = "ICLV model: CE",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 1
)

Test_Apollo_Truncated <- Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),]
database <- Test_Apollo_Truncated

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_Emission     = 0, 
                b_Performance       = 0, 
                b_Price            = 0,  
                lambda             = 1, 
                gamma_Education   = 0, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
                gamma_Employment =0,
                gamma_Experts =0,
                gamma_Cons =0,
                gamma_BP =0,
                gamma_Charity =0,
                gamma_Certainty=0,
                zeta_Q13   = 1, 
                zeta_Q14   = 1, 
                zeta_Q15   = 1, 
                tau_Q13_1  =-2, 
                tau_Q13_2  =-1, 
                tau_Q13_3  = 1, 
                tau_Q13_4  = 2, 
                tau_Q14_1  =-2, 
                tau_Q14_2  =-1, 
                tau_Q14_3  = 1, 
                tau_Q14_4  = 2, 
                tau_Q15_1  =-2, 
                tau_Q15_2  =-1, 
                tau_Q15_3  = 1, 
                tau_Q15_4  = 2)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

## DEFINE RANDOM COMPONENTS                                    

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"), 
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_Education*Education + gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Employment*Employment + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  
  return(randcoeff)
}


## GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

## DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
  op_settings1 = list(outcomeOrdered = Q13CurrentThreatToSelf, 
                      V              = zeta_Q13*LV, 
                      tau            = c(tau_Q13_1, tau_Q13_2, tau_Q13_3, tau_Q13_4),
                      rows           = (Task==1),
                      componentName  = "indic_Q13")
  op_settings2 = list(outcomeOrdered = Q14FutureThreatToSelf, 
                      V              = zeta_Q14*LV, 
                      tau            = c(tau_Q14_1, tau_Q14_2, tau_Q14_3, tau_Q14_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q14")
  op_settings3 = list(outcomeOrdered = Q15ThreatToEnvironment, 
                      V              = zeta_Q15*LV, 
                      tau            = c(tau_Q15_1, tau_Q15_2, tau_Q15_3, tau_Q15_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q15")
  P[["indic_Q13"]]     = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]]      = apollo_op(op_settings3, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = ( b_Emission*(Emission_A==0) + b_Performance*(Performance_A==0)+ b_Price*Price_A )
  V[['B']] = ( b_Emission*(Emission_B)+ b_Performance*(Performance_B)+ b_Price*Price_B + lambda*LV )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V,
    componentName= "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### MODEL ESTIMATION

## Optional: calculate LL before model estimation
apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
CEmodel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(bootstrapSE=10))
apollo_modelOutput(CEmodel,modelOutput_settings = list(printPVal=TRUE))


## Model prediction accuracy
ICLV_Predictions <- data.frame(CEmodel$avgCP) ## Getting probabilities of choosing each option from the model
ICLV_Predictions[ICLV_Predictions$CEmodel.avgCP < 0.5,] <- 0
ICLV_Predictions[ICLV_Predictions$CEmodel.avgCP >= 0.5,] <- 1
ICLV_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(ICLV_Predictions$CEmodel.avgCP),rep(1:n(), each = 4)))
ICLV_Predictions$Match <- ICLV_Predictions$Actual==ICLV_Predictions$ICLV_Predictions.CEmodel.avgCP
ICLV_Predictions$Match[ICLV_Predictions$Match==TRUE] <- 1
ICLV_Predictions$Match[ICLV_Predictions$Match==FALSE] <- 0
round(100/length(ICLV_Predictions$Match)*length(ICLV_Predictions$Match[ICLV_Predictions$Match==0]),3)
# 44.524
round(100/length(ICLV_Predictions$Match)*length(ICLV_Predictions$Match[ICLV_Predictions$Match==1]),3)
# 55.746

## WTP:
apollo_deltaMethod(CEmodel, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(CEmodel, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))


#### Section 6D: ICLV Q6 ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

database = Test_Apollo

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "ICLV",
  modelDescr = "ICLV model: CV Q6",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 1
)

# database = read.csv("apollo_drugChoiceData.csv",header=TRUE)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_bid     = 0, 
                b_bid_Alt = 0,
                lambda            = 1, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
                gamma_Experts =0,
                gamma_Cons =0,
                gamma_BP =0,
                gamma_Charity =0,
                gamma_Certainty =0,
                zeta_Q13   = 1, 
                zeta_Q14   = 1, 
                zeta_Q15   = 1, 
                tau_Q13_1  =-2, 
                tau_Q13_2  =-1, 
                tau_Q13_3  = 1, 
                tau_Q13_4  = 2, 
                tau_Q14_1  =-2, 
                tau_Q14_2  =-1, 
                tau_Q14_3  = 1, 
                tau_Q14_4  = 2, 
                tau_Q15_1  =-2, 
                tau_Q15_2  =-1, 
                tau_Q15_3  = 1, 
                tau_Q15_4  = 2)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

## DEFINE RANDOM COMPONENTS                                    

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"), 
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  
  
  return(randcoeff)
}


## GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

## DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
  op_settings1 = list(outcomeOrdered = Q13CurrentThreatToSelf, 
                      V              = zeta_Q13*LV, 
                      tau            = c(tau_Q13_1, tau_Q13_2, tau_Q13_3, tau_Q13_4),
                      rows           = (Task==1),
                      componentName  = "indic_Q13")
  op_settings2 = list(outcomeOrdered = Q14FutureThreatToSelf, 
                      V              = zeta_Q14*LV, 
                      tau            = c(tau_Q14_1, tau_Q14_2, tau_Q14_3, tau_Q14_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q14")
  op_settings3 = list(outcomeOrdered = Q15ThreatToEnvironment, 
                      V              = zeta_Q15*LV, 
                      tau            = c(tau_Q15_1, tau_Q15_2, tau_Q15_3, tau_Q15_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q15")
  P[["indic_Q13"]]     = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]]      = apollo_op(op_settings3, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = ( b_bid_Alt*(Bid_Alt==0) )
  V[['B']] = ( b_bid*(Q6Bid)
               + lambda*LV )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q6ResearchResponse,
    V            = V,
    componentName= "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### MODEL ESTIMATION

## Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
CVmodel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel,modelOutput_settings = list(printPVal=TRUE))
CVModel <- CVmodel
## Model prediction accuracy
ICLV_CVQ6_Predictions <- data.frame(CVModel$avgCP) ## Getting probabilities of choosing each option from the model
ICLV_CVQ6_Predictions[ICLV_CVQ6_Predictions$CVModel.avgCP < 0.5,] <- 0
ICLV_CVQ6_Predictions[ICLV_CVQ6_Predictions$CVModel.avgCP >= 0.5,] <- 1
ICLV_CVQ6_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(ICLV_CVQ6_Predictions$CVModel.avgCP),rep(1:n(), each = 4)))
ICLV_CVQ6_Predictions$Match <- ICLV_CVQ6_Predictions$Actual==ICLV_CVQ6_Predictions$ICLV_CVQ6_Predictions.CVModel.avgCP
ICLV_CVQ6_Predictions$Match[ICLV_CVQ6_Predictions$Match==TRUE] <- 1
ICLV_CVQ6_Predictions$Match[ICLV_CVQ6_Predictions$Match==FALSE] <- 0
round(100/length(ICLV_CVQ6_Predictions$Match)*length(ICLV_CVQ6_Predictions$Match[ICLV_CVQ6_Predictions$Match==0]),3)
# 56.567
round(100/length(ICLV_CVQ6_Predictions$Match)*length(ICLV_CVQ6_Predictions$Match[ICLV_CVQ6_Predictions$Match==1]),3)
# 43.433

## WTP:
apollo_deltaMethod(CVModel, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(CVModel, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))


#### Section 6D: ICLV Q7 ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r


rm(list = ls())
install.packages("Rcpp")
library(Rcpp)
install.packages("rngWELL")
library(rngWELL)
install.packages("randtoolbox")
library(randtoolbox)
install.packages("apollo")
library(apollo)

Test_Apollo$Q7TreatmentResponse <- Test_Apollo$Q7TreatmentResponse+1
database = Test_Apollo

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "ICLV",
  modelDescr = "ICLV model: CV Q7",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 1
)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_bid     = 0, 
                b_bid_Alt = 0,
                lambda            = 1, 
                gamma_Education   = 0, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
                gamma_Employment =0,
                gamma_Experts =0,
                gamma_Cons =0,
                gamma_BP =0,
                gamma_Charity =0,
                zeta_Q13   = 1, 
                zeta_Q14   = 1, 
                zeta_Q15   = 1, 
                tau_Q13_1  =-2, 
                tau_Q13_2  =-1, 
                tau_Q13_3  = 1, 
                tau_Q13_4  = 2, 
                tau_Q14_1  =-2, 
                tau_Q14_2  =-1, 
                tau_Q14_3  = 1, 
                tau_Q14_4  = 2, 
                tau_Q15_1  =-2, 
                tau_Q15_2  =-1, 
                tau_Q15_3  = 1, 
                tau_Q15_4  = 2)

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c()

## DEFINE RANDOM COMPONENTS                                    

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=100,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"), 
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_Education*Education + gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Employment*Employment + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + eta
  
  return(randcoeff)
}


## GROUP AND VALIDATE INPUTS

apollo_inputs = apollo_validateInputs()

## DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
  op_settings1 = list(outcomeOrdered = Q13CurrentThreatToSelf, 
                      V              = zeta_Q13*LV, 
                      tau            = c(tau_Q13_1, tau_Q13_2, tau_Q13_3, tau_Q13_4),
                      rows           = (Task==1),
                      componentName  = "indic_Q13")
  op_settings2 = list(outcomeOrdered = Q14FutureThreatToSelf, 
                      V              = zeta_Q14*LV, 
                      tau            = c(tau_Q14_1, tau_Q14_2, tau_Q14_3, tau_Q14_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q14")
  op_settings3 = list(outcomeOrdered = Q15ThreatToEnvironment, 
                      V              = zeta_Q15*LV, 
                      tau            = c(tau_Q15_1, tau_Q15_2, tau_Q15_3, tau_Q15_4), 
                      rows           = (Task==1),
                      componentName  = "indic_Q15")
  P[["indic_Q13"]]     = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]]      = apollo_op(op_settings3, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = ( b_bid_Alt*(Bid_Alt==0) )
  V[['B']] = ( b_bid*(Q7Bid) + lambda*LV )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=2, B=3),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V,
    componentName= "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### MODEL ESTIMATION

## Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
CVmodel7 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel7,modelOutput_settings = list(printPVal=TRUE))


## Model prediction accuracy
ICLV_CVQ7_Predictions <- data.frame(CVModel7$avgCP) ## Getting probabilities of choosing each option from the model
ICLV_CVQ7_Predictions[ICLV_CVQ7_Predictions$CVModel7.avgCP < 0.5,] <- 0
ICLV_CVQ7_Predictions[ICLV_CVQ7_Predictions$CVModel7.avgCP >= 0.5,] <- 1
ICLV_CVQ7_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(ICLV_CVQ7_Predictions$CVModel7.avgCP),rep(1:n(), each = 4)))
ICLV_CVQ7_Predictions$Match <- ICLV_CVQ7_Predictions$Actual==ICLV_CVQ7_Predictions$ICLV_CVQ7_Predictions.CVModel7.avgCP
ICLV_CVQ7_Predictions$Match[ICLV_CVQ7_Predictions$Match==TRUE] <- 1
ICLV_CVQ7_Predictions$Match[ICLV_CVQ7_Predictions$Match==FALSE] <- 0
round(100/length(ICLV_CVQ7_Predictions$Match)*length(ICLV_CVQ7_Predictions$Match[ICLV_CVQ7_Predictions$Match==0]),3)
# 44.524
round(100/length(ICLV_CVQ7_Predictions$Match)*length(ICLV_CVQ7_Predictions$Match[ICLV_CVQ7_Predictions$Match==1]),3)
# 55.746

## WTP:
apollo_deltaMethod(CVModel7, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(CVModel7, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))




#### Section 7: CBA section: ####


#### Section 7A: Distributional weights: ####
## Explicit Weights using sensitivity analysis for the elasticity of marginal utility of income
Weights <- cbind(data.frame("e0"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0),
                 data.frame("e0.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.5),
                 data.frame("e0.75"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.75),
                 data.frame("e1"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1),
                 data.frame("e1.3"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.3),
                 data.frame("e1.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.5))

summary(Weights$e0)
summary((Weights$e0*Full_Final$Q6WTP))
summary((Weights$e0*Full_Final$Q7WTP))
round(summary((Weights$e0*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e0*(Full_Final$PerformanceCoef*100))),2)

summary(Weights$e0.5)
summary((Weights$e0.5*Full_Final$Q6WTP))
summary((Weights$e0.5*Full_Final$Q7WTP))
round(summary((Weights$e0.5*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e0.5*(Full_Final$PerformanceCoef*100))),2)

summary(Weights$e0.75)
summary((Weights$e0.75*Full_Final$Q6WTP))
summary((Weights$e0.75*Full_Final$Q7WTP))
round(summary((Weights$e0.75*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e0.75*(Full_Final$PerformanceCoef*100))),2)

summary(Weights$e1)
summary((Weights$e1*Full_Final$Q6WTP))
summary((Weights$e1*Full_Final$Q7WTP))
round(summary((Weights$e1*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e1*(Full_Final$PerformanceCoef*100))),2)

summary(Weights$e1.3)
summary((Weights$e1.3*Full_Final$Q6WTP))
summary((Weights$e1.3*Full_Final$Q7WTP))
round(summary((Weights$e1.3*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e1.3*(Full_Final$PerformanceCoef*100))),2)

summary(Weights$e1.5)
summary((Weights$e1.5*Full_Final$Q6WTP))
summary((Weights$e1.5*Full_Final$Q7WTP))
round(summary((Weights$e1.5*(Full_Final$EmissionCoef*100))),2)
round(summary((Weights$e1.5*(Full_Final$PerformanceCoef*100))),2)

View(round(cbind(e05*Full_Final$Q6WTP,e075*Full_Final$Q6WTP,e1*Full_Final$Q6WTP,e125*Full_Final$Q6WTP,e15*Full_Final$Q6WTP),2))
e05 <- data.frame("e0.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.5)
e075 <- data.frame("e0.75"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.75)
e1 <- data.frame("e1"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1)
e13 <- data.frame("e1.3"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.3)
e15 <- data.frame("e1.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.5)


## Implicit weights calculated by the procedure suggested in Pearce, Atkinson and Mourato (2006):
## I use groups below ("poor") and above ("rich") mean sample income
# Q6: 
round(mean(Full_Final$Q6WTP[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$Q6WTP[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)

# Q7: 
round(mean(Full_Final$Q7WTP[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$Q7WTP[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)

# Emission MWTP: 
round(mean(Full_Final$EmissionCoef[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$EmissionCoef[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)

# Performance MWTP: 
round(mean(Full_Final$PerformanceCoef[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$PerformanceCoef[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)


#### Section 7B: Net Benefits: ####
# Figures estimated in Thesis C5
Households <- 27800000
Products <- 23000000
ResearchPoint <- 20000000
ResearchLower <- 5000000
ResearchUpper <- 100000000

WWTPPoint <- 1370000000
WWTPLower <- 1000000000
WWTPUpper <- 5000000000

CosmeticsPoint <- 1010000000
CosmeticsLower <- 213000000
CosmeticsUpper <- 2500000000

Full_Final <- cbind(Full_Final,
      data.frame("ResearchPointNB"=Full_Final$Q6WTP - (ResearchPoint/Households)),
      data.frame("ResearchLowerNB"=Full_Final$Q6WTP - (ResearchLower/Households)),
      data.frame("ResearchUpperNB"=Full_Final$Q6WTP - (ResearchUpper/Households)),
      data.frame("WWTPPointNB"=Full_Final$Q7WTP - (WWTPPoint/Households)),
      data.frame("WWTPLowerNB"=Full_Final$Q7WTP - (WWTPLower/Households)),
      data.frame("WWTPUpperNB"=Full_Final$Q7WTP - (WWTPUpper/Households)),
      data.frame("CosmeticsPointNB"=Full_Final$EmissionCoef*100 - (CosmeticsPoint/Products)),
      data.frame("CosmeticsLowerNB"=Full_Final$EmissionCoef*100 - (CosmeticsPoint/Products)),
      data.frame("CosmeticsUpperNB"=Full_Final$EmissionCoef*100 - (CosmeticsPoint/Products)))

## Implicit weights using net benefits instead:
# Q6 using point, lower and upper bound cost estimates:
round(mean(Full_Final$ResearchPointNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$ResearchPointNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$ResearchLowerNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$ResearchLowerNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$ResearchUpperNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$ResearchUpperNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)

# Q7: 
round(mean(Full_Final$WWTPPointNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$WWTPPointNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$WWTPLowerNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$WWTPLowerNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$WWTPUpperNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$WWTPUpperNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)

# Emission MWTP: 
round(mean(Full_Final$CosmeticsPointNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$CosmeticsPointNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$CosmeticsLowerNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$CosmeticsLowerNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)
round(mean(Full_Final$CosmeticsUpperNB[Full_Final$Q24AIncome < mean(Full_Final$Q24AIncome)])/
        mean(Full_Final$CosmeticsUpperNB[Full_Final$Q24AIncome > mean(Full_Final$Q24AIncome)]),2)




####  END OF SCRIPT ####