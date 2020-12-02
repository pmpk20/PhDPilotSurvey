#### Survey data analysis script: SETUP  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: Opening the raw data and converting into usable dataframes
# Note: may be messy in parts if I am rushing a solution.


options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
Full_Final <- data.frame(read.csv("FinalData.csv")) # Import the final data for ease


#### Section 1: Importing ####


FullSurvey <- data.frame(read.csv("FullSurvey.csv")) ## Imports from the excel file straight from the survey companies website.
# Full_Long <- data.frame(read.csv("Full_Long.csv"))
# Full_Final <- data.frame(read.csv("FinalData.csv")) ## Imports from the excel file straight from the survey companies website.
# FullSurvey <- FullSurvey[ !(FullSurvey$ï..Respondent %in% c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)), ] ## Drop protest rows


#### Section 2: Naming ####


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
} ## Use this if the previous one didn't work. An R update made the previous one not work. 

# FullSurvey2$Order[FullSurvey2$Order == 2] <-0 ## The order dummy should be 0 for Q6 > Q7 and 1 for Q7 > Q6


#### Section 3: Coding ####


## Here I update the age categories to take the midpoint of the brackets.
## There is almost certainlty an easier way but I wrote this ages ago and it works
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 0] <- 21.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 1] <- 32.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 2] <- 47.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 3] <- 63
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 4] <- 71
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 5.0] <- NA
FullSurvey2$Q2Age <- with(FullSurvey2, impute(FullSurvey2$Q2Age, 'random')) ## I replace the missing with a random imputed value
FullSurvey2$Q2Age <- as.numeric(FullSurvey2$Q2Age) ## Fixes error where the impute type isn't right.


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


## Mutate merges two columns
FullSurvey2 <- mutate(Q7Bid2 = coalesce(FullSurvey2$Q7Bid2Lower,FullSurvey2$Q7Bid2Upper),.data = FullSurvey2)
FullSurvey2 <- mutate(Q7Response2 = coalesce(FullSurvey2$Q7TreatmentUpperResponse,FullSurvey2$Q7TreatmentLowerResponse),.data = FullSurvey2)


## These are fine so just copy across
FullSurvey2$Q6Bid <- FullSurvey$Q6Bid
FullSurvey2$Q7Bid <- FullSurvey$Q7Bid
FullSurvey2$Q7Bid2 <- FullSurvey$Q7Bid2


## The following section codes all the attributes as their actual values.
FullSurvey2$Q9Performance <- FullSurvey2$Q9Performance +1
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 2] <- 0.05
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 3] <- 0.50
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 1] <- 0.10
FullSurvey2$Q9Emission <- FullSurvey2$Q9Emission +1
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 2] <- 0.40
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 3] <- 0.90
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 1] <- 0.10
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 1] <- 2.5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 2] <- 5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 0] <- 1


## Converting automatic assignment into exact levels
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 1] <- 0.05
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 0] <- 0.10
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 0] <- 0.10
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 1] <- 0.40
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 2] <- 0.40
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 2] <- 1
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 1] <- 2.5
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 0] <- 0.5


## Converting automatic assignment into exact levels
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 0] <- 0.10
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 1] <- 0.05
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 0] <- 0.10
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 1] <- 0.90
FullSurvey2$Q11Price <- FullSurvey2$Q11Price+1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 1] <- 0.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 2] <- 1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 3] <- 2.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 4] <- 5


## Converting automatic assignment into exact levels
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 1] <- 0.05
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 2] <- 0.50
FullSurvey2$Q12Performance[FullSurvey2$Q12Performance == 0] <- 0.10
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 1] <- 0.40
FullSurvey2$Q12Emission[FullSurvey2$Q12Emission == 0] <- 0.10
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


#### Section 4: Manipulating ####


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


## This little section adds in the data about firms responsibility
## It just adds the variables together, renames them, and adds back
Responsibility <- data.frame(cbind(Full$Q17_Firms,Full$Q17_Cons,Full$Q17_Gov,Full$Q17_LA,Full$Q17_Other))
colnames(Responsibility) <- c("Q17_Firms","Q17_Cons","Q17_Gov","Q17_LA","Q17_Other")
Full <- cbind(Full,"Responsibility" =rowSums(Responsibility))


Fulls <- Full # Later I use the Full dataframe but just without choies as A or B
Full$av_A <- rep(1,nrow(Full)) # Add a vector of ones to show that the alternative choice is always available to respondents.
Full$av_B <- rep(1,nrow(Full)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
Full$Choice[Full$Choice == 0] <- "A"  ## Necessary here to change numeric to string
Full$Choice[Full$Choice == 1] <- "B" ## The MFORMULA looks for _B or _A so choice must be SQ or ALT


#### MLOGIT SETUP #### 


# The data manipulation now moves into the CE specific manipulation.
library(mlogit) 

## Here the dataframe Full is reshaped from wide to long format for use in the MLOGIT estimations.
Full_Long <- mlogit.data(Full, shape = "wide", choice = "Choice",
                         varying = 16:21, sep = "_", id.var = "ID")

## Coding attribute levels in percentage points
Full_Long$Performance[Full_Long$Performance == 0.05] <- 5
Full_Long$Performance[Full_Long$Performance == 0.10] <- 10
Full_Long$Performance[Full_Long$Performance == 0.50] <- 50
Full_Long$Emission[Full_Long$Emission == 0.1] <- 10
Full_Long$Emission[Full_Long$Emission == 0.9] <- 90 
Full_Long$Emission[Full_Long$Emission == 0.4] <- 40 


#### TRUNCATION #### 


## These are respondent IDs obtained by visual inspection of text responses
ProtestVotes <- c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)

### Truncation Rule Two:
AllCriteria <- data.frame("IDs" = unique(Full_Long$ID[ (Full_Long$Q25Understanding >=7) &
                                                         (Full_Long$Q8DominatedTest == 0) &
                                                         (Full_Long$Q12CECertainty >= 1) &
                                                         (Full_Long$Q20Consequentiality >= 1) ])) 


## Here checking if any of the remaining respondents were on the protest list: 
AllCriteria <- AllCriteria[ !(AllCriteria$IDs %in% c(ProtestVotes)),]
Full_Full <- Full_Final[ (Full_Final$ID) %in% c(AllCriteria),] ## Fully truncated:
# Full_Full <- Full_Long[ (Full_Long$ID) %in% c(AllCriteria),] ## Can truncate the Full_Long dataframe too if needing to estimate in MLOGIT
Full_Excluded <- Full_Final[ !(Full_Final$ID) %in% c(AllCriteria),] ## The excluded responses
nrow(Full_Full)/8


#### APOLLO SETUP #### 


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


## Each alternative is always available so I just rep the 1 value for all respondents
Test_Apollo$av_A <- rep(1,nrow(Full)) 
Test_Apollo$av_B <- rep(1,nrow(Full)) 


## I think I change this later but basically changing [0,1] to [1,2] for the code
Test_Apollo$Q6ResearchResponse <- Test_Apollo$Q6ResearchResponse +1
Test_Apollo$Q7TreatmentResponse <- Test_Apollo$Q7TreatmentResponse +1
Test_Apollo$Bid_Alt <- rep(0,nrow(Test_Apollo)) ## I don't use this anymore but it was the alternative scenario for the CV estimation


## Here I recode the attribute levels:
## I did a lot of testing on which levels worked best and I think this is those:
## The negative sign for performance represents the expected WTA
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.05] <- -5
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.10] <- -10
Test_Apollo$Performance_B[Test_Apollo$Performance_B == 0.50] <- -50
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.1] <- 10
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.9] <- 90 
Test_Apollo$Emission_B[Test_Apollo$Emission_B == 0.4] <- 40 


## Could also just do Test_Apollo$Choice <- Test_Apollo$Choice+1
## But Apollo wanted data in [1,2] rather than [0,1]
Test_Apollo$Choice[Test_Apollo$Choice == 1] <- 2
Test_Apollo$Choice[Test_Apollo$Choice == 0] <- 1


##  Apollo requires a variable called database:
database = Test_Apollo

## Making some changes to the variable coding from [0,1] to [1,2]
database$Q7Response2 <- database$Q7Response2+1
database$Q6ResearchResponse <- database$Q6ResearchResponse-1


## Providing a truncated sample
Test_Truncated <- Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
Fulls2 <- Fulls[ (Fulls$ID) %in% c(AllCriteria),] ## The excluded responses
# database <- Test_Truncated ## Use this to estimate with the truncated sample instead


# Import All Models:
## This saves a lot of re-estimation:

CLModel <- readRDS("CLmodel.rds") ## The conditional logit
MNLFull<- readRDS("MNL1.rds") ## Full sample multinomial logit with covariates
MNLTrunc<- readRDS("MNL2.rds") ## Truncated sample multinomial logit with covariates
MXLAttributes<- readRDS("MXL19.rds") ## MXL full sample but no covariates 
MXLFull<- readRDS("MXLModel20.rds") ## MXL WTP-space, three negative lognormals, uncorrelated, full sample
MXLTrunc<- readRDS("MXLModel21.rds") ## Same as above but truncated sample
LCMFull<- readRDS("LCMStandard3CNoSD.rds") ## 3-class no SD LCM
LCMTrunc<- readRDS("LCMStandard3CNoSDTRUNCATED.rds") ## Same but truncated sample
ICLVFull<- readRDS("CE2.rds") ## The full sample CE ICLV
ICLVTrunc <- readRDS("CEmodel4.rds") ## Truncated sample CE ICLV
CVmodel6N <- readRDS("CVmodel6N.rds") ## Q6 CV Full sample ICLV
CVmodel6NT <- readRDS("CVmodel6NT.rds") ## Q6 CV truncated sample ICLV
CVmodel7N <- readRDS("CVmodel7N.rds") ## Q7 CV Full sample ICLV
CVmodel7NT <- readRDS("CVmodel7NT.rds") ## Q7 CV truncated sample ICLV


## Make a function to estimate model prediction accuracy from Apollo objects
Predictions <- function(Model,data){
  Pred <- data.frame(Model$avgCP) ## Getting probabilities of choosing each option from the model
  Pred[Pred$Model.avgCP < 0.5,] <- 0
  Pred[Pred$Model.avgCP >= 0.5,] <- 1
  Pred <- cbind("Actual"=data$Choice,"Predicted"=slice(data.frame(Pred$Model.avgCP),rep(1:n(), each = 4)))
  Pred$Match <- ifelse(Pred$Actual==Pred$Pred.Model.avgCP,1,0)
  print(paste0("Correct: ",(round(100/length(Pred$Match)*length(Pred$Match[Pred$Match==1]),2)),"%"))
}


###### The rest of this is commented out code that may be useful for specific outputs:

## If you need all models prediction accuracy:
# Predictions(CLModel,Fulls)
# Predictions(MNLFull,Fulls)
# Predictions(MNLTrunc,Fulls2)
# Predictions(MXLAttributes,Fulls)
# Predictions(MXLFull,Fulls)
# Predictions(MXLTrunc,Fulls2)
# Predictions(LCMFull,Fulls)
# Predictions(LCMTrunc,Fulls2)
# Predictions(ICLVFull,Fulls)
# Predictions(ICLVTrunc,Fulls2)


## First step to estimate bivariate-probit ICLV
# FullSurvey2$Q7OKBR <- (ifelse(FullSurvey2$Q7TreatmentResponse == 1 & FullSurvey2$Q7TreatmentUpperResponse==1, FullSurvey2$Q7Bid2Upper,
#                               ifelse(FullSurvey2$Q7TreatmentResponse == 1 & FullSurvey2$Q7TreatmentUpperResponse==0, FullSurvey2$Q7Bid,
#                                      ifelse(FullSurvey2$Q7TreatmentResponse == 0 & FullSurvey2$Q7TreatmentLowerResponse==1, FullSurvey2$Q7Bid2Lower,
#                                             ifelse(FullSurvey2$Q7TreatmentResponse == 0 & FullSurvey2$Q7TreatmentLowerResponse==0, 0,0)))))


## Reporting details of all imported models:
# MNLFull<- data.frame(apollo_modelOutput(readRDS("MNL1.rds"),modelOutput_settings = list(printPVal=TRUE)))
# MNLTrunc<- data.frame(apollo_modelOutput(readRDS("MNL2.rds"),modelOutput_settings = list(printPVal=TRUE)))
# MXLAttributes<- data.frame(apollo_modelOutput(readRDS("MXL19.rds"),modelOutput_settings = list(printPVal=TRUE)))
# MXLFull<- data.frame(apollo_modelOutput(readRDS("MXLModel20.rds"),modelOutput_settings = list(printPVal=TRUE)))
# MXLTrunc<- data.frame(apollo_modelOutput(readRDS("MXLModel21.rds"),modelOutput_settings = list(printPVal=TRUE)))
# LCMFull<- data.frame(apollo_modelOutput(readRDS("LCMStandard3CNoSD.rds"),modelOutput_settings = list(printPVal=TRUE)))
# LCMTrunc<- data.frame(apollo_modelOutput(readRDS("LCMStandard3CNoSDTRUNCATED.rds"),modelOutput_settings = list(printPVal=TRUE)))
# ICLVFull<- data.frame(apollo_modelOutput(readRDS("CE2.rds"),modelOutput_settings = list(printPVal=TRUE)))
# ICLVTrunc <- data.frame(apollo_modelOutput(readRDS("CE3.rds"),modelOutput_settings = list(printPVal=TRUE)))
# 

## Exporting details of al model parameters:
## Extremely ugly table do not replicate
# xtable(rbind(cbind(rownames(MNLFull),round(MNLFull$Estimate,3)),
# cbind(rownames(MNLTrunc),round(MNLTrunc$Estimate,3)),
# cbind(rownames(MXLAttributes),round(MXLAttributes$Estimate,3)),
# cbind(rownames(MXLFull),round(MXLFull$Estimate,3)),
# cbind(rownames(MXLTrunc),round(MXLTrunc$Estimate,3)),
# cbind(rownames(LCMFull),round(LCMFull$Estimate,3)),
# cbind(rownames(LCMTrunc),round(LCMTrunc$Estimate,3)),
# cbind(rownames(ICLVFull),round(ICLVFull$Estimate,3)),
# cbind(rownames(ICLVTrunc),round(ICLVTrunc$Estimate,3))),digits=3)