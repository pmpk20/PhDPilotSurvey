#Peter King
####################################################################################
############### Introduction: Full survey data analysis script  ##########################
####################################################################################

############ TO DO:
# - Translate FullAnalysis code

############ Packages:
install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
install.packages("stargazer") ## To export to LaTeX code.
install.packages("dplyr")
library(dplyr)
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.

############ Setup and manipulation:
FullSurvey <- data.frame(read.csv("FullSurvey.csv")) ## Imports from the excel file straight from the survey companies website.

FullSurvey <- FullSurvey[ -c(2,4,14,15,16,23,24,26,27,53,54,55,56,57,58,68,69)] ## Drop rows of no importance to the quantitative analysis, namely text responses.

colnames(FullSurvey) <- c("ID","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
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
                           "Q16BP","Q18Charity","Q19Knowledge","Q20Consequentiality",
                           "Q21Experts","Q22Education","Q23Employment",
                           "Q24RonaImpact","Q24AIncome","Q25Understanding")  
## Renaming the survey from original names to made up ones to link to the survey better.

FullSurvey2 <- FullSurvey ## Create a backup of the FullSurvey data

for (i in colnames(FullSurvey)){
  if (is.factor(FullSurvey[[i]]) == TRUE){
    FullSurvey2[[i]] <- as.numeric(FullSurvey[[i]])-1
  }
} 


FullSurvey2$Order[FullSurvey2$Order == 2] <-0 ## The order dummy should be 0 for Q6 > Q7 and 1 for Q7 > Q6

## Here I update the age categories to take the midpoint of the brackets.
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 0] <- 21.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 1] <- 32.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 2] <- 47.5
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 3] <- 63
FullSurvey2$Q2Age[FullSurvey2$Q2Age == 4] <- 71

## The loop got the distances ordered incorrectly and also didn't use midpoints.
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 7
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 6
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 2
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 7] <- 3
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 6] <- 1
FullSurvey2$Q3Distance <- FullSurvey2$Q3Distance + 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 0] <- 1
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 1] <- 6.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 2] <- 15.5
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 3] <- 35
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 4] <- 50
FullSurvey2$Q3Distance[FullSurvey2$Q3Distance == 5] <- mean(FullSurvey2$Q3Distance) ## I replace the missing with the mean value although they could also be dropped or set to zero.

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
FullSurvey2 <- mutate(Q7Bid2 = coalesce(FullSurvey2$Q7Bid2Lower,FullSurvey2$Q7Bid2Upper),.data = FullSurvey2)
FullSurvey2 <- mutate(Q7Response2 = coalesce(FullSurvey2$Q7TreatmentUpperResponse,FullSurvey2$Q7TreatmentLowerResponse),.data = FullSurvey2)

## The following section codes all the attributes as their actual values.
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 1] <- 0.05
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 2] <- 0.5
FullSurvey2$Q9Performance[FullSurvey2$Q9Performance == 0] <- 0.1
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 1] <- 0.4
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 2] <- 0.9
FullSurvey2$Q9Emission[FullSurvey2$Q9Emission == 0] <- 0.1
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 1] <- 2.5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 2] <- 5
FullSurvey2$Q9Price[FullSurvey2$Q9Price == 0] <- 1

FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 1] <- 0.05
FullSurvey2$Q10Performance[FullSurvey2$Q10Performance == 0] <- 0.1
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 1] <- 0.4
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 2] <- 0.4
FullSurvey2$Q10Emission[FullSurvey2$Q10Emission == 0] <- 0.1
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 1] <- 1
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 2] <- 2.5
FullSurvey2$Q10Price[FullSurvey2$Q10Price == 0] <- 0.5

FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 1] <- 0.05
FullSurvey2$Q11Performance[FullSurvey2$Q11Performance == 0] <- 0.1
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 1] <- 0.9
FullSurvey2$Q11Emission[FullSurvey2$Q11Emission == 0] <- 0.1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 0] <- 0.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 1] <- 1
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 2] <- 2.5
FullSurvey2$Q11Price[FullSurvey2$Q11Price == 3] <- 5

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

## More reordering here
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 2] <- 3
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 1] <- 2
FullSurvey2$Q18Charity[FullSurvey2$Q18Charity == 3] <- 1

## Same problem with Q5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 4] <- 5
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 1] <- 4
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 2] <- 1
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 0] <- 2
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 3] <- 0
FullSurvey2$Q19Knowledge[FullSurvey2$Q19Knowledge == 4] <- 3

## Reordering the consequentiality beliefs
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 0] <- 3
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 1] <- 0
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 2] <- 1
FullSurvey2$Q20Consequentiality[FullSurvey2$Q20Consequentiality == 3] <- 2

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
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 9] <- mean(FullSurvey2$Q24AIncome) ## Using mean-replacement but could also drop
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 7] <- 5000
FullSurvey2$Q24AIncome[FullSurvey2$Q24AIncome == 0] <- 250.00

## Updating the final survey question
FullSurvey2$Q25Understanding[FullSurvey2$Q25Understanding == 1] <- 9
FullSurvey2$Q25Understanding <- FullSurvey2$Q25Understanding +1

## Adding an ID column which replaces the respondent category in the original dataset.
FullSurvey2$ID <- seq.int(nrow(FullSurvey2))

## Aim of the function is to express all variables in the FullSurvey data as factors
for (i in colnames(FullSurvey2)){
  if (is.factor(FullSurvey[[i]]) == TRUE){
    contrasts(FullSurvey2[,i]) <- contr.sum(nlevels(FullSurvey2[,i]))
  }
} 

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
  data.frame(rep(FullSurvey2[,35:38], times=1)))[,]))

##  Chopping and reorganising the columns of the Full dataframe into a new order which includes the attributes and their levels alongside all the choices in a single vector.
### The final argument creates a variable called TASK
Full <- data.frame(Full[,1:13],Full[,18],DBPrice_B, DBPerformance_B, DBEmission_B,
                    Full[,55:57],Choices, Full[,19],Full[,23],Full[,27],
                    Full[,31],Full[,39:54],
                    rep(1:4,times=nrow(FullSurvey2)))

##  Assigning column names for ease of analysis.
colnames(Full) <- c("ID","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
                     "Q5Knowledge","Q6Bid","Q7Bid","Q6ResearchResponse",
                     "Q6ResearchCertainty","Q7TreatmentResponse",
                     "Q7TreatmentCertainty","Q8DominatedTest",
                     "Price_B","Performance_B","Emission_B", 
                     "Performance_A","Emission_A","Price_A","Choice",
                     "Q9Block","Q10Block","Q11Block","Q12Block","Q12CECertainty",
                     "Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment",
                     "Q16BP","Q18Charity","Q19Knowledge","Q20Consequentiality",
                     "Q21Experts","Q22Education","Q23Employment",
                     "Q24RonaImpact","Q24AIncome","Q25Understanding","Q7Bid2","Q7Response2",
                     "Task")  
Fulls <- Full
Full$av_A <- rep(1,nrow(Full)) # Add a vector of ones to show that the alternative choice is always available to respondents.
Full$av_B <- rep(1,nrow(Full)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
Full$Choice[Full$Choice == 0] <- "A"  ## Necessary here to change numeric to string
Full$Choice[Full$Choice == 1] <- "B" ## The MFORMULA looks for _B or _A so choice must be SQ or ALT

# The data manipulation now moves into the CE specific manipulation.
library(mlogit) 

## Here the dataframe Full is reshaped from wide to long format for use in the MLOGIT estimations.
Full_Long <- mlogit.data(Full, shape = "wide", choice = "Choice",
                          varying = 15:20, sep = "_", id.var = "ID")

################  Sample truncation:
# Passing the Q8 dominated test scenario
Full_Dominated <- Full_Long[Full_Long$Q8DominatedTest == 0]
# 28.65% failure rate (192/670 failed)

### Trimming by having certainty in their CE choices.
Full_Certain <- Full_Dominated[Full_Dominated$Q12CECertainty == 2]
# 68% failure (368/670 failed)

###  Believing the survey responses to be consequential.
Full_Cons <- Full_Certain[Full_Certain$Q20Consequentiality == 1]
# Only 16% (112/670 passed everything)


##########################################################  
####### Descriptive Graphics
##########################################################  

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
Full_Cons$Q14BP[Full_Cons$Q16BP == 0] <- "Not watched" 
Full_Cons$Q14BP[Full_Cons$Q16BP == 1] <- "Watched"
Full_Cons$Q14BP[Full_Cons$Q16BP == 2] <- "Watched"

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
  labs(x = "Likelihood of accepting option B",y="Price attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))


## Emission Curve: Plotting choice versus emission levels 
EmissionsCurve <- ggplot(Fulls, aes(Choice,Emission_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Emission curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Emission attribute")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))

## Performance Curve: Plotting choice versus performance levels 
PerformanceCurve <- ggplot(Fulls, aes(Choice,Performance_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Performance curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Performance attribute")+
  scale_x_continuous(breaks = 0:1,labels=c(0,1))

grid.arrange(PriceCurve, EmissionsCurve, PerformanceCurve)


##########################################################  
####### CE
##########################################################  


## Have to do the manipulation section again to ignore the changes made in the graphics section
library(mlogit) #Already have package installed
Full_Long <- mlogit.data(Full, shape = "wide", choice = "Choice",
                          varying = 15:20, sep = "_", id.var = "ID")

Full_Long$Performance[Full_Long$Performance == 0.05] <- 5
Full_Long$Performance[Full_Long$Performance == 0.10] <- 10
Full_Long$Performance[Full_Long$Performance == 0.50] <- 50
Full_Long$Emission[Full_Long$Emission == 0.1] <- 10
Full_Long$Emission[Full_Long$Emission == 0.9] <- 90 
Full_Long$Emission[Full_Long$Emission == 0.4] <- 40 

## To trim the sample: 
Full_Dominated <- Full_Long[Full_Long$Q8DominatedTest == 0]
# Full_Understanding <- Full_Dominated[Full_Dominated$Q25Understanding >= 5]
Full_Certain <- Full_Dominated[Full_Dominated$Q12CECertainty == 2]
Full_Cons <- Full_Certain[Full_Certain$Q20Consequentiality == 1]


######################## Estimation section:


## A very basic MNL model to test whether the method works 
Base_MNL <- mlogit(Choice ~  Price + Performance + Emission, 
                   Full_Long,
                   alt.subset = c("A","B"),reflevel = "A") 
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

## The full MNL with all covariates:
Full_MNL <- mlogit(Choice ~ Price + Performance + Emission | 
                      Order + Task + Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q16BP + Q18Charity 
                    + Q20Consequentiality
                    + Q21Experts +Q22Education+ Q23Employment
                    +  Q24AIncome, 
                    Full_Long, alt.subset = c("A", "B"), 
                    reflevel = "A") 
summary(Full_MNL) ## Summarises the MNL output

## I then estimate the same specification but with a MIXED LOGIT specification.
MXLFull <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  Full_Long, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=123)
summary(MXLFull)

## Same as above but with only certain responses
MXLFullD <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  Full_Cons, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bfgs",panel=TRUE,seed=123)
summary(MXLFullD)

## Compare attribute MWTP by sample
WTPs <- data.frame("Full sample" = 
                     c(-1*coef(MXLFull)["Emission"]/coef(MXLFull)["Price"],
                       -1*coef(MXLFull)["Performance"]/coef(MXLFull)["Price"])
                   ,"Truncated" = 
                     c(-1*coef(MXLFullD)["Emission"]/coef(MXLFullD)["Price"],
                       -1*coef(MXLFullD)["Performance"]/coef(MXLFullD)["Price"]),
                   "Welfare" = 
                     c(-1*(coef(MXLFullD)["Emission"]/coef(MXLFullD)["Price"] * 100),
                       -1*(coef(MXLFullD)["Performance"]/coef(MXLFullD)["Price"] *100)))
WTPs

## Plot conditional distribution of MWTP. 
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(rpar(MXLFull,"Price"), main="Scatterplot of wt vs. mpg")
plot(rpar(MXLFullD,"Price"), main="Scatterplot of wt vs disp")
AIC(MXLFullD)

## Bootstrapped clustered individual standard errors: 
library(clusterSEs)
CBSM <- cluster.bs.mlogit(MXLFullD, Full_Cons, ~ ID, boot.reps=100,seed = 123)

WTPbs <- data.frame("Emission" = c(CBSM$ci[4,1]/CBSM$ci[2,1],CBSM$ci[4,2]/CBSM$ci[2,2]),
                    "Performance"=c(CBSM$ci[3,1]/CBSM$ci[2,1],CBSM$ci[3,2]/CBSM$ci[2,2]))
WTPbs <- t(WTPbs)
WTPbs <- -1* WTPbs
WTPs <- cbind(WTPbs[,1],WTPs[,2],WTPbs[,2])
colnames(WTPs) <- c("Lower","Mean","Upper")
round(WTPs,3)


##############  GMNL is an alternative to MLOGIT
library(gmnl)


## Replicating the MNL
MNL_GM <- gmnl(  Choice ~ Price + Performance + Emission | 
                   Order + Task + Q1Gender + Q2Age + Q3Distance
                 + Q4Trips + Q16BP + Q18Charity 
                 + Q20Consequentiality
                 + Q21Experts +Q22Education+ Q23Employment
                 +  Q24AIncome,
                 data = Full_Long,
                 model = "mnl",alt.subset = c("A","B"),reflevel = "A")
summary(MNL_GM)

## GMNL has an inbuilt WTP function:
wtp.gmnl(MNL_GM,"Price",3)

## Replicating the MXL
GMNL_MXLDefault <- gmnl(Choice ~ Price + Performance + Emission | 1 | 0|
                          Order + Task + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity 
                        + Q20Consequentiality
                        + Q21Experts +Q22Education+ Q23Employment
                        +  Q24AIncome, data = Full_Long,
                        model = "mixl",
                        ranp = c( Price = "n"),
                        mvar = list(Price = c("Q18Charity")),
                        R = 10,
                        haltons = NA
                        ,seed = 123,reflevel = "A")
summary(GMNL_MXLDefault)
wtp.gmnl(GMNL_MXLDefault,"Price",3)
coef(GMNL_MXLDefault)["Performance"]/coef(GMNL_MXLDefault)["Price"]
coef(GMNL_MXLDefault)["Emission"]/coef(GMNL_MXLDefault)["Price"]

## GMNL has a plot function for the conditional distribution of the random parameters:
plot(GMNL_MXLDefault, par = "Price",type = "density", col = "grey",wrt="Price")


############ Estimating LATENT-CLASS MODELS

## Two class model:
LC_GM <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
              + Q4Trips + Q16BP + Q18Charity
              + Q21Experts +Q22Education+ Q23Employment
              +  Q24AIncome,
              data = Full_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) ## 516.2835
BIC(LC_GM) ## 586.065

## Three class model:
LC_GM3 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                 0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
               + Q4Trips + Q16BP + Q18Charity
               + Q21Experts +Q22Education+ Q23Employment
               +  Q24AIncome,
               data = Full_Cons,
               model = 'lc',
               panel = TRUE,
               Q = 3)
summary(LC_GM3)
AIC(LC_GM3) # 502.6724
BIC(LC_GM3) # 629.921

LC_GM4 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                 0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
               + Q4Trips + Q16BP + Q18Charity
               + Q21Experts +Q22Education+ Q23Employment
               +  Q24AIncome,
               data = Full_Cons,
               model = 'lc',
               panel = TRUE,
               Q = 4)
summary(LC_GM4)
AIC(LC_GM4) # 502.6724
BIC(LC_GM4) # 629.921


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
ClassProbs <- LC_GM$Qir ## Thankfully GMNL has an inbuilt method of calculating individual likelihood of class-memberships
colnames(ClassProbs) <- c(1,2) ## Name columns as one of the classes. Here I'm using the 2-class model but this can easily be augmented if the 2+ models fit better. 
Classes <- data.frame("Classes" = as.integer(colnames(ClassProbs)[apply(round(ClassProbs,4),1,which.max)])) ## This picks the class that is most likely for each individual
Full_Cons <- cbind(Full_Cons,slice(.data = Classes,rep(1:n(), times = nrow(Full_Cons)/length(unique(Full_Cons$ID)))))


## Plotting the confidence intervals of coefficients:
plot_ci_lc(LC_GM,var = c("Price"))

## LCM class-specific WTP:
-1* (coef(LC_GM)["class.1.Performance"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.1.Emission"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.2.Performance"]/coef(LC_GM)["class.2.Price"])
-1* (coef(LC_GM)["class.2.Emission"]/coef(LC_GM)["class.2.Price"])

## Sample average WTP 
wtp_bar <- (-coef(LC_GM)["class.1.Emission"] / coef(LC_GM)["class.1.Price"]) * shares(LC_GM)[1] + 
  (-coef(LC_GM)["class.2.Emission"] / coef(LC_GM)["class.2.Price"]) * shares(LC_GM)[2]
wtp_bar


##########################################################  
####### CVM
##########################################################  


#################### Setup using packages and manipulation:

## Have to do some R magic here to install a package not on CRAN
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")
install.packages("interval")
install.packages("DCchoice")
library(DCchoice)

## Creating new dataframes depending on ordering or consequentiality. 
Full_NormalOrder <-Full_Long[Full_Long$Order == 0]
Full_OtherOrder <-Full_Long[Full_Long$Order == 1]
Full_Consequential <-Full_Long[Full_Long$Q20Consequentiality == 1]
Full_Inconsequential <-Full_Long[Full_Long$Q20Consequentiality != 1]

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

#################### Graphing ordering effects:

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
#################### Plotting KMT survival functions:


## This section deals with Q6 and Q7 respectively but uses a non-parametric Kaplan-Meier-Turnbull survival function:
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = Full_Order1)
summary(ResearchKMT)
plot(ResearchKMT)

## Reporting the KMT for Q7.
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = Full_Order2)
summary(TreatmentKMT)
plot(TreatmentKMT)


## Plot both KMT functions together in one plot. 
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(ResearchKMT, main="Q6 Kaplan-Meier-Turnbull survival function.")
plot(TreatmentKMT, main="Q7 Kaplan-Meier-Turnbull survival function.")


#################### Estimating WTP. Q6 then Q7 and exploring ordering and consequentiality


## The Q6 model
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity
                        + Q21Experts + Q22Education + Q23Employment
                        +  Q24AIncome | Q6Bid, data = Full_Long,dist="logistic")
summary(Research_SB) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_SB)
bootCI(Research_SB)

####### Testing ordering effects: 
Research_Order1 <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity 
                            + Q20Consequentiality
                            + Q21Experts +Q22Education+ Q23Employment
                            +  Q24AIncome | Q6Bid, data = Full_NormalOrder,dist="logistic")
summary(Research_Order1) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Research_Order2 <- sbchoice(Q6ResearchResponse ~  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity 
                            + Q20Consequentiality
                            + Q21Experts +Q22Education+ Q23Employment
                            +  Q24AIncome | Q6Bid, data = Full_OtherOrder,dist="logistic")
summary(Research_Order2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_Order1)
krCI(Research_Order2)


#################### Q7 WTP elicitation:


## Repeating the same as above but for Q7 the DBDC question:
Treatment_DB <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = Full_Long,dist="logistic")
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)

## Analysing only the firt bound if anchoring is an issue
Treatment1_SB <- sbchoice(Q7TreatmentResponse ~ Order  + Q1Gender + Q2Age + Q3Distance
                          + Q4Trips + Q16BP + Q18Charity
                          + Q21Experts + Q22Education + Q23Employment
                          +  Q24AIncome | Q7Bid ,data = Full_Long,dist="logistic")
summary(Treatment1_SB)
krCI(Treatment1_SB)

## Estimating Q7 by both orders using DBDC:
Treatment_DBOrder1 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid + Q7Bid2,data = Full_NormalOrder,dist="logistic")
summary(Treatment_DBOrder1)
krCI(Treatment_DBOrder1)
Treatment_DBOrder2 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid + Q7Bid2,data = Full_OtherOrder,dist="logistic")
summary(Treatment_DBOrder2)
krCI(Treatment_DBOrder2)

###### Still testing ordering effects using SBDC:
Treatment_SBOrder1 <- sbchoice(Q7TreatmentResponse  ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid,data = Full_NormalOrder,dist="logistic")
summary(Treatment_SBOrder1)
krCI(Treatment_SBOrder1)
Treatment_SBOrder2 <- sbchoice(Q7TreatmentResponse  ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q20Consequentiality
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid,data = Full_OtherOrder,dist="logistic")
summary(Treatment_SBOrder2)
krCI(Treatment_SBOrder2)


## Splitting CVM by consequentiality beliefs:
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


#################### Estimating QOV:


## In this section I directly compare the Full-bound Full-round Q6 and Q7 WTP valuations
### My suggestion is that this difference in treatment - research is akin to QOV.
Research <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                     + Q4Trips + Q16BP + Q18Charity
                     + Q21Experts + Q22Education + Q23Employment
                     +  Q24AIncome| Q6Bid, data = Full_Order1,dist="logistic")
Treatment <- sbchoice(Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance
                      + Q4Trips + Q16BP + Q18Charity
                      + Q21Experts + Q22Education + Q23Employment
                      +  Q24AIncome| Q7Bid, data = Full_Order2,dist="logistic")
summary(Research)
summary(Treatment)
Full_Order1 <- cbind(Full_Order1,
                      apply(Full_Order1, 
                            1, 
                            function(i) c(krCI(Research,individual = data.frame(Q1Gender = Full_Order1$Q1Gender[i], Q2Age = Full_Order1$Q2Age[i], Q3Distance = Full_Order1$Q3Distance[i],Q4Trips = Full_Order1$Q4Trips[i], Q16BP = Full_Order1$Q16BP[i],Q18Charity = Full_Order1$Q18Charity[i],Q21Experts = Full_Order1$Q21Experts[i],Q22Education = Full_Order1$Q22Education[i], Q23Employment = Full_Order1$Q23Employment[i], Q24AIncome = Full_Order1$Q24AIncome[i]))$out[4,1])))
colnames(Full_Order1)[55] <- "Q6WTP"
Full_Order2 <- cbind(Full_Order2,
                      apply(Full_Order2, 
                            1, 
                            function(i) c(krCI(Treatment,individual = data.frame(Q1Gender = Full_Order2$Q1Gender[i], Q2Age = Full_Order2$Q2Age[i], Q3Distance = Full_Order2$Q3Distance[i],Q4Trips = Full_Order2$Q4Trips[i], Q16BP = Full_Order2$Q16BP[i],Q18Charity = Full_Order2$Q18Charity[i],Q21Experts = Full_Order2$Q21Experts[i],Q22Education = Full_Order2$Q22Education[i], Q23Employment = Full_Order2$Q23Employment[i], Q24AIncome = Full_Order2$Q24AIncome[i]))$out[4,1])))
colnames(Full_Order2)[55] <- "Q7WTP"


## Plotting precaution  
library(ggplot2)
ggplot() + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male",
    `2` = "Other")))+
  geom_smooth(aes(x=Q24AIncome,y=Q6WTP,color="red"),data=Full_Order1,method="lm",se=F) +
  geom_smooth(aes(x=Q24AIncome,y=Q7WTP,color="blue"),data=Full_Order2,method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for Research", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Difference between Q6 and Q7 WTP")


## In this section I calculate each respondents QOV 
### This differs from above which elicits sample QOV by taking best-case sample WTP


## In this experimental code I fit WTP to an average respondent and then examine the difference in median WTP by ordering effects only. 
Research_WTP <- sbchoice(Q6ResearchResponse ~ Order +  Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome | Q6Bid, data = FullSurvey2,dist="logistic")
O1 <- krCI(Research_WTP,individual = data.frame(Order=0, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome)))
O2 <- krCI(Research_WTP,individual = data.frame(Order=1, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))
Treatment_DBWTP <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order +  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity
                            + Q21Experts + Q22Education + Q23Employment
                            +  Q24AIncome | Q7Bid + Q7Bid2,data = FullSurvey2,dist="logistic")
O1 <- krCI(Treatment_DBWTP,individual = data.frame(Order=0, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome)))
O2 <- krCI(Treatment_DBWTP,individual = data.frame(Order=1, Q1Gender = mean(FullSurvey2$Q1Gender), Q2Age = mean(FullSurvey2$Q2Age), Q3Distance = mean(FullSurvey2$Q3Distance),Q4Trips = mean(FullSurvey2$Q4Trips), Q16BP = mean(FullSurvey2$Q16BP),Q18Charity = mean(FullSurvey2$Q18Charity),Q21Experts = mean(FullSurvey2$Q21Experts),Q22Education = mean(FullSurvey2$Q22Education), Q23Employment = mean(FullSurvey2$Q23Employment), Q24AIncome = mean(FullSurvey2$Q24AIncome)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))
i=1
## With this function I append bootstrapped individual WTP to the original dataframe 
FullSurvey2 <- cbind(FullSurvey2,
                      apply(FullSurvey2, 
                            1, 
                            function(i) c(krCI(Research_WTP,individual = data.frame(Order= FullSurvey2$Order[i], Q1Gender = FullSurvey2$Q1Gender[i], Q2Age = FullSurvey2$Q2Age[i], Q3Distance = FullSurvey2$Q3Distance[i],Q4Trips = FullSurvey2$Q4Trips[i], Q16BP = FullSurvey2$Q16BP[i],Q18Charity = FullSurvey2$Q18Charity[i],Q21Experts = FullSurvey2$Q21Experts[i],Q22Education = FullSurvey2$Q22Education[i], Q23Employment = FullSurvey2$Q23Employment[i], Q24AIncome = FullSurvey2$Q24AIncome[i]))$out[4,1])))
colnames(FullSurvey2)[55] <- "Q6WTP"
FullSurvey2 <- cbind(FullSurvey2,
                      apply(FullSurvey2, 
                            1, 
                            function(i) c(krCI(Treatment_DBWTP,individual = data.frame(Order= FullSurvey2$Order[abs(i)], Q1Gender = FullSurvey2$Q1Gender[abs(i)], Q2Age = FullSurvey2$Q2Age[abs(i)], Q3Distance = FullSurvey2$Q3Distance[abs(i)],Q4Trips = FullSurvey2$Q4Trips[abs(i)], Q16BP = FullSurvey2$Q16BP[abs(i)],Q18Charity = FullSurvey2$Q18Charity[abs(i)],Q21Experts = FullSurvey2$Q21Experts[abs(i)],Q22Education = FullSurvey2$Q22Education[abs(i)], Q23Employment = FullSurvey2$Q23Employment[abs(i)], Q24AIncome = FullSurvey2$Q24AIncome[abs(i)]))$out[4,1])))
colnames(FullSurvey2)[56] <- "Q7WTP"

FullSurvey2 <- cbind(FullSurvey2,(FullSurvey2$Q7WTP - FullSurvey2$Q6WTP ))
colnames(FullSurvey2)[57] <- "Precaution"
### NOTE: Q6 is research (delaying, preserving, postponing), Q7 is tackling (immediately) 
ggplot(FullSurvey2) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male")))+
  geom_smooth(aes(x=Q24AIncome,y=Q6WTP,color="red"),method="lm",se=F) +
  geom_smooth(aes(x=Q24AIncome,y=Q7WTP,color="blue"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for Research", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Difference between Q6 and Q7 WTP")

## Full look at sample QOV:
summary(Full_Order1$Q6WTP - Full_Order2$Q7WTP)
## Individual QOV within the sample:
summary(FullSurvey2$Precaution)

