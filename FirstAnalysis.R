#Peter King
####################################################################################
############### Introduction: First 100 Data Analysis Script  ##########################
####################################################################################

############ TO DO:
# - Fix apollo code

############ Packages:
install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
install.packages("stargazer") ## To export to LaTeX code.
install.packages("dplyr")
library(dplyr)
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.

############ Setup and manipulation:
FirstSurvey <- data.frame(read.csv("FirstHundred.csv")) ## Imports from the excel file straight from the survey companies website.

FirstSurvey <- FirstSurvey[ -c(2,4,14,15,16,23,24,26,27,53,54,55,56,57,58,68,69)] ## Drop rows of no importance to the quantitative analysis, namely text responses.

colnames(FirstSurvey) <- c("ID","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
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

FirstSurvey2 <- FirstSurvey ## Create a backup of the FirstSurvey data

for (i in colnames(FirstSurvey)){
  if (is.factor(FirstSurvey[[i]]) == TRUE){
    FirstSurvey2[[i]] <- as.numeric(FirstSurvey[[i]])-1
  }
} 
## The loop above codes all the variables as numbers.
### However, it produces many mistakes which the following section deals with:


FirstSurvey2$Order[FirstSurvey2$Order == 2] <-0 ## The order dummy should be 0 for Q6 > Q7 and 1 for Q7 > Q6

## Here I update the age categories to take the midpoint of the brackets.
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 0] <- 21.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 1] <- 32.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 2] <- 47.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 3] <- 63
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 4] <- 71

## The loop got the distances ordered incorrectly and also didn't use midpoints.
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 2] <- 7
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 3] <- 6
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 1] <- 2
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 7] <- 3
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 6] <- 1
FirstSurvey2$Q3Distance <- FirstSurvey2$Q3Distance + 1
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 0] <- 1
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 1] <- 6.5
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 2] <- 15.5
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 3] <- 35
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 4] <- 50
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 5] <- mean(FirstSurvey2$Q3Distance) ## I replace the missing with the mean value although they could also be dropped or set to zero.

## Reordering the knowledge categories to reflect higher knowledge = higher value
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 4] <- 5
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 1] <- 6
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 6] <- 4
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 3] <- 1
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 0] <- 3

## Changing to be unsure > quite sure > very sure
FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 1] <-3
FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 0] <- 1
FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 3] <- 0

## Same here, the coder was confused over the ordering.
FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 1] <-3
FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 0] <- 1
FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 3] <- 0

## More reordering here
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 1] <- 3
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 2] <- 1
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 3] <- 2

## More reordering here
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 1] <- 3
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 2] <- 1
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 3] <- 2

## Previously skipped or missed questions were set = 2 now they're NAs for ease of merging the two columns.
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 2] <- NA
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 2] <- NA

## As respondents did EITHER the upper or lower question there should only be one column. This requires using mutate and coalesce to merge the lower and upper responses.
FirstSurvey2 <- mutate(Q7Bid2 = coalesce(FirstSurvey2$Q7Bid2Lower,FirstSurvey2$Q7Bid2Upper),.data = FirstSurvey2)
FirstSurvey2 <- mutate(Q7Response2 = coalesce(FirstSurvey2$Q7TreatmentUpperResponse,FirstSurvey2$Q7TreatmentLowerResponse),.data = FirstSurvey2)

## The following section codes all the attributes as their actual values.
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 1] <- 0.05
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 2] <- 0.5
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 0] <- 0.1
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 1] <- 0.4
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 2] <- 0.9
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 0] <- 0.1
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 1] <- 2.5
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 2] <- 5
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 0] <- 1

FirstSurvey2$Q10Performance[FirstSurvey2$Q10Performance == 1] <- 0.05
FirstSurvey2$Q10Performance[FirstSurvey2$Q10Performance == 0] <- 0.1
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 1] <- 0.4
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 2] <- 0.4
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 0] <- 0.1
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 1] <- 1
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 2] <- 2.5
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 0] <- 0.5

FirstSurvey2$Q11Performance[FirstSurvey2$Q11Performance == 1] <- 0.05
FirstSurvey2$Q11Performance[FirstSurvey2$Q11Performance == 0] <- 0.1
FirstSurvey2$Q11Emission[FirstSurvey2$Q11Emission == 1] <- 0.9
FirstSurvey2$Q11Emission[FirstSurvey2$Q11Emission == 0] <- 0.1
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 0] <- 0.5
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 1] <- 1
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 2] <- 2.5
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 3] <- 5

FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 1] <- 0.05
FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 2] <- 0.5
FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 0] <- 0.1
FirstSurvey2$Q12Emission[FirstSurvey2$Q12Emission == 1] <- 0.4
FirstSurvey2$Q12Emission[FirstSurvey2$Q12Emission == 0] <- 0.1
FirstSurvey2$Q12Price[FirstSurvey2$Q12Price == 1] <- 1
FirstSurvey2$Q12Price[FirstSurvey2$Q12Price == 2] <- 2.5
FirstSurvey2$Q12Price[FirstSurvey2$Q12Price == 0] <- 0.5
FirstSurvey2$Q12Price[FirstSurvey2$Q12Price == 3] <- 5


## Again certainty was confused so reordering here for higher number = more certain.
FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 1] <- 3
FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 0] <- 1
FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 3] <- 0

## The coder used zeros so changing that here by moving each value up one.
FirstSurvey2$Q13CurrentThreatToSelf <- FirstSurvey2$Q13CurrentThreatToSelf + 1
FirstSurvey2$Q14FutureThreatToSelf <- FirstSurvey2$Q14FutureThreatToSelf + 1
FirstSurvey2$Q15ThreatToEnvironment <- FirstSurvey2$Q15ThreatToEnvironment + 1

## More reordering of none > some > all
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 2] <- 3
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 0] <- 2
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 1] <- 0
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 3] <- 1

## More reordering here
FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 2] <- 3
FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 1] <- 2
FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 3] <- 1

## Same problem with Q5
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 4] <- 5
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 1] <- 4
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 2] <- 1
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 0] <- 2
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 3] <- 0
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 4] <- 3

## Reordering the consequentiality beliefs
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 0] <- 3
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 1] <- 0
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 2] <- 1
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 3] <- 2

## Belief in experts used a zero so just moving up one
FirstSurvey2$Q21Experts <- FirstSurvey2$Q21Experts +1

## Have to reorder education to GCSE > A level > Bachelor > Postgrad
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 4] <- 5
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 3] <- 4
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 1] <- 3
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 2] <- 1
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 0] <- 2
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 5] <- 0

## New order: NEET > Retired > Student > Part > Self > Full
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 0] <- 7
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 3] <- 0
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 6] <- 3
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 7] <- 6
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 2] <- 7
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 4] <- 2
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 7] <- 4

## Should be a dummy here with 2 for prefer not to say
FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 1] <- 3
FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 2] <- 1
FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 3] <- 2

## Changing the income to the midpoint of the brackets
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 8] <- 750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 4] <- 2750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 5] <- 3500.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 2] <- 1750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 1] <- 1250.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 3] <- 2250.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 6] <- 4500.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 9] <- mean(FirstSurvey2$Q24AIncome) ## Using mean-replacement but could also drop
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 7] <- 5000
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 0] <- 250.00

## Updating the final survey question
FirstSurvey2$Q25Understanding[FirstSurvey2$Q25Understanding == 1] <- 9
FirstSurvey2$Q25Understanding <- FirstSurvey2$Q25Understanding +1

## Adding an ID column which replaces the respondent category in the original dataset.
FirstSurvey2$ID <- seq.int(nrow(FirstSurvey2))

## Aim of the function is to express all variables in the FirstSurvey data as factors
for (i in colnames(FirstSurvey2)){
  if (is.factor(FirstSurvey[[i]]) == TRUE){
    contrasts(FirstSurvey2[,i]) <- contr.sum(nlevels(FirstSurvey2[,i]))
  }
} 

## Create a dataframe with the attributes and levels for Option A of the DCE. 
OptionA <- data.frame("Performance" =c(0,0,0), 
                        "Emission" =c(0,0,0),
                        "Price" =c(0,0,0))

library(dplyr) ## Essential library for data manipulation

## First is a dataframe that transforms the FirstSurvey data into an appropriate format for the estimation.
### The code repeats each row of the FirstSurvey data for each choice the respondent made. Therefore, each respondent now has four rows one for Q9, Q10, Q11, Q12
First <- cbind(slice(.data = FirstSurvey2,rep(1:n(), each = 4)),slice(.data = OptionA,rep(1:n(), times = 4)))

##  Creating a dataframe with all the levels that the Price attribute took for alternative B in the CE. 
DBPrice_B <- data.frame(Price_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Price"],
                                           FirstSurvey2["Q10Price"],
                                           FirstSurvey2["Q11Price"],
                                           FirstSurvey2["Q12Price"]),
                                times=1)))[,]))

##  Creating a dataframe with all the levels that the Performance attribute took for alternative B in the CE. 
DBPerformance_B <- data.frame(Performance_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Performance"],
                                           FirstSurvey2["Q10Performance"],
                                           FirstSurvey2["Q11Performance"],
                                           FirstSurvey2["Q12Performance"]),
                                times=1)))[,]))

##  Creating a dataframe with all the levels that the Emission attribute took for alternative B in the CE. 
DBEmission_B <- data.frame(Emission_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Emission"],
                                           FirstSurvey2["Q10Emission"],
                                           FirstSurvey2["Q11Emission"],
                                           FirstSurvey2["Q12Emission"]),
                                times=1)))[,]))

##  Creating a single column that contains all respondents CE choices. 
Choices <- data.frame(Choice = c(t(
  data.frame(rep(FirstSurvey2[,35:38], times=1)))[,]))

##  Chopping and reorganising the columns of the First dataframe into a new order which includes the attributes and their levels alongside all the choices in a single vector.
### The final argument creates a variable called TASK
First <- data.frame(First[,1:13],First[,18],DBPrice_B, DBPerformance_B, DBEmission_B,
                    First[,55:57],Choices, First[,19],First[,23],First[,27],
                    First[,31],First[,39:54],
                   rep(1:4,times=nrow(FirstSurvey2)))

##  Assigning column names for ease of analysis.
colnames(First) <- c("ID","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
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
Firsts <- First
First$av_A <- rep(1,nrow(First)) # Add a vector of ones to show that the alternative choice is always available to respondents.
First$av_B <- rep(1,nrow(First)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
First$Choice[First$Choice == 0] <- "A"  ## Necessary here to change numeric to string
First$Choice[First$Choice == 1] <- "B" ## The MFORMULA looks for _B or _A so choice must be SQ or ALT

# The data manipulation now moves into the CE specific manipulation.
library(mlogit) 

## Here the dataframe First is reshaped from wide to long format for use in the MLOGIT estimations.
First_Long <- mlogit.data(First, shape = "wide", choice = "Choice",
              varying = 15:20, sep = "_", id.var = "ID")

## To trim the sample according to:
###  Passing the Q8 dominated test scenario
First_Dominated <- First_Long[First_Long$Q8DominatedTest == 0]
###  Understanding the survey more than 5/10
First_Understanding <- First_Dominated[First_Dominated$Q25Understanding >= 5]
### Trimming by having certainty in their CE choices. 
First_Certain <- First_Dominated[First_Dominated$Q12CECertainty == 2]
###  Believing the survey responses to be consequential.
First_Cons <- First_Understanding[First_Understanding$Q20Consequentiality == 1]


##########################################################  
####### Descriptive Graphics
##########################################################  

## In this section I plot the percent of the sample choosing each alternative
### To do so I make a dataframe called "Acceptance" which calculates the percentage in each sample choosing each alternative.
Acceptance <- data.frame("First_Long" =c(length(First_Long$ID[(((First_Long$alt == "A") & (First_Long$Choice ==TRUE)))]),length(First_Long$ID[(((First_Long$alt == "A") & (First_Long$Choice ==FALSE)))])), 
                         "First_Cons" =c(length(First_Cons$ID[(((First_Cons$alt == "A") & (First_Cons$Choice ==TRUE)))]),length(First_Cons$ID[(((First_Cons$alt == "A") & (First_Cons$Choice ==FALSE)))])),
                         "First_Dominated" =c(length(First_Dominated$ID[(((First_Dominated$alt == "A") & (First_Dominated$Choice ==TRUE)))]),length(First_Dominated$ID[(((First_Dominated$alt == "A") & (First_Dominated$Choice ==FALSE)))])),
                         "First_Certain" =c(length(First_Certain$ID[(((First_Certain$alt == "A") & (First_Certain$Choice ==TRUE)))]),length(First_Certain$ID[(((First_Certain$alt == "A") & (First_Certain$Choice ==FALSE)))])))
Acceptance <- t(Acceptance) ## Transposed is easier to work with
Acceptance <- cbind(Acceptance,rowSums(Acceptance),deparse.level = 2) ## Add total responses
Acceptance <- cbind(Acceptance,100/Acceptance[,3]*Acceptance[,1],100/Acceptance[,3]*Acceptance[,2],c(1,2,3,4),deparse.level = 2) ## Add percentage accepting and rejecting A and number the questions.
colnames(Acceptance) <- c("A","B","Total","Acceptance","Rejected","Dataset")
Acceptance <- data.frame(Acceptance)

library(ggplot2)
## Plotting status quo acceptance by truncation strategy. 
P <- ggplot(data=Acceptance, aes(x=factor(Dataset), y=Acceptance)) +
  geom_bar(position="stack",stat="identity") + 
  scale_x_discrete(name="Data set",breaks = 1:4, labels=c(rownames(Acceptance)))+
  coord_cartesian(ylim=c(1,100))
p 


################# The same approach to above but better:
## Acc3 is acceptance of each option in the first sample
Acc3 <- rbind(t(data.frame("Q9"=c(c(length(First_Long$ID[(( (First_Long$Task == 1) & (First_Long$alt == "A") & (First_Long$Choice ==TRUE)) )] )),c(length(First_Long$ID[(( (First_Long$Task == 1) & (First_Long$alt == "A") & (First_Long$Choice ==FALSE)) )] ))))),
              t(data.frame("Q10"=c(c(length(First_Long$ID[(( (First_Long$Task == 2) & (First_Long$alt == "A") & (First_Long$Choice ==TRUE)) )] )),c(length(First_Long$ID[(( (First_Long$Task == 2) & (First_Long$alt == "A") & (First_Long$Choice ==FALSE)) )] ))))),
              t(data.frame("Q11"=c(c(length(First_Long$ID[(( (First_Long$Task == 3) & (First_Long$alt == "A") & (First_Long$Choice ==TRUE)) )] )),c(length(First_Long$ID[(( (First_Long$Task == 3) & (First_Long$alt == "A") & (First_Long$Choice ==FALSE)) )] ))))),
              t(data.frame("Q12"=c(c(length(First_Long$ID[(( (First_Long$Task == 4) & (First_Long$alt == "A") & (First_Long$Choice ==TRUE)) )] )),c(length(First_Long$ID[(( (First_Long$Task == 4) & (First_Long$alt == "A") & (First_Long$Choice ==FALSE)) )] ))))))
Acc3 <- cbind(Acc3,c(1,2,3,4),round(Acc3[,1:2]/sum(Acc3[1,1:2])*100,3))
colnames(Acc3) <- c("A","B","Question","APercent","BPercent")
Acc3 <- data.frame(Acc3)

## Acc4 is acceptance of each option in the sample eliminating those failing the dominance test
Acc4 <- rbind(t(data.frame("Q9"=c(c(length(First_Dominated$ID[(( (First_Dominated$Task == 1) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==TRUE)) )] )),c(length(First_Dominated$ID[(( (First_Dominated$Task == 1) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==FALSE)) )] ))))),
              t(data.frame("Q10"=c(c(length(First_Dominated$ID[(( (First_Dominated$Task == 2) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==TRUE)) )] )),c(length(First_Dominated$ID[(( (First_Dominated$Task == 2) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==FALSE)) )] ))))),
              t(data.frame("Q11"=c(c(length(First_Dominated$ID[(( (First_Dominated$Task == 3) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==TRUE)) )] )),c(length(First_Dominated$ID[(( (First_Dominated$Task == 3) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==FALSE)) )] ))))),
              t(data.frame("Q12"=c(c(length(First_Dominated$ID[(( (First_Dominated$Task == 4) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==TRUE)) )] )),c(length(First_Dominated$ID[(( (First_Dominated$Task == 4) & (First_Dominated$alt == "A") & (First_Dominated$Choice ==FALSE)) )] ))))))
Acc4 <- cbind(Acc4,c(1,2,3,4),round(Acc4[,1:2]/sum(Acc4[1,1:2])*100,3))
colnames(Acc4) <- c("A","B","Question","APercent","BPercent")
Acc4 <- data.frame(Acc4)
Acc4

## Acc5 is acceptance of each option in the sample relying on certainty
Acc5 <- rbind(t(data.frame("Q9"=c(c(length(First_Certain$ID[(( (First_Certain$Task == 1) & (First_Certain$alt == "A") & (First_Certain$Choice ==TRUE)) )] )),c(length(First_Certain$ID[(( (First_Certain$Task == 1) & (First_Certain$alt == "A") & (First_Certain$Choice ==FALSE)) )] ))))),
              t(data.frame("Q10"=c(c(length(First_Certain$ID[(( (First_Certain$Task == 2) & (First_Certain$alt == "A") & (First_Certain$Choice ==TRUE)) )] )),c(length(First_Certain$ID[(( (First_Certain$Task == 2) & (First_Certain$alt == "A") & (First_Certain$Choice ==FALSE)) )] ))))),
              t(data.frame("Q11"=c(c(length(First_Certain$ID[(( (First_Certain$Task == 3) & (First_Certain$alt == "A") & (First_Certain$Choice ==TRUE)) )] )),c(length(First_Certain$ID[(( (First_Certain$Task == 3) & (First_Certain$alt == "A") & (First_Certain$Choice ==FALSE)) )] ))))),
              t(data.frame("Q12"=c(c(length(First_Certain$ID[(( (First_Certain$Task == 4) & (First_Certain$alt == "A") & (First_Certain$Choice ==TRUE)) )] )),c(length(First_Certain$ID[(( (First_Certain$Task == 4) & (First_Certain$alt == "A") & (First_Certain$Choice ==FALSE)) )] ))))))
Acc5 <- cbind(Acc5,c(1,2,3,4),round(Acc5[,1:2]/sum(Acc5[1,1:2])*100,3))
colnames(Acc5) <- c("A","B","Question","APercent","BPercent")
Acc5 <- data.frame(Acc5)
Acc5

## Acc6 is acceptance of each option in the consequential sample
Acc6 <- rbind(t(data.frame("Q9"=c(c(length(First_Cons$ID[(( (First_Cons$Task == 1) & (First_Cons$alt == "A") & (First_Cons$Choice ==TRUE)) )] )),c(length(First_Cons$ID[(( (First_Cons$Task == 1) & (First_Cons$alt == "A") & (First_Cons$Choice ==FALSE)) )] ))))),
              t(data.frame("Q10"=c(c(length(First_Cons$ID[(( (First_Cons$Task == 2) & (First_Cons$alt == "A") & (First_Cons$Choice ==TRUE)) )] )),c(length(First_Cons$ID[(( (First_Cons$Task == 2) & (First_Cons$alt == "A") & (First_Cons$Choice ==FALSE)) )] ))))),
              t(data.frame("Q11"=c(c(length(First_Cons$ID[(( (First_Cons$Task == 3) & (First_Cons$alt == "A") & (First_Cons$Choice ==TRUE)) )] )),c(length(First_Cons$ID[(( (First_Cons$Task == 3) & (First_Cons$alt == "A") & (First_Cons$Choice ==FALSE)) )] ))))),
              t(data.frame("Q12"=c(c(length(First_Cons$ID[(( (First_Cons$Task == 4) & (First_Cons$alt == "A") & (First_Cons$Choice ==TRUE)) )] )),c(length(First_Cons$ID[(( (First_Cons$Task == 4) & (First_Cons$alt == "A") & (First_Cons$Choice ==FALSE)) )] ))))))
Acc6 <- cbind(Acc6,c(1,2,3,4),round(Acc6[,1:2]/sum(Acc6[1,1:2])*100,3))
colnames(Acc6) <- c("A","B","Question","APercent","BPercent")
Acc6 <- data.frame(Acc6)
Acc6


## Combining line plots of acceptance rates over questions by dataset
### Note no major differences by question or data. 
ggplot()+
  geom_line(aes(x=Question,y=APercent,color="red"),data=Acc3)+
  geom_line(aes(x=Question,y=APercent,color="blue"),data=Acc4)+
  geom_line(aes(x=Question,y=APercent,color="green"),data=Acc5)+
  geom_line(aes(x=Question,y=APercent,color="orange"),data=Acc6)+
  scale_x_continuous(name="Question",breaks = 1:4, 
                     labels=c(rownames(Acc3)))+
  scale_y_continuous(name="Percent choosing Option A",
                     breaks=waiver(),limits = c(25,75),
                     n.breaks = 10, labels = function(x) paste0(x, "%"))+
  scale_color_discrete(name = "Lines", 
                       labels = c("Full sample", "Dominated","Certainty","Consequentiality"))+
  ggtitle("Percentage choosing the status quo by question and truncation strategy.")

##  
Sames <-data.frame(c(c(length(FirstSurvey2$ID[(( (FirstSurvey2$Q9Choice == 1) & (FirstSurvey2$Q10Choice == 1) & (FirstSurvey2$Q11Choice == 1) & (FirstSurvey2$Q12Choice == 1) ))])),c(length(FirstSurvey2$ID[(( (FirstSurvey2$Q9Choice == 0) & (FirstSurvey2$Q10Choice == 0) & (FirstSurvey2$Q11Choice == 0) & (FirstSurvey2$Q12Choice == 0) ))]))))
colnames(Sames) <- c("Always same")
rownames(Sames) <- c("A","B")

################ 
 
First_Cons <- data.frame(First_Cons) ## Change into dataframe format 

First_Long <- data.frame(First_Long) ## Same here - ggplot2 throws a fit without it.

## Here the Blue-Planet question is manipulated to be either of two values
First_Cons$Q14BP[First_Cons$Q16BP == 0] <- "Not watched" 
First_Cons$Q14BP[First_Cons$Q16BP == 1] <- "Watched"
First_Cons$Q14BP[First_Cons$Q16BP == 2] <- "Watched"

## Categorising charity involvement.
First_Cons$Q18Charity[First_Cons$Q18Charity == 0] <- "No involvement"
First_Cons$Q18Charity[First_Cons$Q18Charity == 1] <- "Donated or joined"

## Rephrasing consequentiality into strings for use in graphics. 
### The strings used here and above are changed back later for econometric analysis. 
First_Long$Q20Consequentiality[First_Long$Q20Consequentiality == 0] <- "Inconsequential"
First_Long$Q20Consequentiality[First_Long$Q20Consequentiality == 1] <- "Consequential"


## Using these two packages for graphics.
library(scales)
library(ggplot2) 

## Plot 1 (of 2):
P1 <- ggplot(First_Long, aes(Q3Distance,Q4Trips)) + 
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
P2 <- ggplot(First_Long, aes(Q3Distance,Q4Trips)) + 
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


## Demand Curve: Plotting choice versus price levels 
A1 <- ggplot(Firsts, aes(Choice,Price_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Demand curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Price attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))


## Emission Curve: Plotting choice versus emission levels 
A2 <- ggplot(Firsts, aes(Choice,Emission_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Emission curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Emission attribute")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))

## Performance Curve: Plotting choice versus performance levels 
A3 <- ggplot(Firsts, aes(Choice,Performance_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Performance curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Performance attribute")+
  scale_x_continuous(breaks = 0:1,labels=c(0,1))

grid.arrange(A1, A2, A3)
##########################################################  
####### CE
##########################################################  


## Have to do the manipulation section again to ignore the changes made in the graphics section
library(mlogit) #Already have package installed
First_Long <- mlogit.data(First, shape = "wide", choice = "Choice",
                         varying = 15:20, sep = "_", id.var = "ID")

First_Long$Performance[First_Long$Performance == 0.05] <- -5
First_Long$Performance[First_Long$Performance == 0.10] <- -10
First_Long$Performance[First_Long$Performance == 0.50] <- -50
First_Long$Emission[First_Long$Emission == 0.1] <- 10
First_Long$Emission[First_Long$Emission == 0.9] <- 90 
First_Long$Emission[First_Long$Emission == 0.4] <- 40 

## To trim the sample: 
First_Dominated <- First_Long[First_Long$Q8DominatedTest == 0]
# First_Understanding <- First_Dominated[First_Dominated$Q25Understanding >= 5]
First_Certain <- First_Dominated[First_Dominated$Q12CECertainty == 2]
First_Cons <- First_Certain[First_Certain$Q20Consequentiality == 1]


######################## Estimation section:


## A very basic MNL model to test whether the method works 
Base_MNL <- mlogit(Choice ~  Price + Performance + Emission, 
                   First_Long,
                   alt.subset = c("A","B"),reflevel = "A") 
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

## The full MNL with all covariates:
Pilot_MNL <- mlogit(Choice ~ Price + Performance + Emission | 
                      Order + Task + Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q16BP + Q18Charity 
                    + Q20Consequentiality
                    + Q21Experts +Q22Education+ Q23Employment
                    +  Q24AIncome, 
                    First_Long, alt.subset = c("A", "B"), 
                    reflevel = "A") 
summary(Pilot_MNL) ## Summarises the MNL output

## I then estimate the same specification but with a MIXED LOGIT specification.
MXLFull <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  First_Long, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bhhh",panel=TRUE,seed=123)
summary(MXLFull)

## Same as above but with only certain responses
MXLFullD <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Order + Task + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  First_Certain, rpar=c(Price="n"),
  R=1000,correlation = FALSE,
  reflevel="A",halton=NA,method="bhhh",panel=TRUE,seed=123)
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

# Long: AIC = 561.1715, Dist = 61%, R^2 = 0.19, LLH= -263.59
#     3.29, -6.76 
# Dominated: AIC = 408.9245, Dist = 73%, R^2 = 0.19, LLH= -187
#     1.05, -3.386 
# Cons: AIC = 103.8874, Dist = 54%, R^2 = 0.33, LLH= -34.944
#     0.0026, -9.189 
# Certain: AIC = 200.5288, Dist = 724%, R^2 = 0.20, LLH= -83.26
#     0.44, -2.449 

## Can use AIC and BIC to compare model fits:
AIC(Pilot_MNL) ## 215.1842
AIC(MXLFull)   ## 561.1319
AIC(MXLFullD)   ## 200.5288
 
  
## Clustering and bootstrapping:
library(clusterSEs)
CBSM <- cluster.bs.mlogit(MXLFullD, First_Certain, ~ ID, boot.reps=100,seed = 123)

WTPbs <- data.frame("Emission" = c(CBSM$ci[4,1]/CBSM$ci[2,1],CBSM$ci[4,2]/CBSM$ci[2,2]),
                    "Performance"=c(CBSM$ci[3,1]/CBSM$ci[2,1],CBSM$ci[3,2]/CBSM$ci[2,2]))
WTPbs <- t(WTPbs)
WTPbs <- -1* WTPbs
WTPs <- cbind(WTPbs[,1],WTPs[,2],WTPbs[,2])
colnames(WTPs) <- c("Lower","Mean","Upper")
round(WTPs,3)

library(stargazer)
stargazer(summary(MXLFull)$CoefTable, title = "MXLFull", align = TRUE,report="p*")


## Calculating consumer surplus:
First_Understanding_CS1 <- First_Long
First_Understanding_CS1$Price <- First_Understanding_CS1$Price * 1.1
First_Understanding_CS1$Emission <- First_Understanding_CS1$Emission * 0.9
Va1 <- logsum(MXLFullD,data=First_Long)
Va0 <- logsum(MXLFullD,data = First_Understanding_CS1)
surplus <- - (Va1 - Va0) / coef(MXLFullD)["Price"]
summary(surplus)


##############  GMNL is an alternative to MLOGIT
library(gmnl)

## Replicating the MNL
MNL_GM <- gmnl(  Choice ~ Price + Performance + Emission | 
                   Order + Task + Q1Gender + Q2Age + Q3Distance
                 + Q4Trips + Q16BP + Q18Charity 
                 + Q20Consequentiality
                 + Q21Experts +Q22Education+ Q23Employment
                 +  Q24AIncome,
                 data = First_Long,
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
                        +  Q24AIncome, data = First_Long,
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
              data = First_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) ## 547.1257
BIC(LC_GM) ## 618.08

## Three class model:
LC_GM3 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1+  Q1Gender + Q2Age + Q3Distance
               + Q4Trips + Q16BP + Q18Charity
               + Q21Experts +Q22Education+ Q23Employment
               +  Q24AIncome,
              data = First_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 3)
summary(LC_GM3)
AIC(LC_GM3) # 547.4673
BIC(LC_GM3) # 676.8547


## Function from https://rpubs.com/msarrias1986/335556 which calculates the share of the sample in each class
exp(coef(LC_GM5)["(class)2"]) / (exp(0) + exp(coef(LC_GM5)["(class)2"]))
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

## Reports class-shares:
shares(LC_GM)
shares(LC_GM3)

## LCM class-specific WTP:
-1* (coef(LC_GM)["class.1.Performance"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.1.Emission"]/coef(LC_GM)["class.1.Price"])
-1* (coef(LC_GM)["class.2.Performance"]/coef(LC_GM)["class.2.Price"])
-1* (coef(LC_GM)["class.2.Emission"]/coef(LC_GM)["class.2.Price"])



##########################################################  
####### CVM
##########################################################  



##########################################################################
############### Probits with the AOD package                           

## Install required packages to estimate Probit and reqport the marginal effects 
install.packages("aod")
install.packages("mfx")
library(mfx)
require(aod)

## Model for Q6:
CVMProbit <-glm(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + 
                  Q3Distance + Q4Trips + Q16BP + 
                  Q18Charity + Q21Experts +Q22Education+ 
                  Q23Employment +  Q24AIncome, family = binomial(link = "probit"),data = FirstSurvey2)
summary(CVMProbit) ## Report the model
confint(CVMProbit) ## Estimate confidence interval -default 95%
wald.test(b = coef(CVMProbit), Sigma = vcov(CVMProbit), Terms=2) ## Attempt a Wald-test
stargazer(CVMProbit, title = "CVMProbit", align = TRUE,report="p*") ## Export results to LaTeX code

CVMME <- probitmfx(formula = Q6ResearchResponse ~ Order + Q1Gender + Q2Age + 
                     Q3Distance + Q4Trips + Q16BP + 
                     Q18Charity + Q21Experts +Q22Education+ 
                     Q23Employment +  Q24AIncome,data = FirstSurvey2,robust = TRUE)
CVMME ## Report Marginal Effects


## Model for Q7:
QOVProbit <-glm(Q7TreatmentResponse ~ Order + Q1Gender + Q2Age + 
                  Q3Distance + Q4Trips + Q16BP + 
                  Q18Charity + Q21Experts +Q22Education+ 
                  Q23Employment +  Q24AIncome, 
                family = binomial(link = "probit"),data = FirstSurvey2)
summary(QOVProbit)
QOVME <- probitmfx(formula = Q7TreatmentResponse ~ Order + Q1Gender + Q2Age + 
                     Q3Distance + Q4Trips + Q16BP + 
                     Q18Charity + Q21Experts +Q22Education+ 
                     Q23Employment +  Q24AIncome,data = FirstSurvey2,robust = TRUE)
confint(QOVProbit)
wald.test(b = coef(QOVProbit), Sigma = vcov(QOVProbit), Terms=2)
## All the same commands as the Q6 models

##########################################################################
############### CVM: DChoice package                  #####################

## Have to do some R magic here to install a package not on CRAN
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")
install.packages("interval")
install.packages("DCchoice")
library(DCchoice)


## Creating new dataframes depending on ordering or consequentiality. 
First_NormalOrder <-First_Long[First_Long$Order == 0]
First_OtherOrder <-First_Long[First_Long$Order == 1]
First_Consequential <-First_Long[First_Long$Q20Consequentiality == 1]
First_Inconsequential <-First_Long[First_Long$Q20Consequentiality != 1]

FirstSurvey2 <- data.frame(FirstSurvey2)
First_OtherOrders <- FirstSurvey2[ (FirstSurvey2$Order ==1) ,]


## Here I construct dataframes which calculate acceptance rates for each CVM question by ordering 
Q6 <- t(data.frame("Normal" = c(length(FirstSurvey2$Q6ResearchResponse[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q6ResearchResponse ==0)]),length(FirstSurvey2$Q6ResearchResponse[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q6ResearchResponse ==1)])),
           "Alternate" = c(length(FirstSurvey2$Q6ResearchResponse[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q6ResearchResponse ==0)]),length(FirstSurvey2$Q6ResearchResponse[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q6ResearchResponse ==1)]))))
Q6 <- data.frame(cbind(Q6,Q6[,2]/sum(Q6[2,]),c(1,2)))
colnames(Q6) <- c("Reject","Accept","Percentage","Order")

Q7 <- t(data.frame("Normal" = c(length(FirstSurvey2$Q7TreatmentResponse[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q7TreatmentResponse ==0)]),length(FirstSurvey2$Q7TreatmentResponse[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q7TreatmentResponse ==1)])),
                   "Alternate" = c(length(FirstSurvey2$Q7TreatmentResponse[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q7TreatmentResponse ==0)]),length(FirstSurvey2$Q7TreatmentResponse[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q7TreatmentResponse ==1)]))))
Q7 <- data.frame(cbind(Q7,Q7[,2]/sum(Q7[2,]),c(1,2)))
colnames(Q7) <- c("Reject","Accept","Percentage","Order")

Q7b <- t(data.frame("Normal" = c(length(FirstSurvey2$Q7Response2[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q7Response2 ==0)]),length(FirstSurvey2$Q7Response2[(FirstSurvey2$Order ==0) & (FirstSurvey2$Q7Response2 ==1)])),
                   "Alternate" = c(length(FirstSurvey2$Q7Response2[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q7Response2 ==0)]),length(FirstSurvey2$Q7Response2[(FirstSurvey2$Order ==1) & (FirstSurvey2$Q7Response2 ==1)]))))
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
Ordering <- data.frame(rbind("Normal order"=data.frame("Accepting higher bid"=c(length(unique(First_Long$ID[ (First_Long$Q7Response2 == 0) & (First_Long$Q7Bid > First_Long$Q7Bid2) & (First_Long$Order == 0) ]))),
           "Rejecting higher bid" = c(length(unique(First_Long$ID[ (First_Long$Q7Response2 == 1) & (First_Long$Q7Bid > First_Long$Q7Bid2) & (First_Long$Order == 0) ])))),
      "Reversed"=data.frame("Accepting higher bid"=c(length(unique(First_Long$ID[ (First_Long$Q7Response2 == 0) & (First_Long$Q7Bid > First_Long$Q7Bid2) & (First_Long$Order == 1) ]))),
           "Rejecting higher bid" = c(length(unique(First_Long$ID[ (First_Long$Q7Response2 == 1) & (First_Long$Q7Bid > First_Long$Q7Bid2) & (First_Long$Order == 1) ]))))))


## The Q6 model: actually much better than the AOD approach above.
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity
                        + Q21Experts + Q22Education + Q23Employment
                        +  Q24AIncome | Q6Bid, data = First_Long,dist="logistic")
summary(Research_SB) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
### NOTE: The moodel CURRENTLY only produces realistic WTP when the distribution is "logistic" not the default "log-logistic"  
## Two methods to estimate confidence intervals:
krCI(Research_SB)
bootCI(Research_SB)
Research_Order1 <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity 
                        + Q20Consequentiality
                        + Q21Experts +Q22Education+ Q23Employment
                        +  Q24AIncome | Q6Bid, data = First_NormalOrder,dist="logistic")
summary(Research_Order1) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Research_Order2 <- sbchoice(Q6ResearchResponse ~  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity 
                            + Q20Consequentiality
                            + Q21Experts +Q22Education+ Q23Employment
                            +  Q24AIncome | Q6Bid, data = First_OtherOrder,dist="logistic")
summary(Research_Order2) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
krCI(Research_Order1)
bootCI(Research_Order1)
krCI(Research_Order2)
bootCI(Research_Order2)

## In this experimental code I fit WTP to an average respondent and then examine the difference in median WTP by ordering effects only. 
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity
                        + Q21Experts + Q22Education + Q23Employment
                        +  Q24AIncome| Q6Bid, data = FirstSurvey2,dist="logistic")
O1 <- bootCI(Research_SB,individual = data.frame(Order=0, Q1Gender = mean(FirstSurvey2$Q1Gender), Q2Age = mean(FirstSurvey2$Q2Age), Q3Distance = mean(FirstSurvey2$Q3Distance),Q4Trips = mean(FirstSurvey2$Q4Trips), Q16BP = mean(FirstSurvey2$Q16BP),Q18Charity = mean(FirstSurvey2$Q18Charity),Q21Experts = mean(FirstSurvey2$Q21Experts),Q22Education = mean(FirstSurvey2$Q22Education), Q23Employment = mean(FirstSurvey2$Q23Employment), Q24AIncome = mean(FirstSurvey2$Q24AIncome)))
O2 <- bootCI(Research_SB,individual = data.frame(Order=1, Q1Gender = mean(FirstSurvey2$Q1Gender), Q2Age = mean(FirstSurvey2$Q2Age), Q3Distance = mean(FirstSurvey2$Q3Distance),Q4Trips = mean(FirstSurvey2$Q4Trips), Q16BP = mean(FirstSurvey2$Q16BP),Q18Charity = mean(FirstSurvey2$Q18Charity),Q21Experts = mean(FirstSurvey2$Q21Experts),Q22Education = mean(FirstSurvey2$Q22Education), Q23Employment = mean(FirstSurvey2$Q23Employment), Q24AIncome = mean(FirstSurvey2$Q24AIncome)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))

## With this function I append bootstrapped individual WTP to the original dataframe 
FirstSurvey2 <- cbind(FirstSurvey2,
      apply(FirstSurvey2, 
            1, 
            function(i) c(krCI(Research_SB,individual = data.frame(Order= FirstSurvey2$Order[i], Q1Gender = FirstSurvey2$Q1Gender[i], Q2Age = FirstSurvey2$Q2Age[i], Q3Distance = FirstSurvey2$Q3Distance[i],Q4Trips = FirstSurvey2$Q4Trips[i], Q16BP = FirstSurvey2$Q16BP[i],Q18Charity = FirstSurvey2$Q18Charity[i],Q21Experts = FirstSurvey2$Q21Experts[i],Q22Education = FirstSurvey2$Q22Education[i], Q23Employment = FirstSurvey2$Q23Employment[i], Q24AIncome = FirstSurvey2$Q24AIncome[i]))$out[4,1])))
colnames(FirstSurvey2)[55] <- "Q6WTP"


## Repeating the same as above but for Q7 the DBDC question:
Treatment_DB <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order + Task + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = First_Certain,dist="logistic")
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)

## Analysing only the firt bound if anchoring is an issue
Treatment1_SB <- sbchoice(Q7TreatmentResponse ~ Order  + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome | Q7Bid ,data = FirstSurvey2,dist="logistic")
summary(Treatment1_SB)
krCI(Treatment1_SB)

## Here I include the first bound bid level to test whether it affects WTP and the answer is it absolutely ruins it.
Treatment2_SB <- sbchoice(Q7Response2 ~ Order + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome +Q7Bid  | Q7Bid2,data = FirstSurvey2,dist="logistic")
summary(Treatment2_SB)
krCI(Treatment2_SB)


## Here I estimate Q7 WTP when Q7 first bound was the first valuation task respondents did. 
Treatment_SB <- sbchoice(Q7TreatmentResponse ~ Q1Gender | Q7Bid, data = First_OtherOrders,dist="logistic")
summary(Treatment_SB)
krCI(Treatment_SB)


## Splitting CVM by ordering of questions.
Treatment_DBOrder1 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity 
                         + Q20Consequentiality
                         + Q21Experts +Q22Education+ Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = First_NormalOrder,dist="logistic")
summary(Treatment_DBOrder1)
krCI(Treatment_DBOrder1)
bootCI(Treatment_DBOrder1)
Treatment_DBOrder2 <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity 
                         + Q20Consequentiality
                         + Q21Experts +Q22Education+ Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = First_OtherOrder,dist="logistic")
summary(Treatment_DBOrder2)
krCI(Treatment_DBOrder2)
bootCI(Treatment_DBOrder2)


## Splitting CVM by consequentiality beliefs:
Research_Consequential <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance
                                   + Q4Trips + Q16BP + Q18Charity 
                                   + Q21Experts +Q22Education+ Q23Employment
                                   +  Q24AIncome | Q6Bid, data = First_Consequential,dist="logistic")
summary(Research_Consequential) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Research_Inconsequential <- sbchoice(Q6ResearchResponse ~  Q1Gender + Q2Age + Q3Distance
                                     + Q4Trips + Q16BP + Q18Charity
                                     + Q21Experts +Q22Education+ Q23Employment
                                     +  Q24AIncome | Q6Bid, data = First_Inconsequential,dist="logistic")
summary(Research_Inconsequential) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
bootCI(Research_Consequential)
bootCI(Research_Inconsequential)
Treatment_Consequential <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity 
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid + Q7Bid2,data = First_Consequential,dist="logistic")
summary(Treatment_Consequential)
Treatment_Inconsequential <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance
                               + Q4Trips + Q16BP + Q18Charity
                               + Q21Experts +Q22Education+ Q23Employment
                               +  Q24AIncome | Q7Bid + Q7Bid2,data = First_Inconsequential,dist="logistic")
summary(Treatment_Inconsequential)
bootCI(Treatment_Consequential)
bootCI(Treatment_Inconsequential)



## In this experimental code I fit WTP to an average respondent and then examine the difference in median WTP by ordering effects only. 
Treatment_DBWTP <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order +  Q1Gender + Q2Age + Q3Distance
                            + Q4Trips + Q16BP + Q18Charity
                            + Q21Experts + Q22Education + Q23Employment
                            +  Q24AIncome | Q7Bid + Q7Bid2,data = FirstSurvey2,dist="logistic")
O1 <- bootCI(Treatment_DBWTP,individual = data.frame(Order=0, Q1Gender = mean(FirstSurvey2$Q1Gender), Q2Age = mean(FirstSurvey2$Q2Age), Q3Distance = mean(FirstSurvey2$Q3Distance),Q4Trips = mean(FirstSurvey2$Q4Trips), Q16BP = mean(FirstSurvey2$Q16BP),Q18Charity = mean(FirstSurvey2$Q18Charity),Q21Experts = mean(FirstSurvey2$Q21Experts),Q22Education = mean(FirstSurvey2$Q22Education), Q23Employment = mean(FirstSurvey2$Q23Employment), Q24AIncome = mean(FirstSurvey2$Q24AIncome)))
O2 <- bootCI(Treatment_DBWTP,individual = data.frame(Order=1, Q1Gender = mean(FirstSurvey2$Q1Gender), Q2Age = mean(FirstSurvey2$Q2Age), Q3Distance = mean(FirstSurvey2$Q3Distance),Q4Trips = mean(FirstSurvey2$Q4Trips), Q16BP = mean(FirstSurvey2$Q16BP),Q18Charity = mean(FirstSurvey2$Q18Charity),Q21Experts = mean(FirstSurvey2$Q21Experts),Q22Education = mean(FirstSurvey2$Q22Education), Q23Employment = mean(FirstSurvey2$Q23Employment), Q24AIncome = mean(FirstSurvey2$Q24AIncome)))
data.frame("Order 1" = c(median(O1$medWTP)), "Order 2" = c(median(O2$medWTP)),"Ordering effect" = c(abs(median(O1$medWTP)-median(O2$medWTP))))

## With this function I append bootstrapped individual WTP to the original dataframe 
FirstSurvey2 <- cbind(FirstSurvey2,
                      apply(FirstSurvey2, 
                            1, 
                            function(i) c(krCI(Treatment_DBWTP,individual = data.frame(Order= FirstSurvey2$Order[i], Q1Gender = FirstSurvey2$Q1Gender[i], Q2Age = FirstSurvey2$Q2Age[i], Q3Distance = FirstSurvey2$Q3Distance[i],Q4Trips = FirstSurvey2$Q4Trips[i], Q16BP = FirstSurvey2$Q16BP[i],Q18Charity = FirstSurvey2$Q18Charity[i],Q21Experts = FirstSurvey2$Q21Experts[i],Q22Education = FirstSurvey2$Q22Education[i], Q23Employment = FirstSurvey2$Q23Employment[i], Q24AIncome = FirstSurvey2$Q24AIncome[i]))$out[4,1])))
colnames(FirstSurvey2)[56] <- "Q7WTP"

FirstSurvey2 <- cbind(FirstSurvey2,(FirstSurvey2$Q7WTP - FirstSurvey2$Q6WTP ))
colnames(FirstSurvey2)[57] <- "Precaution"
### NOTE: Q6 is research (delaying, preserving, postponing), Q7 is tackling (immediately) 

## Plotting precaution  
library(ggplot2)
ggplot(FirstSurvey2) + 
 facet_wrap( ~Q1Gender)+
 geom_smooth(aes(x=Q24AIncome,y=Q6WTP,color="red"),method="loess",se=F) +
 geom_smooth(aes(x=Q24AIncome,y=Q7WTP,color="blue"),method="loess",se=F) +
 scale_color_discrete(name = "Lines", 
                          labels = c("WTP for Research", "WTP for treatment"))+
     ggtitle("Relationship between precaution and income") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Difference between Q6 and Q7 WTP")



## This section deals with Q6 and Q7 respectively but uses a non-parametric Kaplan-Meier-Turnbull survival function:
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = FirstSurvey2)
summary(ResearchKMT)
plot(ResearchKMT)

## Reporting the KMT for Q7.
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = FirstSurvey2)
summary(TreatmentKMT)
plot(TreatmentKMT)


## Plot both KMT functions together in one plot. 
layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), widths=c(1,1), heights=c(4,4))
plot(ResearchKMT, main="Q6 Kaplan-Meier-Turnbull survival function.")
plot(TreatmentKMT, main="Q7 Kaplan-Meier-Turnbull survival function.")


## Curiosity code - checking whether the CVM estimation works for the CE and it does.
ChoiceKMT <- turnbull.sb(formula = Choice ~ Price_B,data = Firsts)
summary(ChoiceKMT)
plot(ChoiceKMT)
Choice_SB <- sbchoice(Choice ~ Order + Task + Q1Gender + Q2Age + Q3Distance
                      + Q4Trips + Q16BP + Q18Charity
                      + Q21Experts + Q22Education + Q23Employment
                      +  Q24AIncome | Price_B, data = Firsts,dist="logistic")
summary(Choice_SB) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


##########################################################  
## The following code is experimental:
##########################################################  


#############################################################################
## APOLLO: MNL
#############################################################################

library(apollo)
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MY_Apollo_example",
  modelDescr ="Simple MNL model on my Pilot data",
  indivID    ="ID",
  HB = FALSE,
  mixing=FALSE
)

Test_Apollo <- data.frame(Firsts$ID,Firsts$Task, Firsts$Q1Gender,
                          Firsts$Q2Age,Firsts$Q3Distance,Firsts$Q4Trips,
                          Firsts$Q16BP,Firsts$Q18Charity,
                          Firsts$Q20Consequentiality,Firsts$Q21Experts,
                          Firsts$Q22Education,First$Q23Employment,
                          Firsts$Q25Understanding,Firsts[,15:20],
                          Firsts$Choice,mean(Firsts$Q24AIncome))
colnames(Test_Apollo) <- c("ID","Task","Q1Gender","Age","Distance",
                           "Trips","BP","Charity",
                           "Consequentiality",
                           "Experts","Education","Employment","Survey",
                           "Price_B","Performance_B","Emission_B",
                           "Performance_A","Emission_A","Price_A"
                           ,"Choice","Income")

# Tests_Dominated <- Test_Apollo[!Test_Apollo$ID %in% c(Test_Apollo$ID[ ((Test_Apollo$Task == 1) & (Test_Apollo$Choice ==1) & (grepl("SQ",rownames(Test_Apollo),fixed = TRUE) == FALSE)) ]),]
# Tests_Understanding <- Tests_Dominated[!Tests_Dominated$ID %in% c( unique(Tests_Dominated$ID[Tests_Dominated$Survey <= 5])),]
# Test_Apollo <- Tests_Understanding

Test_Apollo$Choice[Test_Apollo$Choice == 1] <- 2
Test_Apollo$Choice[Test_Apollo$Choice == 0] <- 1
database = Test_Apollo


choiceAnalysis_settings <- list(
  alternatives = c(A=1, B=2),
  avail        = list(A=1, B=1),
  choiceVar    = Test_Apollo$Choice,
  explanators  = Test_Apollo[,c("Price_B","Performance_B","Emission_B","Q1Gender","Age","Distance",
                                "Trips","BP","Charity",
                                "Education","Employment",
                                "Income")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, Test_Apollo)

apollo_beta=c(asc_A     = 0,
              asc_B    = 0,
              b_Price    = 0,
              b_Performance   = 0,
              b_Emission   = 0,
              b_Q1Gender =0,
              b_Age      = 0,
              b_Distance = 0,
              b_Trips    = 0,
              b_BP       = 0,
              b_Charity  = 0,
              b_Education  = 0,
              b_Employment = 0,
              b_Income     = 0)

apollo_fixed = c("asc_A")
database <- Test_Apollo
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = asc_A  + 
    b_Price * Test_Apollo$Price_A +
    b_Performance * Test_Apollo$Performance_A +
    b_Emission * Test_Apollo$Emission_A
  
  V[['B']]  = asc_B  + b_Price * Test_Apollo$Price_B +
    b_Performance * Test_Apollo$Performance_B  +
    b_Emission * Test_Apollo$Emission_B +
    b_Q1Gender * Test_Apollo$Q1Gender + 
    b_Age * Test_Apollo$Age +
    b_Distance * Test_Apollo$Distance + 
    b_Trips * Test_Apollo$Trips +
    b_BP * Test_Apollo$BP +
    b_Charity * Test_Apollo$Charity + 
    b_Education * Test_Apollo$Education +
    b_Employment * Test_Apollo$Employment + 
    b_Income * Test_Apollo$Income
  
  mnl_settings = list(
    alternatives  = c(A=1, B=2), 
    avail         = list(A=1, B=1), 
    choiceVar     = Choice,
    V             = V
  )
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik")) 
apollo_modelOutput(model)
  

#############################################################################
## ICLV model:
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r
#############################################################################

## LOAD LIBRARY AND DEFINE CORE SETTINGS
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "Apollo_ICLV",
  modelDescr = "ICLV model example",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 1
)

database = Test_Apollo
  
# read.csv("apollo_drugChoiceData.csv",header=TRUE)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(b_brand_Artemis    = 0, 
                b_brand_Novum      = 0, 
                b_brand_BestValue  = 0, 
                b_brand_Supermarket= 0, 
                b_brand_PainAway   = 0, 
                b_country_CH       = 0, 
                b_country_DK       = 0, 
                b_country_USA      = 0, 
                b_country_IND      = 0, 
                b_country_RUS      = 0, 
                b_country_BRA      = 0, 
                b_char_standard    = 0, 
                b_char_fast        = 0, 
                b_char_double      = 0, 
                b_risk             = 0, 
                b_price            = 0,  
                lambda             = 1, 
                gamma_reg_user     = 0, 
                gamma_university   = 0, 
                gamma_age_50       = 0, 
                zeta_quality       = 1, 
                zeta_ingredient    = 1, 
                zeta_patent        = 1, 
                zeta_dominance     = 1, 
                tau_quality_1      =-2, 
                tau_quality_2      =-1, 
                tau_quality_3      = 1, 
                tau_quality_4      = 2, 
                tau_ingredients_1  =-2, 
                tau_ingredients_2  =-1, 
                tau_ingredients_3  = 1, 
                tau_ingredients_4  = 2, 
                tau_patent_1       =-2, 
                tau_patent_2       =-1, 
                tau_patent_3       = 1, 
                tau_patent_4       = 2, 
                tau_dominance_1    =-2, 
                tau_dominance_2    =-1, 
                tau_dominance_3    = 1, 
                tau_dominance_4    = 2)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_brand_Artemis", "b_country_USA", "b_char_standard")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####

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
  
  randcoeff[["LV"]] = gamma_reg_user*regular_user + gamma_university*university_educated + gamma_age_50*over_50 + eta
  
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of indicators
  ol_settings1 = list(outcomeOrdered = attitude_quality, 
                      V              = zeta_quality*LV, 
                      tau            = c(tau_quality_1, tau_quality_2, tau_quality_3, tau_quality_4),
                      rows           = (task==1),
                      componentName  = "indic_quality")
  ol_settings2 = list(outcomeOrdered = attitude_ingredients, 
                      V              = zeta_ingredient*LV, 
                      tau            = c(tau_ingredients_1, tau_ingredients_2, tau_ingredients_3, tau_ingredients_4), 
                      rows           = (task==1),
                      componentName  = "indic_ingredients")
  ol_settings3 = list(outcomeOrdered = attitude_patent, 
                      V              = zeta_patent*LV, 
                      tau            = c(tau_patent_1, tau_patent_2, tau_patent_3, tau_patent_4), 
                      rows           = (task==1),
                      componentName  = "indic_patent")
  ol_settings4 = list(outcomeOrdered = attitude_dominance, 
                      V              = zeta_dominance*LV, 
                      tau            = c(tau_dominance_1, tau_dominance_2, tau_dominance_3, tau_dominance_4), 
                      rows           = (task==1),
                      componentName  = "indic_dominance")
  P[["indic_quality"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_ingredients"]] = apollo_ol(ol_settings2, functionality)
  P[["indic_patent"]]      = apollo_ol(ol_settings3, functionality)
  P[["indic_dominance"]]   = apollo_ol(ol_settings4, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = ( b_brand_Artemis*(brand_1=="Artemis") + b_brand_Novum*(brand_1=="Novum") 
                  + b_country_CH*(country_1=="Switzerland") + b_country_DK*(country_1=="Denmark") + b_country_USA*(country_1=="USA") 
                  + b_char_standard*(char_1=="standard") + b_char_fast*(char_1=="fast acting") + b_char_double*(char_1=="double strength") 
                  + b_risk*side_effects_1
                  + b_price*price_1 
                  + lambda*LV )
  V[['alt2']] = ( b_brand_Artemis*(brand_2=="Artemis") + b_brand_Novum*(brand_2=="Novum") 
                  + b_country_CH*(country_2=="Switzerland") + b_country_DK*(country_2=="Denmark") + b_country_USA*(country_2=="USA") 
                  + b_char_standard*(char_2=="standard") + b_char_fast*(char_2=="fast acting") + b_char_double*(char_2=="double strength") 
                  + b_risk*side_effects_2
                  + b_price*price_2 
                  + lambda*LV )
  V[['alt3']] = ( b_brand_BestValue*(brand_3=="BestValue") + b_brand_Supermarket*(brand_3=="Supermarket") + b_brand_PainAway*(brand_3=="PainAway") 
                  + b_country_USA*(country_3=="USA") + b_country_IND*(country_3=="India") + b_country_RUS*(country_3=="Russia") + b_country_BRA*(country_3=="Brazil") 
                  + b_char_standard*(char_3=="standard") + b_char_fast*(char_3=="fast acting") 
                  + b_risk*side_effects_3
                  + b_price*price_3 )
  V[['alt4']] = ( b_brand_BestValue*(brand_4=="BestValue") + b_brand_Supermarket*(brand_4=="Supermarket") + b_brand_PainAway*(brand_4=="PainAway") 
                  + b_country_USA*(country_4=="USA") + b_country_IND*(country_4=="India") + b_country_RUS*(country_4=="Russia") + b_country_BRA*(country_4=="Brazil") 
                  + b_char_standard*(char_4=="standard") + b_char_fast*(char_4=="fast acting") 
                  + b_risk*side_effects_4
                  + b_price*price_4 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail        = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVar    = best,
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

# ################################################################# #
#### MODEL ESTIMATION                                            

### Optional: calculate LL before model estimation
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(model)

apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

## Predictions 
forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                              prediction_settings=list(modelComponent="indic_quality"))

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

## switch off writing to file                                 
if(sink.number()>0) sink()


#############################################################################
## Apollo HCM
## http://www.apollochoicemodelling.com/files/hybrid_model_classical.r
#############################################################################

apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "hybrid_model_classical",
  modelDescr = "Hybrid choice model on drug choice data, classical estimation",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 25
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

database = read.csv("apollo_drugChoiceData.csv",header=TRUE)

# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(Artemis=11, Novum=12, BestValue=21, Supermarket=22, PainAway=23),
  avail        = with(database,list(
    Artemis=(brand_1=="Artemis")|(brand_2=="Artemis"), 
    Novum=(brand_1=="Novum")|(brand_2=="Novum"),
    BestValue=(brand_3=="BestValue")|(brand_4=="BestValue"),
    Supermarket=(brand_3=="Supermarket")|(brand_4=="Supermarket"),
    PainAway=(brand_3=="PainAway")|(brand_4=="PainAway"))),
  choiceVar    = with(database,
                      (11*((best==1)*(brand_1=="Artemis")+(best==2)*(brand_2=="Artemis"))
                       +12*((best==1)*(brand_1=="Novum")+(best==2)*(brand_2=="Novum"))
                       +21*((best==3)*(brand_3=="BestValue")+(best==4)*(brand_4=="BestValue"))
                       +22*((best==3)*(brand_3=="Supermarket")+(best==4)*(brand_4=="Supermarket"))
                       +23*((best==3)*(brand_3=="PainAway")+(best==4)*(brand_4=="PainAway")))),
  explanators  = database[,c("regular_user","university_educated","over_50")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(mu_brand_Artemis          = 0, sig_brand_Artemis         = 0, 
                gamma_Artemis_reg_user    = 0, gamma_Artemis_university  = 0, 
                gamma_Artemis_age_50      = 0, mu_brand_Novum            = 0, 
                sig_brand_Novum           = 0, gamma_Novum_reg_user      = 0, 
                gamma_Novum_university    = 0, gamma_Novum_age_50        = 0, 
                b_brand_BestValue         = 0, b_brand_Supermarket       = 0, 
                b_brand_PainAway          = 0, b_country_CH              = 0, 
                b_country_DK              = 0, b_country_USA             = 0, 
                b_country_IND             = 0, b_country_RUS             = 0, 
                b_country_BRA             = 0, b_char_standard           = 0, 
                b_char_fast               = 0, b_char_double             = 0, 
                b_risk                    = 0, b_price                   = 0,  
                gamma_LV_reg_user         = 0, gamma_LV_university       = 0, 
                gamma_LV_age_50           = 0, lambda                    = 1, 
                zeta_quality              = 1, zeta_ingredient           = 1, 
                zeta_patent               = 1, zeta_dominance            = 1, 
                tau_quality_1             =-2, tau_quality_2             =-1, 
                tau_quality_3             = 1, tau_quality_4             = 2, 
                tau_ingredients_1         =-2, tau_ingredients_2         =-1, 
                tau_ingredients_3         = 1, tau_ingredients_4         = 2, 
                tau_patent_1              =-2, tau_patent_2              =-1, 
                tau_patent_3              = 1, tau_patent_4              = 2, 
                tau_dominance_1           =-2, tau_dominance_2           =-1, 
                tau_dominance_3           = 1, tau_dominance_4           = 2)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("b_brand_PainAway", "b_country_USA", "b_char_standard")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=500,          
  interNormDraws=c("eta","xi_Artemis","xi_Novum")
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_LV_reg_user*regular_user + gamma_LV_university*university_educated + gamma_LV_age_50*over_50 + eta
  randcoeff[["b_brand_Artemis"]] = mu_brand_Artemis + sig_brand_Artemis * xi_Artemis + gamma_Artemis_reg_user*regular_user + gamma_Artemis_university*university_educated + gamma_Artemis_age_50*over_50  
  randcoeff[["b_brand_Novum"]] = mu_brand_Novum + sig_brand_Novum * xi_Novum + gamma_Novum_reg_user*regular_user + gamma_Novum_university*university_educated + gamma_Novum_age_50*over_50  
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']] = ( b_brand_Artemis*(brand_1=="Artemis") + b_brand_Novum*(brand_1=="Novum") 
                  + b_country_CH*(country_1=="Switzerland") + b_country_DK*(country_1=="Denmark") + b_country_USA*(country_1=="USA") 
                  + b_char_standard*(char_1=="standard") + b_char_fast*(char_1=="fast acting") + b_char_double*(char_1=="double strength") 
                  + b_risk*side_effects_1
                  + b_price*price_1 
                  + lambda*LV )
  V[['alt2']] = ( b_brand_Artemis*(brand_2=="Artemis") + b_brand_Novum*(brand_2=="Novum") 
                  + b_country_CH*(country_2=="Switzerland") + b_country_DK*(country_2=="Denmark") + b_country_USA*(country_2=="USA") 
                  + b_char_standard*(char_2=="standard") + b_char_fast*(char_2=="fast acting") + b_char_double*(char_2=="double strength") 
                  + b_risk*side_effects_2
                  + b_price*price_2 
                  + lambda*LV )
  V[['alt3']] = ( b_brand_BestValue*(brand_3=="BestValue") + b_brand_Supermarket*(brand_3=="Supermarket") + b_brand_PainAway*(brand_3=="PainAway") 
                  + b_country_USA*(country_3=="USA") + b_country_IND*(country_3=="India") + b_country_RUS*(country_3=="Russia") + b_country_BRA*(country_3=="Brazil") 
                  + b_char_standard*(char_3=="standard") + b_char_fast*(char_3=="fast acting") 
                  + b_risk*side_effects_3
                  + b_price*price_3 )
  V[['alt4']] = ( b_brand_BestValue*(brand_4=="BestValue") + b_brand_Supermarket*(brand_4=="Supermarket") + b_brand_PainAway*(brand_4=="PainAway") 
                  + b_country_USA*(country_4=="USA") + b_country_IND*(country_4=="India") + b_country_RUS*(country_4=="Russia") + b_country_BRA*(country_4=="Brazil") 
                  + b_char_standard*(char_4=="standard") + b_char_fast*(char_4=="fast acting") 
                  + b_risk*side_effects_4
                  + b_price*price_4 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail        = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVar    = best,
    V            = V
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Likelihood of indicators
  ol_settings1 = list(outcomeOrdered=attitude_quality, 
                      V=zeta_quality*LV, 
                      tau=c(tau_quality_1, tau_quality_2, tau_quality_3, tau_quality_4),
                      rows=(task==1))
  ol_settings2 = list(outcomeOrdered=attitude_ingredients, 
                      V=zeta_ingredient*LV, 
                      tau=c(tau_ingredients_1, tau_ingredients_2, tau_ingredients_3, tau_ingredients_4), 
                      rows=(task==1))
  ol_settings3 = list(outcomeOrdered=attitude_patent, 
                      V=zeta_patent*LV, 
                      tau=c(tau_patent_1, tau_patent_2, tau_patent_3, tau_patent_4), 
                      rows=(task==1))
  ol_settings4 = list(outcomeOrdered=attitude_dominance, 
                      V=zeta_dominance*LV, 
                      tau=c(tau_dominance_1, tau_dominance_2, tau_dominance_3, tau_dominance_4), 
                      rows=(task==1))
  P[["indic_quality"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_ingredients"]] = apollo_ol(ol_settings2, functionality)
  P[["indic_patent"]]      = apollo_ol(ol_settings3, functionality)
  P[["indic_dominance"]]   = apollo_ol(ol_settings4, functionality)
  
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

# ################################################################# #
#### CHECK FOR COMPUTATIONAL REQUIREMENTS                         ####
# ################################################################# #

speedTest_settings=list(
  nDrawsTry = c(250, 500, 1000),
  nCoresTry = 1:3,
  nRep      = 10
)

apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Estimate model
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)

# ################################################################# #
##### POST-PROCESSING                                            ####
# ################################################################# #

### Print outputs of additional diagnostics to new output file (remember to close file writing when complete)
sink(paste(model$apollo_control$modelName,"_additional_output.txt",sep=""),split=TRUE)

# ----------------------------------------------------------------- #
#---- FUNCTIONS OF MODEL PARAMETERS                              ----
# ----------------------------------------------------------------- #

deltaMethod_settings=list(operation="ratio", parName1="b_risk", parName2="b_price", multPar1 = 1000)
apollo_deltaMethod(model, deltaMethod_settings)

deltaMethod_settings=list(operation="diff", parName1="mu_brand_Artemis", parName2="mu_brand_Novum")
apollo_deltaMethod(model, deltaMethod_settings)

# ----------------------------------------------------------------- #
#---- MODEL PREDICTIONS                                          ----
# ----------------------------------------------------------------- #

base_forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                   modelComponent="choice")

mean(base_forecast[,1]+base_forecast[,2])

database$price_1=1.5*database$price_1
database$price_2=1.5*database$price_2

change_forecast <- apollo_prediction(model, apollo_probabilities, apollo_inputs,
                                     modelComponent="choice")

mean(change_forecast[,1]+change_forecast[,2])

database$price_1=1/1.5*database$price_1
database$price_2=1/1.5*database$price_2

# ----------------------------------------------------------------- #
#---- UNCONDITIONALS AND CONDITIONALS                            ----
# ----------------------------------------------------------------- #

unconditionals <- apollo_unconditionals(model,apollo_probabilities,apollo_inputs)

conditionals <- apollo_conditionals(model,apollo_probabilities,apollo_inputs)

mean(unconditionals[["LV"]])
sd(unconditionals[["LV"]])

summary(conditionals[["LV"]])

regular_user_n=apollo_firstRow(database$regular_user, apollo_inputs)

mean(subset(unconditionals[["LV"]],regular_user_n==0))
mean(subset(unconditionals[["LV"]],regular_user_n==1))
summary(subset(conditionals[["LV"]],regular_user_n==0))
summary(subset(conditionals[["LV"]],regular_user_n==1))

# ----------------------------------------------------------------- #
#---- switch off writing to file                                 ----
# ----------------------------------------------------------------- #

if(sink.number()>0) sink()

#############################################################################
## LCM with RRM and RUM classes
## https://www.advancedrrmmodels.com/latent-class-models
#############################################################################

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
  B_Emission_1 = 0,
  B_Performance_1  = 0,
  # Class 2
  B_Price_2 = 0,
  B_Emission_2 = 0,
  B_Performance_2  = -0.2,
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
  lcpars[["B_Emission"]]  = list(B_Emission_1, B_Emission_2)
  
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
  Emission1_sc = ( 1 / 100 ) * Emission_A
  Emission2_sc = ( 1 / 100 ) * Emission_B
  
  # Compute P-RRM Atrribute levels
  X_Price1 = pmax( 0 , Price2_sc - Price1_sc ) 
  X_Price2 = pmax( 0 , Price1_sc - Price2_sc ) 
  
  X_Performance1 = pmax( 0 , Performance2_sc - Performance1_sc ) 
  X_Performance2 = pmax( 0 , Performance1_sc - Performance2_sc ) 
  
  X_Emission1 =  pmin( 0 , Emission2_sc - Emission1_sc ) 
  X_Emission2 =  pmin( 0 , Emission1_sc - Emission2_sc ) 
  
  
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

apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(estimationRoutine="bfgs",bootstrapSE=10,maxIterations=50)))

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik")) 
apollo_modelOutput(model)