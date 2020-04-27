#Peter King
####################################################################################
############### Introduction: First 100 Data Analysis Script  ##########################
####################################################################################

############ TO DO:
# - Weight CE by certainty
# - Check CVM by ordering
# - Fix clustering


############ Packages:
install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
install.packages("stargazer") ## To export to LaTeX code.
install.packages("dplyr")
library(dplyr)
setwd("H:/PhDFirstSurveySurvey") ## Sets working directory. This is where my Github repo is cloned to.

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
###  Believing the survey responses to be consequential.
First_Cons <- First_Understanding[First_Understanding$Q20Consequentiality == 1]


##########################################################  
####### Descriptive Graphics
##########################################################  

 
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
  labs(x = "Likelihood of accepting option B",y="Price attribute")


## Emission Curve: Plotting choice versus emission levels 
A2 <- ggplot(Firsts, aes(Choice,Emission_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Emission curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Emission attribute")

## Performance Curve: Plotting choice versus performance levels 
A3 <- ggplot(Firsts, aes(Choice,Performance_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Performance curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Performance attribute")

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
  R=10000,correlation = FALSE,
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
AIC(Pilot_MNL): ## 215.1842
AIC(MXLFull):   ## 561.1319
AIC(MXLFullD):   ## 200.5288
 
  
## Clustering and bootstrapping:
library(clusterSEs)
CBSM <- cluster.bs.mlogit(MXLFull, First_Long, ~ ID, boot.reps=100,seed = 123)


library(stargazer)
stargazer(summary(MXLFull)$CoefTable, title = "MXLFull", align = TRUE,report="p*")


## Calculating consumer surplus:
First_Understanding_CS1 <- First_Understanding
First_Understanding_CS1$Price <- First_Understanding_CS1$Price * 1.1
First_Understanding_CS1$Performance <- First_Understanding_CS1$Performance * 0.9
Va1 <- logsum(MXLFull,data=First_Understanding)
Va0 <- logsum(MXLFull,data = First_Understanding_CS1)
surplus <- - (Va1 - Va0) / coef(MXLFull)["Price"]
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
                 data = First_Understanding,
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
                        +  Q24AIncome, data = First_Understanding,
                        model = "mixl",
                        ranp = c( Price = "ln"),
                        mvar = list(Price = c("Q18Charity")),
                        R = 10,
                        haltons = NA
                        ,seed = 123,reflevel = "A")
summary(GMNL_MXLDefault)
wtp.gmnl(GMNL_MXLDefault,"Price",3)
coef(GMNL_MXLDefault)["Performance"]/coef(GMNL_MXLDefault)["Price"]
coef(GMNL_MXLDefault)["Emission"]/coef(GMNL_MXLDefault)["Price"]

## GMNL has a plot function for the conditional distribution of the random parameters:
plot(GMNL_MXLDefault, par = "Q18Charity", effect = "wtp", type = "density", col = "grey",wrt="Price")


############ Estimating LATENT-CLASS MODELS

## Two class model:
LC_GM <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1,
              data = First_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) ## 194.4641
BIC(LC_GM) ## 215.9903

## Three class model:
LC_GM3 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1,
              data = First_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 3)
summary(LC_GM3)
AIC(LC_GM3) # 190.1208
BIC(LC_GM3) # 223.9478

## Four class model:
LC_GM4 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1,
              data = First_Cons,
              model = 'lc',
              panel = TRUE,
              Q = 4)
summary(LC_GM4)
AIC(LC_GM4) # 186.1308
BIC(LC_GM4) # 232.2584

## 5-class model
LC_GM5 <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                 0 | 0 | 1,
               data = First_Cons,
               model = 'lc',
               panel = TRUE,
               Q = 5)
summary(LC_GM5)
AIC(LC_GM5) # 185.2838
BIC(LC_GM5) # 243.7121


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
shares(LC_GM4)
shares(LC_GM5)

## LCM class-specific WTP:
-coef(LC_GM)/coef(LC_GM)["class.1.Price"]
-coef(LC_GM)/coef(LC_GM)["class.2.Price"]



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


## The Q6 model: actually much better than the AOD approach above.
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Task + Q1Gender + Q2Age + Q3Distance
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


## Repeating the same as above but for Q7 the DBDC question:
Treatment_DB <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order + Task + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity
                         + Q21Experts + Q22Education + Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = First_Certain,dist="logistic")
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)

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

apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(estimationRoutine="bfgs",bootstrapSE=10,maxIterations=50)))

  

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