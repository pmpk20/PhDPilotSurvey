#Peter King
####################################################################################
############### Introduction: First 100 Data Analysis Script  ##########################
####################################################################################

####################################################################################
############### Section 1: Import Data  ##########################
####################################################################################
 
install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
install.packages("stargazer") ## To export to LaTeX code.
rm(list = ls()) ## Removes all things in current global environment
############ Importing data:

setwd("H:/PhDFirstSurveySurvey") ## Sets working directory. This is where my Github repo is cloned to.

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
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 1] <- 5
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 2] <- 50
FirstSurvey2$Q9Performance[FirstSurvey2$Q9Performance == 0] <- 10
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 1] <- 40
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 2] <- 90
FirstSurvey2$Q9Emission[FirstSurvey2$Q9Emission == 0] <- 10
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 1] <- 2.5
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 2] <- 5
FirstSurvey2$Q9Price[FirstSurvey2$Q9Price == 0] <- 1

FirstSurvey2$Q10Performance[FirstSurvey2$Q10Performance == 1] <- 5
FirstSurvey2$Q10Performance[FirstSurvey2$Q10Performance == 0] <- 10
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 1] <- 4 
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 2] <- 40
FirstSurvey2$Q10Emission[FirstSurvey2$Q10Emission == 0] <- 10
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 1] <- 1
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 2] <- 2.5
FirstSurvey2$Q10Price[FirstSurvey2$Q10Price == 0] <- 0.5

FirstSurvey2$Q11Performance[FirstSurvey2$Q11Performance == 1] <- 5
FirstSurvey2$Q11Performance[FirstSurvey2$Q11Performance == 0] <- 10
FirstSurvey2$Q11Emission[FirstSurvey2$Q11Emission == 1] <- 90
FirstSurvey2$Q11Emission[FirstSurvey2$Q11Emission == 0] <- 10
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 0] <- 0.5
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 1] <- 1
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 2] <- 2.5
FirstSurvey2$Q11Price[FirstSurvey2$Q11Price == 3] <- 5

FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 1] <- 5
FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 2] <- 50
FirstSurvey2$Q12Performance[FirstSurvey2$Q12Performance == 0] <- 10
FirstSurvey2$Q12Emission[FirstSurvey2$Q12Emission == 1] <- 40
FirstSurvey2$Q12Emission[FirstSurvey2$Q12Emission == 0] <- 10
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
Price_B <- data.frame(Price_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Price"],
                                           FirstSurvey2["Q10Price"],
                                           FirstSurvey2["Q11Price"],
                                           FirstSurvey2["Q12Price"]),
                                times=1)))[,]))

##  Creating a dataframe with all the levels that the Performance attribute took for alternative B in the CE. 
Performance_B <- data.frame(Performance_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Performance"],
                                           FirstSurvey2["Q10Performance"],
                                           FirstSurvey2["Q11Performance"],
                                           FirstSurvey2["Q12Performance"]),
                                times=1)))[,]))

##  Creating a dataframe with all the levels that the Emission attribute took for alternative B in the CE. 
Emission_B <- data.frame(Emission_B = 
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
First <- data.frame(First[,1:13],First[,18],Price_B, Performance_B, Emission_B,
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

First$av_A <- rep(1,nrow(First)) # Add a vector of ones to show that the alternative choice is always available to respondents.
First$av_B <- rep(1,nrow(First)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
First$Choice[First$Choice == 0] <- "A"  ## Necessary here to change numeric to string
First$Choice[First$Choice == 1] <- "B" ## The MFORMULA looks for _SQ or _ALT so choice must be SQ or ALT

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

## Use this way to make a trellis plot which makes comparison between groups easier.
First_Cons$charity_scale = factor(First_Cons$Q18Charity, levels=c("No involvement","Donated or joined"))

## Plot 1 (of 2):
P1 <- ggplot(First_Cons, aes(Q3Distance,Q4Trips)) + 
  geom_point(shape = 1) +
  facet_grid(~charity_scale) + 
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between distance and trips by charity involvement.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Distance",y="Trips")+
  scale_y_continuous(limits = c(0,1))

## Plot 2 (of 2):
P2 <- ggplot(First_Cons, aes(Q3Distance,Q4Trips)) + 
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


##########################################################  
####### CE
##########################################################  


## Have to do the manipulation section again to ignore the changes made in the graphics section
library(mlogit) #Already have package installed
First_Long <- mlogit.data(First, shape = "wide", choice = "Choice",
                         varying = 15:20, sep = "_", id.var = "ID")

## To trim the sample: 
First_Dominated <- First_Long[First_Long$Q8DominatedTest == 0]
First_Understanding <- First_Dominated[First_Dominated$Q25Understanding >= 5]
First_Cons <- First_Understanding[First_Understanding$Q20Consequentiality == 1]

######################## Estimation section:

## A very basic MNL model to test whether the method works 
Base_MNL <- mlogit(Choice ~  Price + Performance + Emission, 
                   First_Cons,
                   alt.subset = c("A","B"),reflevel = "A") 
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

## The full MNL with all covariates:
Pilot_MNL <- mlogit(Choice ~ Price + Performance + Emission | 
                      Order + Q1Gender + Q2Age + Q3Distance
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
    Order + Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  First_Long, rpar=c(Price="n"),
  R=10,correlation = FALSE,
  reflevel="A",halton=NA,method="bhhh",panel=TRUE,seed=123)
summary(MXLFull)

## Can use AIC and BIC to compare model fits:
AIC(Pilot_MNL)
BIC(Pilot_MNL)
AIC(MXLFull)
BIC(MXLFull)

## Report WTP crudely: 
coef(Pilot_MNL)/coef(Pilot_MNL)["Price"]
coef(MXLFull)/coef(MXLFull)["Price"]

## Clustering and bootstrapping:
library(clusterSEs)
cluster.bs.mlogit(MXLFull, First_Long, ~ ID, boot.reps=10,seed = 123)

## Calculating consumer surplus:
First_Understanding_CS1 <- First_Understanding
First_Understanding_CS1$Price <- First_Understanding_CS1$Price * 1.1
First_Understanding_CS1$Health <- First_Understanding_CS1$Health * 0.9
Va1 <- logsum(MXLFull)
Va0 <- logsum(MXLFull,data = First_Understanding_CS1)
surplus <- - (Va1 - Va0) / coef(MXLFull)["Price"]
summary(surplus)


##############  GMNL is an alternative to MLOGIT
library(gmnl)

## Replicating the MNL
MNL_GM <- gmnl(  Choice ~ Price + Performance + Emission | 
                   Q1Gender + Q2Age + Q3Distance
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
                          Q1Gender + Q2Age + Q3Distance
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

## GMNL has a plot function for the conditional distribution of the random parameters:
plot(GMNL_MXLDefault, par = "Q18Charity", effect = "wtp", type = "density", col = "grey",wrt="Price")


############ Estimating LATENT-CLASS MODELS

## Two class model:
LC_GM <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1,
              data = First_Long,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) # 75.516
BIC(LC_GM) # 90.36565

## Function from https://rpubs.com/msarrias1986/335556 which calculates the share of the sample in each class
exp(coef(LC_GM)["(class)2"]) / (exp(0) + exp(coef(LC_GM)["(class)2"]))
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

## The Q6 model: actually much better than the AOD approach above.
Research_SB <- sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Q2Age + Q3Distance
                        + Q4Trips + Q16BP + Q18Charity 
                        + Q20Consequentiality
                        + Q21Experts +Q22Education+ Q23Employment
                        +  Q24AIncome | Q6Bid, data = FirstSurvey2,dist="logistic")
summary(Research_SB) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
### NOTE: The moodel CURRENTLY only produces realistic WTP when the distribution is "logistic" not the default "log-logistic"  

## Two methods to estimate confidence intervals:
krCI(Research_SB)
bootCI(Research_SB)


## Repeating the same as above but for Q7 the DBDC question:
Treatment_DB <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Order + Q1Gender + Q2Age + Q3Distance
                         + Q4Trips + Q16BP + Q18Charity 
                         + Q20Consequentiality
                         + Q21Experts +Q22Education+ Q23Employment
                         +  Q24AIncome | Q7Bid + Q7Bid2,data = First,dist="logistic")
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)


## This section deals with Q6 and Q7 respectively but uses a non-parametric Kaplan-Meier-Turnbull survival function:
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = FirstSurvey2)
summary(ResearchKMT)
plot(ResearchKMT)

## Reporting the KMT for Q7.
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = FirstSurvey2)
summary(TreatmentKMT)
plot(TreatmentKMT)


##########################################################  
## The following code is experimental:
##########################################################  




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
database = Test_Apollo

## Parameters to be estimated and their starting values
## Price and Health attributes used only
apollo_beta = c(# Class 1
  B_Price_1 = 0,
  B_Health_1  = 0,
  # Class 2
  B_Price_2 = 0,
  B_Health_2  = -0.2,
  # Class membership parameters
  s_1     = 0,
  s_2     = 0)

## Define one class as fixed as the utility-differences approach 
apollo_fixed = c("s_1")

## Grouping latent class parameters
apollo_lcPars = function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["B_Price"]] = list(B_Price_1, B_Price_2)
  lcpars[["B_Health"]]  = list(B_Health_1, B_Health_2)
  
  ###Class membership probabilities based on s_1, s_2: use of MNL fomula
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
  
  ## Pairwise comparison and scale of aHealthribues
  
  ## Define PRRM variables
  Price1_sc =( 1 / 1000 ) * Price_ALT
  Price2_sc =( 1 / 1000 ) * Price_SQ
  Health1_sc = ( 1 / 100 ) * Health_ALT
  Health2_sc = ( 1 / 100 ) * Health_SQ 
  
  # Compute P-RRM Atrribute levels
  X_Price1 =  pmax( 0 , Price2_sc - Price1_sc ) 
  X_Price2 =  pmax( 0 , Price1_sc - Price2_sc ) 
  
  X_Health1 = pmin( 0 , Health2_sc - Health1_sc ) 
  X_Health2 = pmin( 0 , Health1_sc - Health2_sc ) 
  
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
  V[['Alt1']]  = B_Price_1 * Price1_sc + B_Health_1 * Health1_sc
  V[['Alt2']]  = B_Price_1 * Price2_sc + B_Health_1 * Health2_sc
  
  ###Calculating probabilities based on MNL function for class 1
  mnl_settings$V = V
  P[[1]] = apollo_mnl(mnl_settings, functionality)
  P[[1]] = apollo_panelProd(P[[1]], apollo_inputs ,functionality)
  
  ### Compute class-specific regrets
  R=list()
  R[['Alt1']]  = B_Price_2 * X_Price1 + B_Health_2 * X_Health1
  R[['Alt2']]  = B_Price_2 * X_Price2 + B_Health_2 * X_Health2 
  
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

## Starting value search:
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=20))
## Starting value search:
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings=list(hessianRoutine="maxLik")) ## Estimate full model
apollo_modelOutput(model) ## Display the model output
