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

setwd("H:/PhDFirstSurveySurvey") # Sets working directory. This is where my Github repo is cloned to.

FirstSurvey <- data.frame(read.csv("FirstHundred.csv"))

FirstSurvey <- FirstSurvey[ -c(2,4,14,15,16,23,24,26,27,53,54,55,56,57,58,68,69)] #Drop rows of no importance to the quantitative analysis, namely text responses.

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

FirstSurvey2 <- FirstSurvey # Create a backup of the FirstSurvey data

for (i in colnames(FirstSurvey)){
  if (is.factor(FirstSurvey[[i]]) == TRUE){
    FirstSurvey2[[i]] <- as.numeric(FirstSurvey[[i]])-1
  }
} 
# Here I make all the columns factors for ease of analysis.

FirstSurvey2$Order[FirstSurvey2$Order == 2] <-0 # Make sure the order dummy is 0 for normal, 1 for 7 then 6.

FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 0] <- 21.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 1] <- 32.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 2] <- 47.5
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 3] <- 63
FirstSurvey2$Q2Age[FirstSurvey2$Q2Age == 4] <- 71

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
FirstSurvey2$Q3Distance[FirstSurvey2$Q3Distance == 5] <- mean(FirstSurvey2$Q3Distance)

FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 4] <- 5
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 1] <- 6
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 6] <- 4
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 3] <- 1
FirstSurvey2$Q5Knowledge[FirstSurvey2$Q5Knowledge == 0] <- 3

FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 1] <-3
FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 0] <- 1
FirstSurvey2$Q6ResearchCertainty[FirstSurvey2$Q6ResearchCertainty == 3] <- 0

FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 1] <-3
FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 0] <- 1
FirstSurvey2$Q7TreatmentCertainty[FirstSurvey2$Q7TreatmentCertainty == 3] <- 0

FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 1] <- 3
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 2] <- 1
FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 3] <- 2

FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 1] <- 3
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 2] <- 1
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 3] <- 2

FirstSurvey2$Q7TreatmentUpperResponse[FirstSurvey2$Q7TreatmentUpperResponse == 2] <- NA
FirstSurvey2$Q7TreatmentLowerResponse[FirstSurvey2$Q7TreatmentLowerResponse == 2] <- NA

FirstSurvey2 <- mutate(Q7Bid2 = coalesce(FirstSurvey2$Q7Bid2Lower,FirstSurvey2$Q7Bid2Upper),.data = FirstSurvey2)
FirstSurvey2 <- mutate(Q7Response2 = coalesce(FirstSurvey2$Q7TreatmentUpperResponse,FirstSurvey2$Q7TreatmentLowerResponse),.data = FirstSurvey2)

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
# Q10 Emission has 4 as a level?

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

FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 1] <- 3
FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 0] <- 1
FirstSurvey2$Q12CECertainty[FirstSurvey2$Q12CECertainty == 3] <- 0

FirstSurvey2$Q13CurrentThreatToSelf <- FirstSurvey2$Q13CurrentThreatToSelf + 1
FirstSurvey2$Q14FutureThreatToSelf <- FirstSurvey2$Q14FutureThreatToSelf + 1
FirstSurvey2$Q15ThreatToEnvironment <- FirstSurvey2$Q15ThreatToEnvironment + 1

FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 2] <- 3
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 0] <- 2
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 1] <- 0
FirstSurvey2$Q16BP[FirstSurvey2$Q16BP == 3] <- 1

FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 2] <- 3
FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 1] <- 2
FirstSurvey2$Q18Charity[FirstSurvey2$Q18Charity == 3] <- 1

FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 4] <- 5
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 1] <- 4
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 2] <- 1
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 0] <- 2
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 3] <- 0
FirstSurvey2$Q19Knowledge[FirstSurvey2$Q19Knowledge == 4] <- 3

FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 0] <- 3
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 1] <- 0
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 2] <- 1
FirstSurvey2$Q20Consequentiality[FirstSurvey2$Q20Consequentiality == 3] <- 2

FirstSurvey2$Q21Experts <- FirstSurvey2$Q21Experts +1

FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 4] <- 5
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 3] <- 4
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 1] <- 3
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 2] <- 1
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 0] <- 2
FirstSurvey2$Q22Education[FirstSurvey2$Q22Education == 5] <- 0

FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 0] <- 7
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 3] <- 0
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 6] <- 3
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 7] <- 6
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 2] <- 7
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 4] <- 2
FirstSurvey2$Q23Employment[FirstSurvey2$Q23Employment == 7] <- 4

FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 1] <- 3
FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 2] <- 1
FirstSurvey2$Q24RonaImpact[FirstSurvey2$Q24RonaImpact == 3] <- 2

FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 8] <- 750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 4] <- 2750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 5] <- 3500.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 2] <- 1750.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 1] <- 1250.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 3] <- 2250.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 6] <- 4500.00
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 9] <- mean(FirstSurvey2$Q24AIncome)
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 7] <- 5000
FirstSurvey2$Q24AIncome[FirstSurvey2$Q24AIncome == 0] <- 250.00

FirstSurvey2$Q25Understanding[FirstSurvey2$Q25Understanding == 1] <- 9
FirstSurvey2$Q25Understanding <- FirstSurvey2$Q25Understanding +1

FirstSurvey2$ID <- seq.int(nrow(FirstSurvey2))


for (i in colnames(FirstSurvey2)){
  if (is.factor(FirstSurvey[[i]]) == TRUE){
    contrasts(FirstSurvey2[,i]) <- contr.sum(nlevels(FirstSurvey2[,i]))
  }
} ## Aim of the function is to express all variables in the FirstSurvey data as factors

data.frame(FirstSurvey2$Q9Performance,FirstSurvey2$Q9Emission,FirstSurvey2$Q9Price)
data.frame(FirstSurvey2$Q10Performance,FirstSurvey2$Q10Emission,FirstSurvey2$Q10Price)
data.frame(FirstSurvey2$Q11Performance,FirstSurvey2$Q11Emission,FirstSurvey2$Q11Price)
data.frame(FirstSurvey2$Q12Performance,FirstSurvey2$Q12Emission,FirstSurvey2$Q12Price)
SQChoices <- data.frame("Performance" =c(0,0,0), 
                        "Emission" =c(0,0,0),
                        "Price" =c(0,0,0))

library(dplyr)
First <- cbind(slice(.data = FirstSurvey2,rep(1:n(), each = 4)),slice(.data = SQChoices,rep(1:n(), times = 4)))
# So TEST is a dataframe that transforms the FirstSurvey data into an appropriate format for the estimation.
# The code repeats each row of the FirstSurvey data for each choice the respondent made. Therefore, each respondent now has three rows one for Q7, Q8, Q9.

Price_B <- data.frame(Price_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Price"],
                                           FirstSurvey2["Q10Price"],
                                           FirstSurvey2["Q11Price"],
                                           FirstSurvey2["Q12Price"]),
                                times=1)))[,]))
Performance_B <- data.frame(Performance_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Performance"],
                                           FirstSurvey2["Q10Performance"],
                                           FirstSurvey2["Q11Performance"],
                                           FirstSurvey2["Q12Performance"]),
                                times=1)))[,]))
Emission_B <- data.frame(Emission_B = 
             c(t(data.frame(rep(data.frame(FirstSurvey2["Q9Emission"],
                                           FirstSurvey2["Q10Emission"],
                                           FirstSurvey2["Q11Emission"],
                                           FirstSurvey2["Q12Emission"]),
                                times=1)))[,]))

Choices <- data.frame(Choice = c(t(
  data.frame(rep(FirstSurvey2[,35:38], times=1)))[,]))
# Forces the responses to Q7, Q8, Q9 from FirstSurvey to be in one long column. Each respondent has three rows.

First <- data.frame(First[,1:13],First[,18],Price_B, Performance_B, Emission_B,
                    First[,55:57],Choices, First[,19],First[,23],First[,27],
                    First[,31],First[,39:54],
                   rep(1:4,times=nrow(FirstSurvey2)))
# Combines and reorders the TEST dataframe. Use View(Test) to see that this dataframe combines the choice sets, responses, and respondent data.

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

# First <- data.frame(First[,1:7],First[,15:17],First[,27:30])
# colnames(First) <- c("ID","Order","Q1Gender","Q2Age","Q3Distance","Q4Trips",
#                      "Q5Knowledge",
#                      "Performance_B","Emission_B","Price_B",
#                      "Performance_A","Emission_A","Price_A","Choice")

First$av_A <- rep(1,nrow(First)) # Add a vector of ones to show that the alternative choice is always available to respondents.
First$av_B <- rep(1,nrow(First)) # Add a vector of ones to show that the status quo is always available to respondents as consistent with theory.
First$Choice[First$Choice == 0] <- "A"  ## Necessary here to change numeric to string
First$Choice[First$Choice == 1] <- "B" ## The MFORMULA looks for _SQ or _ALT so choice must be SQ or ALT


##########################################################  
####### CE
##########################################################  


library(mlogit) #Already have package installed
Test_Long <- mlogit.data(First, shape = "wide", choice = "Choice",
              varying = 15:20, sep = "_", id.var = "ID")

## To trim the sample: 
First_Dominated <- Test_Long[Test_Long$Q8DominatedTest == 0]
First_Understanding <- First_Dominated[First_Dominated$Q25Understanding >= 5]
First_Cons <- First_Understanding[First_Understanding$Q20Consequentiality == 1]


##########################################################  
##########################################################  


First_Cons <- data.frame(First_Cons)
Test_Long <- data.frame(Test_Long)
First_Cons$Q14BP[First_Cons$Q16BP == 0] <- "Not watched"
First_Cons$Q14BP[First_Cons$Q16BP == 1] <- "Watched"
First_Cons$Q14BP[First_Cons$Q16BP == 2] <- "Watched"
First_Cons$Q18Charity[First_Cons$Q18Charity == 0] <- "No involvement"
First_Cons$Q18Charity[First_Cons$Q18Charity == 1] <- "Donated or joined"
Test_Long$Q20Consequentiality[Test_Long$Q20Consequentiality == 0] <- "Inconsequential"
Test_Long$Q20Consequentiality[Test_Long$Q20Consequentiality == 1] <- "Consequential"

library(scales)
library(ggplot2)
First_Cons$charity_scale = factor(First_Cons$Q18Charity, levels=c("No involvement","Donated or joined"))
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

library(gridExtra)
grid.arrange(P1, P2 )


##########################################################  
##########################################################  

library(mlogit) #Already have package installed
Test_Long <- mlogit.data(First, shape = "wide", choice = "Choice",
                         varying = 15:20, sep = "_", id.var = "ID")

## To trim the sample: 
First_Dominated <- Test_Long[Test_Long$Q8DominatedTest == 0]
First_Understanding <- First_Dominated[First_Dominated$Q25Understanding >= 5]
First_Cons <- First_Understanding[First_Understanding$Q20Consequentiality == 1]

Base_MNL <- mlogit(Choice ~  Price + Performance + Emission, 
                   Test_Long,
                   alt.subset = c("A","B"),reflevel = "A") ##Estimating a simple model first
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

Pilot_MNL <- mlogit(Choice ~ Price + Performance + Emission | 
                      Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q16BP + Q18Charity 
                    + Q20Consequentiality
                    + Q21Experts +Q22Education+ Q23Employment
                    +  Q24AIncome, 
                    Test_Long, alt.subset = c("A", "B"), 
                    reflevel = "A") ## Estimating a much larger MNL model with all the independent variables. 
summary(Pilot_MNL) ## Summarises the MNL output

MXLFull <- mlogit(
  Choice ~ Price + Performance + Emission | 
    Q1Gender + Q2Age + Q3Distance
  + Q4Trips + Q16BP + Q18Charity 
  + Q20Consequentiality
  + Q21Experts +Q22Education+ Q23Employment
  +  Q24AIncome,
  Test_Long, rpar=c(Price="n"),
  R=10,correlation = FALSE,
  reflevel="A",halton=NA,method="bhhh",panel=TRUE,seed=123)
summary(MXLFull)
AIC(MXLFull) # 79.83541
BIC(MXLFull)

coef(MXLFull)/coef(MXLFull)["Price"]
library(clusterSEs)
cluster.bs.mlogit(MXLFull, Test_Long, ~ ID, boot.reps=10,seed = 123)

library(gmnl)
LC_GM <- gmnl(Choice ~ Price + Performance + Emission | 0 |
                0 | 0 | 1,
              data = Test_Long,
              model = 'lc',
              panel = TRUE,
              Q = 2)
summary(LC_GM)
AIC(LC_GM) # 75.516
BIC(LC_GM) # 90.36565

##########################################################  
####### CVM
##########################################################  

##########################################################################
############### AOD                           

install.packages("aod")
install.packages("mfx")
library(mfx)
require(aod)
CVMProbit <-glm(Q6ResearchResponse ~ Q1Gender + Q2Age + 
                  Q3Distance + Q4Trips + Q16BP + 
                  Q18Charity + Q21Experts +Q22Education+ 
                  Q23Employment +  Q24AIncome, family = binomial(link = "probit"),data = FirstSurvey2)
summary(CVMProbit)
confint(CVMProbit)
wald.test(b = coef(CVMProbit), Sigma = vcov(CVMProbit), Terms=2)
stargazer(CVMProbit, title = "CVMProbit", align = TRUE,report="p*")


CVMME <- probitmfx(formula = Q6ResearchResponse ~ Q1Gender + Q2Age + 
                     Q3Distance + Q4Trips + Q16BP + 
                     Q18Charity + Q21Experts +Q22Education+ 
                     Q23Employment +  Q24AIncome,data = FirstSurvey2,robust = TRUE)
CVMME

QOVProbit <-glm(Q7TreatmentResponse ~ Q1Gender + Q2Age + 
                  Q3Distance + Q4Trips + Q16BP + 
                  Q18Charity + Q21Experts +Q22Education+ 
                  Q23Employment +  Q24AIncome, 
                family = binomial(link = "probit"),data = FirstSurvey2)
summary(QOVProbit)
QOVME <- probitmfx(formula = Q7TreatmentResponse ~ Q1Gender + Q2Age + 
                     Q3Distance + Q4Trips + Q16BP + 
                     Q18Charity + Q21Experts +Q22Education+ 
                     Q23Employment +  Q24AIncome,data = FirstSurvey2,robust = TRUE)
confint(QOVProbit)
wald.test(b = coef(QOVProbit), Sigma = vcov(QOVProbit), Terms=2)


##########################################################################
############### CVM: DChoice package                  #####################

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Icens")
install.packages("interval")
install.packages("DCchoice")
library(DCchoice)

Research_SB <- sbchoice(Q6ResearchResponse ~ Q1Gender | Q6Bid, data = FirstSurvey2)
summary(Research_SB)
krCI(Research_SB)
bootCI(Research_SB)

Treatment_DB <- dbchoice(Test$Q7TreatmentResponse + Test$Q7Response2 ~ Q1Gender | Test$Q7Bid + Test$Q7Bid2,data = Test)
summary(Treatment_DB)
krCI(Treatment_DB)
bootCI(Treatment_DB)

ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = FirstSurvey2)
summary(ResearchKMT)
plot(ResearchKMT)

TreatmentKMT <- turnbull.db(formula = Test$Q7TreatmentResponse + Test$Q7Response2 ~  Test$Q7Bid + Test$Q7Bid2,data = Test)
summary(TreatmentKMT)
plot(TreatmentKMT)


##########################################################  
##########################################################  
