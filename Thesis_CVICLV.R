#### Survey data analysis script: All ICLVs  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: This script estimates all the ICLV models and experiments in one place 
# Note: Very long and repetitive, again many Apollo-specific things are uncommented given the extensive guidance in the Manual
# Note: Many might fail or not replicate as I am trying to estimate the bivariate-probit ICLV
# The structure here is to estimate CV in the normal Apollo method, then using probit instead, then estimate the standard ICLV with CV data, and then ICLV with probits


## Setup:
library(DCchoice)
library(apollo)
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
source('Thesis_SetupCode.r')


## Cronbach's alpha to use Q13, 14, 15 as indicators for a single construct: 
library(psych)
alpha(x = data.frame(Full_Final$Q13CurrentThreatToSelf,Full_Final$Q14FutureThreatToSelf, Full_Final$Q15ThreatToEnvironment))


#### CV LOGIT MODELS ####


#### Q6Bid-only logistic ####
apollo_initialise()
apollo_control = list(modelName  ="Q6Bid-OnlyLogistic", indivID    ="ID")
apollo_beta=c(b_bid    = 0,
              intercept_b=0,
              intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q6Bid))
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q6ResearchResponse,
    V            = V,
    componentName= "choice")
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(SB,modelOutput_settings = list(printPVal=TRUE))
-SB$estimate["intercept_b"]/SB$estimate["b_bid"]


#### Q7Bid-only logistic ####
apollo_initialise()
apollo_control = list(modelName  ="Q7Bid-OnlyLogistic", indivID    ="ID")
apollo_beta=c(b_bid    = 0,
              intercept_b=0,
              intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q7Bid))
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V)
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(SB2,modelOutput_settings = list(printPVal=TRUE))
-SB2$estimate["intercept_b"]/SB2$estimate["b_bid"]

database$Q7Response2 <- database$Q7Response2

#### Q7Bid-only bivariate FAILED ####
apollo_initialise()
apollo_control = list(modelName  ="Q7Bid-Only bivariate", indivID    ="ID")
apollo_beta=c(b_bid    = 0,intercept_b=0,intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q7Bid))
  mnl_settings = list(
    alternatives = c(A=0, B=1),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V)
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  X = list()
  X[['A']]  = (intercept_a+ b_bid*Bid_Alt)
  X[['B']]  = (intercept_b+ b_bid*Q7Bid2)
  mnl_settings2 = list(
    alternatives = c(A=0, B=1),
    avail        = list(A=1, B=1),
    choiceVar    = Q7Response2,
    V            = X)
  P[["choice2"]] = apollo_mnl(mnl_settings2, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(SB3,modelOutput_settings = list(printPVal=TRUE))
SB3$estimate
summary((dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1  |Q7Bid + Q7Bid2 , data = database,dist="normal")))$coefficients



#### Q7Bid-only bivariate V2 FAILED ####
apollo_initialise()
apollo_control = list(modelName  ="Q7Bid-Only bivariate2", indivID    ="ID")
apollo_beta=c(b_bid    = 0,intercept_b=0,intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Q7Bid))
  V[['B']]  = (intercept_b+ b_bid*(Q7Bid))
  V[['C']]  = (intercept_b+ b_bid*(Q7Bid))
  V[['D']]  = (intercept_b+ b_bid*(Q7Bid2))
    mnl_settings = list(
    alternatives = c(A=0, B=1,C=2,D=3),
    avail        = list(A=1, B=1,C=1,D=1),
    choiceVar    = Q7R,
    V            = V)
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
SB3$estimate
summary((dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1  |Q7Bid + Q7Bid2 , data = database,dist="logistic")))$coefficients
summary((dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1  |Q7Bid + Q7Bid2 , data = database,dist="normal")))$coefficients



apollo_modelOutput(SB3,modelOutput_settings = list(printPVal=TRUE))
summary(Treatment_BidOnly2)$coefficients[2]
SB3$estimate["b_bid"]
-SB3$estimate["intercept_b"]/SB3$estimate["b_bid"]
-SB3$estimate["intercept_b"]/SB3$estimate["b_bid2"]


#### Q6Covariates logistic ####
apollo_initialise()
apollo_control = list(modelName  ="Q6Bid-OnlyLogistic", indivID    ="ID")
apollo_beta=c(b_bid    = 0,
              b_Gender=0,
              b_Order=0,
              b_Age=0,
              b_Distance=0,
              b_Trips=0,
              b_Certainty=0,
              b_BP=0,
              b_Charity=0,
              b_Cons=0,
              b_Experts=0,
              b_Education=0,
              b_Employment=0,
              b_Income=0,
              b_Understanding=0,
              b_Timing=0,
              intercept_b=0,
              intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q6Bid)+b_Gender*Q1Gender +b_Order*Order+
                 b_Age*Age + b_Distance*Distance + b_Trips*Trips+
                 b_Certainty*Q6ResearchCertainty + b_BP*BP+
                 b_Charity*Charity + b_Cons * Consequentiality +
                 b_Experts*Experts + b_Education*Education +
                 b_Employment*Employment + b_Income*Income+
                 b_Understanding*Survey + b_Timing*Timing)
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q6ResearchResponse,
    V            = V,
    componentName= "choice")
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB_Covariates = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(SB_Covariates,modelOutput_settings = list(printPVal=TRUE))



#### Q7Covariates logistic ####
apollo_initialise()
apollo_control = list(modelName  ="Q7CovariatesLogistic", indivID    ="ID")
apollo_beta=c(b_bid    = 0,
              b_Gender=0,
              b_Order=0,
              b_Age=0,
              b_Distance=0,
              b_Trips=0,
              b_Certainty=0,
              b_BP=0,
              b_Charity=0,
              b_Cons=0,
              b_Experts=0,
              b_Education=0,
              b_Employment=0,
              b_Income=0,
              b_Understanding=0,
              b_Timing=0,
              intercept_b=0,
              intercept_a=0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q7Bid)+b_Gender*Q1Gender +b_Order*Order+
                 b_Age*Age + b_Distance*Distance + b_Trips*Trips+
                 b_Certainty*Q7TreatmentCertainty + b_BP*BP+
                 b_Charity*Charity + b_Cons * Consequentiality +
                 b_Experts*Experts + b_Education*Education +
                 b_Employment*Employment + b_Income*Income+
                 b_Understanding*Survey + b_Timing*Timing)
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V,
    componentName= "choice")
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

SB_CovariatesQ7 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(SB_CovariatesQ7,modelOutput_settings = list(printPVal=TRUE))


#### CV PROBIT MODELS ####


#### Thijs Dekker suggestion: ####

library(data.table)
Database <- as.data.table(database)
Database <- as.data.table(FullSurvey2)
apollo_beta=c(intercept_a=0, intercept_b = 0,b_bid   = 0)
apollo_fixed = c("intercept_a")
apollo_inputs = apollo_validateInputs()
apollo_attach(apollo_beta, apollo_inputs)
  
loglike=function(apollo_beta)
{
  # needed to be able to refer to parameters by name
  beta1=as.list(apollo_beta)
  attach(beta1)
  # define utility functions
  Database[,U1:=0 + -0.0095790 *0]
  Database[,U2:=0 + -0.0095790 *Q7Bid2Lower]
  Database[,U3:=0 + -0.0095790 *Q7Bid]
  Database[,U4:=0 + -0.0095790 *Q7Bid2Upper]
  Database[,U:=U4-U3-U2-U1] # work in utiliy differences
  LL <- Database[, .(out_LL = sum(log((1-pnorm(-U)))*(Q7R==4)+log(pnorm(-U))*(Q7R==3))), by = ID][["out_LL"]] # log of likelihood function as defined in slide 31
  # remove beta names from memory so as to avoid double attachment of names in next iteration
  detach(beta1)
  return(LL)
}
loglike(apollo_beta)


#### David Palma suggestion: ####

library(apollo)
apollo_initialise()

apollo_control = list(
  modelName  ="BP",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(b1=0, b2=0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= y,
                     V      = b1*x1 + b2*x2,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  #P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))

apollo_modelOutput(model)

#apollo_saveOutput(model, list(saveEst=FALSE, saveCov=FALSE, 
#                              saveCorr=FALSE, saveModelObject=FALSE))


#### Q6-BidOnly-Normal #### 
library(apollo)

apollo_initialise()
apollo_control = list(
  modelName  ="Q6Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(intercept =0,
              b_bid    = 0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + b_bid*Q6Bid,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))

apollo_modelOutput(model,modelOutput_settings = list(printPVal=TRUE))
-model$estimate["intercept"]/model$estimate["b1"]
-model$estimate["intercept"]/model$estimate["b2"]
model$estimate

## Validation:
summary(sbchoice(Q6ResearchResponse ~ 1  |Q6Bid , data = database,dist="normal"))$coefficients

database$Q6Bid <- database$Q6Bid/100
#### Q6-Covariates-Normal Probit ####

apollo_initialise()
apollo_control = list(
  modelName  ="Q6Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(intercept =0,
              b_bid    = 0,
              b_Gender=0,
              b_Order=0,
              b_Age=0,
              b_Distance=0,
              b_Trips=0,
              b_Certainty=0,
              b_BP=0,
              b_Charity=0,
              b_Cons=0,
              b_Experts=0,
              b_Education=0,
              b_Employment=0,
              b_Income=0,
              b_Understanding=0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + b_bid*Q6Bid+
                       b_Gender*Q1Gender +b_Order*Order+
                       b_Age*Age + b_Distance*Distance + b_Trips*Trips+
                       b_Certainty*Q6ResearchCertainty + b_BP*BP+
                       b_Charity*Charity + b_Cons * Consequentiality +
                       b_Experts*Experts + b_Education*Education +
                       b_Employment*Employment + b_Income*Income+
                       b_Understanding*Survey,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))

apollo_modelOutput(model,modelOutput_settings = list(printPVal=TRUE))
-model$estimate["intercept"]/model$estimate["b_bid"]
model$estimate


## Validating using DCchoice:
Validation2 <-sbchoice(Q6ResearchResponse ~ Order + Q1Gender + Age + Distance
                 + Trips +Q6ResearchCertainty + BP + Charity + Consequentiality
                 + Experts + Education + Employment
                 +  Income + Survey  |Q6Bid , data = database,dist="normal")
summary(Validation2)



#### Q7-BidOnly-Normal #### 
library(apollo)

apollo_initialise()
apollo_control = list(
  modelName  ="Q7Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(intercept =0,
              b_bid    = 0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= Q7TreatmentResponse,
                     V      = intercept + b_bid*Q7Bid,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model3 = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))

apollo_modelOutput(model3,modelOutput_settings = list(printPVal=TRUE))
-model$estimate["intercept"]/model$estimate["b1"]
-model$estimate["intercept"]/model$estimate["b2"]
model3$estimate["intercept"]/model3$estimate["b_bid"]
model3$estimate

## Validation:
Validation3 <- (sbchoice(Q7Response ~ 1  |Q7Bid , data = database,dist="normal"))
Validation3$coefficients["(Intercept)"]/Validation3$coefficients["BID"]

Validation3B <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1 |Q7Bid + Q7Bid2, data = Full_Long,dist="logistic")
summary(Validation3B) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
Validation3B$coefficients


#### Q7-BidOnly-Normal Second Bound #### 
library(apollo)

apollo_initialise()
apollo_control = list(
  modelName  ="Q7Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(intercept =0,
              b_bid    = 0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= Q7Response2,
                     V      = intercept + b_bid*Q7Bid2,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model3B = apollo_estimate(apollo_beta, apollo_fixed, 
                         apollo_probabilities, apollo_inputs, 
                         estimate_settings = list(writeIter=FALSE))
model3B$estimate
summary((sbchoice(Q7Response2 ~ 1  |Q7Bid2 , data = database,dist="normal")))$coefficients


#### Q7-Covariates-Normal Probit ####

apollo_initialise()
apollo_control = list(
  modelName  ="Q7Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",
  noValidation=TRUE)

apollo_beta=c(intercept =0,
              b_bid    = 0,
              b_Gender=0,
              b_Order=0,
              b_Age=0,
              b_Distance=0,
              b_Trips=0,
              b_Certainty=0,
              b_BP=0,
              b_Charity=0,
              b_Cons=0,
              b_Experts=0,
              b_Education=0,
              b_Employment=0,
              b_Income=0,
              b_Understanding=0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  
  op_settings = list(outcomeOrdered= Q7TreatmentResponse,
                     V      = intercept + b_bid*Q7Bid+
                       b_Gender*Q1Gender +b_Order*Order+
                       b_Age*Age + b_Distance*Distance + b_Trips*Trips+
                       b_Certainty*Q7TreatmentCertainty + b_BP*BP+
                       b_Charity*Charity + b_Cons * Consequentiality +
                       b_Experts*Experts + b_Education*Education +
                       b_Employment*Employment + b_Income*Income+
                       b_Understanding*Survey,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['model']] = apollo_op(op_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings = list(writeIter=FALSE))

apollo_modelOutput(model,modelOutput_settings = list(printPVal=TRUE))
-model$estimate["intercept"]/model$estimate["b_bid"]
model$estimate


## Validating using DCchoice:
Validation4 <-sbchoice(Q7TreatmentResponse  ~   Order + Q1Gender + Age + Distance
                       + Trips +Q7TreatmentCertainty + BP + Charity + Consequentiality
                       + Experts + Education + Employment
                       +  Income + Survey  |Q7Bid , data = database,dist="normal")
summary(Validation4)$coefficients


#### FAILED: Q7-BidOnly-Normal bivariate #### 


library(apollo)

apollo_initialise()
apollo_control = list(
  modelName  ="Q7Bid",
  modelDescr ="Binary probit using ordered probit",
  indivID    ="ID",noValidation=TRUE)
apollo_beta=c(intercept =0,BID     = 0)
apollo_fixed = c()
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  op_settings = list(outcomeOrdered= Q7R,
                     V      = intercept + BID *Q7OKBR,
                     tau    = list(0,1,2),
                     coding = c(0,1,2,3))
  P[['model']] = apollo_op(op_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model3C = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs, 
                          estimate_settings = list(writeIter=FALSE))
model3C$estimate
summary((dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1  |Q7Bid + Q7Bid2 , data = database,dist="normal")))$coefficients


#### ICLV Models: Logistic Distribution (Logits) ####


#### ICLV Q6 Full Sample ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r

library(apollo)
library(stats)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "ICLVQ6",
  modelDescr = "ICLV modelCVQ6",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4)

apollo_beta = c(b_bid    = 0,
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

apollo_fixed = c()

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

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  V = list()
  V[['A']]  = (b_bid*(Bid_Alt))
  V[['B']]  = (b_bid*(Q6Bid))+ lambda*LV 
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q6ResearchResponse,
    V            = V,
    componentName= "choice")
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Starting value search:
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=20))

### Estimate model
CVModel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVModel,modelOutput_settings = list(printPVal=TRUE))


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




#### ICLV Q6 Truncated ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r

library(apollo)
library(stats)

Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database = Test_Truncated

apollo_initialise()

apollo_control = list(
  modelName  = "ICLV",
  modelDescr = "ICLV model: CV Q6",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4)
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

apollo_fixed = c()

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

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  V = list()
  V[['A']] = ( b_bid_Alt*(Bid_Alt==0) )
  V[['B']] = ( b_bid*(Q6Bid)+ lambda*LV )
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q6ResearchResponse,
    V            = V,
    componentName= "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel2,modelOutput_settings = list(printPVal=TRUE))


#### ICLV Q7 SB Full Sample ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r

Test_Apollo$Q7TreatmentResponse <- Test_Apollo$Q7TreatmentResponse+1
database = Test_Apollo

library(apollo)
library(stats)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "ICLVQ7",
  modelDescr = "ICLV modelCVQ7",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4)

apollo_beta = c(b_bid    = 0,
                intercept_b=0,
                intercept_a=0,
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

apollo_fixed = c("intercept_a")

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

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  V = list()
  V[['A']]  = (intercept_a+ b_bid*(Bid_Alt))
  V[['B']]  = (intercept_b+ b_bid*(Q7Bid)+ lambda*LV )
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V,
    componentName= "choice")
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# Starting value search:
# apollo_beta = apollo_searchStart(apollo_beta,
#                                  apollo_fixed,
#                                  apollo_probabilities,
#                                  apollo_inputs,
#                                  searchStart_settings=list(nCandidates=20))

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


#### ICLV Q7 SB Truncated ####
## http://www.apollochoicemodelling.com/files/Apollo_example_24.r

library(apollo)
library(stats)

Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database = Test_Truncated

apollo_initialise()

apollo_control = list(
  modelName  = "ICLV",
  modelDescr = "ICLV modelCVQ7T",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4)
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

apollo_fixed = c()

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

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  V = list()
  V[['A']] = ( b_bid_Alt*(Bid_Alt==0) )
  V[['B']] = ( b_bid*(Q7Bid)+ lambda*LV )
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Q7TreatmentResponse,
    V            = V,
    componentName= "choice"
  )
  
  ### Compute probabilities for MNL model component
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel7T = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel7T,modelOutput_settings = list(printPVal=TRUE))



#### ICLV: Experimental Random bid parameter ####


database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1

apollo_control = list(
  modelName  = "ICLVQ6T",
  modelDescr = "ICLV modelCVQ6T",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4,
  noValidation=TRUE)


apollo_beta = c(intercept =0,mu_bid    = 0,sig_bid=0,
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta","draws_price"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  randcoeff[["b_bid"]] = -exp(mu_bid+sig_bid* draws_price)
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + b_bid*Q6Bid+lambda*LV,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel6NTrial = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel6NTrial,modelOutput_settings = list(printPVal=TRUE))


#### ICLV Models: Normal Distribution (Probits) ####


#### CHOSEN: Q6 Normal ICLV: CVmodel6N ####


database$Q6Bid <- database$Q6Bid/100
apollo_control = list(
  modelName  = "ICLVQ6",
  modelDescr = "ICLV modelCVQ6",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4,
  noValidation=TRUE)


apollo_beta = c(intercept =0,b_bid    = 0,
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + b_bid*Q6Bid+lambda*LV,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel6N = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel6N,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CVmodel6N,"CVmodel6N.rds")

#### CHOSEN: Q7 Normal SBDC ICLV CVmodel6NT ####

# Setup:
database$Q7Bid <- database$Q7Bid/100
database$Q7TreatmentResponse <- database$Q7TreatmentResponse-1


apollo_control = list(
  modelName  = "ICLVQ7",
  modelDescr = "ICLV modelCVQ7",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4,
  noValidation=TRUE)


apollo_beta = c(intercept =0,b_bid    = 0,
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q7TreatmentCertainty + eta
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q7TreatmentResponse,
                     V      = intercept + b_bid*Q7Bid+lambda*LV,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel7N = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel7N,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CVmodel7N,"CVmodel7N.rds")


####  CHOSEN: Q6 Normal ICLV Truncated CVmodel7N ####


# Setup the data for all truncated models:
database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1


# Estimate model:
apollo_control = list(
  modelName  = "ICLVQ6T",
  modelDescr = "ICLV modelCVQ6T",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4,
  noValidation=TRUE)


apollo_beta = c(intercept =0,b_bid    = 0,
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
apollo_fixed = c()


apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta"))


apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + b_bid*Q6Bid+lambda*LV,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel6NT = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel6NT,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CVmodel6NT,"CVmodel6NT.rds")


#### CHOSEN: Q7 Normal SBDC ICLV Truncated CVmodel7NT ####


apollo_control = list(
  modelName  = "ICLVQ7",
  modelDescr = "ICLV modelCVQ7T",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4,
  noValidation=TRUE)


apollo_beta = c(intercept =0,b_bid    = 0,
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton",interNDraws=1000,          
  interUnifDraws=c(),interNormDraws=c("eta"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q7TreatmentCertainty + eta
  return(randcoeff)
}


apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  op_settings = list(outcomeOrdered= Q7TreatmentResponse,
                     V      = intercept + b_bid*Q7Bid+lambda*LV,
                     tau    = list(-100,0),
                     coding = c(-1,0,1))
  P[['choice']] = apollo_op(op_settings, functionality)
  # P = apollo_panelProd(P, apollo_inputs, functionality)
  # P = apollo_prepareProb(P, apollo_inputs, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CVmodel7NT = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel7NT,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CVmodel7NT,"CVmodel7NT.rds")


#### Prediction Accuracy #### 


# Q6 ICLV TRUNCATED Prediction accuracy
CE1_Predictions <- data.frame(CE3$avgCP) ## Getting probabilities of choosing each option from the model
CE1_Predictions[CE1_Predictions$CE3.avgCP < 0.5,] <- 0
CE1_Predictions[CE1_Predictions$CE3.avgCP >= 0.5,] <- 1
CE1_Predictions <- cbind("Actual"=data.frame(Fulls2$Choice),"Predicted"=slice(data.frame(CE1_Predictions$CE3.avgCP),rep(1:n(),each=4)))
CE1_Predictions$Match <- ifelse(CE1_Predictions$Fulls2.Choice==CE1_Predictions$CE1_Predictions.CE3.avgCP,1,0)
round(100/length(CE1_Predictions$Match)*length(CE1_Predictions$Match[CE1_Predictions$Match==0]),3)
# 54.035
round(100/length(CE1_Predictions$Match)*length(CE1_Predictions$Match[CE1_Predictions$Match==1]),3)
# 45.965


# Q6 ICLV FULL Prediction accuracy
CVQ61_Predictions <- data.frame(CVmodel6N$avgCP) ## Getting probabilities of choosing each option from the model
CVQ61_Predictions[CVQ61_Predictions$CVmodel6N.avgCP < 0.5,] <- 0
CVQ61_Predictions[CVQ61_Predictions$CVmodel6N.avgCP >= 0.5,] <- 1
CVQ61_Predictions <- cbind("Actual"=data.frame(Fulls$Choice),"Predicted"=slice(data.frame(CVQ61_Predictions$CVmodel6N.avgCP),rep(1:n(),each=4)))
CVQ61_Predictions$Match <- ifelse(CVQ61_Predictions$Fulls.Choice==CVQ61_Predictions$CVQ61_Predictions.CVmodel6N.avgCP,1,0)
round(100/length(CVQ61_Predictions$Match)*length(CVQ61_Predictions$Match[CVQ61_Predictions$Match==0]),3)
# 55.522
round(100/length(CVQ61_Predictions$Match)*length(CVQ61_Predictions$Match[CVQ61_Predictions$Match==1]),3)
# 44.478


# Q6 ICLV TRUNCATED Prediction accuracy
CVQ6_Predictions <- data.frame(CVmodel6NT$avgCP) ## Getting probabilities of choosing each option from the model
CVQ6_Predictions[CVQ6_Predictions$CVmodel6NT.avgCP < 0.5,] <- 0
CVQ6_Predictions[CVQ6_Predictions$CVmodel6NT.avgCP >= 0.5,] <- 1
CVQ6_Predictions <- cbind("Actual"=data.frame(Fulls2$Choice),"Predicted"=slice(data.frame(CVQ6_Predictions$CVmodel6NT.avgCP),rep(1:n(),each=4)))
CVQ6_Predictions$Match <- ifelse(CVQ6_Predictions$Fulls2.Choice==CVQ6_Predictions$CVQ6_Predictions.CVmodel6NT.avgCP,1,0)
round(100/length(CVQ6_Predictions$Match)*length(CVQ6_Predictions$Match[CVQ6_Predictions$Match==0]),3)
# 54.035
round(100/length(CVQ6_Predictions$Match)*length(CVQ6_Predictions$Match[CVQ6_Predictions$Match==1]),3)
# 45.965


# Q7 SB ICLV FULL Prediction accuracy
CVQ71_Predictions <- data.frame(CVmodel7N$avgCP) ## Getting probabilities of choosing each option from the model
CVQ71_Predictions[CVQ71_Predictions$CVmodel7N.avgCP < 0.5,] <- 0
CVQ71_Predictions[CVQ71_Predictions$CVmodel7N.avgCP >= 0.5,] <- 1
CVQ71_Predictions <- cbind("Actual"=data.frame(Fulls$Choice),"Predicted"=slice(data.frame(CVQ71_Predictions$CVmodel7N.avgCP),rep(1:n(),each=4)))
CVQ71_Predictions$Match <- ifelse(CVQ71_Predictions$Fulls.Choice==CVQ71_Predictions$CVQ71_Predictions,1,0)
round(100/length(CVQ71_Predictions$Match)*length(CVQ71_Predictions$Match[CVQ71_Predictions$Match==0]),3)
# 52.985
round(100/length(CVQ71_Predictions$Match)*length(CVQ71_Predictions$Match[CVQ71_Predictions$Match==1]),3)
# 47.015


# Q7 SB ICLV TRUNCATED Prediction accuracy
CVQ7_Predictions <- data.frame(CVmodel7NT$avgCP) ## Getting probabilities of choosing each option from the model
CVQ7_Predictions[CVQ7_Predictions$CVmodel7NT.avgCP < 0.5,] <- 0
CVQ7_Predictions[CVQ7_Predictions$CVmodel7NT.avgCP >= 0.5,] <- 1
CVQ7_Predictions <- cbind("Actual"=data.frame(Fulls2$Choice),"Predicted"=slice(data.frame(CVQ7_Predictions$CVmodel7NT.avgCP),rep(1:n(),each=4)))
CVQ7_Predictions$Match <- ifelse(CVQ7_Predictions$Fulls2.Choice==CVQ7_Predictions$CVQ7_Predictions.CVmodel7NT.avgCP,1,0)
round(100/length(CVQ7_Predictions$Match)*length(CVQ7_Predictions$Match[CVQ7_Predictions$Match==0]),3)
# 52.281
round(100/length(CVQ7_Predictions$Match)*length(CVQ7_Predictions$Match[CVQ7_Predictions$Match==1]),3)
# 47.719


#### CV WTP:  ####

## Get the unconditional/conditional values of parameters: 
CVunconditionals <- apollo_unconditionals(CVmodel6NT,apollo_probabilities,apollo_inputs)
CVconditionals <- apollo_conditionals(CVmodel6NT,apollo_probabilities,apollo_inputs)


median((CVmodel6NT$estimate["intercept"]+CVunconditionals$LV/CVunconditionals$b_bid))

# This approach works for the CE so maybe for the CV:
median(-CVmodel6NT$estimate["intercept"]+(CVunconditionals$LV)/CVmodel6NT$estimate["b_bid"])
median((CVunconditionals$b_Performance+CVunconditionals$LV/CVunconditionals$b_Price))
median((CVunconditionals$b_Emission+CVunconditionals$LV/CVunconditionals$b_Price))


# To validate with DCchoice package:
summary(sbchoice(Q6ResearchResponse ~ 1  |Q6Bid , data = database,dist="normal"))