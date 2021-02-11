#### Survey data analysis script: All ICLVs  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: This script estimates all the Choice Experiment ICLV models in one place 
# Note: Very long and repetitive, again many Apollo-specific things are uncommented given the extensive guidance in the Manual


#### CE Models ####


#### CEmodelMNL ####

library(apollo)

### Initialise code
database <- Test_Apollo
apollo_initialise()


### Set core controls
apollo_control = list(
  modelName  = "CEmodelMNL", modelDescr = "CEmodelMNL",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,b_Emission     = 0, 
                b_Performance       = 0, 
                b_Price            = 0,  
                lambda             = 1, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
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


apollo_fixed = c("asc_A")

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
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income +  gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
  
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
  V[['A']] = asc_A + ( b_Emission*(Emission_A==0) + b_Performance*(Performance_A==0)+ b_Price*Price_A )
  V[['B']] = asc_B + ( b_Emission*(Emission_B)+ b_Performance*(Performance_B)+ b_Price*Price_B + lambda*LV )
  
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

# Starting value search:
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=20))

## Optional: calculate LL before model estimation
apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)


### Estimate model
CEmodelMNL = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodelMNL,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CEmodelMNL, file="CEmodelMNL.rds")
# Using the unconditional distributions of the parameters to calculate Performance and Emissions MWTP respectively
CEmodelMNLunconditionals <- apollo_unconditionals(CEmodelMNL,apollo_probabilities,apollo_inputs)
round(median((CEmodelMNL$estimate["b_Performance"]+CEmodelMNLunconditionals$LV/CEmodelMNL$estimate["b_Price"])),3)
round(median((CEmodelMNL$estimate["b_Emission"]+CEmodelMNLunconditionals$LV/CEmodelMNL$estimate["b_Price"])),3)



#### CEmodelMNLTrunc ####

library(apollo)

database <- Test_Truncated
apollo_initialise()


### Set core controls
apollo_control = list(
  modelName  = "CEmodelMNLTrunc", modelDescr = "CEmodelMNLTrunc",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,b_Emission     = 0, 
                b_Performance       = 0, 
                b_Price            = 0,  
                lambda             = 1, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
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

apollo_fixed = c("asc_A")

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
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income +  gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
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
  V[['A']] = asc_A + ( b_Emission*(Emission_A==0) + b_Performance*(Performance_A==0)+ b_Price*Price_A )
  V[['B']] = asc_B + ( b_Emission*(Emission_B)+ b_Performance*(Performance_B)+ b_Price*Price_B + lambda*LV )
  
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


### Estimate model
CEmodelMNLTrunc = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodelMNLTrunc,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CEmodelMNLTrunc, file="CEmodelMNLTrunc.rds")
# Using the unconditional distributions of the parameters to calculate Performance and Emissions MWTP respectively
CEmodelMNLTruncLunconditionals <- apollo_unconditionals(CEmodelMNLTrunc,apollo_probabilities,apollo_inputs)
round(median((CEmodelMNLTrunc$estimate["b_Performance"]+CEmodelMNLTruncLunconditionals$LV/CEmodelMNLTrunc$estimate["b_Price"])),3)
round(median((CEmodelMNLTrunc$estimate["b_Emission"]+CEmodelMNLTruncLunconditionals$LV/CEmodelMNLTrunc$estimate["b_Price"])),3)



#### CE ICLV 2 MXL WTP space: CHOSEN ICLVFULL ####

library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  = "ICLV model: CE2",
  modelDescr = "ICLV model: CE2",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4
)

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0, 
                lambda             = 1, 
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
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"), 
  intraDrawsType='',
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Employment*Employment + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  
  return(randcoeff)
}

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
  P[["indic_Q13"]] = apollo_op(op_settings1, functionality)
  P[["indic_Q14"]] = apollo_op(op_settings2, functionality)
  P[["indic_Q15"]] = apollo_op(op_settings3, functionality)
  
  ### Likelihood of choices
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']] = b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B+ lambda*LV )
  
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

# Starting value search:
apollo_beta = apollo_searchStart(apollo_beta,
                                 apollo_fixed,
                                 apollo_probabilities,
                                 apollo_inputs,
                                 searchStart_settings=list(nCandidates=20))

## Optional: calculate LL before model estimation
apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

### Estimate model
CEmodel2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodel2,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(CEmodel2)
saveRDS(CEmodel2, file="CE2.rds")
# readRDS("CE2.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(CE2,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
Full_Final <- cbind(Full_Full,"LV"=slice(data.frame(unconditionals$LV[,2]),rep(1:n(), each = 8)))
ggplot(Full_Final, aes(x=Full_Final$unconditionals.LV...2., y=Q7WTP)) + geom_point()




#### CE ICLV 2 MXL WTP space truncated: Previous ####

library(apollo)

Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database = Test_Truncated
apollo_initialise()

apollo_control = list(
  modelName  = "ICLV model: CE2",
  modelDescr = "ICLV model: CE2",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

apollo_beta = c(mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0, 
                lambda             = 1, 
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Employment*Employment + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
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
  V[['A']] = b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B+ lambda*LV )
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V,
    componentName= "choice")
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# # Starting value search:
# apollo_beta = apollo_searchStart(apollo_beta,
#                                  apollo_fixed,
#                                  apollo_probabilities,
#                                  apollo_inputs,
#                                  searchStart_settings=list(nCandidates=20))
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

CEmodel3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodel3,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(CEmodel3)
saveRDS(CEmodel3, file="CE3.rds")
# readRDS("CE3.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(CE3,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)



#### CEModel4: CHOSEN ICLVTRUNC ####

library(apollo)

Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database = Test_Truncated
apollo_initialise()


apollo_control = list(
  modelName  = "ICLVTrunc",
  modelDescr = "ICLVTrunc",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0, 
                lambda             = 1, 
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
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

apollo_fixed = c("asc_A")

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income+ gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  randcoeff[["asc_B"]] = asc_BB
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
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B+ lambda*LV )
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V,
    componentName= "choice")
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# # Starting value search:
# apollo_beta = apollo_searchStart(apollo_beta,
#                                  apollo_fixed,
#                                  apollo_probabilities,
#                                  apollo_inputs,
#                                  searchStart_settings=list(nCandidates=20))
# apollo_llCalc(apollo_beta, apollo_probabilities, apollo_inputs)

CEmodel4 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodel4,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(CEmodel4)
saveRDS(CEmodel4,"CEmodel4.rds")


#### CE WTP ####
# Using the unconditional distributions of the parameters to calculate Performance and Emissions MWTP respectively
unconditionals2 <- apollo_unconditionals(ICLVFull,apollo_probabilities,apollo_inputs)
median((unconditionals2$b_Performance+unconditionals2$LV/unconditionals2$b_Price))
median((unconditionals2$b_Emission+unconditionals2$LV/unconditionals2$b_Price))

# Same, but withe (old) truncated model:
unconditionals2 <- apollo_unconditionals(ICLVTrunc,apollo_probabilities,apollo_inputs)
conditionals2 <- apollo_conditionals(ICLVTrunc,apollo_probabilities,apollo_inputs)
median((unconditionals2$b_Performance+unconditionals2$LV/unconditionals2$b_Price))
median((unconditionals2$b_Emission+unconditionals2$LV/unconditionals2$b_Price))


## Here I copy and paste the working parts of the apollo_unconditionals code
## This is because it would not work normally for the new truncated model; it did not like adding in the ASC
CEmodel4 <- readRDS("CEmodel4.rds")
CEmodel4 <- ICLVTrunc
apollo_beta = CEmodel4 $estimate
apollo_fixed = CEmodel4 $apollo_fixed
apollo_compareInputs(apollo_inputs)
apollo_control = apollo_inputs[["apollo_control"]]
database = apollo_inputs[["database"]]
draws = apollo_inputs[["draws"]]
apollo_randCoeff = apollo_inputs[["apollo_randCoeff"]]
apollo_draws = apollo_inputs[["apollo_draws"]]
apollo_lcPars = apollo_inputs[["apollo_lcPars"]]
apollo_checkArguments(apollo_probabilities, apollo_randCoeff, 
                      apollo_lcPars)
env <- list2env(c(as.list(apollo_beta), apollo_inputs$database, 
                  apollo_inputs$draws), hash = TRUE, parent = parent.frame())
environment(apollo_randCoeff) <- env
randcoeff <- apollo_randCoeff(apollo_beta, apollo_inputs)
if (any(sapply(randcoeff, is.function))) {
  randcoeff = lapply(randcoeff, function(f) if (is.function(f)) {
    environment(f) <- env
    return(f())
  }
  else {
    return(f)
  })
}
if (apollo_draws$intraNDraws == 0) {
  nObsPerIndiv <- as.vector(table(database[, apollo_control$indivID]))
  nIndiv <- length(nObsPerIndiv)
  firstRows <- rep(1, nIndiv)
  for (i in 2:nIndiv) firstRows[i] <- firstRows[i - 1] + 
    nObsPerIndiv[i - 1]
  j = 1
  for (j in 1:length(randcoeff)) {
    randcoeff[[j]] = randcoeff[[j]][firstRows ]
  }
}

median((randcoeff$b_Performance+randcoeff$LV/randcoeff$b_Price))
median((randcoeff$b_Emission+randcoeff$LV/randcoeff$b_Price))


# CE ICLV FULL Prediction accuracy
CE_Predictions <- data.frame(CE2$avgCP) ## Getting probabilities of choosing each option from the model
CE_Predictions[CE_Predictions$CE2.avgCP < 0.5,] <- 0
CE_Predictions[CE_Predictions$CE2.avgCP >= 0.5,] <- 1
CE_Predictions <- cbind("Actual"=data.frame(Fulls$Choice),"Predicted"=slice(data.frame(CE_Predictions$CE2.avgCP),rep(1:n(),each=4)))
CE_Predictions$Match <- ifelse(CE_Predictions$Fulls.Choice==CE_Predictions$CE_Predictions.CE2.avgCP,1,0)
round(100/length(CE_Predictions$Match)*length(CE_Predictions$Match[CE_Predictions$Match==0]),3)
# 55.522
round(100/length(CE_Predictions$Match)*length(CE_Predictions$Match[CE_Predictions$Match==1]),3)
# 44.478
