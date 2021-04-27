#### Survey data analysis script: All in one  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Is Precaution Motivated More By Regret-Minimisation or Utility-Maximisation?

#### Upload Previous Models ####


RRM2010 <- readRDS("RRM2010.rds") ## Original RRM
PRRRM <- readRDS("PRRRM.rds") ## PRRM Model
GRRM <- readRDS("GRRM.rds") ## G-RRM Model
MURRM <- readRDS("MURRM.rds") ## Mu-RRM Model

##  ICLV Models:
CEmodelMNL <- readRDS("CEmodelMNL.rds")
CEmodel2 <- readRDS("CE2.rds")

##  Hybrid-ICLV Models:
IRRLV2010 <- readRDS("IRRLV2010.rds") ##  Hybrid with 2010 RRM
IRRLVPRRM <- readRDS("IRRLVPRRMF.rds") ##  Hybrid with PRRM
IRRLVGRRM <- readRDS("IRRLVGRRMF.rds") ##  Hybrid with G-RRM
IRRLVMURRM <- readRDS("IRRLVMURRMF.rds") ##  Hybrid with Mu-RRM



# To run this you MUST first run everything in the model itself except the estimation, otherwise uses the wrong inputs:
CEWTP  = function(Model){
  CEmodel4 <- Model # Enter requisite model here
  apollo_beta = CEmodel4$estimate
  apollo_fixed = CEmodel4$apollo_fixed
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
  return(randcoeff)
  # # This WTP here is fit for reporting in text:
  # print(paste0("Performance:",(round(median((randcoeff$b_Performance+randcoeff$LV/randcoeff$b_Price)),3))))
  # print(paste0("Emission:",(round(median((randcoeff$b_Emission+randcoeff$LV/randcoeff$b_Price)),3))))
}
randcoeff <- CEWTP(CEmodelMNL)


#### Model1: RRM2010 ####


library(apollo)
apollo_initialise()

apollo_control = list(modelName  ="RRM_2010",modelDescr ="RRM 2010 model",indivID    ="ID")
database <- Test_Apollo

apollo_beta=c(B_Performance = 0,B_Emission = 0,B_Price  = 0)
apollo_fixed = c()

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ###Attaches parameters and data so that variables can be referred by name
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Pairwise comparison and scale of attribues
  Performance_1 = ( Performance_B - Performance_A )
  Performance_2 = ( Performance_A - Performance_B )
  
  Emission_1 = ( Emission_B - Emission_A ) 
  Emission_2 = ( Emission_A - Emission_B )
  
  Price_1 = ( Price_B - Price_A )
  Price_2 = ( Price_A - Price_B )
  
  ### List of regret functions
  R = list()
  R[['A']]  =  - log( 1 + exp( B_Performance  * Performance_1 ) ) - log( 1 + exp( B_Emission * Emission_1 ) - log( 1 + exp( B_Price * Price_1 )  ) )
  R[['B']]  =  - log( 1 + exp( B_Performance  * Performance_2 ) ) - log( 1 + exp( B_Emission * Emission_2 ) - log( 1 + exp( B_Price * Price_2 )  ) )
  
  
  ###Input for calculating MNL probabilities
  mnl_settings = list(
    alternatives  = c(A=1, B=2), 
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = R
  )
  
  
  ###Calculating probabilities based on MNL function
  P = list()
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)  # Multiply likelihood of observations from the same individual
  P = apollo_prepareProb(P, apollo_inputs, functionality)# Check that probabilities are in the appropiate format to be returned.
  return(P)
}


###Estimate the model                        
RRM2010 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
###Display the output to console with p-values
apollo_modelOutput(RRM2010,modelOutput_settings=list(printPVal=TRUE))

saveRDS(RRM2010,"RRM2010.rds")
RRM2010 <- readRDS("RRM2010.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(RRM2010,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)

apollo_deltaMethod(RRM2010, deltaMethod_settings=list(operation="ratio", parName1="B_Performance", parName2="B_Price"))
apollo_deltaMethod(RRM2010, deltaMethod_settings=list(operation="ratio", parName1="B_Emission", parName2="B_Price"))


round(- RRM2010$estimate["B_Performance"]/(1+1/exp(RRM2010$estimate["B_Performance"]))/- RRM2010$estimate["B_Price"]/(1+1/exp(RRM2010$estimate["B_Price"])),3)
round(- RRM2010$estimate["B_Emission"]/(1+1/exp(RRM2010$estimate["B_Emission"]))/- RRM2010$estimate["B_Price"]/(1+1/exp(RRM2010$estimate["B_Price"])),3)


#### Model2: PRRM ####


library(apollo)

database <- Test_Apollo
apollo_initialise()

###Options controlling the running of the code
apollo_control = list(
  modelName  ="P_RRM",
  modelDescr ="P-RRM model",
  indivID    ="ID"
)

# ###Reading in database
# database <- read.delim("Shopping_data_with_headers.dat",header=TRUE)

###Parameters to be estimated and their starting values
apollo_beta=c(B_Performance = 0,
              B_Emission = 0,
              B_Price  = 0)

###Fixed parameters: should be in quotes
apollo_fixed = c()
###Search the user work space for all necessary input 
apollo_inputs = apollo_validateInputs()

###Probability function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ###Attaches parameters and data so that variables can be referred by name
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  
  
  P_PerformanceA =  ( pmax( 0 , Performance_B - Performance_A ) )
  P_PerformanceB =  ( pmax( 0 , Performance_A - Performance_B ) )

  P_EmissionA = ( pmax( 0 , Emission_B - Emission_A ) )
  P_EmissionB = ( pmax( 0 , Emission_A - Emission_B ) )

  P_PriceA = ( pmin( 0 , Price_B - Price_A )  )
  P_PriceB = ( pmin( 0 , Price_A - Price_B )  )

  
  ### List of regret functions
  R = list()
  R[['A']]  =  B_Performance * P_PerformanceA + B_Emission * P_EmissionA + B_Price * P_PriceA
  R[['B']]  =  B_Performance * P_PerformanceB + B_Emission * P_EmissionB + B_Price * P_PriceB
  # R[['Alt3']]  =  B_FSG * P_FSG3 + B_FSO * P_FSO3 + B_TT * P_TT3
  # R[['Alt4']]  =  B_FSG * P_FSG4 + B_FSO * P_FSO4 + B_TT * P_TT4
  # R[['Alt5']]  =  B_FSG * P_FSG5 + B_FSO * P_FSO5 + B_TT * P_TT5
  
  ###Input for calculating MNL probabilities
  mnl_settings = list(
    alternatives  = c(A=1, B=2), 
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = R
  )
  
  ###Calculating probabilities based on MNL function
  P = list()
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)  # Multiply likelihood of observations from the same individual
  P = apollo_prepareProb(P, apollo_inputs, functionality)# Check that probabilities are in the appropiate format to be returned.
  return(P)
}

###Estimate the model                         
PRRRM = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(PRRRM,modelOutput_settings=list(printPVal=TRUE))
saveRDS(PRRRM,"PRRRM.rds")
PRRRM <- readRDS("PRRRM.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(PRRRM,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)

apollo_deltaMethod(PRRRM, deltaMethod_settings=list(operation="ratio", parName1="B_Performance", parName2="B_Price"))
apollo_deltaMethod(PRRRM, deltaMethod_settings=list(operation="ratio", parName1="B_Emission", parName2="B_Price"))

round(- PRRRM$estimate["B_Performance"]/(1+1/exp(PRRRM$estimate["B_Performance"]))/- PRRRM$estimate["B_Price"]/(1+1/exp(PRRRM$estimate["B_Price"])),3)
round(- PRRRM$estimate["B_Emission"]/(1+1/exp(PRRRM$estimate["B_Emission"]))/- PRRRM$estimate["B_Price"]/(1+1/exp(PRRRM$estimate["B_Price"])),3)


#### Model3: GRRM ####


library(apollo)

###Preparing environment
apollo_initialise()

###Options controlling the running of the code
apollo_control = list(
  modelName  ="GRRM",
  modelDescr ="GRRM model",
  indivID    ="ID"
)

###Reading in database
database <- Test_Apollo

###Parameters to be estimated and their starting values
apollo_beta=c(B_Performance = 0,
              B_Emission = 0,
              B_Price  = 0,
              gamma = 0)

###Fixed parameters: should be in quotes 
apollo_fixed = c()

###Search the user work space for all necessary input 
apollo_inputs = apollo_validateInputs()


###Probability function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ###Attaches parameters and data so that variables can be referred by name
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Pairwise comparison and scale of attribues
  
  ### Floorspace Groceries
  Performance_1 = ( Performance_B - Performance_A ) 
  Performance_2 = ( Performance_A - Performance_B )

  ### Floorspace Other
  Emission_1 = ( Emission_B - Emission_A )
  Emission_2 = ( Emission_A - Emission_B )

    ### Travel time
  Price_1 = ( Price_B - Price_A ) 
  Price_2 = ( Price_A - Price_B ) 
  
  ### List of regret functions
  R = list()
  R[['A']]  =  - log( gamma + exp( B_Performance * Performance_1 ) ) - log( gamma + exp( B_Emission * Emission_1 ) ) - log( gamma + exp( B_Price * Price_1 ) ) 
  R[['B']]  =  - log( gamma + exp( B_Performance * Performance_2 ) ) - log( gamma + exp( B_Emission * Emission_2 ) ) - log( gamma + exp( B_Price * Price_2 ) ) 
  
  
  ###Input for calculating MNL probabilities
  mnl_settings = list(
    alternatives  = c(A=1, B=2), 
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = R
  )
  
  ###Calculating probabilities based on MNL function
  P = list()
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)  # Multiply likelihood of observations from the same individual
  P = apollo_prepareProb(P, apollo_inputs, functionality)# Check that probabilities are in the appropiate format to be returned.
  return(P)
  
  
}

###Estimate the model  
###In estimate_settings we set the constraint for gamma not to be higher than 1
###See ?maxBFGS help file for details on how to construct constraints
GRRM = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, estimate_settings = 
                          list("constraints"=list(ineqA=matrix(c(0,0,0,-1), nrow=1), ineqB=matrix(1))))

###Display the output to console with p-values
apollo_modelOutput(GRRM,modelOutput_settings=list(printPVal=TRUE))

saveRDS(GRRM,"GRRM.rds")
GRRM <- readRDS("GRRM.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(GRRM,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)

apollo_deltaMethod(GRRM, deltaMethod_settings=list(operation="ratio", parName1="B_Performance", parName2="B_Price"))
apollo_deltaMethod(GRRM, deltaMethod_settings=list(operation="ratio", parName1="B_Emission", parName2="B_Price"))

round(- GRRM$estimate["B_Performance"]/(1+1/exp(GRRM$estimate["B_Performance"]))/- GRRM$estimate["B_Price"]/(1+1/exp(GRRM$estimate["B_Price"])),3)
round(- GRRM$estimate["B_Emission"]/(1+1/exp(GRRM$estimate["B_Emission"]))/- GRRM$estimate["B_Price"]/(1+1/exp(GRRM$estimate["B_Price"])),3)

#### Model4: MURRM ####

library(apollo)

###Preparing environment
apollo_initialise()

###Options controlling the running of the code
apollo_control = list(
  modelName  ="muRRM",
  modelDescr ="muRRM model",
  indivID    ="ID"
)


###Reading in database
database <- Test_Apollo

###Parameters to be estimated and their starting values
apollo_beta=c(B_Performance = 0,
              B_Emission = 0,
              B_Price  = 0,
              mu    = 1)
###Fixed parameters: should be in quotes 
apollo_fixed = c()

###Search the user work space for all necessary input 
apollo_inputs = apollo_validateInputs()


###Probability function
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ###Attaches parameters and data so that variables can be referred by name
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Pairwise comparison and scale of attribues
  Performance_1 = ( Performance_B - Performance_A )
  Performance_2 = ( Performance_A - Performance_B )
  
  Emission_1 = ( Emission_B - Emission_A ) 
  Emission_2 = ( Emission_A - Emission_B )

  Price_1 = ( Price_B - Price_A )
  Price_2 = ( Price_A - Price_B )

  ### List of regret functions
  R = list()
  R[['A']]  =  mu * ( - log( 1 + exp( ( B_Performance  ) * Performance_1 ) ) - log( 1 + exp( ( B_Emission  ) * Emission_1 ) ) - log( 1 + exp( ( B_Price ) * Price_1 )  ) )
  R[['B']]  =  mu * ( - log( 1 + exp( ( B_Performance  ) * Performance_2 ) ) - log( 1 + exp( ( B_Emission  ) * Emission_2 ) ) - log( 1 + exp( ( B_Price ) * Price_2 )  ) )
 
  
  ###Input for calculating MNL probabilities
  mnl_settings = list(
    alternatives  = c(A=1, B=2), 
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = R
  )
  
  
  ###Calculating probabilities based on MNL function
  P = list()
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)  # Multiply likelihood of observations from the same individual
  P = apollo_prepareProb(P, apollo_inputs, functionality)# Check that probabilities are in the appropiate format to be returned.
  return(P)
}


###Estimate the model                     
MURRM = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

###Display the output to console with p-values
apollo_modelOutput(MURRM,modelOutput_settings=list(printPVal=TRUE))

saveRDS(MURRM,"MURRM.rds")
MURRM <- readRDS("MURRM.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(MURRM,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)

apollo_deltaMethod(MURRM, deltaMethod_settings=list(operation="ratio", parName1="B_Performance", parName2="B_Price"))
apollo_deltaMethod(MURRM, deltaMethod_settings=list(operation="ratio", parName1="B_Emission", parName2="B_Price"))

round(- MURRM$estimate["B_Performance"]/(1+1/exp(MURRM$estimate["B_Performance"]))/- MURRM$estimate["B_Price"]/(1+1/exp(MURRM$estimate["B_Price"])),3)
round(- MURRM$estimate["B_Emission"]/(1+1/exp(MURRM$estimate["B_Emission"]))/- MURRM$estimate["B_Price"]/(1+1/exp(MURRM$estimate["B_Price"])),3)


#### Model5: ICLV-RRM2010 #### 
library(apollo)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "IRRLV2010F",
  modelDescr = "IRRLV2010F",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

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

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
  randcoeff[["Performance_1"]] =   ( Performance_B - Performance_A ) 
  randcoeff[["Performance_2"]] =   ( Performance_A - Performance_B ) 
  randcoeff[["Emission_1"]] =  ( Emission_B - Emission_A ) 
  randcoeff[["Emission_2"]] =  ( Emission_A - Emission_B ) 
  randcoeff[["Price_1"]] =  ( Price_B - Price_A ) 
  randcoeff[["Price_2"]] =  ( Price_A - Price_B ) 
  
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
  
  R = list()
  R[['A']]  =  asc_A- log( 1 + exp( b_Performance  * Performance_1 ) ) - log( 1 + exp( b_Emission * Emission_1 ) - log( 1 + exp( b_Price * Price_1 )  ) )
  R[['B']]  =   asc_B + lambda*LV- log( 1 + exp( b_Performance  * Performance_2 ) ) - log( 1 + exp( b_Emission * Emission_2 ) - log( 1 + exp( b_Price * Price_2 )  ) )
  
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1),
    componentName= "choice")
  
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


IRRLV2010F = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(IRRLV2010F,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = IRRLV2010F,"IRRLV2010F.rds")

print(paste0("Performance:",(round(median((IRRLV2010$estimate["b_Performance"]/IRRLV2010$estimate["b_Price"]+randcoeff$LV)),3))))
print(paste0("Emission:",(round(median((IRRLV2010$estimate["b_Emission"]/IRRLV2010$estimate["b_Price"]+randcoeff$LV)),3))))

IRRLV2010 <- readRDS("IRRLV2010.rds")
D5 <- data.frame(apollo_modelOutput(IRRLV2010,modelOutput_settings = list(printPVal=TRUE)))
D5.1 <- cbind(D5$Estimate,D5$p.1.sided.)
rownames(D5.1) <- row.names(D5)
round(D5.1,3)
xtable::xtable(round(D5.1,3),digits=3)

randcoeff <- CEWTP(IRRLV2010)
median(round(- IRRLV2010$estimate["b_Performance"]/(1+1/exp(IRRLV2010$estimate["b_Performance"]))+median(randcoeff$LV)/- IRRLV2010$estimate["b_Price"]/(1+1/exp(IRRLV2010$estimate["b_Price"])),3))
median(round(- IRRLV2010$estimate["b_Emission"]/(1+1/exp(IRRLV2010$estimate["b_Emission"]))+randcoeff$LV/- IRRLV2010$estimate["b_Price"]/(1+1/exp(IRRLV2010$estimate["b_Price"])),3))



#### Model6: ICLV-PRRM ####
## https://www.advancedrrmmodels.com/latent-class-models


library(apollo)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "IRRLVPRRMF",
  modelDescr = "IRRLVPRRMF",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

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

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
  randcoeff[["P_PerformanceA"]] =  ( pmax( 0 , Performance_B - Performance_A ) )
  randcoeff[["P_PerformanceB"]] =  ( pmax( 0 , Performance_A - Performance_B ) )
  randcoeff[["P_EmissionA"]] = ( pmax( 0 , Emission_B - Emission_A ) )
  randcoeff[["P_EmissionB"]] = ( pmax( 0 , Emission_A - Emission_B ) )
  randcoeff[["P_PriceA"]] = ( pmin( 0 , Price_B - Price_A )  )
  randcoeff[["P_PriceB"]] = ( pmin( 0 , Price_A - Price_B )  )
  
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
  
  R = list()
  R[['A']]  = asc_A  + 
    (b_Price*P_PriceA) + 
    (b_Performance*P_PerformanceA) + 
    (b_Emission*P_EmissionA) 
  R[['B']]  = asc_B  + 
    (b_Price*P_PriceB) + 
    (b_Performance*P_PerformanceB) + 
    (b_Emission*P_EmissionB) + lambda*LV 
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1),
    componentName= "choice")
  
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


IRRLVPRRMF = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(IRRLVPRRMF,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = IRRLVPRRMF,"IRRLVPRRMF.rds")

print(paste0("Performance:",(round(median((IRRLVPRRM$estimate["b_Performance"]/IRRLVPRRM$estimate["b_Price"]+randcoeff$LV)),3))))
print(paste0("Emission:",(round(median((IRRLVPRRM$estimate["b_Emission"]/IRRLVPRRM$estimate["b_Price"]+randcoeff$LV)),3))))

IRRLVPRRM <- readRDS("IRRLVPRRMF.rds")
D2 <- data.frame(apollo_modelOutput(IRRLVPRRM,modelOutput_settings = list(printPVal=TRUE)))
D2.1 <- cbind(D2$Estimate,D2$p.1.sided.)
rownames(D2.1) <- row.names(D2)
round(D2.1,3)
xtable::xtable(round(D2.1,3),digits=3)

#### Model7: ICLV-GRRM ####
## https://www.advancedrrmmodels.com/latent-class-models


library(apollo)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "IRRLVGRRMF",
  modelDescr = "IRRLVGRRMF",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,b_Emission     = 0, 
                b_Performance       = 0, 
                b_Price            = 0,  
                lambda             = 1, 
                gamma=0,
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
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
  randcoeff[["Performance_1"]] =   ( Performance_B - Performance_A ) 
  randcoeff[["Performance_2"]] =   ( Performance_A - Performance_B ) 
  randcoeff[["Emission_1"]] =  ( Emission_B - Emission_A ) 
  randcoeff[["Emission_2"]] =  ( Emission_A - Emission_B ) 
  randcoeff[["Price_1"]] =  ( Price_B - Price_A ) 
  randcoeff[["Price_2"]] =  ( Price_A - Price_B ) 
  
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
  
  R = list()
  R[['A']]  = asc_A-
    log( gamma + exp( b_Performance * Performance_1 ) ) - log( gamma + exp( b_Emission * Emission_1 ) ) - log( gamma + exp( b_Price * Price_1 ) )     
  R[['B']]  = asc_B  + lambda*LV +
    log( gamma + exp( b_Performance * Performance_2 ) ) - log( gamma + exp( b_Emission * Emission_2 ) ) - log( gamma + exp( b_Price * Price_2 ) )     
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1),
    componentName= "choice")
  
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


IRRLVGRRMF = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(IRRLVGRRMF,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = IRRLVGRRMF,"IRRLVGRRMF.rds")

print(paste0("Performance:",(round(median((IRRLVGRRM$estimate["b_Performance"]/IRRLVGRRM$estimate["b_Price"]+randcoeff$LV)),3))))
print(paste0("Performance:",(round(median((IRRLVGRRM$estimate["b_Emission"]/IRRLVGRRM$estimate["b_Price"]+randcoeff$LV)),3))))



IRRLVGRRM <- readRDS("IRRLVGRRMF.rds")
D3 <- data.frame(apollo_modelOutput(IRRLVGRRM,modelOutput_settings = list(printPVal=TRUE)))
D3.1 <- cbind(D3$Estimate,D3$p.1.sided.)
rownames(D3.1) <- row.names(D3)
round(D3.1,3)
xtable::xtable(round(D3.1,3),digits=3)

#### Model8: ICLV-MuRRM ####
## https://www.advancedrrmmodels.com/latent-class-models


library(apollo)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "IRRLVMURRMF",
  modelDescr = "IRRLVMURRMF",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,b_Emission     = 0, 
                b_Performance       = 0, 
                b_Price            = 0,  
                lambda             = 1, 
                mu=1,
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
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["asc_B"]] = asc_BB
  randcoeff[["Performance_1"]] =   ( Performance_B - Performance_A ) 
  randcoeff[["Performance_2"]] =   ( Performance_A - Performance_B ) 
  randcoeff[["Emission_1"]] =  ( Emission_B - Emission_A ) 
  randcoeff[["Emission_2"]] =  ( Emission_A - Emission_B ) 
  randcoeff[["Price_1"]] =  ( Price_B - Price_A ) 
  randcoeff[["Price_2"]] =  ( Price_A - Price_B ) 
  
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
  
  R = list()
  R[['A']]  = asc_A + mu * ( - log( 1 + exp( ( b_Performance  ) * Performance_1 ) ) - log( 1 + exp( ( b_Emission  ) * Emission_1 ) ) - log( 1 + exp( ( b_Price ) * Price_1 )  ) )
  R[['B']]  = asc_B + lambda*LV +mu * ( - log( 1 + exp( ( b_Performance  ) * Performance_2 ) ) - log( 1 + exp( ( b_Emission  ) * Emission_2 ) ) - log( 1 + exp( ( b_Price ) * Price_2 )  ) )
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1),
    componentName= "choice")
  
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


IRRLVMURRMF = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(IRRLVMURRM,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = IRRLVMURRMF,"IRRLVMURRMF.rds")

print(paste0("Performance:",(round(median((IRRLVMURRM$estimate["b_Performance"]/IRRLVMURRM$estimate["b_Price"]+randcoeff$LV)),3))))
print(paste0("Performance:",(round(median((IRRLVMURRM$estimate["b_Emission"]/IRRLVMURRM$estimate["b_Price"]+randcoeff$LV)),3))))


IRRLVMURRM <- readRDS("IRRLVMURRMF.rds")
D4 <- data.frame(apollo_modelOutput(IRRLVMURRM,modelOutput_settings = list(printPVal=TRUE)))
D4.1 <- cbind(D4$Estimate,D4$p.1.sided.)
rownames(D4.1) <- row.names(D4)
round(D4.1,3)
xtable::xtable(round(D4.1,3),digits=3)


#### Model9: ICLV-MNL ####

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
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy +  gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
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
CEmodelMNL = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodelMNL,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CEmodelMNL, file="CEmodelMNL.rds")
CEmodelMNL <- readRDS("CEmodelMNL.rds")
DMNL <- data.frame(apollo_modelOutput(CEmodelMNL,modelOutput_settings = list(printPVal=TRUE)))
DMNL.1 <- cbind(DMNL$Estimate,DMNL$p.1.sided.)
rownames(DMNL.1) <- row.names(DMNL)
round(DMNL.1,3)
xtable::xtable(round(DMNL.1,3),digits=3)

# Using the unconditional distributions of the parameters to calculate Performance and Emissions MWTP respectively
CEmodelMNLunconditionals <- apollo_unconditionals(CEmodelMNL,apollo_probabilities,apollo_inputs)
round(median((CEmodelMNL$estimate["b_Performance"]+randcoeff$LV/CEmodelMNL$estimate["b_Price"])),3)
round(median((CEmodelMNL$estimate["b_Emission"]+randcoeff$LV/CEmodelMNL$estimate["b_Price"])),3)


#### Model10: ICLV-MXL ####

library(apollo)

### Initialise code
apollo_initialise()
database <- Test_Apollo
### Set core controls
apollo_control = list(
  modelName  = "ICLV model: CE2",
  modelDescr = "ICLV model: CE2",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 4
)

### Vector of parameters, including any that are kept fixed in estimation
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

## Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")

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
  
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  randcoeff[["asc_B"]] = asc_BB
  
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
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + lambda*LV+ b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B )
  
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
# xtable::xtable(round(data.frame(apollo_modelOutput(CE2,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
# Full_Final <- cbind(Full_Full,"LV"=slice(data.frame(unconditionals$LV[,2]),rep(1:n(), each = 8)))
# ggplot(Full_Final, aes(x=Full_Final$unconditionals.LV...2., y=Q7WTP)) + geom_point()
CEmodel2 <- readRDS("CE2.rds")
DCE <- data.frame(apollo_modelOutput(CEmodel2,modelOutput_settings = list(printPVal=TRUE)))
DCE.1 <- cbind(DCE$Estimate,DCE$p.1.sided.)
rownames(DCE.1) <- row.names(DCE)
round(DCE.1,3)
xtable::xtable(round(DCE.1,3),digits=3)

randcoeff <- CEWTP(CEmodel2)

#### Other RRM Models: ####

#### Random-Regret MNL ####

library(apollo)
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="Apollo_example_7",
  modelDescr ="Simple RRM model on mode choice SP data",
  indivID    ="ID",nCores    = 4
)


database <- read.csv("H:/dos/PhD/Other/Training courses/CMC/apollo_modeChoiceData.csv",header = TRUE)
database <- Test_Apollo
Test_Apollo$Price_A[Test_Apollo$Price_A == 0] <- 1
Test_Apollo$Performance_A[Test_Apollo$Performance_A == 0] <- 1
Test_Apollo$Emission_A[Test_Apollo$Emission_A == 0] <- 1
Test_Apollo$Price_B <- Test_Apollo$Price_B +1
Test_Apollo$Emission_B <- Test_Apollo$Emission_B +1
Test_Apollo$Performance_B <- Test_Apollo$Performance_B +1
database = Test_Apollo


### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc_A               = 0,
              asc_B               = 0,
              b_Price              = 0,
              b_Performance              = 0,
              b_Emission                = 0)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_A")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of regret functions: these must use the same names as in mnl_settings, order is irrelevant
  R = list()
  R[['A']]  = asc_A  + 
    log(1+exp(b_Price*(Price_B   - Price_A))) + 
    log(1+exp(b_Performance*(Performance_B  - Performance_A))) + 
    log(1+exp(b_Emission*(Emission_B - Emission_A))) 
  R[['B']]  = asc_B  + 
    log(1+exp(b_Price*(Price_A   - Price_B))) + 
    log(1+exp(b_Performance*(Performance_A  - Performance_B))) + 
    log(1+exp(b_Emission*(Emission_A - Emission_B)))
  
  ### Define settings for RRM model, which is MNL with negative regret as utility
  mnl_settings <- list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1)
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

RRMNL1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(RRMNL1,modelOutput_settings = list(printPVal=TRUE))


#### Random-Regret LCM ####
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
  nCores     = 4
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


Test_Apollo$Price_A[Test_Apollo$Price_A == 0] <- 1
Test_Apollo$Performance_A[Test_Apollo$Performance_A == 0] <- 1
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


#### Random-Regret ICLV 1 ####
## https://www.advancedrrmmodels.com/latent-class-models


library(apollo)

database = Test_Apollo
apollo_initialise()

apollo_control = list(
  modelName  = "IRRLV",
  modelDescr = "IRRLV",
  indivID    = "ID", mixing     = TRUE, nCores     = 4)

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

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Performance","draws_Emission"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
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
  
  R = list()
  R[['A']]  = asc_A  + 
    log(1+exp(b_Price*(Price_B   - Price_A))) + 
    log(1+exp(b_Performance*(Performance_B  - Performance_A))) + 
    log(1+exp(b_Emission*(Emission_B - Emission_A))) 
  R[['B']]  = asc_B  + 
    log(1+exp(b_Price*(Price_A   - Price_B))) + 
    log(1+exp(b_Performance*(Performance_A  - Performance_B))) + 
    log(1+exp(b_Emission*(Emission_A - Emission_B))) + lambda*LV 
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = lapply(R, "*", -1),
    componentName= "choice")
  
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


IRRLV = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(IRRLV,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = IRRLVF,"IRRLVF.rds")

print(paste0("Performance:",(round(median((IRRLV$estimate["b_Performance"]+randcoeff$LV/IRRLV$estimate["b_Price"])),3))))
print(paste0("Performance:",(round(median((IRRLV$estimate["b_Emission"]+randcoeff$LV/IRRLV$estimate["b_Price"])),3))))

IRRLV <- readRDS("IRRLV.rds")
D1 <- data.frame(apollo_modelOutput(IRRLV,modelOutput_settings = list(printPVal=TRUE)))
D1.1 <- cbind(D1$Estimate,D1$p.1.sided.)
rownames(D1.1) <- row.names(D1)
round(D1.1,3)
round(median((IRRLV$estimate["b_Performance"]+randcoeff$LV/IRRLV$estimate["b_Price"])),3)
round(median((randcoeff$b_Emission+randcoeff$LV/randcoeff$b_Price)),3)


CEWTP  = function(Model){
  if (is.null(apollo_inputs$silent)) 
    silent = FALSE
  else silent = apollo_inputs$silent
  CEmodel4 <- Model # Enter requisite model here
  apollo_beta = CEmodel4$estimate
  apollo_fixed = CEmodel4$apollo_fixed
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
  if (!silent) 
    apollo_print("Unconditional distributions computed")
  return(randcoeff)
  # # This WTP here is fit for reporting in text:
  # print(paste0("Performance:",(round(median((randcoeff$b_Performance+randcoeff$LV/randcoeff$b_Price)),3))))
  # print(paste0("Emission:",(round(median((randcoeff$b_Emission+randcoeff$LV/randcoeff$b_Price)),3))))
}

View(cbind(IRRLV2010$estimate,IRRLVGRRM$estimate, IRRLVMURRM$estimate,IRRLVPRRM$estimate))


#### ICVLVRRM ####


#### ICLVQ6RRM ####
## https://www.advancedrrmmodels.com/latent-class-models


library(apollo)

# Setup the data for all truncated models:
database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1
apollo_initialise()

apollo_control = list(
  modelName  = "ICLVQ6RRM",modelDescr = "ICLVQ6RRM",
  indivID    = "ID",mixing     = TRUE,nCores     = 4,
  noValidation=TRUE)

apollo_beta = c(intercept      = 0,b_bid     = 0, 
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

apollo_fixed = c()

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta"))

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q6ResearchCertainty + eta
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
  op_settings = list(outcomeOrdered= Q6ResearchResponse,
                     V      = intercept + log(1+exp(b_bid*(Q6Bid - Bid_Alt))) +lambda*LV,
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


ICLVQ6RRM = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(ICLVQ6RRM,modelOutput_settings = list(printPVal=TRUE))
saveRDS(object = ICLVQ6RRM,"ICLVQ6RRM.rds")

print(paste0("Performance:",(round(median((IRRLV$estimate["b_Performance"]+randcoeff$LV/IRRLV$estimate["b_Price"])),3))))
print(paste0("Performance:",(round(median((IRRLV$estimate["b_Emission"]+randcoeff$LV/IRRLV$estimate["b_Price"])),3))))

