#### Peter King Thesis Replication Code: Latent-Class Model ####


### 2-Class Latest Version ####
rm(list=ls())
Sys.setlocale("LC_ALL","C")

library(apollo)
database <- data.frame(read.csv("Test_Apollo.csv",encoding="latin1"))
apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard3CNoSD",modelDescr ="LCMStandard3CNoSD",
  indivID    ="ID",nCores     = 4)

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
                delta_b         = 0,
                delta_c = 0)

apollo_fixed = c("asc_1")


apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  ### Define lists of parameters for each class
  
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  ### Class allocation probabilities
  ### These are the probabilities of a binary logit model
  classAlloc_settings = list(
    V = list(A = 0,
             B = delta_b,
             C = delta_c))
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities
  P = list()
  PClass = list()
  
  MNL = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice,
    V            = list() )
  ### Loop over classes
  S = 3 # number of classes
  for(s in 1:S){
    MNL$V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    MNL$V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    label = paste0("choiceClass",s)
    PClass[[label]] = apollo_mnl(MNL, functionality)
    ### Take product across observation for same individual
    PClass[[label]] = apollo_panelProd(PClass[[label]], apollo_inputs, functionality)
  }
  ### Mix the probabilities from each class
  lc_settings   = list(inClassProb=PClass, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
x = list(writeIter=FALSE)#, estimationRoutine="bhhh")
LCMStandard3CNoSD  = apollo_estimate(apollo_beta, apollo_fixed, 
                                                  apollo_probabilities, apollo_inputs,
                                                  estimate_settings=x)

# LCMStandard3CAllSD_Winter_Trunc = apollo_estimate(apollo_beta, apollo_fixed,
#                                                  apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard3CNoSD ,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard3CNoSD ,"LCMStandard3CNoSD.rds")
apollo_saveOutput(LCMStandard3CNoSD ,saveOutput_settings = list(printPVal=TRUE))


LCMStandard3CNoSD_Model <- readRDS("LCMStandard3CNoSD.rds")
# LCMStandard3CNoSD_WTP <- data.frame(read.csv("MXLConditionalsF_AttOnlyFull_WTP.csv"))
LCMStandard3CNoSD_Estimates <- data.frame(read.csv("LCMStandard3CNoSD_estimates.csv"))



## So this code outputs a LaTeX table of estimate, p.v stars and s.e in brackets ## 
### To make it easy, just change the model name here and the code will output the table for your model:
Estimates <- LCMStandard3CNoSD_Estimates

xtable(data.frame(Estimates$X,
                  paste(
                    ifelse(
                      Estimates$Rob.p.val.0. < 0.01,
                      paste0(round(Estimates$Estimate, 3), "***"),
                      ifelse(
                        Estimates$Rob.p.val.0. < 0.05,
                        paste0(round(Estimates$Estimate, 3), "**"),
                        ifelse(
                          Estimates$Rob.p.val.0. < 0.1,
                          paste0(round(Estimates$Estimate, 3), "*"),
                          round(Estimates$Estimate, 3))))),
                  round(Estimates$Rob.std.err.,3),round(Estimates$Rob.p.val.0.,3)),digits=3)

