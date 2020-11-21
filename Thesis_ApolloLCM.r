#### Survey data analysis script: All Latent-Class Models  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: This script estimates all the LCM models and experiments in one place ####

#### LCMStandard2CNoSD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard2CNoSD",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                delta_b = 0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b 
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard2CNoSD = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard2CNoSD,modelOutput_settings = list(printPVal=TRUE))


#### LCMStandard3CNoSD ####

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
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b
  V[["class_c"]] = delta_c
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard3CNoSD = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard3CNoSD,modelOutput_settings = list(printPVal=TRUE))


#### LCMStandard4CNoSD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard4CNoSD",modelDescr ="LCMStandard4CNoSD",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Price_d       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Performance_d       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                beta_Emission_d       =0,
                delta_b         = 0,
                delta_c = 0,
                delta_d = 0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c,beta_Price_d)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c,beta_Performance_d)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c,beta_Emission_d)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b
  V[["class_c"]] = delta_c
  V[["class_d"]] = delta_d
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3,class_d=4), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard4CNoSD = apollo_estimate(apollo_beta, apollo_fixed, 
                            apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard4CNoSD,modelOutput_settings = list(printPVal=TRUE))


#### LCMStandard2CAllSD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard2CAllSD",modelDescr ="LCM1",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                delta_b = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard2CAllSD = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard2CAllSD,modelOutput_settings = list(printPVal=TRUE))


#### LCMStandard3CAllSD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard3CAllSD",modelDescr ="LCMStandard2CAllSD",
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
                delta_b = 0,
                delta_c = 0,
                A_gamma_Gender=0,
                A_gamma_Age      = 0,
                A_gamma_Distance = 0,
                A_gamma_Trips    = 0,
                A_gamma_BP       = 0,
                A_gamma_Charity  = 0,
                A_gamma_Education  = 0,
                A_gamma_Employment = 0,
                A_gamma_Income     = 0,
                A_gamma_Order      = 0,
                A_gamma_Task       = 0,
                A_gamma_Cons       = 0,
                A_gamma_Experts    = 0,
                A_gamma_Understanding =0,
                A_gamma_Certainty=0,
                B_gamma_Gender=0,
                B_gamma_Age      = 0,
                B_gamma_Distance = 0,
                B_gamma_Trips    = 0,
                B_gamma_BP       = 0,
                B_gamma_Charity  = 0,
                B_gamma_Education  = 0,
                B_gamma_Employment = 0,
                B_gamma_Income     = 0,
                B_gamma_Order      = 0,
                B_gamma_Task       = 0,
                B_gamma_Cons       = 0,
                B_gamma_Experts    = 0,
                B_gamma_Understanding =0,
                B_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ A_gamma_Gender*Q1Gender + A_gamma_Age*Age +
    A_gamma_Distance * Distance + 
    A_gamma_Trips * Trips +
    A_gamma_BP * BP +
    A_gamma_Charity * Charity + 
    A_gamma_Education * Education +
    A_gamma_Employment * Employment + 
    A_gamma_Income * Income +
    A_gamma_Order * Order +      
    A_gamma_Task * Task +       
    A_gamma_Cons * Consequentiality +       
    A_gamma_Experts * Experts+
    A_gamma_Understanding*Survey +
    A_gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c+ B_gamma_Gender*Q1Gender + B_gamma_Age*Age +
    B_gamma_Distance * Distance + 
    B_gamma_Trips * Trips +
    B_gamma_BP * BP +
    B_gamma_Charity * Charity + 
    B_gamma_Education * Education +
    B_gamma_Employment * Employment + 
    B_gamma_Income * Income +
    B_gamma_Order * Order +      
    B_gamma_Task * Task +       
    B_gamma_Cons * Consequentiality +       
    B_gamma_Experts * Experts+
    B_gamma_Understanding*Survey +
    B_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard3CAllSD = apollo_estimate(apollo_beta, apollo_fixed, 
                           apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCMStandard3CAllSD,modelOutput_settings = list(printPVal=TRUE))


#### LCMStandard4CAllSD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard4CAllSD",modelDescr ="LCMStandard4CAllSD",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Price_d       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Performance_d       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                beta_Emission_d       =0,
                delta_b = 0,
                delta_c = 0,
                delta_d = 0,
                A_gamma_Gender=0,
                A_gamma_Age      = 0,
                A_gamma_Distance = 0,
                A_gamma_Trips    = 0,
                A_gamma_BP       = 0,
                A_gamma_Charity  = 0,
                A_gamma_Education  = 0,
                A_gamma_Employment = 0,
                A_gamma_Income     = 0,
                A_gamma_Order      = 0,
                A_gamma_Task       = 0,
                A_gamma_Cons       = 0,
                A_gamma_Experts    = 0,
                A_gamma_Understanding =0,
                A_gamma_Certainty=0,
                B_gamma_Gender=0,
                B_gamma_Age      = 0,
                B_gamma_Distance = 0,
                B_gamma_Trips    = 0,
                B_gamma_BP       = 0,
                B_gamma_Charity  = 0,
                B_gamma_Education  = 0,
                B_gamma_Employment = 0,
                B_gamma_Income     = 0,
                B_gamma_Order      = 0,
                B_gamma_Task       = 0,
                B_gamma_Cons       = 0,
                B_gamma_Experts    = 0,
                B_gamma_Understanding =0,
                B_gamma_Certainty=0,
                D_gamma_gender=0,
                D_gamma_Age      = 0,
                D_gamma_Distance = 0,
                D_gamma_Trips    = 0,
                D_gamma_BP       = 0,
                D_gamma_Charity  = 0,
                D_gamma_Education  = 0,
                D_gamma_Employment = 0,
                D_gamma_Income     = 0,
                D_gamma_Order      = 0,
                D_gamma_Task       = 0,
                D_gamma_Cons       = 0,
                D_gamma_Experts    = 0,
                D_gamma_Understanding =0,
                D_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c,beta_Price_d)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c,beta_Performance_d)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c,beta_Emission_d)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ A_gamma_Gender*Q1Gender + A_gamma_Age*Age +
    A_gamma_Distance * Distance + 
    A_gamma_Trips * Trips +
    A_gamma_BP * BP +
    A_gamma_Charity * Charity + 
    A_gamma_Education * Education +
    A_gamma_Employment * Employment + 
    A_gamma_Income * Income +
    A_gamma_Order * Order +      
    A_gamma_Task * Task +       
    A_gamma_Cons * Consequentiality +       
    A_gamma_Experts * Experts+
    A_gamma_Understanding*Survey +
    A_gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c+ B_gamma_Gender*Q1Gender + B_gamma_Age*Age +
    B_gamma_Distance * Distance + 
    B_gamma_Trips * Trips +
    B_gamma_BP * BP +
    B_gamma_Charity * Charity + 
    B_gamma_Education * Education +
    B_gamma_Employment * Employment + 
    B_gamma_Income * Income +
    B_gamma_Order * Order +      
    B_gamma_Task * Task +       
    B_gamma_Cons * Consequentiality +       
    B_gamma_Experts * Experts+
    B_gamma_Understanding*Survey +
    B_gamma_Certainty*Q12CECertainty
  V[["class_d"]] = delta_d+ D_gamma_gender*Q1Gender + D_gamma_Age*Age +
    D_gamma_Distance * Distance + 
    D_gamma_Trips * Trips +
    D_gamma_BP * BP +
    D_gamma_Charity * Charity + 
    D_gamma_Education * Education +
    D_gamma_Employment * Employment + 
    D_gamma_Income * Income +
    D_gamma_Order * Order +      
    D_gamma_Task * Task +       
    D_gamma_Cons * Consequentiality +       
    D_gamma_Experts * Experts+
    D_gamma_Understanding*Survey +
    D_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3,class_d=4), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard4CAllSD = apollo_estimate(apollo_beta, apollo_fixed, 
                                     apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard4CAllSD,modelOutput_settings = list(printPVal=TRUE))


#### LCmodelMXL: 2-class No SD ####

apollo_initialise()

apollo_control = list(
  modelName  ="2-class LCM",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                delta_b = 0,
                gamma_gender=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_gender*Q1Gender
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL = apollo_estimate(apollo_beta, apollo_fixed, 
                          apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCmodelMXL,modelOutput_settings = list(printPVal=TRUE))


#### LCmodelMXL2: 2-class More SD ####

apollo_initialise()

apollo_control = list(
  modelName  ="2-class LCM",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                delta_b = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL2 = apollo_estimate(apollo_beta, apollo_fixed, 
                             apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCmodelMXL2,modelOutput_settings = list(printPVal=TRUE))


#### LCmodelMXL3SD: 3-class More SD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCmodelMXL3SD",modelDescr ="LCmodelMXL3SD",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Price_c_mu     = -3,
                b_log_Price_c_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Perf_c_mu     = -3,
                b_log_Perf_c_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                b_log_Em_c_mu     = -3,
                b_log_Em_c_sig    = 0, 
                delta_b = 0,
                delta_c = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0,
                C_gamma_Gender=0,
                C_gamma_Age      = 0,
                C_gamma_Distance = 0,
                C_gamma_Trips    = 0,
                C_gamma_BP       = 0,
                C_gamma_Charity  = 0,
                C_gamma_Education  = 0,
                C_gamma_Employment = 0,
                C_gamma_Income     = 0,
                C_gamma_Order      = 0,
                C_gamma_Task       = 0,
                C_gamma_Cons       = 0,
                C_gamma_Experts    = 0,
                C_gamma_Understanding =0,
                C_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Price_c"]] = -exp(b_log_Price_c_mu + b_log_Price_c_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Perf_c"]] = -exp(b_log_Perf_c_mu + b_log_Perf_c_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  randcoeff[["beta_Em_c"]] = -exp(b_log_Em_c_mu + b_log_Em_c_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b,beta_Perf_c)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b,beta_Em_c)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c +C_gamma_Gender*Q1Gender + C_gamma_Age*Age +
    C_gamma_Distance * Distance + 
    C_gamma_Trips * Trips +
    C_gamma_BP * BP +
    C_gamma_Charity * Charity + 
    C_gamma_Education * Education +
    C_gamma_Employment * Employment + 
    C_gamma_Income * Income +
    C_gamma_Order * Order +      
    C_gamma_Task * Task +       
    C_gamma_Cons * Consequentiality +       
    C_gamma_Experts * Experts+
    C_gamma_Understanding*Survey +
    C_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL3SD = apollo_estimate(apollo_beta, apollo_fixed, 
                              apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCmodelMXL3SD,modelOutput_settings = list(printPVal=TRUE))


#### MXL LCM with LV ####

library(apollo)
apollo_initialise()

apollo_control = list(
  modelName  ="MXLLCMLV",modelDescr ="MXLLCMLV",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                delta_b = 0,
                Lambda=1,
                gamma_Age       = 0, 
                gamma_Gender    = 0,
                gamma_Distance  = 0, 
                gamma_Income =0,
                gamma_Employment =0,
                gamma_Experts =0,
                gamma_Cons =0,
                gamma_BP =0,
                gamma_Charity =0,
                gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=1000,          
  interUnifDraws=c(),      
  interNormDraws=c("eta","draws_Price","draws_Perf","draws_Em"))

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*Income + gamma_Employment*Employment + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +Lambda*LV
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
MXLLCMLV = apollo_estimate(apollo_beta, apollo_fixed, 
                           apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(MXLLCMLV,modelOutput_settings = list(printPVal=TRUE))



database <- Test_Truncated
#### LCMStandard2CNoSDTRUNCATED ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard2CNoSDTRUNCATED",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                delta_b = 0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b 
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard2CNoSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                    apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard2CNoSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard2CNoSDTRUNCATED,"LCMStandard2CNoSDTRUNCATED.rds")


#### LCMStandard3CNoSDTRUNCATED ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard3CNoSDTRUNCATED",modelDescr ="LCMStandard3CNoSD",
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
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b
  V[["class_c"]] = delta_c
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard3CNoSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                    apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard3CNoSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard3CNoSDTRUNCATED,"LCMStandard3CNoSDTRUNCATED.rds")


#### LCMStandard4CNoSDTRUNCATED ####


apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard4CNoSDTRUNCATED",modelDescr ="LCMStandard4CNoSD",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Price_d       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Performance_d       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                beta_Emission_d       =0,
                delta_b         = 0,
                delta_c = 0,
                delta_d = 0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c,beta_Price_d)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c,beta_Performance_d)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c,beta_Emission_d)
  
  V=list()
  V[["class_a"]] = 0 
  V[["class_b"]] = delta_b
  V[["class_c"]] = delta_c
  V[["class_d"]] = delta_d
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3,class_d=4), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

LCMStandard4CNoSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                    apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard4CNoSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard4CNoSDTRUNCATED,"LCMStandard4CNoSDTRUNCATED.rds")


#### LCMStandard2CAllSDTRUNCATED ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard2CAllSDTRUNCATED",modelDescr ="LCM1",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                delta_b = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard2CAllSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                     apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard2CAllSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard2CAllSDTRUNCATED,"LCMStandard2CAllSDTRUNCATED.rds")


#### LCMStandard3CAllSDTRUNCATED ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard3CAllSDTRUNCATED",modelDescr ="LCMStandard2CAllSD",
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
                delta_b = 0,
                delta_c = 0,
                A_gamma_Gender=0,
                A_gamma_Age      = 0,
                A_gamma_Distance = 0,
                A_gamma_Trips    = 0,
                A_gamma_BP       = 0,
                A_gamma_Charity  = 0,
                A_gamma_Education  = 0,
                A_gamma_Employment = 0,
                A_gamma_Income     = 0,
                A_gamma_Order      = 0,
                A_gamma_Task       = 0,
                A_gamma_Cons       = 0,
                A_gamma_Experts    = 0,
                A_gamma_Understanding =0,
                A_gamma_Certainty=0,
                B_gamma_Gender=0,
                B_gamma_Age      = 0,
                B_gamma_Distance = 0,
                B_gamma_Trips    = 0,
                B_gamma_BP       = 0,
                B_gamma_Charity  = 0,
                B_gamma_Education  = 0,
                B_gamma_Employment = 0,
                B_gamma_Income     = 0,
                B_gamma_Order      = 0,
                B_gamma_Task       = 0,
                B_gamma_Cons       = 0,
                B_gamma_Experts    = 0,
                B_gamma_Understanding =0,
                B_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ A_gamma_Gender*Q1Gender + A_gamma_Age*Age +
    A_gamma_Distance * Distance + 
    A_gamma_Trips * Trips +
    A_gamma_BP * BP +
    A_gamma_Charity * Charity + 
    A_gamma_Education * Education +
    A_gamma_Employment * Employment + 
    A_gamma_Income * Income +
    A_gamma_Order * Order +      
    A_gamma_Task * Task +       
    A_gamma_Cons * Consequentiality +       
    A_gamma_Experts * Experts+
    A_gamma_Understanding*Survey +
    A_gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c+ B_gamma_Gender*Q1Gender + B_gamma_Age*Age +
    B_gamma_Distance * Distance + 
    B_gamma_Trips * Trips +
    B_gamma_BP * BP +
    B_gamma_Charity * Charity + 
    B_gamma_Education * Education +
    B_gamma_Employment * Employment + 
    B_gamma_Income * Income +
    B_gamma_Order * Order +      
    B_gamma_Task * Task +       
    B_gamma_Cons * Consequentiality +       
    B_gamma_Experts * Experts+
    B_gamma_Understanding*Survey +
    B_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard3CAllSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                     apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCMStandard3CAllSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard3CAllSDTRUNCATED,"LCMStandard3CAllSDTRUNCATED.rds")


#### LCMStandard4CAllSDTRUNCATED ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCMStandard4CAllSD",modelDescr ="LCMStandard4CAllSD",
  indivID    ="ID",nCores     = 4)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                beta_Price_a       = 0,
                beta_Price_b       = 0,
                beta_Price_c       = 0,
                beta_Price_d       = 0,
                beta_Performance_a       = 0,
                beta_Performance_b       = 0,
                beta_Performance_c       = 0,
                beta_Performance_d       = 0,
                beta_Emission_a       =0,
                beta_Emission_b       =0,
                beta_Emission_c       =0,
                beta_Emission_d       =0,
                delta_b = 0,
                delta_c = 0,
                delta_d = 0,
                A_gamma_Gender=0,
                A_gamma_Age      = 0,
                A_gamma_Distance = 0,
                A_gamma_Trips    = 0,
                A_gamma_BP       = 0,
                A_gamma_Charity  = 0,
                A_gamma_Education  = 0,
                A_gamma_Employment = 0,
                A_gamma_Income     = 0,
                A_gamma_Order      = 0,
                A_gamma_Task       = 0,
                A_gamma_Cons       = 0,
                A_gamma_Experts    = 0,
                A_gamma_Understanding =0,
                A_gamma_Certainty=0,
                B_gamma_Gender=0,
                B_gamma_Age      = 0,
                B_gamma_Distance = 0,
                B_gamma_Trips    = 0,
                B_gamma_BP       = 0,
                B_gamma_Charity  = 0,
                B_gamma_Education  = 0,
                B_gamma_Employment = 0,
                B_gamma_Income     = 0,
                B_gamma_Order      = 0,
                B_gamma_Task       = 0,
                B_gamma_Cons       = 0,
                B_gamma_Experts    = 0,
                B_gamma_Understanding =0,
                B_gamma_Certainty=0,
                D_gamma_gender=0,
                D_gamma_Age      = 0,
                D_gamma_Distance = 0,
                D_gamma_Trips    = 0,
                D_gamma_BP       = 0,
                D_gamma_Charity  = 0,
                D_gamma_Education  = 0,
                D_gamma_Employment = 0,
                D_gamma_Income     = 0,
                D_gamma_Order      = 0,
                D_gamma_Task       = 0,
                D_gamma_Cons       = 0,
                D_gamma_Experts    = 0,
                D_gamma_Understanding =0,
                D_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c,beta_Price_d)
  lcpars[["beta_Performance"]] = list(beta_Performance_a, beta_Performance_b,beta_Performance_c,beta_Performance_d)
  lcpars[["beta_Emission"]] = list(beta_Emission_a, beta_Emission_b,beta_Emission_c,beta_Emission_d)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b+ A_gamma_Gender*Q1Gender + A_gamma_Age*Age +
    A_gamma_Distance * Distance + 
    A_gamma_Trips * Trips +
    A_gamma_BP * BP +
    A_gamma_Charity * Charity + 
    A_gamma_Education * Education +
    A_gamma_Employment * Employment + 
    A_gamma_Income * Income +
    A_gamma_Order * Order +      
    A_gamma_Task * Task +       
    A_gamma_Cons * Consequentiality +       
    A_gamma_Experts * Experts+
    A_gamma_Understanding*Survey +
    A_gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c+ B_gamma_Gender*Q1Gender + B_gamma_Age*Age +
    B_gamma_Distance * Distance + 
    B_gamma_Trips * Trips +
    B_gamma_BP * BP +
    B_gamma_Charity * Charity + 
    B_gamma_Education * Education +
    B_gamma_Employment * Employment + 
    B_gamma_Income * Income +
    B_gamma_Order * Order +      
    B_gamma_Task * Task +       
    B_gamma_Cons * Consequentiality +       
    B_gamma_Experts * Experts+
    B_gamma_Understanding*Survey +
    B_gamma_Certainty*Q12CECertainty
  V[["class_d"]] = delta_d+ D_gamma_gender*Q1Gender + D_gamma_Age*Age +
    D_gamma_Distance * Distance + 
    D_gamma_Trips * Trips +
    D_gamma_BP * BP +
    D_gamma_Charity * Charity + 
    D_gamma_Education * Education +
    D_gamma_Employment * Employment + 
    D_gamma_Income * Income +
    D_gamma_Order * Order +      
    D_gamma_Task * Task +       
    D_gamma_Cons * Consequentiality +       
    D_gamma_Experts * Experts+
    D_gamma_Understanding*Survey +
    D_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3,class_d=4), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Performance[[s]]*Performance_A + beta_Price[[s]]*Price_A + beta_Emission[[s]]*Emission_A
    V[['alt2']]  = asc_2 + beta_Performance[[s]]*Performance_B + beta_Price[[s]]*Price_B + beta_Emission[[s]]*Emission_B
    
    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCMStandard4CAllSD = apollo_estimate(apollo_beta, apollo_fixed, 
                                     apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard4CAllSD,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard4CAllSDTRUNCATED,"LCMStandard4CAllSDTRUNCATED.rds")


#### LCmodelMXL2CNoSDTruncated ####


apollo_initialise()

apollo_control = list(
  modelName  ="LCmodelMXL2CNoSDTruncated",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                delta_b = 0,
                gamma_gender=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_gender*Q1Gender
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL2CNoSDTruncated = apollo_estimate(apollo_beta, apollo_fixed, 
                             apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCmodelMXL,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCmodelMXL2CNoSDTruncated,"LCmodelMXL2CNoSDTruncated.rds")


#### LCmodelMXL3CNoSDTruncated ####


apollo_initialise()

apollo_control = list(
  modelName  ="LCmodelMXL2CNoSDTruncated",modelDescr ="2-class LCM",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                delta_b = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL2CNoSDTruncated = apollo_estimate(apollo_beta, apollo_fixed, 
                              apollo_probabilities, apollo_inputs)
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Show output in screen
apollo_modelOutput(LCmodelMXL2CNoSDTruncated,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCmodelMXL2CNoSDTruncated,"LCmodelMXL2CNoSDTruncated.rds")


#### LCmodelMXL3CSDTruncated: 3-class More SD ####

apollo_initialise()

apollo_control = list(
  modelName  ="LCmodelMXL3CSDTruncated",modelDescr ="LCmodelMXL3SD",
  indivID    ="ID",nCores     = 4,mixing=TRUE)

apollo_beta = c(asc_1           = 0,
                asc_2           = 0,
                b_log_Price_a_mu     = -3,
                b_log_Price_a_sig    = 0, 
                b_log_Price_b_mu     = -3,
                b_log_Price_b_sig    = 0, 
                b_log_Price_c_mu     = -3,
                b_log_Price_c_sig    = 0, 
                b_log_Perf_a_mu     = -3,
                b_log_Perf_a_sig    = 0, 
                b_log_Perf_b_mu     = -3,
                b_log_Perf_b_sig    = 0, 
                b_log_Perf_c_mu     = -3,
                b_log_Perf_c_sig    = 0, 
                b_log_Em_a_mu     = -3,
                b_log_Em_a_sig    = 0, 
                b_log_Em_b_mu     = -3,
                b_log_Em_b_sig    = 0, 
                b_log_Em_c_mu     = -3,
                b_log_Em_c_sig    = 0, 
                delta_b = 0,
                delta_c = 0,
                gamma_Gender=0,
                gamma_Age      = 0,
                gamma_Distance = 0,
                gamma_Trips    = 0,
                gamma_BP       = 0,
                gamma_Charity  = 0,
                gamma_Education  = 0,
                gamma_Employment = 0,
                gamma_Income     = 0,
                gamma_Order      = 0,
                gamma_Task       = 0,
                gamma_Cons       = 0,
                gamma_Experts    = 0,
                gamma_Understanding =0,
                gamma_Certainty=0,
                C_gamma_Gender=0,
                C_gamma_Age      = 0,
                C_gamma_Distance = 0,
                C_gamma_Trips    = 0,
                C_gamma_BP       = 0,
                C_gamma_Charity  = 0,
                C_gamma_Education  = 0,
                C_gamma_Employment = 0,
                C_gamma_Income     = 0,
                C_gamma_Order      = 0,
                C_gamma_Task       = 0,
                C_gamma_Cons       = 0,
                C_gamma_Experts    = 0,
                C_gamma_Understanding =0,
                C_gamma_Certainty=0)

apollo_fixed = c("asc_1")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 100,
  interNormDraws = c("draws_Price","draws_Perf","draws_Em")
)

apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["beta_Price_a"]] = -exp(b_log_Price_a_mu + b_log_Price_a_sig * draws_Price)
  randcoeff[["beta_Price_b"]] = -exp(b_log_Price_b_mu + b_log_Price_b_sig * draws_Price)
  randcoeff[["beta_Price_c"]] = -exp(b_log_Price_c_mu + b_log_Price_c_sig * draws_Price)
  randcoeff[["beta_Perf_a"]] = -exp(b_log_Perf_a_mu + b_log_Perf_a_sig * draws_Perf)
  randcoeff[["beta_Perf_b"]] = -exp(b_log_Perf_b_mu + b_log_Perf_b_sig * draws_Perf)
  randcoeff[["beta_Perf_c"]] = -exp(b_log_Perf_c_mu + b_log_Perf_c_sig * draws_Perf)
  randcoeff[["beta_Em_a"]] = -exp(b_log_Em_a_mu + b_log_Em_a_sig * draws_Em)
  randcoeff[["beta_Em_b"]] = -exp(b_log_Em_b_mu + b_log_Em_b_sig * draws_Em)
  randcoeff[["beta_Em_c"]] = -exp(b_log_Em_c_mu + b_log_Em_c_sig * draws_Em)
  return(randcoeff)
}

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["beta_Price"]] = list(beta_Price_a, beta_Price_b,beta_Price_c)
  lcpars[["beta_Performance"]] = list(beta_Perf_a, beta_Perf_b,beta_Perf_c)
  lcpars[["beta_Emission"]] = list(beta_Em_a, beta_Em_b,beta_Em_c)
  
  V=list()
  V[["class_a"]] = 0
  V[["class_b"]] = delta_b +gamma_Gender*Q1Gender + gamma_Age*Age +
    gamma_Distance * Distance + 
    gamma_Trips * Trips +
    gamma_BP * BP +
    gamma_Charity * Charity + 
    gamma_Education * Education +
    gamma_Employment * Employment + 
    gamma_Income * Income +
    gamma_Order * Order +      
    gamma_Task * Task +       
    gamma_Cons * Consequentiality +       
    gamma_Experts * Experts+
    gamma_Understanding*Survey +
    gamma_Certainty*Q12CECertainty
  V[["class_c"]] = delta_c +C_gamma_Gender*Q1Gender + C_gamma_Age*Age +
    C_gamma_Distance * Distance + 
    C_gamma_Trips * Trips +
    C_gamma_BP * BP +
    C_gamma_Charity * Charity + 
    C_gamma_Education * Education +
    C_gamma_Employment * Employment + 
    C_gamma_Income * Income +
    C_gamma_Order * Order +      
    C_gamma_Task * Task +       
    C_gamma_Cons * Consequentiality +       
    C_gamma_Experts * Experts+
    C_gamma_Understanding*Survey +
    C_gamma_Certainty*Q12CECertainty
  
  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2,class_c=3), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = Choice)
  
  ### Loop over classes
  s=1
  while(s<=length(pi_values)){
    
    V=list()
    V[['alt1']]  = asc_1 + beta_Price[[s]]* (Price_A + beta_Performance[[s]]*Performance_A + beta_Emission[[s]]*Emission_A)
    V[['alt2']]  = asc_2 + beta_Price[[s]]* (Price_B + beta_Performance[[s]]*Performance_B + beta_Emission[[s]]*Emission_B)
    
    mnl_settings$V = V
    
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    P[[s]] = apollo_avgInterDraws(P[[s]], apollo_inputs, functionality)
    
    # mnl_settings$componentName = paste0("Class_",s)
    # P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    # P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    # 
    s=s+1}
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

#apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
#apollo_outOfSample(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Estimate model
LCmodelMXL3CSDTruncated = apollo_estimate(apollo_beta, apollo_fixed, 
                                apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCmodelMXL3CSDTruncated,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCmodelMXL3CSDTruncated,"LCmodelMXL3CSDTruncated.rds")


#### Post-Estimation Codes #### 


### Reporting WTP:
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Performance_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Performance_b", parName2="beta_Price_b"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_a", parName2="beta_Price_a"))
apollo_deltaMethod(LCmodel, list(operation="ratio", parName1="beta_Emission_b", parName2="beta_Price_b"))

unconditionals=apollo_lcUnconditionals(LCmodel,apollo_probabilities,apollo_inputs)
classes=length(unconditionals[["pi_values"]])
colMeans(matrix(unlist(unconditionals[["pi_values"]]),ncol=classes,byrow=FALSE))

### look at WTP for different combinations
Performance_ClassMWTP=mapply("/",unconditionals[["beta_Performance"]],unconditionals[["beta_Price"]],SIMPLIFY=FALSE)
Emission_ClassMWTP=mapply("/",unconditionals[["beta_Emission"]],unconditionals[["beta_Price"]],SIMPLIFY=FALSE)
colMeans(matrix(unlist(Performance_ClassMWTP),ncol=classes,byrow=FALSE))
colMeans(matrix(unlist(Emission_ClassMWTP),ncol=classes,byrow=FALSE))

### weighted WTP
Performance_ClassMWTP_unconditional=rowSums(mapply("*",Performance_ClassMWTP,unconditionals[["pi_values"]],SIMPLIFY=TRUE))
Emission_ClassMWTP_unconditional=rowSums(mapply("*",Emission_ClassMWTP,unconditionals[["pi_values"]],SIMPLIFY=TRUE))
mean(Performance_ClassMWTP_unconditional)
mean(Emission_ClassMWTP_unconditional)


## Model prediction accuracy
LC2_Predictions <- data.frame(LCmodel$avgCP) ## Getting probabilities of choosing each option from the model
LC2_Predictions[LC2_Predictions$LCmodel.avgCP < 0.5,] <- 0
LC2_Predictions[LC2_Predictions$LCmodel.avgCP >= 0.5,] <- 1
LC2_Predictions <- cbind("Actual"=Fulls$Choice,"Predicted"=slice(data.frame(LC2_Predictions$LCmodel.avgCP),rep(1:n(), each = 4)))
LC2_Predictions$Match <- LC2_Predictions$Actual==LC2_Predictions$LC2_Predictions.LCmodel.avgCP
LC2_Predictions$Match[LC2_Predictions$Match==TRUE] <- 1
LC2_Predictions$Match[LC2_Predictions$Match==FALSE] <- 0
round(100/length(LC2_Predictions$Match)*length(LC2_Predictions$Match[LC2_Predictions$Match==0]),3)
# 44.524
round(100/length(LC2_Predictions$Match)*length(LC2_Predictions$Match[LC2_Predictions$Match==1]),3)
# 55.746

## Model individual level class-allocation
LC2_Class <- data.frame(apollo_lcConditionals(model = LCmodel,apollo_probabilities,apollo_inputs)) ## Getting probabilities of choosing each option from the model
colnames(LC2_Class) <- c(1,2) ## Name columns as each option 
LC2_Class <- data.frame("Class" = as.integer(colnames(LC2_Class)[apply(round(LC2_Class,4),1,which.max)])) ## This picks the class that is most likely for each individual
colnames(LC2_Class) <- "LCM2Class"
Full_Final <- cbind(Full_Final,"LCM2Class"=slice(data.frame(LC2_Class$LCM2Class),rep(1:n(), each = 8)))
colnames(Full_Final)[69] <- "LCM2Class"




