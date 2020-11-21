#### Survey data analysis script: All Mixed Logit models  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: This script contains all MXL specification searches here ####


# Setup:
library(apollo)
library(stats)
apollo_initialise()


#### MXL1: Pref-space Normal ####
apollo_control = list(
  modelName ="MXL1",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_b_Price    =0,
                sig_b_Price    =0,
                b_Performance = 0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_b_Price + sig_b_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(MXLmodel, deltaMethod_settings=list(operation="ratio", parName1="b_Emission", parName2="mu_b_Price"))
apollo_deltaMethod(MXLmodel, deltaMethod_settings=list(operation="ratio", parName1="b_Performance", parName2="mu_b_Price"))


#### MXL2: WTP-space Normal ####
apollo_control = list(
  modelName ="MXL2",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_b_Price    =0,
                sig_b_Price    =0,
                b_Performance    =0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_b_Price + sig_b_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel2 = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel2,modelOutput_settings = list(printPVal=TRUE))


#### MXL3: Pref-space Unif ####
apollo_control = list(
  modelName ="MXL3",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_b_Price    =0,
                sig_b_Price    =0,
                b_Performance = 0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Price"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_b_Price + sig_b_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel3 = apollo_estimate(apollo_beta, apollo_fixed,
                           apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel3,modelOutput_settings = list(printPVal=TRUE))

MXLmodel3$estimate["b_Emission"]/MXLmodel3$estimate["mu_b_Price"]
MXLmodel3$estimate["b_Performance"]/MXLmodel3$estimate["mu_b_Price"]

apollo_deltaMethod(MXLmodel3, deltaMethod_settings=list(operation="ratio", parName1="b_Emission", parName2="mu_b_Price"))
apollo_deltaMethod(MXLmodel3, deltaMethod_settings=list(operation="ratio", parName1="b_Performance", parName2="mu_b_Price"))


#### MXL4: WTP-space Unif ####
apollo_control = list(
  modelName ="MXL4",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_b_Price    =0,
                sig_b_Price    =0,
                b_Performance = 0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Price"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_b_Price + sig_b_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel4 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel4,modelOutput_settings = list(printPVal=TRUE))


#### MXL5: Pref-space  price fixed, others normal ####
apollo_control = list(
  modelName ="MXL5",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel5 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel5,modelOutput_settings = list(printPVal=TRUE))

MXLmodel5$estimate["mu_b_Emission"]/MXLmodel5$estimate["b_Price"]
MXLmodel5$estimate["mu_b_Performance"]/MXLmodel5$estimate["b_Price"]

apollo_deltaMethod(MXLmodel5, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="b_Price"))
apollo_deltaMethod(MXLmodel5, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="b_Price"))


#### MXL6: WTP-space price fixed, others normal ####
apollo_control = list(
  modelName ="MXL6",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel6 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel6,modelOutput_settings = list(printPVal=TRUE))



#### MXL7: Pref-space  price fixed, others unif ####
apollo_control = list(
  modelName ="MXL7",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Performance","draws_Emission"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel7 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel7,modelOutput_settings = list(printPVal=TRUE))

MXLmodel7$estimate["mu_b_Emission"]/MXLmodel7$estimate["b_Price"]
MXLmodel7$estimate["mu_b_Performance"]/MXLmodel7$estimate["b_Price"]

apollo_deltaMethod(MXLmodel7, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="b_Price"))
apollo_deltaMethod(MXLmodel7, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="b_Price"))


#### MXL8: WTP-space price fixed, others normal ####
apollo_control = list(
  modelName ="MXL8",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Performance","draws_Emission"),
  interNormDraws = c(),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel8 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel8,modelOutput_settings = list(printPVal=TRUE))



#### MXL9: Pref-space  price fixed, perf unif, em normal ####
apollo_control = list(
  modelName ="MXL9",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Performance"),
  interNormDraws = c("draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel9 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel9,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(MXLmodel9, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="b_Price"))
apollo_deltaMethod(MXLmodel9, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="b_Price"))


#### MXL10: WTP-space price fixed, perf unif, em normal ####
apollo_control = list(
  modelName ="MXL10",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Emission"),
  interNormDraws = c("draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel10 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel10,modelOutput_settings = list(printPVal=TRUE))

MXLmodel10$estimate["mu_b_Emission"]
MXLmodel10$estimate["mu_b_Performance"]


#### MXL11: Pref-space  price fixed, perf normal, em unif ####
apollo_control = list(
  modelName ="MXL11",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Emission"),
  interNormDraws = c("draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel11 = apollo_estimate(apollo_beta, apollo_fixed,
                            apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel11,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(MXLmodel11, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="b_Price"))
apollo_deltaMethod(MXLmodel11, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="b_Price"))


#### MXL12: WTP-space price fixed, perf normal, em unif ####
apollo_control = list(
  modelName ="MXL12",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                b_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Performance"),
  interNormDraws = c("draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel12 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel12,modelOutput_settings = list(printPVal=TRUE))

MXLmodel12$estimate["mu_b_Emission"]
MXLmodel12$estimate["mu_b_Performance"]


#### MXL13: Pref-space  price and perf normal, em unif ####
apollo_control = list(
  modelName ="MXL13",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Emission"),
  interNormDraws = c("draws_Price","draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel13 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel13,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(MXLmodel13, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="mu_Price"))
apollo_deltaMethod(MXLmodel13, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="mu_Price"))


#### MXL14: WTP-space price fixed, perf normal, em unif ####
apollo_control = list(
  modelName ="MXL14",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Emission"),
  interNormDraws = c("draws_Price","draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel14 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel14,modelOutput_settings = list(printPVal=TRUE))

MXLmodel14$estimate["mu_b_Emission"]
MXLmodel14$estimate["mu_b_Performance"]

#### MXL15: Pref-space  price unif, both normal ####
apollo_control = list(
  modelName ="MXL15",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c("draws_Price"),
  interNormDraws = c("draws_Emission","draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  (mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel15 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel15,modelOutput_settings = list(printPVal=TRUE))

apollo_deltaMethod(MXLmodel15, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Emission", parName2="mu_Price"))
apollo_deltaMethod(MXLmodel15, deltaMethod_settings=list(operation="ratio", parName1="mu_b_Performance", parName2="mu_Price"))


#### MXL16: WTP-space price unif, both normal ####
apollo_control = list(
  modelName ="MXL16",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                mu_b_Performance = 0,
                sig_b_Performance = 0,
                mu_b_Emission = 0,
                sig_b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price","draws_Emission","draws_Performance"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  (mu_b_Performance + sig_b_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_b_Emission + sig_b_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel16 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel16,modelOutput_settings = list(printPVal=TRUE))

MXLmodel16$estimate["mu_b_Emission"]
MXLmodel16$estimate["mu_b_Performance"]
exp(MXLmodel16$estimate["mu_b_Emission"]+MXLmodel16$estimate["sig_b_Emission"]^2/2)

#### MXL17: Pref-space  lognormal price ####
apollo_control = list(
  modelName ="MXL17",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                b_Performance = 0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*Price_A + b_Performance*Performance_A + b_Emission*Emission_A
  V[['B']] = asc_B + b_Price*Price_B + b_Performance*Performance_B + b_Emission*Emission_B
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel17 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel17,modelOutput_settings = list(printPVal=TRUE))

MXLmodel17$estimate["b_Emission"]/-exp(MXLmodel18$estimate["mu_Price"]+MXLmodel18$estimate["sig_Price"]^2/2)
MXLmodel17$estimate["b_Performance"]/-exp(MXLmodel18$estimate["mu_Price"]+MXLmodel18$estimate["sig_Price"]^2/2)


#### MXL18: WTP-space price lognormals ####
apollo_control = list(
  modelName ="MXL18",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =0,
                sig_Price    =0,
                b_Performance = 0,
                b_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MXLmodel18 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel18,modelOutput_settings = list(printPVal=TRUE))


#### MXL19: WTP all lognormals #### 
apollo_control = list(
  modelName ="MXL19",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price","draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
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

MXLmodel19 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel19,modelOutput_settings = list(printPVal=TRUE))

-exp(MXLmodel19$estimate["mu_Performance"]+MXLmodel19$estimate["sig_Performance"]^2/2)
-exp(MXLmodel19$estimate["mu_Emission"]+MXLmodel19$estimate["sig_Emission"]^2/2)
-exp(MXLmodel19$estimate["mu_Price"]+MXLmodel19$estimate["sig_Price"]^2/2)


#### MXL20: WTP all lognormals plus covariates #### 
apollo_control = list(
  modelName ="MXL20",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,
                mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0,
                b_Gender = 0,
                b_Age      = 0,
                b_Distance = 0,
                b_Trips    = 0,
                b_BP       = 0,
                b_Charity  = 0,
                b_Education  = 0,
                b_Employment = 0,
                b_Income     = 0,
                b_Order      = 0,
                b_Task       = 0,
                b_Cons       = 0,
                b_Experts    = 0,
                b_Understanding =0,
                b_Certainty=0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price","draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  randcoeff[["asc_B"]] = asc_BB + b_Gender*Q1Gender + b_Age*Age +
    b_Distance * Distance + 
    b_Trips * Trips +
    b_BP * BP +
    b_Charity * Charity + 
    b_Education * Education +
    b_Employment * Employment + 
    b_Income * Income +
    b_Order * Order +      
    b_Task * Task +       
    b_Cons * Consequentiality +       
    b_Experts * Experts+
    b_Understanding*Survey +
    b_Certainty*Q12CECertainty
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
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
# 
MXLmodel20 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel20,modelOutput_settings = list(printPVal=TRUE))


#### MXL20: WTP all lognormals plus covariates TRUNCATED #### 
Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database <- Test_Truncated
apollo_control = list(
  modelName ="MXL20",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_BB      = 0,
                mu_Price    =-3,
                sig_Price=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0,
                b_Gender = 0,
                b_Age      = 0,
                b_Distance = 0,
                b_Trips    = 0,
                b_BP       = 0,
                b_Charity  = 0,
                b_Education  = 0,
                b_Employment = 0,
                b_Income     = 0,
                b_Order      = 0,
                b_Task       = 0,
                b_Cons       = 0,
                b_Experts    = 0,
                b_Understanding =0,
                b_Certainty=0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price","draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + sig_Emission * draws_Emission )
  randcoeff[["asc_B"]] = asc_BB + b_Gender*Q1Gender + b_Age*Age +
    b_Distance * Distance + 
    b_Trips * Trips +
    b_BP * BP +
    b_Charity * Charity + 
    b_Education * Education +
    b_Employment * Employment + 
    b_Income * Income +
    b_Order * Order +      
    b_Task * Task +       
    b_Cons * Consequentiality +       
    b_Experts * Experts+
    b_Understanding*Survey +
    b_Certainty*Q12CECertainty
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
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
# 
MXLmodel21 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel21,modelOutput_settings = list(printPVal=TRUE))

xtable::xtable(cbind(data.frame(rownames(MXL20)),round(ldply(MXL20$Estimate,.fun =function(b2) { (exp(-(MXL20$Estimate[2]+(b2*1)))/(1 +exp(-(MXL20$Estimate[2]+(b2*1))))^2)*b2  }),3)))
xtable::xtable(cbind(data.frame(rownames(MXL21)),round(ldply(MXL21$Estimate,.fun =function(b2) { (exp(-(MXL20$Estimate[2]+(b2*1)))/(1 +exp(-(MXL21$Estimate[2]+(b2*1))))^2)*b2  }),3)))

xtable::xtable(round(data.frame(apollo_modelOutput(MXLmodel20,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
xtable::xtable(round(data.frame(apollo_modelOutput(MXLmodel21,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)


#### MXL22: Correlated MXL19 #### 
apollo_control = list(
  modelName ="MXL19",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)
apollo_beta = c(asc_A      = 0,
                asc_B      = 0,
                mu_Price    =-3,
                sig_Price=0,
                b_perf_price_sig=0,
                emission_perf_sig=0,
                emission_price_sig=0,
                mu_Performance = -3,
                sig_Performance = 0,
                mu_Emission = -3,
                sig_Emission = 0)
apollo_fixed = c("asc_A")
apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 1000,
  interUnifDraws = c(),
  interNormDraws = c("draws_Price","draws_Performance","draws_Emission"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c())
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  randcoeff[["b_Price"]] =  -exp(mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  -exp(mu_Performance +b_perf_price_sig *draws_Price + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  -exp(mu_Emission + emission_perf_sig*draws_Performance + emission_price_sig*draws_Price + sig_Emission * draws_Emission )
  return(randcoeff)}
apollo_inputs = apollo_validateInputs()
apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['A']] = asc_A + b_Price*(Price_A + b_Performance*Performance_A + b_Emission*Emission_A)
  V[['B']] = asc_B + b_Price*(Price_B + b_Performance*Performance_B + b_Emission*Emission_B)
  mnl_settings = list(
    alternatives  = c(A=1, B=2),
    avail         = list(A=1, B=1),
    choiceVar     = Choice,
    V             = V)
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


MXLmodel22 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel22,modelOutput_settings = list(printPVal=TRUE))


View(cbind(t(round(cbind("1"=data.frame(MXLmodel$estimate["b_Emission"]/MXLmodel$estimate["mu_b_Price"]),
      "2"=data.frame(MXLmodel2$estimate["b_Emission"]),
      "3"=data.frame(MXLmodel3$estimate["b_Emission"]/MXLmodel3$estimate["mu_b_Price"]),
      "4"=data.frame(MXLmodel4$estimate["b_Emission"]),
      "5"=data.frame(MXLmodel5$estimate["mu_b_Emission"]/MXLmodel5$estimate["b_Price"]),
      "6"=data.frame(MXLmodel6$estimate["mu_b_Emission"]),
      "7"=data.frame(MXLmodel7$estimate["mu_b_Emission"]/MXLmodel7$estimate["b_Price"]),
      "8"=data.frame(MXLmodel8$estimate["mu_b_Emission"]),
      "9"=data.frame(MXLmodel9$estimate["mu_b_Emission"]/MXLmodel9$estimate["b_Price"]),
      "10"=data.frame(MXLmodel10$estimate["mu_b_Emission"]),
      "11"=data.frame(MXLmodel11$estimate["mu_b_Emission"]/MXLmodel11$estimate["b_Price"]),
      "12"=data.frame(MXLmodel12$estimate["mu_b_Emission"]),
      "13"=data.frame(MXLmodel13$estimate["mu_b_Emission"]/MXLmodel13$estimate["mu_Price"]),
      "14"=data.frame(MXLmodel14$estimate["mu_b_Emission"])),3)),t(round(cbind("1"=data.frame(MXLmodel$estimate["b_Performance"]/MXLmodel$estimate["mu_b_Price"]),
              "2"=data.frame(MXLmodel2$estimate["b_Performance"]),
              "3"=data.frame(MXLmodel3$estimate["b_Performance"]/MXLmodel3$estimate["mu_b_Price"]),
              "4"=data.frame(MXLmodel4$estimate["b_Performance"]),
              "5"=data.frame(MXLmodel5$estimate["mu_b_Performance"]/MXLmodel5$estimate["b_Price"]),
              "6"=data.frame(MXLmodel6$estimate["mu_b_Performance"]),
              "7"=data.frame(MXLmodel7$estimate["mu_b_Performance"]/MXLmodel7$estimate["b_Price"]),
              "8"=data.frame(MXLmodel8$estimate["mu_b_Performance"]),
              "9"=data.frame(MXLmodel9$estimate["mu_b_Performance"]/MXLmodel9$estimate["b_Price"]),
              "10"=data.frame(MXLmodel10$estimate["mu_b_Performance"]),
              "11"=data.frame(MXLmodel11$estimate["mu_b_Performance"]/MXLmodel11$estimate["b_Price"]),
              "12"=data.frame(MXLmodel12$estimate["mu_b_Performance"]),
              "13"=data.frame(MXLmodel13$estimate["mu_b_Performance"]/MXLmodel13$estimate["mu_Price"]),
              "14"=data.frame(MXLmodel14$estimate["mu_b_Performance"])),3))))