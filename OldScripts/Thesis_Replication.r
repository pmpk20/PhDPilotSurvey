#### Thesis Replication  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Estimation of the Value of precautionary restrictions on microplastics.


# R version 4.0.2 (2020-06-22)
# RStudio Version 1.3.959
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Package information:
# stats     graphics  grDevices utils     datasets  methods   base     
# Rcpp_1.0.5          BiocManager_1.30.10 compiler_4.0.2      RSGHB_1.2.2        
# prettyunits_1.1.1   miscTools_0.6-26    tools_4.0.2         digest_0.6.25      
# pkgbuild_1.0.8      lattice_0.20-41     Matrix_1.2-18       cli_2.0.2          
# rstudioapi_0.11     maxLik_1.4-4        mvtnorm_1.1-1       SparseM_1.78       
# xfun_0.15           coda_0.19-4         MatrixModels_0.4-1  grid_4.0.2         
# glue_1.4.1          R6_2.4.1            randtoolbox_1.30.1  processx_3.4.2     
# fansi_0.4.1         callr_3.4.3         ps_1.3.3            mcmc_0.9-7         
# MASS_7.3-51.6       assertthat_0.2.1    mnormt_2.0.2        xtable_1.8-4       
# numDeriv_2016.8-1.1 Deriv_4.1.0         quantreg_5.55       sandwich_2.5-1     
# tinytex_0.24        MCMCpack_1.4-9      rngWELL_0.10-6      tmvnsim_1.0-2      
# crayon_1.3.4        zoo_1.8-8           apollo_0.2.1   


# rm(list=ls())
pkgbuild::has_build_tools()
library(Hmisc)
library(xtable)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DCchoice)
library(apollo)
options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation
setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
Full_Final <- data.frame(read.csv("FinalData.csv")) # Import the final data for ease
Full_Full <- data.frame(read.csv("Full_Full.csv")) # Import the final data for ease
Test_Apollo <- data.frame(read.csv("Test_Apollo.csv")) # Import the final data for ease
Test_Truncated <- data.frame(read.csv("Test_Truncated.csv")) # Import the final data for ease
FullSurvey2 <- data.frame(read.csv("FullSurvey2.csv"))
# May need the following packages if above do not work: 
## Run the following on first run:
# Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Rtools\\bin", sep = ";"))
# pkgbuild::find_rtools(debug = TRUE)
# install.packages("farver")
# install.packages("lifecycle")
# install.packages("mnormt")
# install.packages("RSGHB")
# install.packages("Rcpp") ## Necessary dependency for rngWELL
# install.packages("rngWELL") ## Can sometimes fix APOLLO issues
# install.packages("randtoolbox") ## Necessary for APOLLO random draws
# # install.packages("apollo") ## Most complex and powerful library for discrete choice
# install.packages("H:/dos/PhD/Other/Training courses/CMC/apollo_libraries/apollo_0.2.1.zip", repos = NULL, type = "win.binary")
# install.packages("mlogit") ## MLOGIT is the best DCE package in R so far.
# install.packages("gmnl") ## Very similar to MLOGIT but more flexibility.
# install.packages("stargazer") ## To export to LaTeX code.
# install.packages("dplyr") ## For data manipulation
# install.packages("Hmisc") ## For random imputation


## Import Data: 
Full_Final <- data.frame(read.csv("FinalData.csv")) ## Imports from the excel file straight from the survey companies website.


#### Section 2: Sample truncation ####


## ID of protestors ID'd by eyeballing text responses.
ProtestVotes <- c(14,24,33,39,44,61,79,106,121,130,149,163,182,200,203,211,214,215,217,
                  239,244,249,251,252,267,282,290,306,320,326,327,341,343,362,363,364,
                  371,374,380,393,399,407,414,425,426,464,467,477,479,480,519,524,536,
                  545,547,557,567,579,590,591,595,614,629,639,649,651,654,665,674,680,
                  915,931,932,933,935,940,950,953,959,960,975,978,989,996,1002,1011,1024,1026,1027,1028)

## Combining all protest rules: 
AllCriteria <- data.frame("IDs" = unique(Full_Final$ID[ (Full_Final$Q25Understanding >=7) &
                                                          (Full_Final$Q8DominatedTest == 0) &
                                                          (Full_Final$Q12CECertainty >= 1) &
                                                          (Full_Final$Q20Consequentiality >= 1) ])) 

## Here checking if any of the remaining respondents were on the protest list: 
AllCriteria <- AllCriteria[ !(AllCriteria$IDs %in%c(ProtestVotes)),]

## Apply all rules:
Full_Full <- Full_Final[ (Full_Final$ID) %in% c(AllCriteria),] ## Fully truncated:

# Verify final sample size - N=304
nrow(Full_Full)/8



#### CE Chapter  ####



#### Conditional Logit: C4 ####
Test_Apollo <- data.frame(read.csv("Test_Apollo.csv")) # Import the final data for ease
Test_Apollo$Performance_B <- Test_Apollo$Performance_B*-1
database<- Test_Apollo
library(apollo)

apollo_initialise()

apollo_control = list(
  modelName  ="CLModel", indivID    ="ID")

apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              b_Price    = 0,
              b_Performance   = 0,
              b_Emission      = 0)

apollo_fixed = c("asc_A")

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P = list()
  
  V = list()
  V[['A']]  = asc_A  + b_Price * Price_A +  b_Performance  * Performance_A + b_Emission * Emission_A
  V[['B']]  = asc_B  + b_Price * Price_B +  b_Performance  * Performance_B  + b_Emission * Emission_B
  
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

CLmodel = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
# ,estimate_settings = list(bootstrapSE=1,bootstrapSeed = 24))

apollo_modelOutput(CLmodel,modelOutput_settings = list(printPVal=TRUE))

## WTP calculations: 
apollo_deltaMethod(CLmodel, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(CLmodel, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))


#### Multinomial Logit: C4 ####

Test_Apollo <- data.frame(read.csv("Test_Apollo.csv")) # Import the final data for ease
Test_Apollo$Performance_B <- Test_Apollo$Performance_B*-1
database<- Test_Apollo
library(apollo)
library(stats)
apollo_control = list(
  modelName  ="Replicating Full_MNL",
  indivID    ="ID"
)

## Set parameters and their initial values here 
apollo_beta=c(asc_A      = 0,
              asc_B      = 0,
              b_Price    = 0,
              b_Performance   = 0,
              b_Emission      = 0,
              b_Gender = 0,
              b_Age      = 0,
              b_Distance = 0,
              b_Trips    = 0,
              b_BP       = 0,
              b_Charity  = 0,
              b_Education  = 0,
              b_Income     = 0,
              b_Order      = 0,
              b_Task       = 0,
              b_Cons       = 0,
              b_Experts    = 0,
              b_Understanding =0,
              b_Certainty=0)

## Set one of the ASCs as zero using the utility-difference approach: 
apollo_fixed = c("asc_A")

## Check model is good so far 
apollo_inputs = apollo_validateInputs()


apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ## Create list of probabilities P
  P = list()
  
  ## Must specify SDs against the ASC directly
  asc_B1 = asc_B + b_Gender*Q1Gender + b_Age*Age +
    b_Distance * Distance + 
    b_Trips * Trips +
    b_BP * BP +
    b_Charity * Charity + 
    b_Education * Education +
    b_Income * IncomeDummy +
    b_Order * Order +      
    b_Task * Task +       
    b_Cons * Consequentiality +       
    b_Experts * Experts+
    b_Understanding*Survey +
    b_Certainty*Q12CECertainty
  
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['A']]  = asc_A        + b_Performance  * Performance_A + b_Emission * Emission_A + b_Price * Price_A
  V[['B']]  = asc_B1  + b_Performance  * Performance_B  + b_Emission * Emission_B + b_Price * Price_B
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives = c(A=1, B=2),
    avail        = list(A=1, B=1),
    choiceVar    = Choice,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

MNL2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(MNL2,modelOutput_settings = list(printPVal=TRUE))
saveRDS(MNL2,"MNL2.rds")
xtable(data.frame(apollo_modelOutput(MNL2,modelOutput_settings = list(printPVal=TRUE))),digits=3)

apollo_deltaMethod(MNL2, list(operation="ratio", parName1="b_Performance", parName2="b_Price"))
apollo_deltaMethod(MNL2, list(operation="ratio", parName1="b_Emission", parName2="b_Price"))



#### Mixed Logit: C4 ####


database <- Test_Truncated
apollo_control = list(
  modelName ="MXL20ID",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 4)


apollo_beta = c(asc_A      = 0,      asc_BB      = 0,
                mu_Price    =-3,     sig_Price=0,
                mu_Performance = -3, sig_Performance = 0,
                mu_Emission = -3,    sig_Emission = 0,
                b_Gender = 0,   b_Age      = 0,
                b_Distance = 0, b_Trips    = 0,
                b_BP       = 0, b_Charity  = 0,
                b_Education  = 0, 
                b_Income     = 0, b_Order      = 0,
                b_Task       = 0, b_Cons       = 0,
                b_Experts    = 0, b_Understanding =0,
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
    b_Income * IncomeDummy +
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
MXLmodel21ID = apollo_estimate(apollo_beta, apollo_fixed,
                               apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel21ID,modelOutput_settings = list(printPVal=TRUE))
saveRDS(MXLmodel21ID,"MXLmodel21ID.rds")
xtable(data.frame(apollo_modelOutput(MXLmodel21ID,modelOutput_settings = list(printPVal=TRUE))),digits=3)


c(MXLmodel21ID$estimate["mu_Performance"],MXLmodel21ID$estimate["mu_Emission"])



#### Latent-Class: C4 ####
database <- Test_Truncated
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
# estimate_settings=list(writeIter=FALSE,bootstrapSE=10)

### Estimate model
LCMStandard3CNoSDTRUNCATED = apollo_estimate(apollo_beta, apollo_fixed, 
                                             apollo_probabilities, apollo_inputs)
apollo_modelOutput(LCMStandard3CNoSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))
saveRDS(LCMStandard3CNoSDTRUNCATED,"LCMStandard3CNoSDTRUNCATED.rds")
xtable(data.frame(apollo_modelOutput(LCMStandard3CNoSDTRUNCATED,modelOutput_settings = list(printPVal=TRUE))),digits=3)
data.frame(LCMStandard3CNoSDTRUNCATED$componentReport$model$param)[4:7,]
round(cbind(rbind("Performance_A"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Performance_a", parName2="beta_Price_a")))$Value,
            "Emission_A"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Emission_a", parName2="beta_Price_a")))$Value),
      rbind("Performance_B"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Performance_b", parName2="beta_Price_b")))$Value,
            "Emission_B"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Emission_b", parName2="beta_Price_b")))$Value),
      rbind("Performance_C"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Performance_c", parName2="beta_Price_c")))$Value,
            "Emission_C"=data.frame(apollo_deltaMethod(LCMStandard3CNoSDTRUNCATED, list(operation="ratio", parName1="beta_Emission_c", parName2="beta_Price_c")))$Value)),3)


#### ICELV: C4 ####

library(apollo)

Test_Truncated =Test_Apollo[ (Test_Apollo$ID) %in% c(AllCriteria),] 
database = Test_Truncated
apollo_initialise()


apollo_control = list(
  modelName  = "ICLVTruncID",
  modelDescr = "ICLVTruncID",
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
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_Cons*Consequentiality + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty + eta
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

CEmodel4ID = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(CEmodel4ID,modelOutput_settings = list(printPVal=TRUE))
apollo_saveOutput(CEmodel4ID)
saveRDS(CEmodel4ID, file="CEmodel4ID.rds")
CEWTP(CEmodel4ID)
xtable::xtable(round(data.frame(apollo_modelOutput(CEmodel4ID,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
round(median((randcoeff$b_Performance+randcoeff$LV/randcoeff$b_Price)),3)
round(median((randcoeff$b_Emission+randcoeff$LV/randcoeff$b_Price)),3)



#### CV Chapter  ####
library(Icens)
library(DCchoice)


#### Q6: C5  ####


## KMT
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = Full_Long)
summary(ResearchKMT)
plot(ResearchKMT,main="Q6 Kaplan-Meier-Turnbull survival function")


## Bid:
Research_BidOnly_Trunc <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Full_Full,dist="logistic")
summary(Research_BidOnly_Trunc) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


## Covariates:
Research_TruncatedID <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                   Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                 Q20Consequentiality+ Q21Experts+ Q25Understanding + Q6ResearchCertainty| Q6Bid, data = Full_Final,dist="normal")
summary(Research_TruncatedID)
krCI(Research_TruncatedID)
round(data.frame(c(summary(Research_TruncatedID)$glm.summary[12])),3)

xtable(cbind(round(data.frame(c(summary(Research_TruncatedID)$glm.summary[12])),3)["coefficients.Estimate"],
      rbind(0,data.frame(round(probitmfx(formula = Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                           Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                           Q20Consequentiality+ Q21Experts+ Q25Understanding + Q6ResearchCertainty+Q6Bid,data = Full_Final,robust = TRUE)$mfxest[,1],3))),
      round(data.frame(c(summary(Research_TruncatedID)$glm.summary[12])),3)["coefficients.Std..Error"],
      round(data.frame(c(summary(Research_TruncatedID)$glm.summary[12])),3)["coefficients.Pr...z.."]))


## Q6 ICLV:

# Setup the data for all truncated models:
database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1


# Estimate model:
apollo_control = list(
  modelName  = "CVmodel6NTNewID",
  modelDescr = "CVmodel6NTNewID",
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
                gamma_BP =0,
                gamma_Charity =0,
                gamma_Certainty=0,
                gamma_Cons=0,
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
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_BP*BP + gamma_Charity*Charity +gamma_Certainty*Q12CECertainty +gamma_Cons*Consequentiality + eta
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

CVmodel6NTNewID = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel6NTNewID,modelOutput_settings = list(printPVal=TRUE))
xtable::xtable(round(data.frame(apollo_modelOutput(CVmodel6NTNewID,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
saveRDS(CVmodel6NTNewID,"CVmodel6NTNewID.rds")
CVmodel6NTNewID <- readRDS("CVmodel6NTNewID.rds")
# WTP:
Model <- CVmodel6NTNewID
CVunconditionals7F <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs)
ModelWTP <-apply((-Model$estimate["intercept"]/Model$estimate["b_bid"]+CVunconditionals7F$LV)*100,MARGIN = 1,FUN = mean)
CVmodel6NTNewIDWTP <- median(-Model$estimate["intercept"]+CVunconditionals7F$LV/Model$estimate["b_bid"])


#### Q7 SB: C5 ####

## KMT:
TreatmentKMTSB <- turnbull.sb(formula = Q7TreatmentResponse  ~  Q7Bid ,data = FullSurvey2)
summary(TreatmentKMTSB)
plot(TreatmentKMTSB,main="Q7 Single-Bound KMT survival function")


## Bid:
Treatment_BidOnly_Trunc <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Full_Full,dist="logistic")
summary(Treatment_BidOnly_Trunc) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


## Covariates:
Treatment2_SBID <- sbchoice(Q7TreatmentResponse ~  Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                              Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                              Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty | Q7Bid ,data = Full_Full,dist="logistic")
summary(Treatment2_SBID)
krCI(Treatment2_SBID)
round(data.frame(c(summary(Treatment2_SBID)$glm.summary[12])),3)
View(round(krCI(Treatment2_SBID)$out,2))


xtable(cbind(round(data.frame(c(summary(Treatment2_SBID)$glm.summary[12])),3)["coefficients.Estimate"],
             rbind(0,data.frame(round(probitmfx(formula = Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                                  Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                                  Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty+Q7Bid,data = Full_Full,robust = TRUE)$mfxest[,1],3))),
             round(data.frame(c(summary(Treatment2_SBID)$glm.summary[12])),3)["coefficients.Std..Error"],
             round(data.frame(c(summary(Treatment2_SBID)$glm.summary[12])),3)["coefficients.Pr...z.."]),digits=3)


## Q7 ICLV:

# Setup the data for all truncated models:
database <- Test_Truncated
database$Q6Bid <- database$Q6Bid/100
database$Q7Bid <- database$Q7Bid/100
database$Q6ResearchResponse <-database$Q6ResearchResponse-1
database$Q7TreatmentResponse <-database$Q7TreatmentResponse-1

apollo_control = list(
  modelName  = "CVmodel7NTNewID",
  modelDescr = "CVmodel7NTNewID",
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
                gamma_BP =0,
                gamma_Charity =0,
                gamma_Certainty=0,
                gamma_Cons=0,
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
  randcoeff[["LV"]] = gamma_Age*Age +gamma_Gender*Q1Gender + gamma_Distance*Distance + gamma_Income*IncomeDummy + gamma_Experts*Experts + gamma_BP*BP + gamma_Charity*Charity + gamma_Certainty*Q12CECertainty +gamma_Cons*Consequentiality + eta
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

CVmodel7NTNewID = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

apollo_modelOutput(CVmodel7NTNewID,modelOutput_settings = list(printPVal=TRUE))
saveRDS(CVmodel7NTNewID,"CVmodel7NTNewID.rds")
CVmodel7NTNewID <- readRDS("CVmodel7NTNewID.rds")
xtable::xtable(round(data.frame(apollo_modelOutput(CVmodel7NTNewID,modelOutput_settings = list(printPVal=TRUE))),3),digits=3)
# WTP:
Model <- CVmodel7NTNewID
CVunconditionals7F <- apollo_unconditionals(Model,apollo_probabilities,apollo_inputs)
ModelWTP <-apply((-Model$estimate["intercept"]/Model$estimate["b_bid"]+CVunconditionals7F$LV)*100,MARGIN = 1,FUN = mean)
CVmodel7NTNewIDWTP <- median(-Model$estimate["intercept"]/Model$estimate["b_bid"]+CVunconditionals7F$LV)*100



#### Q7 DB: C5  ####

## KMT:
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = FullSurvey2)
summary(TreatmentKMT)
plot(TreatmentKMT,main="Q7 Double-Bound KMT survival function")


## Bid:
Treatment_BidOnly2_Trunc <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1 |Q7Bid + Q7Bid2, data = Full_Full,dist="normal")
summary(Treatment_BidOnly2_Trunc) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
View(round(krCI(Treatment_BidOnly2_Trunc)$out,2))


## Covariates:
Treatment_Truncated_DB_ID <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                        Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                        Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty| Q7Bid + Q7Bid2,data = Full_Full,dist="normal")
summary(Treatment_Truncated_DB_ID)
krCI(Treatment_Truncated_DB_ID)
# bootCI(Treatment_Truncated_DB_ID)
round(data.frame((summary(Treatment_Truncated_DB_ID)$coef)),3)

xtable(cbind(round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,1]),3),
             rbind(0,data.frame(round(probitmfx(formula = Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                                  Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                                  Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty+Q7Bid,data = Full_Full,robust = TRUE)$mfxest[,1],3))),
             round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,2]),3),
             round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,4]),3)),digits=3)


