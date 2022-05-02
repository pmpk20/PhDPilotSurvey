#### Thesis Replicating Chapter Four: Choice Experiment Analysis ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Estimation of the Value of precautionary restrictions on microplastics.


----------------------------------------------------------------------------------------------------------
#### Section 0: Replication Information ####
## Putting sessionInfo() here in case it helps
----------------------------------------------------------------------------------------------------------
  
  
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19042)
  
# Matrix products: default
# locale:
# [1] LC_COLLATE=English_United Kingdom.1252 
# [2] LC_CTYPE=English_United Kingdom.1252   
# [3] LC_MONETARY=English_United Kingdom.1252
# [4] LC_NUMERIC=C                           
# [5] LC_TIME=English_United Kingdom.1252 

## PACKAGE INFORMATION ##:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] DCchoice_0.1.0 scales_1.1.1   magrittr_2.0.1 dplyr_1.0.7    apollo_0.2.5  
# [6] raster_3.4-13  sp_1.4-5      
# 
# loaded via a namespace (and not attached):
# [1] tidyselect_1.1.1    zoo_1.8-9           purrr_0.3.4        
# [4] splines_4.1.1       lattice_0.20-44     colorspace_2.0-2   
# [7] generics_0.1.0      vctrs_0.3.8         MCMCpack_1.5-0     
# [10] utf8_1.2.2          survival_3.2-11     rlang_0.4.11       
# [13] pkgbuild_1.2.0      pillar_1.6.2        glue_1.4.2         
# [16] MLEcens_0.1-4       matrixStats_0.60.0  lifecycle_1.0.0    
# [19] MatrixModels_0.5-0  munsell_0.5.0       mvtnorm_1.1-2      
# [22] codetools_0.2-18    coda_0.19-4         miscTools_0.6-26   
# [25] callr_3.7.0         SparseM_1.81        ps_1.6.0           
# [28] RSGHB_1.2.2         quantreg_5.86       parallel_4.1.1     
# [31] fansi_0.5.0         Rcpp_1.0.7          xtable_1.8-4       
# [34] conquer_1.0.2       BiocManager_1.30.16 tmvnsim_1.0-2      
# [37] mcmc_0.9-7          maxLik_1.5-2        mnormt_2.0.2       
# [40] interval_1.1-0.7    Icens_1.62.0        digest_0.6.27      
# [43] processx_3.5.2      numDeriv_2016.8-1.1 grid_4.1.1         
# [46] cli_3.0.1           tools_4.1.1         sandwich_3.0-1     
# [49] perm_1.0-0.0        tibble_3.1.3        Formula_1.2-4      
# [52] crayon_1.4.1        pkgconfig_2.0.3     MASS_7.3-54        
# [55] ellipsis_0.3.2      Matrix_1.3-4        prettyunits_1.1.1  
# [58] randtoolbox_1.30.1  rstudioapi_0.13     R6_2.5.0           
# [61] rngWELL_0.10-6      compiler_4.1.1 



----------------------------------------------------------------------------------------------------------
#### Section 1: PACKAGE SETUP ####
## Installs packages and imports data.
----------------------------------------------------------------------------------------------------------
  
  
  
  rm(list=ls())
pkgbuild::has_build_tools()
library(Hmisc)
library(xtable)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DCchoice)
library(apollo)
library(mfx)
options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation


## Data Imports: 
Full_Final <- data.frame(read.csv("FinalData.csv")) # Import the final data for ease
Test_Apollo <- data.frame(read.csv("Test_Apollo.csv")) # Import the final data for ease



----------------------------------------------------------------------------------------------------------
#### Conditional Logit: C4 ####
----------------------------------------------------------------------------------------------------------
  

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


----------------------------------------------------------------------------------------------------------
#### Multinomial Logit: C4 ####
----------------------------------------------------------------------------------------------------------

  
  
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




library(scales)
library(magrittr)
library(dplyr)
library(apollo)

database <- data.frame(read.csv("Test_Apollo.csv"))
database$Performance_B <- database$Performance_B*-1


----------------------------------------------------------------------------------------------------------
#### MXL19: Attributes only #### 
----------------------------------------------------------------------------------------------------------


apollo_control = list(
  modelName ="MXL19",  indivID   ="ID",  
  mixing    = TRUE, nCores    = 10)


apollo_beta = c(asc_A      = 0,      asc_B      = 0,
                mu_Price    =0,     sig_Price=0,
                mu_Performance = 0, sig_Performance = 0,
                mu_Emission = 0,    sig_Emission = 0)
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
  randcoeff[["b_Price"]] =  (mu_Price + sig_Price * draws_Price )
  randcoeff[["b_Performance"]] =  (mu_Performance + sig_Performance * draws_Performance )
  randcoeff[["b_Emission"]] =  (mu_Emission + sig_Emission * draws_Emission )
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

MXLmodel19 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs)

apollo_modelOutput(MXLmodel19,modelOutput_settings = list(printPVal=TRUE))

MXL19WTP <- apollo_conditionals(MXLmodel19,apollo_probabilities,apollo_inputs)
write.csv(MXL19WTP,"MXL19WTP.csv")


MXLmodel19C <- read.csv("MXL19WTP.csv")
MXLWTPConditionalF <-data.frame(cbind("Price"=MXLmodel19C$b_Price.post.mean,
                                      "Performance"=MXLmodel19C$b_Performance.post.mean,
                                      "Emissions"=MXLmodel19C$b_Emission.post.mean))
MXLWTPConditionalF %>% summarise(across(everything(),list(mean)))
