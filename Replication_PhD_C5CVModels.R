#### Thesis Replicating Chapter Five: Contingent Valuation Analysis ###############
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
#### Section 2: Question Six All Models ####
## Estimates bid-only then variables models
## See Section 5.3
----------------------------------------------------------------------------------------------------------
  
  

## Kaplan-Meier Turnbull Model for Section 5.3.1:
ResearchKMT <- turnbull.sb(formula = Q6ResearchResponse ~ Q6Bid,data = Full_Final)
summary(ResearchKMT)
plot(ResearchKMT,main="Q6 Kaplan-Meier-Turnbull survival function")


## Bid:-Only Model for Section 5.3.2:
Research_BidOnly_Trunc <- sbchoice(Q6ResearchResponse ~ 1 | Q6Bid, data = Full_Final,dist="logistic")
summary(Research_BidOnly_Trunc) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


## Model with covariates for Section 5.3.3:
Research_FinalModel <- sbchoice(Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                   Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                   Q20Consequentiality+ Q21Experts+ Q25Understanding + Q6ResearchCertainty| Q6Bid, data = Full_Final,dist="normal")
summary(Research_FinalModel)


## This estimates point, lower, and upper bounds for WTP
krCI(Research_FinalModel) 


## Here I export the model results to LaTeX in one formula:
xtable(cbind(round(data.frame(c(summary(Research_FinalModel)$glm.summary[12])),3)["coefficients.Estimate"],
             rbind(0,data.frame(round(probitmfx(formula = Q6ResearchResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                                  Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                                  Q20Consequentiality+ Q21Experts+ Q25Understanding + Q6ResearchCertainty+Q6Bid,data = Full_Final,robust = TRUE)$mfxest[,1],3))),
             round(data.frame(c(summary(Research_FinalModel)$glm.summary[12])),3)["coefficients.Std..Error"],
             round(data.frame(c(summary(Research_FinalModel)$glm.summary[12])),3)["coefficients.Pr...z.."]),digits=3)



----------------------------------------------------------------------------------------------------------
#### Section 3: Question Seven SBDC Models ####
## Estimates bid-only then variables models
## See Section 5.4.1
----------------------------------------------------------------------------------------------------------
  
  

## Single-Bound Kaplan-Meier Turnbull Model for Section 5.4.1:
TreatmentKMTSB <- turnbull.sb(formula = Q7TreatmentResponse  ~  Q7Bid ,data = Full_Final)
summary(TreatmentKMTSB)
plot(TreatmentKMTSB,main="Q7 Single-Bound KMT survival function")


## Bid:
Treatment_BidOnly <- sbchoice(Q7TreatmentResponse ~ 1 | Q7Bid, data = Full_Final,dist="logistic")
summary(Treatment_BidOnly) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.


## Covariates:
Treatment_SB_Model <- sbchoice(Q7TreatmentResponse ~  Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                              Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                              Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty | Q7Bid ,data = Full_Final,dist="logistic")
summary(Treatment_SB_Model)


## Outputting WTP:
xtable(data.frame(krCI(Treatment_SB_Model)$out),digits=3)



## Results to LaTeX table
### Remove xtable() to see results
xtable(cbind(round(data.frame(c(summary(Treatment_SB_Model)$glm.summary[12])),3)["coefficients.Estimate"],
             rbind(0,data.frame(round(probitmfx(formula = Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                                  Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                                  Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty+Q7Bid,data = Full_Final,robust = TRUE)$mfxest[,1],3))),
             round(data.frame(c(summary(Treatment_SB_Model)$glm.summary[12])),3)["coefficients.Std..Error"],
             round(data.frame(c(summary(Treatment_SB_Model)$glm.summary[12])),3)["coefficients.Pr...z.."]),digits=3)



----------------------------------------------------------------------------------------------------------
#### Section 3: Question Seven DBDC Models ####
## Estimates bid-only then variables models
## See Section 5.4.2
----------------------------------------------------------------------------------------------------------
  


## KMT:
TreatmentKMT <- turnbull.db(formula = Q7TreatmentResponse + Q7Response2 ~  Q7Bid + Q7Bid2,data = Full_Final)
summary(TreatmentKMT)
plot(TreatmentKMT,main="Q7 Double-Bound KMT survival function")


## Bid:
Treatment_BidOnly2_Trunc <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ 1 |Q7Bid + Q7Bid2, data = Full_Final,dist="normal")
summary(Treatment_BidOnly2_Trunc) ## Reports the SBDC analysis for Q6 with mean, median and coefficients.
View(round(krCI(Treatment_BidOnly2_Trunc)$out,2))


## Covariates:
Treatment_Truncated_DB_ID <- dbchoice(Q7TreatmentResponse + Q7Response2 ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                        Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                        Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty| Q7Bid + Q7Bid2,data = Full_Final,dist="normal")
summary(Treatment_Truncated_DB_ID)


## Outputting WTP:
xtable(data.frame(krCI(Treatment_SB_Model)$out),digits=3)



## DBDC Model Output:
xtable(cbind(round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,1]),3),
             rbind(0,data.frame(round(probitmfx(formula = Q7TreatmentResponse ~ Q1Gender + Q2Age + Q3Distance+ Q4Trips +
                                                  Q16BP + Q18Charity + Q22Education+  IncomeDummy+Order+
                                                  Q20Consequentiality+ Q21Experts+ Q25Understanding + Q7TreatmentCertainty+Q7Bid,data = Full_Final,robust = TRUE)$mfxest[,1],3))),
             round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,2]),3),
             round(data.frame((c(summary(Treatment_Truncated_DB_ID)))$coef[,4]),3)),digits=3)


----------------------------------------------------------------------------------------------------------
#### Section 5: Question Seven ICLV ####
## Estimates bid-only then variables models
## See Section 5.5
----------------------------------------------------------------------------------------------------------
  
## Q7 ICLV:

# Setup the data for all models:
database <- Test_Apollo
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
