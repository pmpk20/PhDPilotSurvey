#Peter King
####################################################################################
############### Introduction: Pilot Data Analysis Script  ##########################
####################################################################################
# Useful links: https://cran.r-project.org/web/packages/support.CEs/support.CEs.pdf
# https://pdfs.semanticscholar.org/b0fb/05e51e02d4eda914888ae0590dd65b45ff9a.pdf
# https://rpubs.com/sallychen/313125
# https://www.sciencedirect.com/science/article/pii/S1098301516302911#bib45
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/hec.984


##########################################################################
############### Current Issues:                             ##############
############### -- Correct APOLLO MXL/HCM                       ########## 
############### -- Is the GMNL package worthwhile?              ##########
############### -- Fix the MXL and report in full               ##########
##########################################################################


####################################################################################
############### Section 1: Import Data  ##########################
####################################################################################


install.packages("dplyr") # Useful later for data manipulation
install.packages("mlogit")
install.packages("gmnl")
install.packages("apollo")
rm(list = ls())
############ Importing data:

setwd("H:/PhDPilotSurvey") # Sets working directory. This is where my Github repo is cloned to.

Pilot <- data.frame(read.csv("PhD Survey_ Sample A.csv")) # Imports the pilot survey data as a data.frame


####################################################################################
############### Section 2: Data pre-processing  ##########################
####################################################################################


Pilot <- Pilot[ -c(1,2,8,27)] #Drop rows of no importance to the quantitative analysis, namely text responses.

colnames(Pilot) <- c("Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Q7CE1", "Q8CE2","Q9CE3","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey") #Renames columns for ease of analysis

Pilot2 <- Pilot # Create a backup of the Pilot data

Pilot2 <- data.frame(Pilot2) # Force to the data.frame format

for (i in colnames(Pilot)){
  if (is.factor(Pilot[[i]]) == TRUE){
    Pilot2[[i]] <- as.numeric(Pilot[[i]])-1
  }
}

Pilot2$Q3Distance[Pilot2$Q3Distance == 1] <- 4
Pilot2$Q3Distance[Pilot2$Q3Distance == 0] <- 1
Pilot2$Q6QOV <- t(t(1-Pilot2$Q6QOV)) # Change the QOV coding so 1 is precaution
Pilot2$Q7CE1[Pilot2$Q7CE1 == 0] <-2 #The CE sets Status Quo as 1 and Alternative to 0 so this switches it around
Pilot2$Q7CE1[Pilot2$Q7CE1 == 1] <-0
Pilot2$Q7CE1[Pilot2$Q7CE1 == 2] <-1
Pilot2$Q8CE2[Pilot2$Q8CE2 == 0] <-2
Pilot2$Q8CE2[Pilot2$Q8CE2 == 1] <-0
Pilot2$Q8CE2[Pilot2$Q8CE2 == 2] <-1
Pilot2$Q9CE3[Pilot2$Q9CE3 == 0] <-2
Pilot2$Q9CE3[Pilot2$Q9CE3 == 1] <-0
Pilot2$Q9CE3[Pilot2$Q9CE3 == 2] <-1
Pilot2$Q17Understanding[Pilot2$Q17Understanding == 0] <-3 # Understanding question should be weak, average, strong not 1,2,0
Pilot2$Q20Education[Pilot2$Q20Education == 2] <-3 #Just shifting every value up one for education
Pilot2$Q20Education[Pilot2$Q20Education == 1] <-2
Pilot2$Q20Education[Pilot2$Q20Education == 0] <-1
Pilot2$Q21Employment[Pilot2$Q21Employment == 0] <-5 #Switch full to 5 for now
Pilot2$Q21Employment[Pilot2$Q21Employment == 1] <-0 #Change NEET to zero
Pilot2$Q21Employment[Pilot2$Q21Employment == 2] <-1 #Change Part to 1
Pilot2$Q21Employment[Pilot2$Q21Employment == 4] <-2 #Self stays same so student changes to 2
Pilot2$Q21Employment[Pilot2$Q21Employment == 5] <-4 #Put full back as highest value
Pilot2$Q22Income[Pilot2$Q22Income == 7] <-0.5 #The only wrong assignment of Employment was the 500-1000 level which it put last?
Pilot2$Q22Income[Pilot2$Q22Income == 6] <-7
Pilot2$Q22Income[Pilot2$Q22Income == 5] <-6
Pilot2$Q22Income[Pilot2$Q22Income == 4] <-5
Pilot2$Q22Income[Pilot2$Q22Income == 3] <-4
Pilot2$Q22Income[Pilot2$Q22Income == 2] <-3
Pilot2$Q22Income[Pilot2$Q22Income == 1] <-2
Pilot2$Q22Income[Pilot2$Q22Income == 0.5] <-1

SpecificChoices <- data.frame("Effectiveness.ALT" =c(0,0,0), 
                              "Env.ALT" =c(90,40,40),
                              "Price.ALT" =c(0,1,1),
                              "Health.ALT" =c(0,0.1, 0.6))

SQChoices <- data.frame("Effectiveness.SQ" =c(0,0,0), 
                              "Env.SQ" =c(0,0,0),
                              "Price.SQ" =c(0,0,0),
                              "Health.SQ" =c(1,1, 1))

Pilot2$ID <- seq.int(nrow(Pilot2)) # Adds an ID column to the Pilot survey. This is anonymous and bears no relation to actual respondents.

for (i in colnames(Pilot2)){
  if (is.factor(Pilot[[i]]) == TRUE){
    contrasts(Pilot2[,i]) <- contr.sum(nlevels(Pilot2[,i]))
  }
} ## Aim of the function is to express all variables in the Pilot data as factors

library(dplyr) # Important for data manipulation

Test <- cbind(slice(.data = Pilot2,rep(1:n(), each = nrow(SpecificChoices))),slice(.data = SpecificChoices,rep(1:n(), times = nrow(Pilot2))),slice(.data = SQChoices,rep(1:n(), times = nrow(Pilot2))))
# So TEST is a dataframe that transforms the PILOT data into an appropriate format for the estimation.
# The code repeats each row of the PILOT data for each choice the respondent made. Therefore, each respondent now has three rows one for Q7, Q8, Q9.

Choices <- data.frame(a = c(t(data.frame(rep(Pilot2[,7:9], times=1)))[,]))
# Forces the responses to Q7, Q8, Q9 from PILOT to be in one long column. Each respondent has three rows.

Test <- data.frame(Test$ID,rep(1:3,times=nrow(Pilot2)),Test[1:6],Choices,Test[10:23],Test[25:32])
# Combines and reorders the TEST dataframe. Use View(Test) to see that this dataframe combines the choice sets, responses, and respondent data.

colnames(Test) <- c("ID","Task","Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Choice","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey","Effectiveness_ALT","Accumulation_ALT","Price_ALT","Health_ALT","Effectiveness_SQ","Accumulation_SQ","Price_SQ","Health_SQ")
# Adds and updates column names.

Tests <- data.frame(Test[,1:2],Test$Choice,Test[,24:31])
# Takes the core elements of the TEST data frame

Test$av_ALT <- rep(1,nrow(Test))
Test$av_SQ <- rep(1,nrow(Test))
Tests <- Test
Test$Choice[Test$Choice == 0] <- "SQ"  ## Necessary here to change numeric to string
Test$Choice[Test$Choice == 1] <- "ALT" ## The MFORMULA looks for _SQ or _ALT so choice must be SQ or ALT




##########################################################################
############### DCE: MULTINOMIAL LOGIT               #####################
##########################################################################




library(mlogit) #Already have package installed
Test_Long <- mlogit.data(Test, shape = "wide", choice = "Choice",
                   varying = 24:31, sep = "_", id.var = "ID",
                   opposite = c("Price", "Effectiveness", "Accumulation", "Health"))
## This creates an MLOGIT object which is TEST coerced to a LONG format.
# write.csv(x = Test_Long,"H:/PhDPilotSurvey/Test_Long.csv") to export

## To trim the sample: 
Pilot_Dominated <- Test_Long[!Test_Long$ID %in% c(Test_Long$ID[ ((Test_Long$Task == 1) & (Test_Long$Choice ==0) & (grepl("SQ",rownames(Test_Long),fixed = TRUE) == FALSE)) ]),]
Pilot_Understanding <- Pilot_Dominated[!Pilot_Dominated$ID %in% c( unique(Pilot_Dominated$ID[Pilot_Dominated$Q23Survey <= 5])),]
Pilot_Cons <- Pilot_Understanding[!Pilot_Understanding$ID %in% c( unique(Pilot_Understanding$ID[Pilot_Understanding$Q18Consequentiality == 0])),]
# Test_Long <- Pilot_Cons

## Basic MNL: 
Base_MNL <- mlogit(Choice ~  Price + Health, 
                   Test_Long,
                   alt.subset = c("SQ","ALT"),reflevel = "SQ") ##Estimating a simple model first
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

## Full MNL:
Pilot_MNL <- mlogit(Choice ~ Price + Health | 
                      Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q6QOV 
                    + Q14BP + Q16Charity 
                    + Q17Understanding+ Q18Consequentiality
                    + Q19Experts +Q20Education+ Q21Employment
                    +  Q22Income+Q23Survey, 
                    Test_Long, alt.subset = c("SQ", "ALT"), 
                    reflevel = "SQ") ## Estimating a much larger MNL model with all the independent variables. 
summary(Pilot_MNL) ## Summarises the MNL output
## key things to change include the placing of the |. 


Pilot_MNLa <- mlogit(Choice ~ Price + Health | 
                      Q1Gender + Q14BP +Q18Consequentiality
                    + Q19Experts +Q21Employment, 
                    Test_Long, alt.subset = c("SQ", "ALT"), 
                    reflevel = "SQ") ## Estimating a much larger MNL model with all the independent variables. 
summary(Pilot_MNLa)

## Likelihood ratio test for models
lrtest(Base_MNL , Pilot_MNL) ## Likelihood Ratio test

## Marginal Effects
effects(Pilot_MNL, covariate = "Price", type = "rr") # Covariate change in column of ALT leads to adjacent column change in probability of selecting that option.
## i.e. changing the price of the ALT leads to a effects(M2, covariate = "Price", type = "rr")[2] change in the probability of selecting the ALT option.

## P values
coef(summary(Pilot_MNL))[,4] ##Returns P Values
PV <- data.frame(coef(summary(Pilot_MNL))[,4],coef(summary(Pilot_MNL))[,1] )
colnames(PV) <- c("PV","Effect")
PV <- subset(PV,PV <=0.05)
PV <- data.frame(row.names(PV), PV$PV, PV$Effect)
colnames(PV) <- c("Variables","PV","Effect")
barplot(PV$Effect, names.arg = PV$Variables,xlab = "Variables",ylab = "Effect",ylim = c(-2,5),axes = TRUE)


## WTP and MRS calculations:
coef(Pilot_MNL)["Heh"]/coef(Pilot_MNL)["Price"] ## For some reason, "Health" comes up as "Heh" ??




##########################################################################
############### DCE: MIXED LOGIT                     #####################
##########################################################################


########### Replication example:
MIXLcorrelated <- mlogit(Choice~Price+Accumulation|0,
                            Test_Long,
                            rpar=c(Price="n",
                            Accumulation="n"),
                            R=600,
                            halton=NULL,
                            print.level=0,
                            panel=TRUE,correlation = TRUE)
summary(MIXLcorrelated)
MIXLuncorrelated <- mlogit(Choice~Price+Accumulation|0,
              Test_Long,
              rpar=c(Price="n",
                     Accumulation="n"),
              R=600,
              halton=NULL,
              print.level=0,
              panel=TRUE,correlation = FALSE,seed=123,start=as.vector(rep(0,4)))
summary(MIXLuncorrelated)


## Tests of the MXL models
lr.corr <- lrtest(MIXLcorrelated, MIXLuncorrelated)
library(car)
lh.corr <- linearHypothesis(MIXLcorrelated, c("chol.Price:Accumulation = 0","chol.Price:Price = 0", "chol.Accumulation:Accumulation = 0"))
wd.corr <- waldtest(MIXLcorrelated, correlation = FALSE)
sc.corr <- scoretest(MIXLuncorrelated, correlation = TRUE)
statpval <- function(x){
  if (inherits(x, "anova"))
    result <- as.matrix(x)[2, c("Chisq", "Pr(>Chisq)")]
  if (inherits(x, "htest")) result <- c(x$statistic, x$p.value)
  names(result) <- c("stat", "p-value")
  round(result, 3)
}
sapply(list(wald = wd.corr, lh = lh.corr, score = sc.corr, lr = lr.corr),
       statpval)


# Extracts individual-level parameters
indpar <- fitted(MIXLcorrelated, type = "parameters")
head(indpar)

Variables <- c('Q1Gender + Q2Age + 
                 Q3Distance + Q4Trips + Q6QOV+ Q10Action +  
                 Q11Self + Q12Others + Q13Marine + Q14BP + 
                 Q16Charity + Q17Understanding+ 
                 Q18Consequentiality + Q19Experts +Q20Education+ 
                 Q21Employment +  Q22Income+Q23Survey')

# Estimates the full MXL model with all covariates
MXLFull <- mlogit(
  Choice ~  Price + Health|  Q1Gender + Q2Age + 
    Q3Distance + Q4Trips + Q6QOV+ Q10Action +  
    Q11Self + Q12Others + Q13Marine + Q14BP +
    Q16Charity + Q17Understanding+ 
    Q18Consequentiality + Q19Experts +Q20Education+ 
    Q21Employment +  Q22Income+Q23Survey,
  Test_Long, rpar=c(Price="n"),R=10,correlation = FALSE,
  halton=NA,method="bhhh",panel=TRUE,seed=123)
summary(MXLFull)
## Can remove intercept by replacing "Health | Q1Gender" with "Health | +0 + Q1Gender"  


# Testing Pvalues to find a good model
PV <- data.frame(coef(summary(MXLFull))[,4],coef(summary(MXLFull))[,1] )
colnames(PV) <- c("PV","Effect")
PV <- subset(PV,PV <=0.05)
PV <- data.frame(row.names(PV), PV$PV, PV$Effect)
colnames(PV) <- c("Variables","PV","Effect")
barplot(PV$Effect, names.arg = PV$Variables,xlab = "Variables",ylab = "Effect",ylim = c(-2,5),axes = TRUE)
length(PV$PV)


# Reports MRS in WTP Space 
summary(rpar(MXLFull,"Health",norm="Price"))
mean(rpar(MXLFull, "Health", norm = "Price"))


##########################################################################
############### DCE: GMNL package                    #####################
##########################################################################


## MIXL model with observed heterogeneity
library(gmnl)
mixl.hier <- gmnl(Choice ~  Price +  Q1Gender + Q2Age + 
                    Q3Distance + Q4Trips + Q6QOV+ Q10Action +  
                    Q11Self + Q12Others + Q13Marine + Q14BP + 
                    Q16Charity + Q17Understanding+ 
                    Q18Consequentiality + Q19Experts +Q20Education+ 
                    Q21Employment +  Q22Income+Q23Survey
                  | 1 | 0 | Accumulation  - 1,
                  data = Test_Long,
                  model = "mixl",
                  ranp = c( Price = "n"),
                  mvar = list(c("Accumulation")),
                  R = 30,
                  haltons = NA)
summary(mixl.hier)
wtp.gmnl(mixl.hier,wrt = "Price")


##########################################################################
############### DCE: NESTED LOGIT                    #####################
##########################################################################


# Can setup a NL object:
Test_LongNL <- mlogit.data(Test, shape = "wide", choice = "Choice",
                         varying = 24:31, sep = "_", id.var = "ID",
                         group.var = "Price")

# No luck estimating a NL model as I can't think of the nests


##########################################################################
############### Trimmed sample #####################
##########################################################################


Test_Long <- data.frame(read.csv("Test_Long.csv"))
Test_Long$alt <- as.numeric(Test_Long$alt)
Test_Long$alt[Test_Long$alt == 2] <- 0
Test_Long$Choice <- as.integer(Test_Long$Choice)

Pilot_Dominated <- Tests[!Tests$ID %in% c(Tests$ID[ ((Tests$Task == 1) & (Tests$Choice ==0))]),]
Pilot_Understanding <- Pilot_Dominated[!Pilot_Dominated$ID %in% c( unique(Pilot_Dominated$ID[Pilot_Dominated$Q23Survey <= 5])),]
Pilot_Cons <- Pilot_Understanding[!Pilot_Understanding$ID %in% c( unique(Pilot_Understanding$ID[Pilot_Understanding$Q18Consequentiality == 0])),]



##########################################################################
############### Apollo: MNL: WORKS                   #####################
##########################################################################
# rm(list = ls())
library(apollo)
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MY_Apollo_example",
  modelDescr ="Simple MNL model on my Pilot data",
  indivID    ="ID",
  HB = FALSE,
  mixing=FALSE
)

Test_Apollo <- data.frame(Tests$ID,Tests$Task, Tests$Q1Gender,Tests$Q2Age,Tests$Q3Distance,Tests$Q4Trips,Tests$Q6QOV,Tests$Q10Action,Tests$Q11Self,Tests$Q12Others,Tests$Q13Marine,Tests$Q14BP,Tests$Q16Charity,Tests$Q17Understanding,Tests$Q18Consequentiality,Tests$Q19Experts,Tests$Q20Education,Tests$Q21Employment,Tests$Q23Survey,Tests[,24:33],Tests$Choice,mean(Tests$Q22Income))
Test_Apollo$Tests.Choice[Test_Apollo$Tests.Choice == 1] <- 2
Test_Apollo$Tests.Choice[Test_Apollo$Tests.Choice == 0] <- 1
Test_Apollo$av_ALT <- as.integer(Test_Apollo$av_ALT)
Test_Apollo$av_SQ  <- as.integer(Test_Apollo$av_SQ)
colnames(Test_Apollo) <- c("ID","Task","Q1Gender","Age","Distance","Trips","QOV","Action",
                           "Self","Others","Marine","BP","Charity","Understanding","Consequentiality",
                           "Experts","Education","Employment","Survey",
                           "Effectiveness_ALT","Accumulation_ALT","Price_ALT","Health_ALT",
                           "Effectiveness_SQ","Accumulation_SQ","Price_SQ","Health_SQ",
                           "av_ALT","av_SQ","Choice","Income")


choiceAnalysis_settings <- list(
  alternatives = c(SQ=1, ALT=2),
  avail        = list(SQ=Test_Apollo$av_SQ, ALT=Test_Apollo$av_ALT),
  choiceVar    = Test_Apollo$Choice,
  explanators  = Test_Apollo[,c("Price_ALT","Health_ALT","Q1Gender","Age","Distance",
                                "Trips","BP","Charity","Understanding",
                                "Consequentiality","Education","Employment",
                                "Income")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, Test_Apollo)

apollo_beta=c(asc_SQ     = 0,
              asc_ALT    = 0,
              b_Price    = 0,
              b_Health   = 0,
              b_Q1Gender =0,
              b_Age      = 0,
              b_Distance = 0,
              b_Trips    = 0,
              b_BP       = 0,
              b_Charity  = 0,
              b_Understanding      = 0,
              b_Education  = 0,
              b_Employment = 0,
              b_Income     = 0)

apollo_fixed = c("asc_SQ")
database <- Test_Apollo
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['SQ']]  = asc_SQ  + 
    b_Price * Test_Apollo$Price_SQ +
    b_Health * Test_Apollo$Health_SQ
  
  V[['ALT']]  = asc_ALT  + b_Price * Test_Apollo$Price_ALT +
    b_Health * Test_Apollo$Health_ALT  +
    b_Q1Gender * Test_Apollo$Q1Gender + 
    b_Age * Test_Apollo$Age +
    b_Distance * Test_Apollo$Distance + 
    b_Trips * Test_Apollo$Trips +
    b_BP * Test_Apollo$BP +
    b_Charity * Test_Apollo$Charity + 
    b_Understanding * Test_Apollo$Understanding +
    b_Education * Test_Apollo$Education +
    b_Employment * Test_Apollo$Employment + 
    b_Income * Test_Apollo$Income

  mnl_settings = list(
    alternatives  = c(SQ=1, ALT=2), 
    avail         = list(SQ=Test_Apollo$av_SQ, ALT=Test_Apollo$av_ALT), 
    choiceVar     = Choice,
    V             = V
  )
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings = list(estimationRoutine="bhhh",bootstrapSE=10,maxIterations=50)))


##########################################################################
############### Apollo: MXL: FAULT                   #####################
##########################################################################


library(apollo)
apollo_initialise()
apollo_control = list(
  modelName  ="MMNL",
  modelDescr ="Simple MMNL model on mode choice SP data",
  indivID    ="ID",
  mixing     = TRUE,
  nCores     = 1
)
data("apollo_modeChoiceData")
database = apollo_modeChoiceData
rm(apollo_modeChoiceData)
database = subset(database,database$SP==1)
database$mean_income = mean(database$income)
apollo_beta=c(asc_car  = 0,
              asc_bus  = 0,
              asc_air  = 0,
              asc_rail = 0,
              mu_tt    = 0,
              sigma_tt = 1,
              b_c      = 0)
apollo_fixed = c("asc_car")

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_tt"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_tt"]] = -exp(mu_tt + sigma_tt*draws_tt)
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['car']]  = asc_car  + b_tt*time_car  + b_c*cost_car
  V[['bus']]  = asc_bus  + b_tt*time_bus  + b_c*cost_bus 
  V[['air']]  = asc_air  + b_tt*time_air  + b_c*cost_air   
  V[['rail']] = asc_rail + b_tt*time_rail + b_c*cost_rail  
  mnl_settings = list(
    alternatives  = c(car=1, bus=2, air=3, rail=4), 
    avail         = list(car=av_car, bus=av_bus, 
                         air=av_air, rail=av_rail), 
    choiceVar     = choice,
    V             = V
  )
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs))

predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)
database$cost_rail = 1.1*database$cost_rail
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs)
change=(predictions_new-predictions_base)/predictions_base
change=change[,-ncol(change)]
summary(change)


##########################################################################
############### Apollo: LCM: WORKS                   #####################
##########################################################################


rm(list = ls())
library(apollo)
apollo_initialise()

apollo_control = list(
  modelName  ="Apollo_example_18",
  modelDescr ="Simple LC model using my Pilot",
  indivID    ="ID",
  nCores     = 3,
  noDiagnostics = FALSE
)
Test_Apollo <- data.frame(Tests$ID,Tests$Task, Tests$Q1Gender,Tests$Q2Age,Tests$Q3Distance,Tests$Q4Trips,Tests$Q6QOV,Tests$Q10Action,Tests$Q11Self,Tests$Q12Others,Tests$Q13Marine,Tests$Q14BP,Tests$Q16Charity,Tests$Q17Understanding,Tests$Q18Consequentiality,Tests$Q19Experts,Tests$Q20Education,Tests$Q21Employment,Tests$Q23Survey,Tests[,24:33],Tests$Choice,mean(Tests$Q22Income))
Test_Apollo$Tests.Choice[Test_Apollo$Tests.Choice == 1] <- 2
Test_Apollo$Tests.Choice[Test_Apollo$Tests.Choice == 0] <- 1
Test_Apollo$av_ALT <- as.integer(Test_Apollo$av_ALT)
Test_Apollo$av_SQ  <- as.integer(Test_Apollo$av_SQ)
colnames(Test_Apollo) <- c("ID","Task","Q1Gender","Age","Distance","Trips","QOV","Action",
                           "Self","Others","Marine","BP","Charity","Understanding","Consequentiality",
                           "Experts","Education","Employment","Survey","Effectiveness_ALT","Accumulation_ALT","Price_ALT","Health_ALT","Effectiveness_SQ","Accumulation_SQ","Price_SQ","Health_SQ",
                           "av_ALT","av_SQ","Choice","Income")


database = Test_Apollo
apollo_beta = c(asc_SQ          = 0,
                asc_ALT         = 0,
                beta_Price_SQ   = 0,
                beta_Price_ALT  = 0,
                beta_Health_SQ  = 0,
                beta_Health_ALT = 0,
                delta_ALT       = 0,
                delta_SQ        = 0)

apollo_fixed = c("asc_SQ","delta_SQ")

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["beta_Price"]] = list(beta_Price_SQ, beta_Price_ALT)
  lcpars[["beta_Health"]] = list(beta_Health_SQ, beta_Health_ALT)
  
  V=list()
  V[["class_SQ"]] = delta_SQ
  V[["class_ALT"]] = delta_ALT
  
  mnl_settings = list(
    alternatives = c(class_SQ=1, class_ALT=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  
  return(lcpars)
}

apollo_inputs = apollo_validateInputs(database = Test_Apollo)

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  mnl_settings = list(
    alternatives = c(SQ=1, ALT=2),
    avail        = list(SQ=1, ALT=1),
    choiceVar    = Choice
  )
  
  s=1
  while(s<=2){
    V=list()
    V[['SQ']]  = asc_SQ + beta_Price[[s]]*Price_SQ + beta_Health[[s]]*Health_SQ 
    V[['ALT']]  = asc_ALT + beta_Price[[s]]*Price_ALT + beta_Health[[s]]*Health_ALT + delta_ALT
    
    mnl_settings$V = V
    P[[s]] = apollo_mnl(mnl_settings, functionality)
    P[[s]] = apollo_panelProd(P[[s]], apollo_inputs ,functionality)
    
    s=s+1
  }
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)
apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs))



##########################################################################
############### Apollo: HCM: FAULT                  #####################
##########################################################################


library(apollo)
apollo_initialise()
apollo_control = list(
  modelName  = "hybrid_model_classical",
  modelDescr = "Hybrid choice model on drug choice data, classical estimation",
  indivID    = "ID",
  mixing     = TRUE,
  nCores     = 25
)

database = read.csv("apollo_drugChoiceData.csv",header=TRUE)

choiceAnalysis_settings <- list(
  alternatives = c(Artemis=11, Novum=12, BestValue=21, Supermarket=22, PainAway=23),
  avail        = with(database,list(
    Artemis=(brand_1=="Artemis")|(brand_2=="Artemis"), 
    Novum=(brand_1=="Novum")|(brand_2=="Novum"),
    BestValue=(brand_3=="BestValue")|(brand_4=="BestValue"),
    Supermarket=(brand_3=="Supermarket")|(brand_4=="Supermarket"),
    PainAway=(brand_3=="PainAway")|(brand_4=="PainAway"))),
  choiceVar    = with(database,
                      (11*((best==1)*(brand_1=="Artemis")+(best==2)*(brand_2=="Artemis"))
                       +12*((best==1)*(brand_1=="Novum")+(best==2)*(brand_2=="Novum"))
                       +21*((best==3)*(brand_3=="BestValue")+(best==4)*(brand_4=="BestValue"))
                       +22*((best==3)*(brand_3=="Supermarket")+(best==4)*(brand_4=="Supermarket"))
                       +23*((best==3)*(brand_3=="PainAway")+(best==4)*(brand_4=="PainAway")))),
  explanators  = database[,c("regular_user","university_educated","over_50")]
)

apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

apollo_beta = c(mu_brand_Artemis          = 0, sig_brand_Artemis         = 0, 
                gamma_Artemis_reg_user    = 0, gamma_Artemis_university  = 0, 
                gamma_Artemis_age_50      = 0, mu_brand_Novum            = 0, 
                sig_brand_Novum           = 0, gamma_Novum_reg_user      = 0, 
                gamma_Novum_university    = 0, gamma_Novum_age_50        = 0, 
                b_brand_BestValue         = 0, b_brand_Supermarket       = 0, 
                b_brand_PainAway          = 0, b_country_CH              = 0, 
                b_country_DK              = 0, b_country_USA             = 0, 
                b_country_IND             = 0, b_country_RUS             = 0, 
                b_country_BRA             = 0, b_char_standard           = 0, 
                b_char_fast               = 0, b_char_double             = 0, 
                b_risk                    = 0, b_price                   = 0,  
                gamma_LV_reg_user         = 0, gamma_LV_university       = 0, 
                gamma_LV_age_50           = 0, lambda                    = 1, 
                zeta_quality              = 1, zeta_ingredient           = 1, 
                zeta_patent               = 1, zeta_dominance            = 1, 
                tau_quality_1             =-2, tau_quality_2             =-1, 
                tau_quality_3             = 1, tau_quality_4             = 2, 
                tau_ingredients_1         =-2, tau_ingredients_2         =-1, 
                tau_ingredients_3         = 1, tau_ingredients_4         = 2, 
                tau_patent_1              =-2, tau_patent_2              =-1, 
                tau_patent_3              = 1, tau_patent_4              = 2, 
                tau_dominance_1           =-2, tau_dominance_2           =-1, 
                tau_dominance_3           = 1, tau_dominance_4           = 2)

apollo_fixed = c("b_brand_PainAway", "b_country_USA", "b_char_standard")

apollo_draws = list(
  interDrawsType="halton", 
  interNDraws=500,          
  interNormDraws=c("eta","xi_Artemis","xi_Novum")
)

apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV"]] = gamma_LV_reg_user*regular_user + gamma_LV_university*university_educated + gamma_LV_age_50*over_50 + eta
  randcoeff[["b_brand_Artemis"]] = mu_brand_Artemis + sig_brand_Artemis * xi_Artemis + gamma_Artemis_reg_user*regular_user + gamma_Artemis_university*university_educated + gamma_Artemis_age_50*over_50  
  randcoeff[["b_brand_Novum"]] = mu_brand_Novum + sig_brand_Novum * xi_Novum + gamma_Novum_reg_user*regular_user + gamma_Novum_university*university_educated + gamma_Novum_age_50*over_50  
  
  return(randcoeff)
}

apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P = list()
  V = list()
  V[['alt1']] = ( b_brand_Artemis*(brand_1=="Artemis") + b_brand_Novum*(brand_1=="Novum") 
                  + b_country_CH*(country_1=="Switzerland") + b_country_DK*(country_1=="Denmark") + b_country_USA*(country_1=="USA") 
                  + b_char_standard*(char_1=="standard") + b_char_fast*(char_1=="fast acting") + b_char_double*(char_1=="double strength") 
                  + b_risk*side_effects_1
                  + b_price*price_1 
                  + lambda*LV )
  V[['alt2']] = ( b_brand_Artemis*(brand_2=="Artemis") + b_brand_Novum*(brand_2=="Novum") 
                  + b_country_CH*(country_2=="Switzerland") + b_country_DK*(country_2=="Denmark") + b_country_USA*(country_2=="USA") 
                  + b_char_standard*(char_2=="standard") + b_char_fast*(char_2=="fast acting") + b_char_double*(char_2=="double strength") 
                  + b_risk*side_effects_2
                  + b_price*price_2 
                  + lambda*LV )
  V[['alt3']] = ( b_brand_BestValue*(brand_3=="BestValue") + b_brand_Supermarket*(brand_3=="Supermarket") + b_brand_PainAway*(brand_3=="PainAway") 
                  + b_country_USA*(country_3=="USA") + b_country_IND*(country_3=="India") + b_country_RUS*(country_3=="Russia") + b_country_BRA*(country_3=="Brazil") 
                  + b_char_standard*(char_3=="standard") + b_char_fast*(char_3=="fast acting") 
                  + b_risk*side_effects_3
                  + b_price*price_3 )
  V[['alt4']] = ( b_brand_BestValue*(brand_4=="BestValue") + b_brand_Supermarket*(brand_4=="Supermarket") + b_brand_PainAway*(brand_4=="PainAway") 
                  + b_country_USA*(country_4=="USA") + b_country_IND*(country_4=="India") + b_country_RUS*(country_4=="Russia") + b_country_BRA*(country_4=="Brazil") 
                  + b_char_standard*(char_4=="standard") + b_char_fast*(char_4=="fast acting") 
                  + b_risk*side_effects_4
                  + b_price*price_4 )
  
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail        = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVar    = best,
    V            = V
  )
  P[["choice"]] = apollo_mnl(mnl_settings, functionality)
  ol_settings1 = list(outcomeOrdered=attitude_quality, 
                      V=zeta_quality*LV, 
                      tau=c(tau_quality_1, tau_quality_2, tau_quality_3, tau_quality_4),
                      rows=(task==1))
  ol_settings2 = list(outcomeOrdered=attitude_ingredients, 
                      V=zeta_ingredient*LV, 
                      tau=c(tau_ingredients_1, tau_ingredients_2, tau_ingredients_3, tau_ingredients_4), 
                      rows=(task==1))
  ol_settings3 = list(outcomeOrdered=attitude_patent, 
                      V=zeta_patent*LV, 
                      tau=c(tau_patent_1, tau_patent_2, tau_patent_3, tau_patent_4), 
                      rows=(task==1))
  ol_settings4 = list(outcomeOrdered=attitude_dominance, 
                      V=zeta_dominance*LV, 
                      tau=c(tau_dominance_1, tau_dominance_2, tau_dominance_3, tau_dominance_4), 
                      rows=(task==1))
  P[["indic_quality"]]     = apollo_ol(ol_settings1, functionality)
  P[["indic_ingredients"]] = apollo_ol(ol_settings2, functionality)
  P[["indic_patent"]]      = apollo_ol(ol_settings3, functionality)
  P[["indic_dominance"]]   = apollo_ol(ol_settings4, functionality)
  
  P = apollo_combineModels(P, apollo_inputs, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

speedTest_settings=list(
  nDrawsTry = c(250, 500, 1000),
  nCoresTry = 1:3,
  nRep      = 10
)

apollo_speedTest(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs, speedTest_settings)

apollo_modelOutput(apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs))




#############################################################################
