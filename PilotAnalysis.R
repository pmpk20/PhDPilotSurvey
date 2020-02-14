#Peter King
####################################################################################
############### Introduction: Pilot Data Analysis Script  ##########################
####################################################################################
# Useful links: https://cran.r-project.org/web/packages/support.CEs/support.CEs.pdf
# https://pdfs.semanticscholar.org/b0fb/05e51e02d4eda914888ae0590dd65b45ff9a.pdf
# https://rpubs.com/sallychen/313125
# https://www.sciencedirect.com/science/article/pii/S1098301516302911#bib45
# https://onlinelibrary.wiley.com/doi/pdf/10.1002/hec.984


####################################################################################
############### Section 1: Import Data  ##########################
####################################################################################


install.packages("dplyr") # Useful later for data manipulation
install.packages("mlogit")
install.packages("gmnl")
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
## List of dependents:
## Q1Gender + Q2Age + Q3Distance + Q4Trips + Q6QOV+ Q10Action +  Q11Self + Q12Others + Q13Marine + Q14BP+ Q15Responsibility + Q16Charity + Q17Understanding+ Q18Consequentiality + Q20Education+ Q21Employment +  Q22Income+Q23Survey

Base_MNL <- mlogit(Choice ~  Price + Health, 
                   Test_Long,
                   alt.subset = c("SQ","ALT"),reflevel = "SQ") ##Estimating a simple model first
summary(Base_MNL) ## Estimates a simplistic mlogit model before adding in individual-specifics

Pilot_MNL <- mlogit(Choice ~ Price + Health | 
                      Q1Gender + Q2Age + Q3Distance
                    + Q4Trips + Q6QOV+ Q10Action +  
                      Q11Self + Q12Others + Q13Marine 
                    + Q14BP + Q16Charity 
                    + Q17Understanding+ Q18Consequentiality
                    + Q19Experts +Q20Education+ Q21Employment
                    +  Q22Income+Q23Survey, 
                    Test_Long, alt.subset = c("SQ", "ALT"), 
                    reflevel = "SQ") ## Estimating a much larger MNL model with all the independent variables. 
summary(Pilot_MNL) ## Summarises the MNL output
## key things to change include the placing of the |. 

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
MIXLcorrelated = mlogit(Choice~Price+Accumulation|0,
                            Test_Long,
                            rpar=c(Price="n",
                            Accumulation="n"),
                            R=600,
                            halton=NULL,
                            print.level=0,
                            panel=TRUE,correlation = TRUE)
summary(MIXLcorrelated)
MIXLuncorrelated = mlogit(Choice~Price+Accumulation|0,
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
indpar <- fitted(MXLCorrelated, type = "parameters")
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
############### Section 3B: Estimation of CVM models #####################
##########################################################################


# ## Estimates a PROBIT model
# summary(glm(Test$Q5CVM1 ~ Test$Q1Gender + Test$Q2Age + Test$Q3Distance + Test$Q4Trips + Test$Q10Action + Test$Q22Income + Test$Q21Employment + Test$Q20Education + Test$Q11Self + Test$Q12Others + Test$Q13Marine + Test$Q14BP + Test$Q15Responsibility + Test$Q16Charity + Test$Q17Understanding, data=Test))


##########################################################################
############### Section 3C: Estimation of QOV models #####################
##########################################################################


# ModelQOV <- (glm(Test$Q6QOV ~ Test$Q1Gender + Test$Q2Age + Test$Q3Distance + Test$Q4Trips + Test$Q10Action + Test$Q22Income + Test$Q21Employment + Test$Q20Education + Test$Q11Self + Test$Q12Others + Test$Q13Marine + Test$Q14BP + Test$Q15Responsibility + Test$Q16Charity + Test$Q17Understanding, data=Test))
# summary(ModelQOV)
# Use update to change for significant P values

##########################################################################
###############                                             ##############
###############     Section 3C: Estimation of QOV models    ############## 
###############                                             ##############
##########################################################################


# PVadjusted("Q6QOV",0.05)
# QOVProbit = Model2

##########################################################################
###############                                             ##############
###############     Section 4: Estimation of other models   ############## 
###############     Blue Planet                             ##############
##########################################################################

# PVadjusted("Q14BP",0.05)
# BPProbit = Model2

##########################################################################
###############                                             ##############
###############     Section 5: Hybrid Choice models?        ############## 
###############                                             ##############
##########################################################################

#
# Read this first: https://transp-or.epfl.ch/documents/talks/IVT10.pdf


##########################################################################
###############                                             ##############
###############     Section 6: Logits with only rational DCE ############# 
###############                                             ##############
##########################################################################

## May perform analysis from this new dataset: Pilot_Dominated

##########################################################################
###############                                             ##############
###############     Section 7: Logits with only good understanding ####### 
###############                                             ##############
##########################################################################
