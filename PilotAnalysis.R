# Script to analyse pilot survey data
# Helpful links: https://cran.r-project.org/web/packages/support.CEs/support.CEs.pdf


#Section One. Import Data
## Import from CSV and convert to data frame with relevant columns
setwd("H:/PhDPilotSurvey")

Pilot <- data.frame(read.csv("PhD Survey_ Sample A.csv"))
Pilot <- Pilot[ -c(1.2,8,27)]
#Section Two. Pre-processing.
## To do. Convert to factors
##        Change column names
colnames(Pilot) <- c("Q1Gender", "Q2Age", "Q3Distance", "Q4Trips","Q5CVM1","Q6QOV","Q7CE1", "Q8CE2","Q9CE3","Q10Action", "Q11Self","Q12Others", "Q13Marine", "Q14BP","Q15Responsibility","Q16Charity", "Q17Understanding", "Q18Consequentiality", "Q19Experts", "Q20Education","Q21Employment", "Q22Income","Q23Survey")
Pilot2 <- Pilot
Pilot2 <- data.frame(Pilot2)

for (i in colnames(Pilot)){
  if (is.factor(Pilot[[i]]) == TRUE){
    Pilot2[[i]] <- as.numeric(Pilot[[i]])-1
  }
}


Pilot2$Q3Distance[Pilot2$Q3Distance == 1] <- 4
Pilot2$Q3Distance[Pilot2$Q3Distance == 0] <- 1
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


Choices <- data.frame("Effectiveness.SQ" =c(0,0,0,0,0,0,0,0,0,0), 
                      "Effectiveness.ALT" =c(100,0,0,0,10,10,10,50,50,50), 
                      "Env.SQ" =c(0,0,0,0,0,0,0,0,0,0),
                      "Env.ALT" =c(100,90,40,90,0,40,90,0,40,90),
                      "Price.SQ" =c(0,0,0,0,0,0,0,0,0,0),
                      "Price.ALT" =c(0,0,1,5,1,5,0,5,0,1),
                      "Health.SQ" =c(0,0,0,0,0,0,0,0,0,0),
                      "Health.ALT" =c(0,0.1, 0.1,0.6,0.6,1,0.1,0.1,0.6,1))

SpecificChoices <- data.frame("Effectiveness.SQ" =c(0,0,0), 
                      "Effectiveness.ALT" =c(100,0,0), 
                      "Env.SQ" =c(0,0,0),
                      "Env.ALT" =c(100,90,40),
                      "Price.SQ" =c(0,0,0),
                      "Price.ALT" =c(0,0,1),
                      "Health.SQ" =c(0,0,0),
                      "Health.ALT" =c(0,0.1, 0.1))

Chosen <- data.frame(Pilot2$Q7CE1,Pilot2$Q8CE2,Pilot2$Q9CE3)
Chosen$Block <- rep(1,nrow(Chosen))
Chosen$ID <- seq.int(nrow(Chosen))
Chosen <- data.frame(Chosen$ID, Chosen$Block, Chosen$Pilot2.Q7CE1,Chosen$Pilot2.Q8CE2, Chosen$Pilot2.Q9CE3)
colnames(Chosen) <- c("ID","BLOCK","Q7","Q8","Q9")

# Works up until here
# I've tried copying the rest with some success
# Before S3 works
install.packages("survival")
install.packages("support.CEs")
library(survival)
library(stats)
library(support.CEs)


design <- Lma.design(
  attribute.names = list(
    Effectiveness = c("100","0","0"),
    Accumulation = c("100","90","40"),
    Health = c("0","0.1","0.1"),
    Price =c("1.0","1.1","1.2")),
  nalternatives = 3,
  nblocks = 2,
  row.renames = FALSE,
  seed = 987)
# Price should be 0,0,1
design
questionnaire(choice.experiment.design = design, quote = FALSE)
desmat3 <- make.design.matrix(
  choice.experiment.design = design,
  optout = TRUE,
  continuous.attributes = c("Effectiveness","Accumulation","Health","Price"),
    unlabeled = FALSE)
data(Chosen)

Chosen[Chosen == 1] <- 2
Chosen[Chosen == 0] <- 1


dataset3 <- make.dataset(
  respondent.dataset = Chosen,
  choice.indicators =
    c("Q7", "Q8", "Q9"),
  design.matrix = desmat3)
clogout3 <- clogit(RES ~ ASC1 + Effectiveness1 + Accumulation1 + Health1  +
                     ASC2 + Effectiveness2 + Accumulation2 + Health2 + ASC3+ Effectiveness3 + Accumulation3 + Health3 +
                     strata(STR), data = dataset3)
clogout3
gofm(clogout3)
mwtp(
  output = clogout3,
  monetary.variables = c("Price1", "Price2", "Price3"),
  nonmonetary.variables = list(
    c("Effectiveness1", "Accumulation1", "Health1"), c("Effectiveness2","Accumulation2", "Health2"), c("Effectiveness3","Accumulation3", "Health3")),
  seed = 987)


# Section Thress: CL Estimation
# Section Thress: CL Estimation
install.packages("survival")
install.packages("support.CEs")
library(survival)
library(stats)
library(support.CEs)
# Case 1
# Choice experiments using the function rotaion.design.
# See "Details" for the data set syn.res1.

des2 <- Lma.design(
  attribute.names = list(
    Eco = c("Conv.", "More", "Most"),
    Price = c("1", "1.1", "1.2")),
  nalternatives = 3,
  nblocks = 2,
  row.renames = FALSE,
  seed = 987)
des2
questionnaire(choice.experiment.design = des2, quote = FALSE)
desmat2 <- make.design.matrix(
  choice.experiment.design = des2,
  optout = TRUE,
  categorical.attributes = c("Eco"),
  continuous.attributes = c("Price"),
  unlabeled = FALSE)
data(syn.res2)
dataset2 <- make.dataset(
  respondent.dataset = syn.res2,
  choice.indicators =
    c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9"),
  design.matrix = desmat2)
clogout2 <- clogit(RES ~ ASC1 + More1 + Most1 + Price1 +
                     ASC2 + More2 + Most2 + Price2 + ASC3 + More3 + Most3 + Price3 +
                     strata(STR), data = dataset2)
clogout2
gofm(clogout2)
mwtp(
  output = clogout2,
  monetary.variables = c("Price1", "Price2", "Price3"),
  nonmonetary.variables = list(
    c("More1", "Most1"), c("More2", "Most2"), c("More3", "Most3")),
  seed = 987)
