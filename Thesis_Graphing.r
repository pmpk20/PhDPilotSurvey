#### Survey data analysis script: All Graphs  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Code description: This script estimates all the graphing and figures in one place ####
############ This section is extremely untidy and disorganised.


#### Section 0: Package Imports ####
library(reshape2)
library(ggplot2)
library(gridExtra)
library(scales)

## Specifically for the spatial plotting: 
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos")
install.packages("sf")
install.packages("raster")
install.packages("tidyverse")
install.packages("RColorBrewer")
library(rnaturalearth)
library(sf)
library(raster)
library(tidyverse)
library(RColorBrewer)


#### Section 1: Reporting means ####


## Truncation Strategy One:
round(mean(Full_Full$Q6WTP),2) ## 23.70
round(mean(Full_Full$Q7WTP),2) ## 45.37
round(mean(Full_Full$PerformanceCoef),3) ## -0.053
round(mean(Full_Full$EmissionCoef),3) ## 0.045


## Truncation Strategy Two:
round(mean(Full_Full$Q6WTP),2) ## 23.72
round(mean(Full_Full$Q7WTP),2) ## 45.33
round(mean(Full_Full$PerformanceCoef),3) ## -0.053
round(mean(Full_Full$EmissionCoef),3) ## 0.045


## Gender: 
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[FullSurvey$Q1Gender=="Male"]) ## Proportion of the sample that is male. Actual: 46, Target: 49
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[FullSurvey$Q1Gender=="Female"]) ## Proportion of the sample that is female. Actual: 53, Target: 51 
100/nrow(FullSurvey)*length(FullSurvey$Q1Gender[(FullSurvey$Q1Gender !="Female")  & (FullSurvey$Q1Gender != "Male")]) ## Estimating the percentage who reported otherwise


## Age: 
mean(FullSurvey2$Q2Age) ## Estimating average age


## Trips: 
mean(FullSurvey2$Q4Trips) ## Estimating average annual trips


## Charity: 
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="No"])
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="Yes"])
100/nrow(FullSurvey)*length(FullSurvey$Q18Charity[FullSurvey$Q18Charity=="Prefer not to say"])


## Education: 
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==0]) ## 0 = Prefer not to say 2.23%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==1]) ## 1 = GCSE 21.94%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==2]) ## 2 = A level 26.56%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==3]) ## 3 = Bachelors 31.64%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==4]) ## 4 = PG 17.61%
# Less than University: 50.74%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==0]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==1]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==2])
# Bachelors or more: 49.25%
100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==3]) + 100/nrow(FullSurvey2)*length(FullSurvey2$Q22Education[FullSurvey2$Q22Education==4])


## Employment: 
100/670*table(FullSurvey2$Q23Employment)


## Income: 
mean(FullSurvey2$Q24AIncome) ## Estimating sample income


## Reporting WTP by responsibility:
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Firms == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Cons == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Gov == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_LA == 0]),2)
round(mean(Full_Final$PerformanceCoef[Full_Final$Q17_Other == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Firms == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Cons == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Gov == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_LA == 0]),2)
round(mean(Full_Final$EmissionCoef[Full_Final$Q17_Other == 0]),2)


#### Section 2: Testing Means ####
## Testing of means: Table `Test of means`


## Rule 1) Timing testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$Q6WTP[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$Q7WTP[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$PerformanceCoef[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Timing/60 < (median(Full_Long$Timing)/60)/100*48],y=Full_Final$EmissionCoef[Full_Final$Timing/60 >= (median(Full_Long$Timing)/60)/100*48]) ## Outcome: Alternative of =/= 


## Rule 2) Understanding testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$Q6WTP[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: NULL
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$Q7WTP[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$PerformanceCoef[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q25Understanding < (median(Full_Long$Q25Understanding))],y=Full_Final$EmissionCoef[Full_Final$Q25Understanding >= (median(Full_Long$Q25Understanding))]) ## Outcome: Alternative of =/= 


## Rule 3) Protest vote testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Protestors==0],y=Full_Final$Q6WTP[Full_Final$Protestors==1]) ## Outcome: NULL
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Protestors==0],y=Full_Final$Q7WTP[Full_Final$Protestors==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Protestors==0],y=Full_Final$PerformanceCoef[Full_Final$Protestors==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Protestors==0],y=Full_Final$EmissionCoef[Full_Final$Protestors==1]) ## Outcome: Alternative of =/= 


## Rule 4) Dominated scenario testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q8DominatedTest==0],y=Full_Final$Q6WTP[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q8DominatedTest==0],y=Full_Final$Q7WTP[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==0],y=Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==0],y=Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==1]) ## Outcome: Alternative of =/= 


## Rule 5) Consequentiality testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.2353
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1],y=Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1],y=Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.007
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1],y=Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1]) ## Outcome: P=0.007
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1],y=Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]) ## Outcome: P<0.000


## Rule 6) Certainty testing: Shows there is a significant difference
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1],y=Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]) ## Outcome: Alternative of =/=

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]) ## Outcome: Alternative of =/= 
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1],y=Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]) ## Outcome: Alternative of =/= 

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1]) ## Outcome: NULL
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1],y=Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL

# Testing all the different combinations of the three levels
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1]) ## Outcome: NULL 
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL 
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1],y=Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]) ## Outcome: NULL 


## Full and truncated sample mean differences
# For Q6:
wilcox.test(x=Full_Final$Q6WTP,y=Full_Full$Q6WTP) ## P=0.049
wilcox.test(x=Full_Final$Q6WTP,y=Full_Full1$Q6WTP) ## P=0.041 
wilcox.test(x=Full_Full$Q6WTP,y=Full_Full1$Q6WTP) ## P=0.16

# For Q7:
wilcox.test(x=Full_Final$Q7WTP,y=Full_Full$Q7WTP) ## P=0.049
wilcox.test(x=Full_Final$Q7WTP,y=Full_Full1$Q7WTP) ## P=0.041 
wilcox.test(x=Full_Full$Q7WTP,y=Full_Full1$Q7WTP) ## P=0.16

# For Performance MWTP:
wilcox.test(x=Full_Final$PerformanceCoef,y=Full_Full$PerformanceCoef) ## P=0.049
wilcox.test(x=Full_Final$PerformanceCoef,y=Full_Full1$PerformanceCoef) ## P=0.041 
wilcox.test(x=Full_Full$PerformanceCoef,y=Full_Full1$PerformanceCoef) ## P=0.16

# For Emission MWTP:
wilcox.test(x=Full_Final$EmissionCoef,y=Full_Full$EmissionCoef) ## P=0.049
wilcox.test(x=Full_Final$EmissionCoef,y=Full_Full1$EmissionCoef) ## P=0.041 
wilcox.test(x=Full_Full$EmissionCoef,y=Full_Full1$EmissionCoef) ## P=0.16


## Testing ordering effects
wilcox.test(x=Full_Final$Q6WTP[Full_Final$Order==0],y=Full_Final$Q6WTP[Full_Final$Order==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$Q7WTP[Full_Final$Order==0],y=Full_Final$Q7WTP[Full_Final$Order==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$EmissionCoef[Full_Final$Order==0],y=Full_Final$EmissionCoef[Full_Final$Order==1]) ## Outcome: Alternative of =/=
wilcox.test(x=Full_Final$PerformanceCoef[Full_Final$Order==0],y=Full_Final$PerformanceCoef[Full_Final$Order==1]) ## Outcome: Alternative of =/=


#### Section 3: Correlation heatmap #### 


# Setting up the data:
Full_Final <- data.frame(read.csv("FinalData.csv")) ## Imports from the excel file straight from the survey companies website.
Full_Final <- Full_Final[ -c(2,grep("av_A", colnames(Full_Final)),grep("av_B", colnames(Full_Final)))] ## Drop columns of no importance to the quantitative analysis, namely text responses.
Full_Final$Choice[Full_Final$Choice==FALSE] <- 0
Full_Final$Choice[Full_Final$Choice==TRUE] <- 1
Full_Final$alt <- as.numeric(Full_Final$alt)


# Code from various StackOverflow threads:
cormat <- round(cor(Full_Final),2) ## store correlations in a matrix
melted_cormat <- melt(cormat) ## Simplify corr matrix
## Get lower triangle
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
##  Get upper triangle
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
reorder_cormat <- function(cormat){
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
## Use the melt function to simplify the matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

## Rename matrix columns
colnames(melted_cormat) <- c("Variable1", "Variable2","Correlation")


## Correlation heatmap
ggheatmap <- ggplot(melted_cormat, aes(Variable2, Variable1, fill = Correlation))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 6, hjust = 1),
        axis.text.y = element_text(angle = 0, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()


#### Section 4: Truncation Plots #### 


#### Section 4A: Certainty ####

rbind(cbind("Q6o0C0"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==0)])/8,"Q6o1C0"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==0)])/8),
      cbind("Q6o0C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==1)])/8,"Q6o1C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==1)])/8),
      cbind("Q6o0C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==0) & (Full_Final$Q6ResearchCertainty ==2)])/8,"Q6o1C1"=length(Full_Final$Q6ResearchCertainty[(Full_Final$Order==1) & (Full_Final$Q6ResearchCertainty ==2)])/8))

rbind(cbind("Q7o0C0"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==0)])/8,"Q7o1C0"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==0)])/8),
      cbind("Q7o0C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==1)])/8,"Q7o1C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==1)])/8),
      cbind("Q7o0C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==0) & (Full_Final$Q7TreatmentCertainty ==2)])/8,"Q7o1C1"=length(Full_Final$Q7TreatmentCertainty[(Full_Final$Order==1) & (Full_Final$Q7TreatmentCertainty ==2)])/8))

### Plotting Certainty effects
Q12Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q12CECertainty, labeller = as_labeller(c(
    `0` = "Unsure\n (N = 40)",
    `1` = "Quite Sure\n (N = 328)",
    `2` = "Very Sure\n (N = 302)")))+
  geom_smooth(aes(y=PerformanceCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by certainty") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=10,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")

Q12GraphB <- ggplot(Full_Final, aes(x=Q12CECertainty)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between CE MWTP and certainty.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Self-reported certainty",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("Unsure","Quite Sure","Very Sure"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

CertaintyGraph <- ggplot(Full_Final, aes(x=Q6ResearchCertainty)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(inherit.aes = FALSE,aes(y=Q7WTP,x=Q7TreatmentCertainty,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by certainty") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Self-reported certainty",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("Unsure","Quite Sure","Very Sure"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

Q20P <- ggplot(Full_Final, aes(as.numeric(Q6ResearchCertainty), abs(PerformanceCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q12CECertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 40)","1 (Quite Sure)\n (N = 328)","2 (Very Sure)\n (N=302)"))+
  scale_y_continuous(name="| PerformanceCoef |",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==0]),3))), color="white")+
  geom_text(x = 1, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==1]),3))), color="white")+
  geom_text(x = 2, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q12CECertainty==2]),3))), color="white")+
  ggtitle("Performance MWTP by certainty beliefs.")

Q20E <- ggplot(Full_Final, aes(as.numeric(Q12CECertainty), abs(EmissionCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q12CECertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 40)","1 (Quite Sure)\n (N = 328)","2 (Very Sure)\n (N=302)"))+
  scale_y_continuous(name="Emission MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==0]),3)), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==1]),3)), color="white")+
  geom_text(x = 2, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q12CECertainty==2]),3)), color="white")+ 
  ggtitle("Emissions MWTP by certainty beliefs.")

grid.arrange(Q20P,Q20E)


Q20Q6 <- ggplot(Full_Final, aes(as.numeric(Q6ResearchCertainty), abs(Q6WTP))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q6ResearchCertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 70)","1 (Quite Sure)\n (N = 277)","2 (Very Sure)\n (N=323)"))+
  scale_y_continuous(name="Q6WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==0]),2)), color="white")+
  geom_text(x = 1, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==1]),2)), color="white")+
  geom_text(x = 2, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q6ResearchCertainty==2]),2)), color="white")+ 
  ggtitle("Mean Q6 WTP by certainty beliefs.")

Q20Q7 <- ggplot(Full_Final, aes(as.numeric(Q7TreatmentCertainty), abs(Q7WTP))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q7ResearchCertainty",limits=c(0, 1,2),
                   labels=c("0 (Unsure)\n (N = 54)","1 (Quite Sure)\n (N = 269)","2 (Very Sure)\n (N=347)"))+
  scale_y_continuous(name="Q7WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==0]),2)), color="white")+
  geom_text(x = 1, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==1]),2)), color="white")+
  geom_text(x = 2, y = 10, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q7TreatmentCertainty==2]),2)), color="white")+ 
  ggtitle("Mean Q7 WTP by certainty beliefs.")

grid.arrange(Q20Q6,Q20Q7)


#### Section 4B: Q8Dominated ####


#### Section 4C: Consequentiality ####


### Plotting Consequentiality effects on CV fitted WTP
Q20GraphSB <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=FullSurvey2.Q7WTPSB,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and consequentiality (Q7 using SB)") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")

### Plotting Consequentiality effects on CE MWTP
Q20GraphC <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between MWTP and consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="CE absolute MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")



Q8EmissionGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))
grid.arrange(Q8PerformanceGraph, Q8EmissionGraph)


grid.arrange(Q12GraphB,CertaintyGraph)



### Plotting MWTP for emissions attribute
EmissionWTP <- ggplot(Full_Final, aes(y= EmissionCoef,x=as.numeric(Q24AIncome))) + 
  geom_point(shape = 1) +
  facet_grid( ~ Q8DominatedTest, labeller = as_labeller(c(
    `0` = "Pass",
    `1` = "Fail")))+
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between income and WTP by Q8") +
  scale_y_continuous(name="Emission MWTP",
                     breaks=waiver(),limits = c(-1,1),
                     n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Emission MWTP")


grid.arrange(ConsBarEmissions, ConsBarPerformance)
grid.arrange(ConsBarQ6, ConsBarQ7)


Dominated <- rbind(cbind(round(median(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest ==0]),4), round(median(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest ==1]),4)),
                   (cbind(round(median(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest ==1]),4))),
                   cbind(round(median(Full_Final$Q6WTP[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$Q6WTP[Full_Final$Q8DominatedTest ==1]),4)),
                   cbind(round(median(Full_Final$Q7WTP[Full_Final$Q8DominatedTest ==0]),4),round(median(Full_Final$Q7WTP[Full_Final$Q8DominatedTest ==1]),4)))
rownames(Dominated) <- c("Performance MWTP","Emission MWTP","Q6 WTP","Q7 WTP")
colnames(Dominated) <- c("Passed","Failed")
Dominated

## Timing (faster than 48% time)
(median(Full_Long$Timing)/60)/100*48
library(ggplot2)
ggplot(FullSurvey2, aes(x=Timing/60)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  geom_vline(aes(xintercept=(median(FullSurvey2$Timing)/60)/100*48),
             color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name="Completion length in minutes")+
  geom_vline(xintercept=((median(FullSurvey2$Timing)/60)/100*48), colour="grey") +
  geom_text(aes(x=((median(FullSurvey2$Timing)/60)/100*48)-2, label="Speeders", y=100), colour="red", angle=0) +
  geom_text(aes(x=((median(FullSurvey2$Timing)/60)/100*48)+2, label="Included", y=100), colour="blue", angle=0)+
  ggtitle("Distribution of survey completion timings.")


### Plotting survey understanding
ggplot(Full_Final, aes(x=Q25Understanding)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  geom_vline(aes(xintercept=7),color="blue", linetype="dashed", size=1)+
  scale_x_continuous(name="Self-reported survey undertanding in 1-10")+
  geom_vline(xintercept=(7), colour="grey") +
  geom_text(aes(x=5, label="Excluded", y=400), colour="red", angle=0) +
  geom_text(aes(x=8, label="Included", y=400), colour="blue", angle=0)+
  ggtitle("Distribution of survey understanding")


### Plotting Q8 Dominance test on MWTP:
Q8PerformanceGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), abs(PerformanceCoef))) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="| Performance MWTP |",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==0]),3))), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q8DominatedTest==1]),3))), color="white")+
  ggtitle("Absolute performance MWTP by dominated test.")

Q8EmissionGraph <- ggplot(Full_Final, aes(as.numeric(Q8DominatedTest), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q8DominatedTest",limits=c(0, 1),
                   labels=c("0 (Pass)\n (N = 478)","1 (Fail)\n (N = 192)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",abs(round(mean(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==0]),3))), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",abs(round(mean(Full_Final$EmissionCoef[Full_Final$Q8DominatedTest==1]),3))), color="white")+
  ggtitle("Emissions MWTP by dominated test.")

grid.arrange(Q8PerformanceGraph, Q8EmissionGraph)

### Plotting Q8 Dominance test
PerformanceWTP <- ggplot(Full_Final, aes(y= PerformanceCoef,x=as.numeric(Q24AIncome))) + 
  geom_point(shape = 1) +
  facet_grid( ~ Q8DominatedTest, labeller = as_labeller(c(
    `0` = "Pass",
    `1` = "Fail")))+
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between income and MWTP by Q8") +
  scale_y_continuous(name="Performance MWTP",
                     breaks=waiver(),limits = c(-1,1),
                     n.breaks = 10)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Performance MWTP")


### Plotting Consequentiality effects
ggplot(data=Fulls, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q20Consequentiality, labeller = as_labeller(c(
    `0` = "Inconsequential",
    `1` = "Consequential",
    `2` = "Don't Know")))+
  geom_smooth(aes(y=Performance,color="red"),method="lm",se=F) +
  geom_smooth(aes(y=Emission,color="blue"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  ggtitle("Relationship between income and WTP by consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="MWTP",breaks = waiver(),n.breaks = 10, 
                     limits=c(-5,5),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="MWTP")



### Plotting Consequentiality effects
Q20Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q20Consequentiality, labeller = as_labeller(c(
    `0` = "No\n (N = 110)",
    `1` = "Yes\n (N = 358)",
    `2` = "Don't Know\n (N = 202)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by consequentiality") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=10,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


Full_Final <- cbind(Full_Final,slice(.data = data.frame(FullSurvey2$Q7WTPSB),rep(1:n(), each = 8)))
FF <- subset(Full_Final,subset = Full_Final$Q20Consequentiality != 2)

### Plotting Consequentiality effects on CV fitted WTP
Q20GraphB <- ggplot(FF, aes(x=as.numeric(Q20Consequentiality))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and consequentiality (Q7 using DB)") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Consequentiality",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2, labels = c("Inconsequential","Consequential"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Q20 Do you think this survey could be policy consequential?",y="Fitted CV WTP")


ConsBarEmissions <- ggplot(Full_Final, aes((Q20Consequentiality), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==0]),3)), color="white")+
  geom_text(x = 1, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==1]),3)), color="white")+
  geom_text(x = 2, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Q20Consequentiality==2]),3)), color="white")+ 
  ggtitle("Mean Emission MWTP by consequentiality beliefs.")

ConsBarPerformance <- ggplot(Full_Final, aes((Q20Consequentiality), PerformanceCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Performance MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==0]),3))), color="white")+
  geom_text(x = 1, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==1]),3))), color="white")+
  geom_text(x = 2, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Q20Consequentiality==2]),3))), color="white")+
  ggtitle("Mean Performance MWTP by consequentiality beliefs.")

ConsBarQ6 <- ggplot(Full_Final, aes((Q20Consequentiality), Q6WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Q6WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==1]),2)), color="white")+
  geom_text(x = 2, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Q20Consequentiality==2]),2)), color="white")+ 
  ggtitle("Mean Q6 WTP by consequentiality beliefs.")

ConsBarQ7 <- ggplot(Full_Final, aes((Q20Consequentiality), Q7WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Q20Consequentiality",limits=c(0,1, 2),
                   labels=c("Not Consequential\n (N = 110)","Don't Know\n (N = 202)","Yes\n (N = 358)"))+
  scale_y_continuous(name="Q7WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==1]),2)), color="white")+
  geom_text(x = 2, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Q20Consequentiality==2]),2)), color="white")+
  ggtitle("Mean Q7 WTP by consequentiality beliefs.")


#### Section 5: Protest Analysis ####


## Setting up data using visually-identified protestors:
FullSurvey3 <- data.frame(read.csv("FullSurvey.csv")) 
Protestors <- c(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)
Protestors <- ifelse(FullSurvey3$ï..Respondent %in%  Protestors== TRUE, 1,0)
Full_Final <- cbind(Full_Final,slice(.data = data.frame(Protestors),rep(1:n(), each = 8)))

ProtestQ6 <- ggplot(Full_Final, aes((Protestors), Q6WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Protestors",limits=c(0,1),
                   labels=c("No protest\n (N = 561)","Protestor\n (N = 109)"))+
  scale_y_continuous(name="Q6WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Protestors==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q6WTP[Full_Final$Protestors==1]),2)), color="white")+ 
  ggtitle("Mean Q6 WTP by protesting.")

ProtestQ7 <- ggplot(Full_Final, aes((Protestors), Q7WTP)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Protestors",limits=c(0,1),
                   labels=c("No protest\n (N = 561)","Protestor\n (N = 109)"))+
  scale_y_continuous(name="Q7WTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Protestors==0]),2)), color="white")+
  geom_text(x = 1, y = 20, label = paste0("Mean: £",round(mean(Full_Final$Q7WTP[Full_Final$Protestors==1]),2)), color="white")+ 
  ggtitle("Mean Q7 WTP by protesting.")


ProtestE <- ggplot(Full_Final, aes((Protestors), EmissionCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Protestors",limits=c(0,1, 2),
                   labels=c("No protest\n (N = 561)","Protestor\n (N = 109)"))+
  scale_y_continuous(name="Emissions MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = 0.02, label = paste0("Mean: £",round(mean(Full_Final$EmissionCoef[Full_Final$Protestors==0]),3)), color="white")+
  geom_text(x = 1, y = 0.01, label = paste0("Mean: -£",abs(round(mean(Full_Final$EmissionCoef[Full_Final$Protestors==1]),3))), color="black")+
  ggtitle("Mean Emission MWTP by protesting")

ProtestP <- ggplot(Full_Final, aes((Protestors), PerformanceCoef)) + 
  stat_summary(fun=mean, geom="col")+
  scale_x_discrete(name="Protestors",limits=c(0,1, 2),
                   labels=c("No protest\n (N = 561)","Protestor\n (N = 109)"))+
  scale_y_continuous(name="Performance MWTP",
                     breaks=waiver(),n.breaks = 10, labels = function(x) paste0("£",x))+
  geom_text(x = 0, y = -0.02, label = paste0("Mean: -£",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Protestors==0]),3))), color="white")+
  geom_text(x = 1, y = -0.02, label = paste0("Mean: £",abs(round(mean(Full_Final$PerformanceCoef[Full_Final$Protestors==1]),3))), color="black")+
  ggtitle("Mean Performance MWTP by protesting")

grid.arrange(ProtestE, ProtestP)
grid.arrange(ProtestQ6, ProtestQ7)

### Plotting protest effects:
ProtestGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Protestors, labeller = as_labeller(c(
    `0` = "Protest\n (N = 561)",
    `1` = "Included\n (N = 109)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by protest votes") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Protesting",y="WTP")


## Comparing the distribution of WTP by protestors and non-protestors
grid.arrange(ggplot(Full_Final[Full_Final$Protestors==0,], aes(x=Full_Final$Q6WTP[Full_Final$Protestors ==0])) + 
               geom_histogram(color="black", fill="white",bins = 50)+
               scale_x_continuous(breaks=waiver(),limits = c(0,50),
                                  n.breaks = 10)+
               ggtitle("Histogram of CV Q6 WTP for protestors."),
             ggplot(Full_Final[Full_Final$Protestors==1,], aes(x=Full_Final$Q6WTP[Full_Final$Protestors ==1])) + 
               geom_histogram(color="black", fill="white",bins = 50)+
               scale_x_continuous(breaks=waiver(),limits = c(0,50),
                                  n.breaks = 10)+
               ggtitle("Histogram of CV Q6 WTP for non-protestors."))

### Plotting protest effects a different way:
ProtestGraph <- ggplot(Full_Final, aes(x=as.numeric(Protestors))) +
  geom_col(aes(y=Q6WTP)) +
  ggtitle("WTP by protesting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  scale_x_continuous(name="Protests",breaks = waiver(),limits = c(-1,2),
                     n.breaks = 2, labels = c("Protested","Did not"))+
  labs(x = "Income",y="Precaution")


#### Section 6: Responsibility beliefs: #### 
Full_Final$Q17_Firms[ (Full_Final$Q17_Cons == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Other == 0) ] <- 2
Full_Final$Q17_Cons[ (Full_Final$Q17_Firms == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Other == 0) ] <- 2
Full_Final$Q17_Cons[Full_Final$Q17_Cons != 2] <- 0
Full_Final$Q17_Cons[Full_Final$Q17_Cons == 2] <- 1
Full_Final$Q17_Other[ (Full_Final$Q17_Cons == 0) & (Full_Final$Q17_Gov == 0) & (Full_Final$Q17_LA == 0) & (Full_Final$Q17_Firms == 0) ] <- 2

## Plotting CV WTP by perceived responsibility Q17_1:  
Q17_FirmsGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q17_Firms, labeller = as_labeller(c(
    `0` = "No",
    `1` = "Yes")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by whether firms are responsible.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), 
                     limits=c(0,75),n.breaks=15,labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Plotting CV WTP by perceived responsibility Q17_2:  
Q17_ConsGraph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q17_Cons, labeller = as_labeller(c(
    `0` = "No\n (N = 286)",
    `1` = "Yes\n (N = 384)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by whether consumers only are responsible.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


#### Section 7: Precaution #### 


## Plotting precaution  
ggplot() + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male",
    `2` = "Other")))+
  geom_smooth(aes(x=Q24AIncome,y=Q6WTP,color="red"),data=FS,method="lm",se=F) +
  geom_smooth(aes(x=Q24AIncome,y=Q7WTP,color="blue"),data=FS,method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for Research", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Income",y="Difference between Q6 and Q7 WTP")


Full_Final <- subset(Full_Final,Q1Gender !=2)
## Plotting the effect of income on precautionary premia, faceted by gender.
Precaution <- ggplot(Full_Final) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female\n (N = 356)",
    `1` = "Male\n (N = 311)")))+
  geom_smooth(aes(x=Q24AIncome,y=Precaution,color="red"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Precautionary premium", "WTP for treatment"))+
  ggtitle("Relationship between precaution and income") +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,40),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Income",y="WTP")


ggplot(Full_Final, aes(x=as.numeric(Q13CurrentThreatToSelf))) + 
  facet_grid( ~ Order, labeller = as_labeller(c(
    `0` = paste("Order 1 (N =",nrow(Full_Final[Full_Final$Order==0,])/8,")"),
    `1` = paste("Order 2 (N =",nrow(Full_Final[Full_Final$Order==1,])/8,")"))))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and precaution given ordering") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 15, labels = function(x) paste0("£",x))+
  labs(x = "Environmental Attitudes",y="WTP")


#### Section 8: Validity #### 


#### Section 8A: Acceptance Rates #### 
Acceptance <- data.frame("Full_Long" =c(length(Full_Long$ID[(((Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)))]),length(Full_Long$ID[(((Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)))])), 
                         "Full_Cons" =c(length(Full_Cons$ID[(((Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)))]),length(Full_Cons$ID[(((Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)))])),
                         "Full_Dominated" =c(length(Full_Dominated$ID[(((Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)))]),length(Full_Dominated$ID[(((Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)))])),
                         "Full_Certain" =c(length(Full_Certain$ID[(((Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)))]),length(Full_Certain$ID[(((Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)))])))
Acceptance <- t(Acceptance) ## Transposed is easier to work with
Acceptance <- cbind(Acceptance,rowSums(Acceptance),deparse.level = 2) ## Add total responses
Acceptance <- cbind(Acceptance,100/Acceptance[,3]*Acceptance[,1],100/Acceptance[,3]*Acceptance[,2],c(1,2,3,4),deparse.level = 2) ## Add percentage accepting and rejecting A and number the questions.
colnames(Acceptance) <- c("A","B","Total","Acceptance","Rejected","Dataset")
Acceptance <- data.frame(Acceptance)


## AcceptanceRate1 is acceptance of each option in the Full sample
AcceptanceRate1 <- rbind(t(data.frame("Q9"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 1) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 1) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 2) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 2) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 3) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 3) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Long$ID[(( (Full_Long$Task == 4) & (Full_Long$alt == "A") & (Full_Long$Choice ==TRUE)) )] )),c(length(Full_Long$ID[(( (Full_Long$Task == 4) & (Full_Long$alt == "A") & (Full_Long$Choice ==FALSE)) )] ))))))
AcceptanceRate1 <- cbind(AcceptanceRate1,c(1,2,3,4),round(AcceptanceRate1[,1:2]/sum(AcceptanceRate1[1,1:2])*100,3))
colnames(AcceptanceRate1) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate1 <- data.frame(AcceptanceRate1)


## AcceptanceRate2 is acceptance of each option in the sample eliminating those failing the dominance test
AcceptanceRate2 <- rbind(t(data.frame("Q9"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 1) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 1) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 2) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 2) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 3) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 3) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 4) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==TRUE)) )] )),c(length(Full_Dominated$ID[(( (Full_Dominated$Task == 4) & (Full_Dominated$alt == "A") & (Full_Dominated$Choice ==FALSE)) )] ))))))
AcceptanceRate2 <- cbind(AcceptanceRate2,c(1,2,3,4),round(AcceptanceRate2[,1:2]/sum(AcceptanceRate2[1,1:2])*100,3))
colnames(AcceptanceRate2) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate2 <- data.frame(AcceptanceRate2)
AcceptanceRate2


## AcceptanceRate3 is acceptance of each option in the sample relying on certainty
AcceptanceRate3 <- rbind(t(data.frame("Q9"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 1) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 1) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 2) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 2) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 3) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 3) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Certain$ID[(( (Full_Certain$Task == 4) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==TRUE)) )] )),c(length(Full_Certain$ID[(( (Full_Certain$Task == 4) & (Full_Certain$alt == "A") & (Full_Certain$Choice ==FALSE)) )] ))))))
AcceptanceRate3 <- cbind(AcceptanceRate3,c(1,2,3,4),round(AcceptanceRate3[,1:2]/sum(AcceptanceRate3[1,1:2])*100,3))
colnames(AcceptanceRate3) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate3 <- data.frame(AcceptanceRate3)
AcceptanceRate3


## AcceptanceRate4 is acceptance of each option in the consequential sample
AcceptanceRate4 <- rbind(t(data.frame("Q9"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 1) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 1) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q10"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 2) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 2) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q11"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 3) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 3) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))),
                         t(data.frame("Q12"=c(c(length(Full_Cons$ID[(( (Full_Cons$Task == 4) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==TRUE)) )] )),c(length(Full_Cons$ID[(( (Full_Cons$Task == 4) & (Full_Cons$alt == "A") & (Full_Cons$Choice ==FALSE)) )] ))))))
AcceptanceRate4 <- cbind(AcceptanceRate4,c(1,2,3,4),round(AcceptanceRate4[,1:2]/sum(AcceptanceRate4[1,1:2])*100,3))
colnames(AcceptanceRate4) <- c("A","B","Question","APercent","BPercent")
AcceptanceRate4 <- data.frame(AcceptanceRate4)
AcceptanceRate4


## Combining line plots of acceptance rates over questions by dataset
### Note no major differences by question or data. 
ggplot()+
  geom_line(aes(x=Question,y=APercent,color="red"),data=AcceptanceRate1)+
  geom_line(aes(x=Question,y=APercent,color="blue"),data=AcceptanceRate2)+
  geom_line(aes(x=Question,y=APercent,color="green"),data=AcceptanceRate3)+
  geom_line(aes(x=Question,y=APercent,color="orange"),data=AcceptanceRate4)+
  scale_x_continuous(name="Question",breaks = 1:4, 
                     labels=c(rownames(AcceptanceRate1)))+
  scale_y_continuous(name="Percent choosing Option A",
                     breaks=waiver(),limits = c(25,75),
                     n.breaks = 10, labels = function(x) paste0(x, "%"))+
  scale_color_discrete(name = "Lines", 
                       labels = c("Full sample", "Dominated","Certainty","Consequentiality"))+
  ggtitle("Percentage choosing the status quo by question and truncation strategy.")


## Abandoned Code: 
# Full_Cons <- data.frame(Full_Cons) ## Change into dataframe format 
# Full_Long <- data.frame(Full_Long) ## Same here - ggplot2 throws a fit without it.
# 
# ## Here the Blue-Planet question is manipulated to be either of two values
# Full_Cons$Q16BP[Full_Cons$Q16BP == 0] <- "Not watched" 
# Full_Cons$Q16BP[Full_Cons$Q16BP == 1] <- "Watched"
# Full_Cons$Q16BP[Full_Cons$Q16BP == 2] <- "Watched"
# 
# ## Categorising charity involvement.
# Full_Cons$Q18Charity[Full_Cons$Q18Charity == 0] <- "No involvement"
# Full_Cons$Q18Charity[Full_Cons$Q18Charity == 1] <- "Donated or joined"
# 
# ## Rephrasing consequentiality into strings for use in graphics. 
# ### The strings used here and above are changed back later for econometric analysis. 
# Full_Long$Q20Consequentiality[Full_Long$Q20Consequentiality == 0] <- "Inconsequential"
# Full_Long$Q20Consequentiality[Full_Long$Q20Consequentiality == 1] <- "Consequential"
# 


#### Section 8B: Demand Curves #### 


## Demand Curve: Plotting choice versus price levels 
PriceCurve <- ggplot(Fulls, aes(Choice,Price_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Demand curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))+
  scale_y_continuous(breaks = 0:3, labels=c(0.5,1.0,2.5,5.0))


## Emission Curve: Plotting choice versus emission levels 
EmissionsCurve <- ggplot(Fulls, aes(Choice,Emission_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Emission curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1, labels=c(0,1))

## Performance Curve: Plotting choice versus performance levels 
PerformanceCurve <- ggplot(Fulls, aes(Choice,Performance_B)) + 
  geom_point(shape = 1) +
  geom_smooth(method="lm",se=T) +
  ggtitle("Performance curve") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likelihood of accepting option B",y="Attribute levels")+
  scale_x_continuous(breaks = 0:1,labels=c(0,1))

grid.arrange(PriceCurve, EmissionsCurve, PerformanceCurve)


#### Chapter Four Specific Graphs ####


#### Section 9: Age ####


AgeGraphCV <- ggplot(Full_Final, aes(x=Q2Age)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by age") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),
                     n.breaks = 10,limits=c(15,80))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

AgeGraphCE <- ggplot(Full_Final, aes(x=Q2Age)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between CE MWTP and age") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),
                     n.breaks = 10,limits=c(15,80))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


#### Section 10:Gender ####


GenderGraphCV <- ggplot(Full_Final, aes(x=Q1Gender)) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between CVM WTP by gender") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP For Research","WTP For Treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Gender",breaks = waiver(),
                     n.breaks = 2, labels = c("Female","Male"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

GenderGraphCE <- ggplot(Full_Final, aes(x=Q1Gender)) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=abs(EmissionCoef),color="red"),method="lm",se=F) +
  ggtitle("Relationship between CE MWTP and gender") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|","Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Gender",breaks = waiver(),
                     n.breaks = 2, labels = c("Female","Male"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


grid.arrange(GenderGraphCE,GenderGraphCV,AgeGraphCE,AgeGraphCV)


rbind("Q6"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$Q6WTP[Full_Final$Q1Gender== 0]),3)),
                                             data.frame("Male"=round(median(Full_Final$Q6WTP[Full_Final$Q1Gender== 1]),3)),
                                             data.frame("18-25"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                             data.frame("26-39"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                             data.frame("40-55"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                             data.frame("56-70"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                             data.frame("71+"=round(median(Full_Final$Q6WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Q7"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$Q7WTP[Full_Final$Q1Gender== 0]),3)),
                                             data.frame("Male"=round(median(Full_Final$Q7WTP[Full_Final$Q1Gender== 1]),3)),
                                             data.frame("18-25"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                             data.frame("26-39"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                             data.frame("40-55"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                             data.frame("56-70"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                             data.frame("71+"=round(median(Full_Final$Q7WTP[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Performance"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$PerformanceCoef[Full_Final$Q1Gender== 0]),3)),
                                                      data.frame("Male"=round(median(Full_Final$PerformanceCoef[Full_Final$Q1Gender== 1]),3)),
                                                      data.frame("18-25"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                      data.frame("26-39"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                      data.frame("40-55"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                      data.frame("56-70"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                      data.frame("71+"=round(median(Full_Final$PerformanceCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Emission"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(median(Full_Final$EmissionCoef[Full_Final$Q1Gender== 0]),3)),
                                                   data.frame("Male"=round(median(Full_Final$EmissionCoef[Full_Final$Q1Gender== 1]),3)),
                                                   data.frame("18-25"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                   data.frame("26-39"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                   data.frame("40-55"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                   data.frame("56-70"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                   data.frame("71+"=round(median(Full_Final$EmissionCoef[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Income"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(mean(Full_Final$Q24AIncome[Full_Final$Q1Gender== 0]),3)),
                                                 data.frame("Male"=round(mean(Full_Final$Q24AIncome[Full_Final$Q1Gender== 1]),3)),
                                                 data.frame("18-25"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                 data.frame("26-39"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                 data.frame("40-55"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                 data.frame("56-70"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                 data.frame("71+"=round(mean(Full_Final$Q24AIncome[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))),
      "Concern"=data.frame(cbind(data.frame(cbind(data.frame("Female"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q1Gender== 0]),3)),
                                                  data.frame("Male"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q1Gender== 1]),3)),
                                                  data.frame("18-25"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[1]]),3)),
                                                  data.frame("26-39"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[2]]),3)),
                                                  data.frame("40-55"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[3]]),3)),
                                                  data.frame("56-70"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[4]]),3)),
                                                  data.frame("71+"=round(mean(Full_Final$Q13CurrentThreatToSelf[Full_Final$Q2Age== unique(Full_Final$Q2Age)[5]]),3)))))))


#### Section 11: Coronavirus impact #### 


## Plotting CV WTP by COVID-19 
COVID_Graph1 <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q24RonaImpact, labeller = as_labeller(c(
    `0` = "No\n (N = 332)",
    `1` = "Yes\n (N = 319)",
    `2` = "Prefer not to say\n (N = 19)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP by effect of COVID-19 on income.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), n.breaks=6,
                     limits=c(20,50),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")


## Plotting CE MWPT by COVID-19  
COVID_Graph2 <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q24RonaImpact, labeller = as_labeller(c(
    `0` = "No\n (N = 332)",
    `1` = "Yes\n (N = 319)",
    `2` = "Prefer not to say\n (N = 19)")))+
  geom_smooth(aes(y=EmissionCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=PerformanceCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and Marginal WTP by effect of COVID-19 on income.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Emission MWTP", "Performance MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="Precautionary WTP",breaks = waiver(), 
                     limits=c(-0.5,0.5),n.breaks=10,labels = function(x) paste0("£",x))+
  labs(x = "Income",y="Precaution")

ggplot(Full_Final) + 
  geom_smooth(aes(y=Q13CurrentThreatToSelf,x=Q24RonaImpact,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q14FutureThreatToSelf,x=Q24RonaImpact,color="green"),method="lm",se=F) +
  geom_smooth(aes(y=Q15ThreatToEnvironment,x=Q24RonaImpact,color="red"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between attitudes and coronavirus impact") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="Attitudes",breaks=waiver(),limits=c(1,5),n.breaks=5)+ 
  scale_x_continuous(name="Q24RonaImpact",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3,
                     labels=c("0\n (N = 332)","1\n (N = 319)","2\n (N = 19)"))

ggplot(Full_Final) + 
  geom_smooth(aes(y=Q6WTP,x=Q24RonaImpact,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,x=Q24RonaImpact,color="green"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q6WTP", "Q7WTP"))+
  ggtitle("Relationship between WTP and coronavirus impact") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  scale_x_continuous(name="Q24RonaImpact",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3,
                     labels=c("0\n (N = 332)","1\n (N = 319)","2\n (N = 19)"))


#### Section 12: Socioeconomics ####


## Plotting CV WTP by employment type:
Q22Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q22Education, labeller = as_labeller(c(
    `0` = "Prefer not to say\n (N = 15)",
    `1` = "GCSE\n (N = 147)",
    `2` = "A-level\n (N = 178)",
    `3` = "Bachelors\n (N = 212)",
    `4` = "Postgraduate\n (N = 118)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by education level.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 3, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


### Relationship between education levels and CV WTP
Q22GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q22Education))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between education and WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="WTP")


### Relationship between education levels and CE WTP
Q22GraphBB <- ggplot(Full_Final, aes(x=as.numeric(Q22Education))) + 
  geom_smooth(aes(y=PerformanceCoef*-1,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between education and CE WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=21,
                     limits=c(0,0.2),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="WTP")

### Alternative to plotting education/wtp
Q22GraphC <- ggplot(Full_Final, aes(x=Q22Education)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between education and income") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Education",breaks = waiver(),limits = c(0,4),
                     n.breaks = 5,labels=c("Prefer not to say\n(N = 15)","GCSE\n(N = 147)","A level\n(N = 178)","Bachelor\n(N = 212)","Postgraduate\n(N = 118)"))+
  scale_y_continuous(name="Income",breaks = waiver(), n.breaks=5,
                     limits=c(0,5000),labels = function(x) paste0("£",x))+
  labs(x = "Education",y="Income")


## Plotting CV WTP by employment type:
Q23Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q23Employment, labeller = as_labeller(c(
    `0` = "Prefer not to say\n (N = 18)",
    `1` = "NEET\n (N = 76)",
    `2` = "Retired\n (N = 52)",
    `3` = "Student\n (N = 30)",
    `4` = "Part-time\n (N = 100)",
    `5` = "Self-employed\n (N = 46)",
    `6` = "Full-time\n (N = 348)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by employment type.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 3, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


ggplot(Full_Final, aes(x=as.numeric(Q23Employment))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between employment and WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                           "Retired\n (N = 52)",
                                           "Student\n (N = 30)",
                                           "Part-time\n (N = 100)",
                                           "Self-employed\n (N = 46)",
                                           "Full-time\n (N = 348)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="WTP")


### Relationship between employment levels and CE WTP
Q23GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q23Employment))) + 
  geom_smooth(aes(y=PerformanceCoef*-1,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between employment and CE WTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                           "Retired\n (N = 52)",
                                           "Student\n (N = 30)",
                                           "Part-time\n (N = 100)",
                                           "Self-employed\n (N = 46)",
                                           "Full-time\n (N = 348)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="WTP")


Q23GraphC <- ggplot(Full_Final, aes(x=Q23Employment)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between employment and income") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Employment",breaks = waiver(),limits = c(0,6),
                     n.breaks = 6,labels=c("Prefer not to say\n (N = 18)","NEET\n (N = 76)",
                                           "Retired\n (N = 52)",
                                           "Student\n (N = 30)",
                                           "Part-time\n (N = 100)",
                                           "Self-employed\n (N = 46)",
                                           "Full-time\n (N = 348)"))+
  scale_y_continuous(name="Income",breaks = waiver(), n.breaks=5,
                     limits=c(0,5000),labels = function(x) paste0("£",x))+
  labs(x = "Employment",y="Income")


#### Section 13: Knowledge #### 


## Plotting Q5 knowledge vs concern about microplastics
KnowledgeGraphA <- ggplot(Full_Final, aes(x=Q5Knowledge)) + 
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between knowledge (Q5) and concern (Q13,14,15)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Q5Knowledge",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("No\nknowledge\n(N = 71)","Little\n(N = 187)","Average\n(N = 252)","Good\n(N = 113)","Strong\nknowledge\n(N = 47)"))+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Income",y="WTP")


## Plotting Q19 knowledge and concern, allowing respondents to update knowledge:
KnowledgeGraphB <- ggplot(Full_Final, aes(x=Q19Knowledge)) + 
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between knowledge (Q19) and concern (Q13,14,15)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Q19Knowledge",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("No\nknowledge\n(N = 43)","Little\n(N = 158)","Average\n(N = 267)","Good\n(N = 159)","Strong\nknowledge\n(N = 43)"))+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Income",y="WTP")


## Plotting WTP by pre-survey microplastic knowledge
Q4GraphB <- ggplot(Full_Final, aes(x=Q4Trips)) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)*12),method="lm",se=F)+
  ggtitle("Relationship between income and trips.") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Annual trips to the coast.",breaks = waiver(),limits = c(0,3),
                     n.breaks = 3,
                     labels=c("0\n (N = 76)","1-2\n (N = 267)","3-5\n (N = 153)","6+\n (N = 174)"))+
  scale_y_continuous(name="Gross annual income",breaks = waiver(), n.breaks=10,
                     limits=c(0,60000),labels = function(x) paste0("£", round(x,2)/1000,",000"))+
  labs(x = "Trips",y="Income")


## Plotting WTP by pre-survey microplastic knowledge
Q5Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q5Knowledge,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q5Knowledge,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q5: Knowledge") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c("1\n(N = 71)","2\n(N = 187)","3\n(N = 252)","4\n(N = 113)","5\n(N = 47)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of Q19 knowledge about microplastics post survey
Q19Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q19Knowledge,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q19Knowledge,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q19: Knowledge") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c("1\n(N = 43)","2\n(N = 158)","3\n(N = 267)","4\n(N = 159)","5\n(N = 43)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


#### Section 14: Environmental Concern #### 


## Plotting the effect of health concern on WTP
Q13Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q13CurrentThreatToSelf,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q13CurrentThreatToSelf,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q13: Current Threat To Self") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of future concerns on WTP
Q14Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q14FutureThreatToSelf,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q14FutureThreatToSelf,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q14: Future Threat To Self") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP in £",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


## Plotting the effect of environmental concern on WTP
Q15Graph <- ggplot(FS) + 
  geom_smooth(aes(x=Q15ThreatToEnvironment,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q15ThreatToEnvironment,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q15: Threat To Environment") +
  scale_x_continuous(name="Likert scale levels",breaks = 1:5, 
                     labels=c(1,2,3,4,5))+
  scale_y_continuous(name="WTP in £",
                     breaks=waiver(),limits = c(10,60),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10)) +
  labs(x = "Likert scale levels",y="Precautionary premium WTP")


#### Section 15: Charity #### 


## Plotting CV WTP by charity involvement:  
Q18Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q18Charity, labeller = as_labeller(c(
    `0` = "No\n (N = 425)",
    `1` = "Yes\n (N = 219)",
    `2` = "Prefer not to say\n (N = 26)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by charity involvement.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Charity vs WTP
Q18GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q18Charity))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and charity involvement.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Charity involvement",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2,labels=c("No involvement\n (N = 451)", "Donated or member\n (N = 219)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Charity",y="WTP")


### Another method of plotting charity on WTP
Q18GraphC <- ggplot(Full_Final, aes(x=as.numeric(Q18Charity))) + 
  geom_smooth(aes(y=abs(PerformanceCoef),color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between WTP and charity involvement.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance MWTP|", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Charity involvement",breaks = waiver(),limits = c(0,1),
                     n.breaks = 2,labels=c("No involvement\n (N = 451)", "Donated or member\n (N = 219)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=10,
                     limits=c(0.00,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Charity",y="WTP")


#### Section 16: Blue-Planet #### 


###### Went a bit crazy here plotting points for Q16:
## First create subsets for each response:
BP0 <- subset(Full_Final,Full_Final$Q16BP==0)
BP1 <- subset(Full_Final,Full_Final$Q16BP==1)
BP2 <- subset(Full_Final,Full_Final$Q16BP==2)
BP <- data.frame("Q6WTPBP0I0"=round(mean(BP0$Q6WTP[BP0$Q24AIncome <= unique(BP0$Q24AIncome)[4]]),2),
                 "Q7WTPBP0I0"=round(mean(BP0$Q7WTP[BP0$Q24AIncome <= unique(BP0$Q24AIncome)[4]]),2),
                 "Q6WTPBP0I1"=round(mean(BP0$Q6WTP[BP0$Q24AIncome > unique(BP0$Q24AIncome)[5]]),2),
                 "Q7WTPBP0I1"=round(mean(BP0$Q7WTP[BP0$Q24AIncome > unique(BP0$Q24AIncome)[5]]),2),
                 "Q6WTPBP1I0"=round(mean(BP1$Q6WTP[BP1$Q24AIncome <= unique(BP1$Q24AIncome)[4]]),2),
                 "Q7WTPBP1I0"=round(mean(BP1$Q7WTP[BP1$Q24AIncome <= unique(BP1$Q24AIncome)[4]]),2),
                 "Q6WTPBP1I1"=round(mean(BP1$Q6WTP[BP1$Q24AIncome > unique(BP1$Q24AIncome)[5]]),2),
                 "Q7WTPBP1I1"=round(mean(BP1$Q7WTP[BP1$Q24AIncome > unique(BP1$Q24AIncome)[5]]),2),
                 "Q6WTPBP2I0"=round(mean(BP2$Q6WTP[BP2$Q24AIncome <= unique(BP2$Q24AIncome)[4]]),2),
                 "Q7WTPBP2I0"=round(mean(BP2$Q7WTP[BP2$Q24AIncome <= unique(BP2$Q24AIncome)[4]]),2),
                 "Q6WTPBP2I1"=round(mean(BP2$Q6WTP[BP2$Q24AIncome > unique(BP2$Q24AIncome)[5]]),2),
                 "Q7WTPBP2I1"=round(mean(BP2$Q7WTP[BP2$Q24AIncome > unique(BP2$Q24AIncome)[5]]),2))

## Plotting Q16 responses for CV WTP with points added:
Q16Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q16BP, labeller = as_labeller(c(
    `0` = "None\n (N = 209)",
    `1` = "Some\n (N = 342)",
    `2` = "All\n (N = 119)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  
  geom_point(data=BP0,aes(y=BP[1,1], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,1]>0,paste0("£", round(BP[1,1],2)),""),y=BP[1,1]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP0,aes(y=BP[1,2], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,2]>0,paste0("£", round(BP[1,2],2)),""),y=BP[1,2]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP0,aes(y=BP[1,3], x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,3]>0,paste0("£", round(BP[1,3],2)),""),y=BP[1,3]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP0,aes(y=BP[1,4], x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP0,size=3,label = ifelse(BP[1,4]>0,paste0("£", round(BP[1,4],2)),""),y=BP[1,4]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  
  geom_point(data=BP1,aes(y=BP[1,5], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,5]>0,paste0("£", round(BP[1,5],2)),""),y=BP[1,5]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP1,aes(y=BP[1,6], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,6]>0,paste0("£", round(BP[1,6],2)),""),y=BP[1,6]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP1,aes(y=BP[1,7]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,7]>0,paste0("£", round(BP[1,7],2)),""),y=BP[1,7]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP1,aes(y=BP[1,8]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP1,size=3,label = ifelse(BP[1,8]>0,paste0("£", round(BP[1,8],2)),""),y=BP[1,8]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  
  geom_point(data=BP2,aes(y=BP[1,9], x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,9]>0,paste0("£", round(BP[1,9],2)),""),y=BP[1,9]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP2,aes(y=BP[1,10]+1, x=as.numeric(unique(Full_Final$Q24AIncome)[1])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,10]>0,paste0("£", round(BP[1,10],2)),""),y=BP[1,10]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[1]))+
  geom_point(data=BP2,aes(y=BP[1,11], x=as.numeric(unique(Full_Final$Q24AIncome)[7])),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,11]>0,paste0("£", round(BP[1,11],2)),""),y=BP[1,11]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  geom_point(data=BP2,aes(y=BP[1,12]-1, x=as.numeric(unique(Full_Final$Q24AIncome)[8])-5),shape = 1)+
  geom_text(data=BP2,size=3,label = ifelse(BP[1,12]>0,paste0("£", round(BP[1,12],2)),""),y=BP[1,12]-4, x=as.numeric(unique(Full_Final$Q24AIncome)[7]))+
  ggtitle("Relationship between income and WTP faceted by Blue-Planet II viewership") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


## Plotting Q16BP simply versus CV WTP:
Q16GraphB <- ggplot(Full_Final, aes(x=Q16BP)) + 
  geom_smooth(aes(y=as.numeric(Q6WTP),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=as.numeric(Q7WTP),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("Relationship between BP viewing and WTP") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Amount of BPII viewed.",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("None","Some","All"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")

## Plotting Q16BP simply versus CE WTP:
Q16GraphC <- ggplot(Full_Final, aes(x=Q16BP)) + 
  geom_smooth(aes(y=as.numeric(abs(PerformanceCoef)),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=as.numeric(EmissionCoef),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("|Performance|", "Emission"))+
  ggtitle("Relationship between BP viewing and CE WTP") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Amount of BPII viewed.",breaks = waiver(),limits = c(0,2),
                     n.breaks = 3, labels = c("None","Some","All"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(0,0.1),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


#### Section 17: Experts #### 


## Plotting CV WTP by belief in experts: 
Q21Graph <- ggplot(Full_Final, aes(x=as.numeric(Q24AIncome))) + 
  facet_grid( ~ Q21Experts, labeller = as_labeller(c(
    `1` = "1: Unconfident\n (N = 16)",
    `2` = "2\n (N = 46)",
    `3` = "3\n (N = 251)",
    `4` = "4\n (N = 237)",
    `5` = "5: Confident\n (N = 120)")))+
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between income and WTP faceted by level of confidence in experts.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Income",breaks = waiver(),limits = c(0,5000),
                     n.breaks = 5, labels = function(x) paste0("£",x))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Income",y="WTP")


### Plotting belief in experts
Q21GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=Q6WTP,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q7WTP,color="red"),method="lm",se=F) +
  ggtitle("Relationship between confidence in experts and WTP (lm fitting).") +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="WTP",breaks = waiver(), n.breaks=20,
                     limits=c(0,75),labels = function(x) paste0("£",x))+
  labs(x = "Experts",y="WTP")


### Plotting the relationship between confidence in experts and WTP (lm fitting).
Q21GraphC <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=Q5Knowledge,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q19Knowledge,color="red"),method="lm",se=F) +
  geom_smooth(aes(y=Q13CurrentThreatToSelf,color="darkorange1"),method="lm",se=F,linetype="dotdash") +
  geom_smooth(aes(y=Q14FutureThreatToSelf,color="darkorange2"),method="lm",se=F,linetype="dashed") +
  geom_smooth(aes(y=Q15ThreatToEnvironment,color="darkorange3"),method="lm",se=F,linetype="dotted") +
  ggtitle("Relationship between confidence in experts and WTP (lm fitting).") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q5 Knowledge", "Q19Knowledge","Q13CurrentThreatToSelf","Q14FutureThreatToSelf","Q15ThreatToEnvironment"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="Likert scale",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c(1,2,3,4,5))+
  labs(x = "Experts",y="Likert scale")


### Another experts plot but on MWTP
Q21GraphD <- ggplot(Full_Final, aes(x=as.numeric(Q21Experts))) + 
  geom_smooth(aes(y=PerformanceCoef,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=EmissionCoef,color="red"),method="lm",se=F) +
  ggtitle("Relationship between confidence in experts and MWTP.") +
  scale_color_discrete(name = "Lines", 
                       labels = c("Performance MWTP", "Emission MWTP"))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Experts",breaks = waiver(),limits = c(1,5),
                     n.breaks = 5, labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  scale_y_continuous(name="MWTP",breaks = waiver(), n.breaks=10,
                     limits=c(-0.5,0.5),labels = function(x) paste0("£",x))+
  labs(x = "Experts",y="WTP")


#### Section 18: Mixed categories ####


## Plot 1 (of 2):
P1 <- ggplot(Full_Long, aes(Q3Distance,Q4Trips)) + 
  geom_point(shape = 1) +
  facet_grid(~Q18Charity) + 
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between distance and trips by charity involvement.") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Distance",y="Trips")+
  scale_y_continuous(limits = c(0,1))

## Plot 2 (of 2):
P2 <- ggplot(Full_Long, aes(Q3Distance,Q4Trips)) + 
  geom_point(shape = 1) +
  facet_grid(~Q16BP) + 
  geom_smooth(method="lm",se=F) +
  ggtitle("Relationship between distance and trips by Blue-Planet II") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin=unit(c(1,1,-0.5,1),"cm"),
        axis.title.y = element_text(size = 12)) +
  labs(x = "Distance",y="Trips")+
  scale_y_continuous(limits = c(0,1))

## Plots both Plot1 and Plot2 together in the same view.
grid.arrange(P1, P2 )

### Plotting age, gender, and concern
KnowledgeGraphC <- ggplot(Full_Final, aes(x=Q2Age)) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male")))+
  geom_smooth(aes(y=(Q13CurrentThreatToSelf),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q14FutureThreatToSelf),color="red"),method="lm",se=F)+
  geom_smooth(aes(y=(Q15ThreatToEnvironment),color="yellow"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between age, gender and concern") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),limits = c(20.0,70.0),
                     n.breaks = 5)+
  scale_y_continuous(name="Likert scale of concern.",breaks = waiver(), n.breaks=5,
                     limits=c(1,5),labels = c("1:\nCompletely Disagree","2","3","4","5:\nCompletely Agree"))+
  labs(x = "Age",y="Concern")


### Age, gender and knowledge
KnowledgeGraphD <- ggplot(Full_Final, aes(x=Q2Age)) + 
  facet_grid( ~ Q1Gender, labeller = as_labeller(c(
    `0` = "Female",
    `1` = "Male")))+
  geom_smooth(aes(y=(Q5Knowledge),color="blue"),method="lm",se=F)+
  geom_smooth(aes(y=(Q19Knowledge),color="red"),method="lm",se=F)+
  scale_color_discrete(name = "Lines", 
                       labels = c("Q5", "Q19"))+
  ggtitle("Relationship between age, gender and concern") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_x_continuous(name="Age",breaks = waiver(),limits = c(20.0,70.0),
                     n.breaks = 5)+
  scale_y_continuous(name="Likert scale of knowledge",breaks = waiver(), n.breaks=5,
                     limits=c(1,5))+
  labs(x = "Age",y="Concern")


#### Section 19: Spatial Plotting: Spatial map and distance-decay #### 


## Importing and transforming a UK outline
UK <- ne_countries(scale = 50, country = "united kingdom", returnclass = "sf")
UK <- st_transform(UK, 27700)
grid <- st_make_grid(UK, cellsize = 1000, what = "centers")
grid <- st_intersection(grid, UK)  
UK <- st_cast(UK, "MULTILINESTRING")
dist <- st_distance(UK, grid) # Here making a vector of actual distances from the coast
df <- data.frame(dist = as.vector(dist),st_coordinates(grid))
Dist <- unique(FullSurvey2$Q3Distance)*1000 # Here making a vector of the unique categories from the survey. The survey was in miles but the grid wasn't so need to scale

## The approach below is to classify the distances from the coast according to the survey categories
ClassifiedQ6 <- data.frame(ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[1], 0,
                                  ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[2], 1,
                                         ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[3], 2,
                                                ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[4], 3,
                                                       ifelse(as.vector(dist) < sort(unique(as.vector(Dist)))[5], 4,4))))))

ClassifiedQ7 <- ClassifiedQ6 # Used later for the same method just different WTP

## Here classifying Q6 fitted WTP by distance from the coast
Q6D1 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[1]]),3)
Q6D2 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[2]]),3)
Q6D3 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[3]]),3)
Q6D4 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[4]]),3)
Q6D5 <- round(mean(Full_Final$Q6WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[5]]),3)

# Replacing the distance categories using WTP
ClassifiedQ6 <- ifelse(ClassifiedQ6==0, Q6D1,
                       ifelse(ClassifiedQ6==1, Q6D2,
                              ifelse(ClassifiedQ6==2, Q6D3,
                                     ifelse(ClassifiedQ6==3, Q6D4,
                                            ifelse(ClassifiedQ6==4, Q6D5,Q6D5)))))
colnames(ClassifiedQ6) <- "ClassifiedQ6"

# Now same but for Q7 WTP
Q7D1 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[1]]),3)
Q7D2 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[2]]),3)
Q7D3 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[3]]),3)
Q7D4 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[4]]),3)
Q7D5 <- round(mean(Full_Final$Q7WTP[Full_Final$Q3Distance == sort(unique(Full_Final$Q3Distance))[5]]),3)

ClassifiedQ7 <- ifelse(ClassifiedQ7==0, Q7D1,
                       ifelse(ClassifiedQ7==1, Q7D2,
                              ifelse(ClassifiedQ7==2, Q7D3,
                                     ifelse(ClassifiedQ7==3, Q7D4,
                                            ifelse(ClassifiedQ7==4, Q7D5,Q7D5)))))
colnames(ClassifiedQ7) <- "ClassifiedQ7"

# Combining the actual distances, the UK outline, and the fitted spatial WTP
df2 <- (data.frame(dist = as.vector(dist),st_coordinates(grid),ClassifiedQ6=ClassifiedQ6,ClassifiedQ7=ClassifiedQ7))

## Plotting the two CV WTP maps:
CQ6 <- ggplot(df2, aes(X, Y, fill = ClassifiedQ6))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(brewer.pal(5, "RdGy")))+ #colors for plotting the distance
  labs(fill = "WTP (Q6 in per person per year)")+ #legend name
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=8,face="bold"),
        legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid"),
        legend.key.size = grid::unit(2, "lines"))+ 
  coord_fixed(1) +
  ggtitle("Spatial distribution of Q6 WTP")

CQ7 <- ggplot(df2, aes(X, Y, fill = ClassifiedQ7))+ #variables
  geom_tile()+ #geometry
  scale_fill_gradientn(colours = rev(brewer.pal(10, "BrBG")))+ #colors for plotting the distance
  labs(fill = "WTP (Q7 in per person per year)")+ #legend name
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.text = element_text(size=8,face="bold"),
        legend.background = element_rect(fill="lightblue",size=0.25, linetype="solid"),
        legend.key.size = grid::unit(1, "lines"))+ 
  coord_fixed(1) +
  ggtitle("Spatial distribution of Q7 WTP")


## Reporting distance-decay WTP:
DistanceDecay <- rbind("Q6"=data.frame("LowIncomeLowDistance"=mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "LowIncomeHighDistance"= mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeLowDistance"=mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeHighDistance"= mean(Full_Final$Q6WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Q7"=data.frame("LowIncomeLowDistance"=mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "LowIncomeHighDistance"= mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeLowDistance"=mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                       "HighIncomeHighDistance"= mean(Full_Final$Q7WTP[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Emissions"=data.frame("LowIncomeLowDistance"=mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                              "LowIncomeHighDistance"= mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                              "HighIncomeLowDistance"=mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                              "HighIncomeHighDistance"= mean(Full_Final$EmissionCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])),
                       "Performance"=data.frame("LowIncomeLowDistance"=mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                                "LowIncomeHighDistance"= mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ]),
                                                "HighIncomeLowDistance"=mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance < median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ]),
                                                "HighIncomeHighDistance"= mean(Full_Final$PerformanceCoef[ (Full_Final$Q3Distance > median(Full_Final$Q3Distance)) & (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ])))


## Plotting the effect of distance from the coast on WTP
Q3Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q3Distance,y=Q7WTP,color="red"),method="lm",se=T) +
  geom_smooth(aes(x=Q3Distance,y=Q6WTP,color="blue"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for research", "WTP for treatment"))+
  ggtitle("WTP by Q3: Distance") +
  scale_x_continuous(name="Distance",breaks=waiver(),limits=c(0,50),
                     n.breaks=5)+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10))


### Plotting the effect of distance:
Q3GraphB <- ggplot(Full_Final, aes(x=as.numeric(Q3Distance))) + 
  geom_smooth(aes(y=as.numeric(Q24AIncome)),method="lm",se=F)+
  ggtitle("Relationship between income and distance from the coast") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="Gross monthly income",breaks = waiver(), n.breaks=10,
                     limits=c(0,5000),labels = function(x) paste0("£", x))+
  scale_x_continuous(name="Distance",breaks=waiver(),limits=c(0,50),
                     n.breaks=5)+ labs(x = "Distance",y="Income")


ggplot(Full_Final) + 
  geom_smooth(aes(y=Q13CurrentThreatToSelf,x=Q3Distance,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q14FutureThreatToSelf,x=Q3Distance,color="green"),method="lm",se=F) +
  geom_smooth(aes(y=Q15ThreatToEnvironment,x=Q3Distance,color="red"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between attitudes and distance from the coast") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="Attitudes",breaks=waiver(),limits=c(1,5),n.breaks=5)+ 
  scale_x_continuous(name="Distance",breaks=waiver(),limits=c(0,50),
                     n.breaks=5)+ labs(x = "Distance",y="Attitudes")

ggplot(Full_Final) + 
  geom_smooth(aes(y=Q13CurrentThreatToSelf,x=Q4Trips,color="blue"),method="lm",se=F) +
  geom_smooth(aes(y=Q14FutureThreatToSelf,x=Q4Trips,color="green"),method="lm",se=F) +
  geom_smooth(aes(y=Q15ThreatToEnvironment,x=Q4Trips,color="red"),method="lm",se=F) +
  scale_color_discrete(name = "Lines", 
                       labels = c("Q13", "Q14","Q15"))+
  ggtitle("Relationship between attitudes and trips to the coast") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 12)) +
  scale_y_continuous(name="Attitudes",breaks=waiver(),limits=c(1,5),n.breaks=5)+ 
  scale_x_continuous(name="Annual trips to the coast.",breaks = waiver(),limits = c(0,3),
                     n.breaks = 3,
                     labels=c("0\n (N = 76)","1-2\n (N = 267)","3-5\n (N = 153)","6+\n (N = 174)"))

## Plotting the effect of number of trips to the coast on WTP
Q4Graph <- ggplot(Full_Final) + 
  geom_smooth(aes(x=Q4Trips,y=Q7WTP,color="blue"),method="lm",se=T) +
  geom_smooth(aes(x=Q4Trips,y=Q6WTP,color="red"),method="lm",se=T) +
  scale_color_discrete(name = "Lines", 
                       labels = c("WTP for treatment", "WTP for research"))+
  ggtitle("WTP by Q4: Trips") +
  scale_x_continuous(name="Annual trips to the coast.",breaks = waiver(),limits = c(0,3),
                     n.breaks = 3,
                     labels=c("0\n (N = 76)","1-2\n (N = 267)","3-5\n (N = 153)","6+\n (N = 174)"))+
  scale_y_continuous(name="WTP",
                     breaks=waiver(),limits = c(0,75),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 10))


#### Section 20: Histograms #### 


## I use this function in all these histograms to make the lines:
fun = function(x, mean, sd, n){
  n * dnorm(x = x, mean = mean, sd = sd)
}

## Plotting a histogram for the precautionary premia
PPHist <- ggplot(FullSurvey2, aes(x=Precaution)) + 
  geom_histogram(color="black", fill="white",binwidth = 1)+
  scale_x_continuous(breaks=waiver(),limits = c(0,40),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  ggtitle("Histogram of respondent precautionary premia.")


EmissionDistribution <- ggplot(Full_Final, aes(x=EmissionCoef)) + 
  geom_histogram(color="black", fill="white",bins = 50)+
  scale_x_continuous(breaks=waiver(),limits = c(-0.5,0.5),
                     n.breaks = 10)+
  ggtitle("Histogram of emission WTP.")


## Plotting a histogram of individual attribute-specific WTP
PerformanceDistribution <- ggplot(Full_Final, aes(x=PerformanceCoef)) + 
  geom_histogram(color="black", fill="white",bins = 50)+
  scale_x_continuous(breaks=waiver(),limits = c(-0.5,0.5),
                     n.breaks = 10)+
  ggtitle("Histogram of performance WTP.")


## Histograms of Q5 and Q19 understanding:
Q5GraphA <- ggplot(Full_Final, aes(x=Q5Knowledge)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun=fun, 
    args = with(Full_Final, c(mean = mean(Q5Knowledge), sd = sd(Q5Knowledge), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 71)","2\n (N = 187)","3\n (N = 252)","4\n (N = 113)","5\n (N = 47)"))+
  ggtitle("Histogram of Q5 Knowledge responses.")


### Plotting knowledge histograms
Q19GraphA <- ggplot(Full_Final, aes(x=Q19Knowledge)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun=fun, 
    args = with(Full_Final, c(mean = mean(Q19Knowledge), sd = sd(Q19Knowledge), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 43)","2\n (N = 158)","3\n (N = 267)","4\n (N = 159)","5\n (N = 43)"))+
  ggtitle("Histogram of Q19 Knowledge responses.")
grid.arrange(Q5GraphA, Q19GraphA)


## Histograms of Q13, 14, 15 concern:
Q13GraphA <- ggplot(Full_Final, aes(x=Q13CurrentThreatToSelf)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun = fun, 
    args = with(Full_Final, c(mean = mean(Q13CurrentThreatToSelf), sd = sd(Q13CurrentThreatToSelf), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 32)","2\n (N = 55)","3\n (N = 290)","4\n (N = 184)","5\n (N = 109)"))+
  ggtitle("Histogram of Q13 Current Threat to Self responses.")


### Q14 histogram
Q14GraphA <- ggplot(Full_Final, aes(x=Q14FutureThreatToSelf)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun = fun, 
    args = with(Full_Final, c(mean = mean(Q14FutureThreatToSelf), sd = sd(Q14FutureThreatToSelf), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 20)","2\n (N = 37)","3\n (N = 204)","4\n (N = 243)","5\n (N = 166)"))+
  ggtitle("Histogram of Q14FutureThreatToSelf responses.")


### Q15 Histogram
Q15GraphA <- ggplot(Full_Final, aes(x=Q15ThreatToEnvironment)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun = fun, 
    args = with(Full_Final, c(mean = mean(Q15ThreatToEnvironment), sd = sd(Q15ThreatToEnvironment), n
                              = 5)))+
  scale_x_continuous(breaks=waiver(),labels=c("1\n (N = 12)","2\n (N = 30)","3\n (N = 154)","4\n (N = 216)","5\n (N = 258)"))+
  ggtitle("Histogram of Q15ThreatToEnvironment responses.")
grid.arrange(Q13GraphA, Q14GraphA,Q15GraphA)


### Q21 Histogram
Q21Hist <- ggplot(Full_Final, aes(x=Q21Experts)) + 
  geom_histogram(aes(y = ..density..),color="black", fill="white",bins = 50)+
  stat_function(fun = fun, 
    args = with(Full_Final, c(mean = mean(Q21Experts), sd = sd(Q21Experts), n
                              = 6)))+
  scale_x_continuous(breaks=waiver(),
                     name="Experts",
                     labels = c("1: Unconfident\n (N = 16)","2\n (N = 46)","3\n (N = 251)","4\n (N = 237)","5: Confident\n (N = 120)"))+
  ggtitle("Histogram of Q21Experts")


#### Section 21: Combining Plots ####


Q3Graph
Q4Graph
Q12Graph
Q13Graph
Q14Graph
Q15Graph
Q16Graph
Q17_ConsGraph
Q17_FirmsGraph
Q18Graph
Q20Graph
Q21Graph

## Combining multiple plots:
grid.arrange(Q3Graph, Q4Graph)
grid.arrange(Q5Graph, Q19Graph)
grid.arrange(Q18GraphB, Q18GraphC)
grid.arrange(KnowledgeGraphA, KnowledgeGraphB)
grid.arrange(KnowledgeGraphD, KnowledgeGraphC)
grid.arrange(PerformanceWTP, EmissionWTP )
grid.arrange(PerformanceDistribution, EmissionDistribution )