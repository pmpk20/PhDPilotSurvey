#### PhD Replication Work  ###############
## Project author: Peter King (p.m.king@bath.ac.uk)
## Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
## Notes: This code replicates the CBA calculations.

## Section 0: Package Import and Function definition
import pandas as pd
import numpy as np
!pip install numpy_financial
import numpy_financial as npf 
    
def NPV(Rate,X):
    Base = pd.DataFrame([X])
    Total = pd.concat([Base],axis=1)
    Total.columns = ["Base"]
    Total.index = ["Totals"]
    BaseCosts = pd.DataFrame([Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"],Total["Base"]])
    BaseCosts = BaseCosts.transpose()
    BaseCosts.columns = range(10)
    return (round(npf.npv(Rate,BaseCosts.iloc[0,:]),2))
    
    
## Section 1: Enter Assumed Figures here:
Households =27800000
Products =23000000

ResearchPointCost =21.818300
ResearchLowerCost =5.729300
ResearchUpperCost =102.531100
ResearchPointBenefit =2143
ResearchLowerBenefit =2011
ResearchUpperBenefit =2285


WWTPPointCost =1370
WWTPLowerCost =1000
WWTPUpperCost =5000
WWTPPointBenefit =2458
WWTPLowerBenefit =2314
WWTPUpperBenefit =2610

CosmeticsPointCost =1010
CosmeticsLowerCost =213
CosmeticsUpperCost =2500
CosmeticsPointBenefit =82.8
CosmeticsLowerBenefit =78.2
CosmeticsUpperBenefit =89.7


## Calculate Annual Net Benefits:


ResearchPointAnnualNB = ResearchPointBenefit - ResearchPointCost
ResearchLowerAnnualNB = ResearchLowerBenefit - ResearchLowerCost
ResearchUpperAnnualNB = ResearchUpperBenefit - ResearchUpperCost


WWTPPointBenefit - WWTPPointCost
WWTPLowerBenefit - WWTPLowerCost
WWTPUpperBenefit - WWTPUpperCost


CosmeticsPointBenefit - CosmeticsPointCost
CosmeticsLowerBenefit - CosmeticsLowerCost
CosmeticsUpperBenefit - CosmeticsUpperCost



## Calculate Discounted Benefits:

ResearchPointNPV = round(NPV(0.035, ResearchPointAnnualNB)/1000,2)
ResearchLowerNPV = round(NPV(0.035, ResearchLowerAnnualNB)/1000,2)
ResearchUpperNPV = round(NPV(0.035, ResearchUpperAnnualNB)/1000,2)



Treatment_MeanIncome_NPV_Lower = round(NPV(0.035, ((83.23 *27.8)-WWTPLowerCost))/1000,2)
Treatment_MeanIncome_NPV_Point = round(NPV(0.035, ((88.43 *27.8)-WWTPPointCost))/1000,2)
Treatment_MeanIncome_NPV_Upper = round(NPV(0.035, ((93.92 *27.8)-WWTPUpperCost))/1000,2)


Treatment_LowIncome_NPV_Lower = round(NPV(0.035, ((77.5 *27.8)-WWTPLowerCost))/1000,2)
Treatment_LowIncome_NPV_Point = round(NPV(0.035, ((84.57 *27.8)-WWTPPointCost))/1000,2)
Treatment_LowIncome_NPV_Upper = round(NPV(0.035, ((93.64 *27.8)-WWTPUpperCost))/1000,2)
pd.DataFrame([Treatment_LowIncome_NPV_Lower,Treatment_LowIncome_NPV_Lower,Treatment_LowIncome_NPV_Lower])

Treatment_HighIncome_NPV_Lower = round(NPV(0.035, ((107.23 *27.8)-WWTPLowerCost))/1000,2)
Treatment_HighIncome_NPV_Point = round(NPV(0.035, ((122.35 *27.8)-WWTPPointCost))/1000,2)
Treatment_HighIncome_NPV_Upper = round(NPV(0.035, ((143.16 *27.8)-WWTPUpperCost))/1000,2)
pd.DataFrame([Treatment_HighIncome_NPV_Lower,Treatment_HighIncome_NPV_Point,Treatment_HighIncome_NPV_Upper])


## Implicit Weights:


## Accompnaying R Code:


# ## Fitting WTP Correctly
# Full_Low <- Full_Final[(Full_Final$Q24AIncome < median(Full_Final$Q24AIncome)) ,]
# Full_High <- Full_Final[ (Full_Final$Q24AIncome > median(Full_Final$Q24AIncome)) ,]
# 
# Treatment_LowIncome_WTP <- sbchoice(Q7TreatmentResponse  ~ 1 |Q7Bid , data = Full_Low,dist="normal")
# Treatment_HighIncome_WTP <- sbchoice(Q7TreatmentResponse  ~ 1 |Q7Bid , data = Full_High,dist="normal")
# 
# Treatment_LowIncome_WTP_Point <- krCI(Treatment_LowIncome_WTP)$out[1,1]
# Treatment_LowIncome_WTP_High <- krCI(Treatment_LowIncome_WTP)$out[1,3]
# Treatment_LowIncome_WTP_Low <- krCI(Treatment_LowIncome_WTP)$out[1,2]
# 
# Treatment_HighIncome_WTP_Point <- krCI(Treatment_HighIncome_WTP)$out[1,1]
# Treatment_HighIncome_WTP_High <- krCI(Treatment_HighIncome_WTP)$out[1,3]
# Treatment_HighIncome_WTP_Low <- krCI(Treatment_HighIncome_WTP)$out[1,2]
# 
# 
# 
# Research_LowIncome_WTP <- sbchoice(Q6ResearchResponse  ~ 1 |Q6Bid , data = Full_Low,dist="normal")
# Research_HighIncome_WTP <- sbchoice(Q6ResearchResponse  ~ 1 |Q6Bid , data = Full_High,dist="normal")
# 
# Research_LowIncome_WTP_Point <- krCI(Research_LowIncome_WTP)$out[1,1]
# Research_LowIncome_WTP_High <- krCI(Research_LowIncome_WTP)$out[1,3]
# Research_LowIncome_WTP_Low <- krCI(Research_LowIncome_WTP)$out[1,2]
# 
# Research_HighIncome_WTP_Point <- krCI(Research_HighIncome_WTP)$out[1,1]
# Research_HighIncome_WTP_High <- krCI(Research_HighIncome_WTP)$out[1,3]
# Research_HighIncome_WTP_Low <- krCI(Research_HighIncome_WTP)$out[1,2]
# 
# cbind(round(Research_LowIncome_WTP_Low,2),
#       round(Research_LowIncome_WTP_Point,2),
#       round(Research_LowIncome_WTP_High,2))
# cbind(round(Research_HighIncome_WTP_Low,2),
#       round(Research_HighIncome_WTP_Point,2),
#       round(Research_HighIncome_WTP_High,2))

# #### Section 7A: Distributional weights: ####
# ## Explicit Weights using sensitivity analysis for the elasticity of marginal utility of income
# Weights <- cbind(data.frame("e0"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0),
#                  data.frame("e0.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.5),
#                  data.frame("e0.75"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^0.75),
#                  data.frame("e1"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1),
#                  data.frame("e1.3"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.3),
#                  data.frame("e1.5"=(mean(Full_Final$Q24AIncome)/Full_Final$Q24AIncome)^1.5))
# 
# ## C5 T11:
# Q6 <- 77.10
# Q7 <- 88.43
# SampleY <- 2192
# Em <- 0.038
# Perf <- 0.045
# 
# cbind(data.frame("e0"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^0)),
#                    data.frame("e0.5"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^0.5)),
#       data.frame("e0.75"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^0.75)),
#                    data.frame("e1"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^1)),
#       data.frame("e1.25"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^1.25)),
#                    data.frame("e1.5"=mean(Q6*mean(SampleY/Full_Final$Q24AIncome)^1.5)))
# 
# cbind(data.frame("e0"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^0)),
#       data.frame("e0.5"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^0.5)),
#       data.frame("e0.75"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^0.75)),
#       data.frame("e1"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^1)),
#       data.frame("e1.25"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^1.25)),
#       data.frame("e1.5"=mean(Q7*mean(SampleY/Full_Final$Q24AIncome)^1.5)))  
# 
# cbind(data.frame("e0"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^0)),
#       data.frame("e0.5"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^0.5)),
#       data.frame("e0.75"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^0.75)),
#       data.frame("e1"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^1)),
#       data.frame("e1.25"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^1.25)),
#       data.frame("e1.5"=mean(Em*mean(SampleY/FullSurvey2$Q24AIncome)^1.5))) 




pd.DataFrame([Treatment_LowIncome_NPV_Lower/Treatment_HighIncome_NPV_Lower,
              Treatment_LowIncome_NPV_Point/Treatment_HighIncome_NPV_Point,
              Treatment_LowIncome_NPV_Upper/Treatment_HighIncome_NPV_Upper])




Research_MeanIncome_NPV_Lower = round(NPV(0.035, ((72.35 *27.8)-ResearchLowerCost))/1000,2)
Research_MeanIncome_NPV_Point = round(NPV(0.035, ((77.10 *27.8)-ResearchPointCost))/1000,2)
Research_MeanIncome_NPV_Upper = round(NPV(0.035, ((82.83 *27.8)-ResearchUpperCost))/1000,2)


Research_LowIncome_NPV_Lower = round(NPV(0.035, ((75.90 *27.8)-ResearchLowerCost))/1000,2)
Research_LowIncome_NPV_Point = round(NPV(0.035, ((82.67 *27.8)-ResearchPointCost))/1000,2)
Research_LowIncome_NPV_Upper = round(NPV(0.035, ((90.72 *27.8)-ResearchUpperCost))/1000,2)
pd.DataFrame([Research_LowIncome_NPV_Lower,Research_LowIncome_NPV_Point,Research_LowIncome_NPV_Upper])

Research_HighIncome_NPV_Lower = round(NPV(0.035, ((107.23 *27.8)-ResearchLowerCost))/1000,2)
Research_HighIncome_NPV_Point = round(NPV(0.035, ((122.35 *27.8)-ResearchPointCost))/1000,2)
Research_HighIncome_NPV_Upper = round(NPV(0.035, ((143.16 *27.8)-ResearchUpperCost))/1000,2)
pd.DataFrame([Research_HighIncome_NPV_Lower,Research_HighIncome_NPV_Point,Research_HighIncome_NPV_Upper])


## Implicit Weights:
pd.DataFrame([Research_LowIncome_NPV_Lower/Research_HighIncome_NPV_Lower,
              Research_LowIncome_NPV_Point/Research_HighIncome_NPV_Point,
              Research_LowIncome_NPV_Upper/Research_HighIncome_NPV_Upper])


## Explicit Weights:
    
pd.DataFrame([round(NPV(0.035, ((77.10 *27.8)-ResearchPointCost))/1000,2),
round(NPV(0.035, ((109.4851  *27.8)-ResearchPointCost))/1000,2),
round(NPV(0.035, ((130.4683  *27.8)-ResearchPointCost))/1000,2),
round(NPV(0.035, ((155.4731  *27.8)-ResearchPointCost))/1000,2),
round(NPV(0.035, ((185.2702  *27.8)-ResearchPointCost))/1000,2),
round(NPV(0.035, ((220.778 *27.8)-ResearchPointCost))/1000,2)])


pd.DataFrame([round(NPV(0.035, ((125.5741  *27.8)-WWTPPointCost))/1000,2),
round(NPV(0.035, ((149.6409  *27.8)-WWTPPointCost))/1000,2),
round(NPV(0.035, ((178.3202  *27.8)-WWTPPointCost))/1000,2),
round(NPV(0.035, ((212.496  *27.8)-WWTPPointCost))/1000,2),
round(NPV(0.035, ((253.2218 *27.8)-WWTPPointCost))/1000,2)])

                   

## Calculate NPV:


## Calculate B:C Ratio:


## Distributional Weights:


pd.concat([pd.DataFrame([round(NPV((2.82*23)-CosmeticsLowerCost)/1000,2)]),
           pd.DataFrame([round(NPV((3.96*23)-CosmeticsPointCost)/1000,2)]),
           pd.DataFrame([round(NPV((3.39*23)-CosmeticsUpperCost)/1000,2)])])/pd.concat([pd.DataFrame([round(NPV((2.20*23)-CosmeticsLowerCost)/1000,2)]),
           pd.DataFrame([round(NPV((2.52*23)-CosmeticsPointCost)/1000,2)]),
           pd.DataFrame([round(NPV((2.83*23)-CosmeticsUpperCost)/1000,2)])])

-2.20 -2.52 -2.83

#### NPV
pd.concat([pd.DataFrame({'-20%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.8)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.8)) )/1000,2)]}),
pd.DataFrame({'-10%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.9)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.9)) )/1000,2)]}),
pd.DataFrame({'-5%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*0.95)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*0.95)) )/1000,2)]}),
pd.DataFrame({'0%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1)) )/1000,2)]}),
pd.DataFrame({'+5%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.05)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.05)) )/1000,2)]}),
pd.DataFrame({'+10%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.1)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.1)) )/1000,2)]}),
pd.DataFrame({'+20%':[round(NPV(((ResearchPointBenefit*0.8)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.9)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*0.95)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.05)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.1)-(ResearchPointCost*1.2)) )/1000,2),
                    round(NPV(((ResearchPointBenefit*1.2)-(ResearchPointCost*1.2)) )/1000,2)]})],axis=1)


#### B:C Ratio
pd.concat([pd.DataFrame({'-20%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.8),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.8),2)]}),
pd.DataFrame({'-10%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.9),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.9),2)]}),
pd.DataFrame({'-5%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*0.95),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*0.95),2)]}),
pd.DataFrame({'0%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1),2)]}),
pd.DataFrame({'+5%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.05),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.05),2)]}),
pd.DataFrame({'+10%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.1),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.1),2)]}),
pd.DataFrame({'+20%':[round(NPV(ResearchPointBenefit*0.8)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*0.9)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*0.95)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.05)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.1)/NPV(ResearchPointCost*1.2),2),
                      round(NPV(ResearchPointBenefit*1.2)/NPV(ResearchPointCost*1.2),2)]})],axis=1)
