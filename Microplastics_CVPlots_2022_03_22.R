#### Willingness-to-pay for precautionary control of microplastics, a comparison of hybrid choice models. Paper ####
## Function: Plots CV vs LV
## Author: Dr Peter King (p.m.king@kent.ac.uk)
## Last change: 22/03/2022
## TODO: tidy up plots


#-------------------------------
#### Replication Information ####
#-------------------------------

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

#-------------------------------
#### Setup: ####
#-------------------------------

library(DCchoice)
FullSurvey2 <- data.frame(read.csv("FullSurvey2.csv")) 

Q1_WTP <- data.frame(readRDS("Q6_SBDCModel_WTP.rds"))
Q2_WTP <- data.frame(readRDS("Q7_SBDCModel_WTP.rds"))

Q1_WTP_Lower <- data.frame(readRDS("Q6_SBDCModel_WTP_Lower.rds"))
Q2_WTP_Lower <- data.frame(readRDS("Q7_SBDCModel_WTP_Lower.rds"))

Q1_WTP_Upper <- data.frame(readRDS("Q6_SBDCModel_WTP_Upper.rds"))
Q2_WTP_Upper <- data.frame(readRDS("Q7_SBDCModel_WTP_Upper.rds"))

Q1_LV <- data.frame(readRDS("Q6ICLV_2022_03_21_ConWTP.rds"))
Q2_LV <- data.frame(readRDS("Q7ICLV_2022_03_21_ConWTP.rds"))

#-------------------------------
#### Merging: ####
#-------------------------------

FullSurvey2 <- cbind(
FullSurvey2,
"Q1WTP"=Q1_WTP$Q1WTP,
"Q2WTP"=Q2_WTP$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,

"Q1WTPLower"=Q1_WTP_Lower$Q1WTP,
"Q2WTPLower"=Q2_WTP_Lower$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,

"Q1WTPUpper"=Q1_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q6_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
"Q2WTPUpper"=Q2_WTP_Upper$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,

"Q1LV"=Q1_LV$post..mean,
"Q2LV"=Q2_LV$post..mean 
)


#-------------------------------
#### Density Plots of WTP: ####
#-------------------------------
WTP <- melt(data.frame(cbind(
  "Q1"=Q1_WTP$apply.FullSurvey2..1..function.i..c.krCI.Q6_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...,
  "Q2"=Q2_WTP$apply.FullSurvey2..1..function.i..c.krCI.Q7_SBDCModel..individual...data.frame.Q1Gender...FullSurvey2.Q1Gender.i...)))


Labels <- c("Q1\n Research Scenario", "Q2\n Treatment Scenario")
CVWTPDensity <-  ggplot(WTP,aes(x=value,y=variable,group=variable,fill=variable))+
  geom_density_ridges()+
  scale_x_continuous(name="WTP", limits=c(0,100),breaks = seq(0,100,10))+
  scale_y_discrete(name="CV Question",
                   label=Labels)+
  ggtitle("Distribution of fitted WTP.")+
  coord_cartesian(xlim = c(0,100),clip='off')+
  geom_vline(xintercept=0,linetype='dashed')+
  scale_fill_manual(name="CV Question",
                    values=c("red","blue"),
                    label=Labels,
                    guide=guide_legend(reverse = TRUE))+
  theme(legend.background=element_blank(),
        legend.box.background = element_rect(colour="black"))

ggsave(CVWTPDensity, device = "jpeg",
       filename = "CVWTPDensity.jpeg",
       width=20,height=15,units = "cm",dpi=500)



#-------------------------------
#### Boxplot of CV vs LV: ####
#-------------------------------




# Replicating Abate et al (2020) box and whiskers:
FullSurvey2$LVQuantilesQ1 <- (ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q1LV < quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
FullSurvey2$LVQuantilesQ2 <- (ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[1],1,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[2],2,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[3],3,ifelse(FullSurvey2$Q2LV < quantile(FullSurvey2$Q2LV, probs = c(0.10,0.25,0.50,0.75,0.90))[4],4,5)))))
# quantile(FullSurvey2$Q1LV, probs = c(0.10,0.25,0.50,0.75,0.90))


Q1Box <- ggplot(FullSurvey2, aes(LVQuantilesQ1, Q1WTP)) +   
  geom_boxplot(coef=5,aes(ymin=min(Q1WTPLower),lower=quantile(Q1WTP,0.25),middle=median(Q1WTP),upper=quantile(Q1WTP,0.75),ymax=max(Q1WTPUpper),group = LVQuantilesQ1),width = 0.1)+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,100),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Q1 WTP by percentile of latent variable estimated in Q1 Hybrid Choice Model.")+
  geom_text(x = 1, y =10+round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==1]),2)),color="red")+
  geom_text(x = 2, y =10+round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==2]),2)),color="red")+
  geom_text(x = 3, y =10+round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==3]),2)),color="red")+
  geom_text(x = 4, y =10+round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==4]),2)),color="red")+
  geom_text(x = 5, y =10+round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q1WTP[FullSurvey2$LVQuantilesQ1==5]),2)),color="red")



Q2Box <- ggplot(FullSurvey2, aes(LVQuantilesQ2, Q2WTP)) +   
  geom_boxplot(coef=5,aes(ymin=min(Q2WTPLower),lower=quantile(Q2WTP,0.25),middle=median(Q2WTP),upper=quantile(Q2WTP,0.75),ymax=max(Q2WTPUpper),group = LVQuantilesQ2),width = 0.1)+
  scale_y_continuous(name="WTP in annual £GBP per HH.",
                     breaks=waiver(),limits = c(0,100),
                     n.breaks = 10, labels = function(x) paste0("£",x))+
  scale_x_discrete(name="Percentiles of latent variable",labels = c("10%\n (N = 67)","25%\n (N = 101)","50%\n (N = 167)","75%\n (N = 167)","90%\n (N = 168)"), limits=c(1,2,3,4,5))+
  ggtitle("Q2 WTP by percentile of latent variable estimated in Q2 Hybrid Choice Model.")+
  geom_text(x = 1, y =10+round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==1]),2)),color="blue")+
  geom_text(x = 2, y =10+round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==2]),2)),color="blue")+
  geom_text(x = 3, y =10+round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==3]),2)),color="blue")+
  geom_text(x = 4, y =10+round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==4]),2)),color="blue")+
  geom_text(x = 5, y =10+round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2) , label = paste0("Mean:   £",round(mean(FullSurvey2$Q2WTP[FullSurvey2$LVQuantilesQ2==5]),2)),color="blue")



ggsave(grid.arrange(Q1Box,Q2Box), device = "jpeg",
       filename = "CVBoxplots_2022_03_23.jpeg",
       width=20,height=15,units = "cm",dpi=500)


write.csv(FullSurvey2,"SurveyData_2022_03_23.csv")