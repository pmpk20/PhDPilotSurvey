#### Peter King Doctoral Thesis: SETUP  ###############
# Project author: Peter King (p.m.king@bath.ac.uk)
# Project title: Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics.
# Function: excludes respondents
## Last change: 02/05/2022


#---------------------------------------------------------------------------------------------------------
#### Section 0: Replication Information ####
## Putting sessionInfo() here in case it helps
#----------------------------------------------------------------------------------------------------------


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



#----------------------------------------------------------------------------------------------------------
#### Section 1: PACKAGE SETUP ####
## Installs packages and imports data.
## Split into optional and libraries
#----------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------------------------

## Back in 2020 I used to write rm() and setwd() for each script
### I keep these in for completeness BUT they are bad practice:
### See: https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

rm(list=ls()) ## Clear workspace
# setwd("H:/PhDPilotSurvey") ## Sets working directory. This is where my Github repo is cloned to.
pkgbuild::has_build_tools() ## Used to have problems installing RTOOls on my laptop.
options(scipen=50) # Makes presentation of tables nicer; changes from scientific notation


#----------------------------------------------------------------------------------------------------------

library(Hmisc) ## For imputing missing income
library(xtable) ## To export to LaTeX
library(dplyr) ## To manipulate data
library(ggplot2) ## For plotting
library(gridExtra) ## Adding plots together
library(mfx) ## Estimates marginal effects
library(apollo) ## For all CE models
library(DCchoice) ## For all CVM models
## Note: DCchoice needs Icens which can only be installed with:
## BiocManager::install("Icens")
## You may have to run install.packages("BiocManager") first to do this



#----------------------------------------------------------------------------------------------------------
# Section 1: Importing Data ####
#----------------------------------------------------------------------------------------------------------


## Imports from the excel file straight from the survey companies website.
Full_Long <- data.frame(read.csv("Full_Long.csv")) 
Full_Final <- data.frame(read.csv("FinalData.csv")) 

#----------------------------------------------------------------------------------------------------------
# Section 2: Identifying those to exclude ####
#----------------------------------------------------------------------------------------------------------



# ## These are respondent IDs obtained by visual inspection of text responses
ProtestVotes <- c(14,24,33,39,44,61,79,106,121,130,149,163,182,200,203,211,214,215,217,
                  239,244,249,251,252,267,282,290,306,320,326,327,341,343,362,363,364,
                  371,374,380,393,399,407,414,425,426,464,467,477,479,480,519,524,536,
                  545,547,557,567,579,590,591,595,614,629,639,649,651,654,665,674,680,
                  915,931,932,933,935,940,950,953,959,960,975,978,989,996,1002,1011,1024,1026,1027,1028)

## For The Record these are the IDs of protestors:
#   [1]   1   3   4   6   7  11  13  14  15  17  18  20  21  22  24  25  28  29  30  32  33  36
# [23]  37  38  39  40  41  42  44  46  49  51  52  55  56  59  60  61  62  64  65  66  67  71
# [45]  72  75  77  79  80  83  84  85  87  91  92  93  94  96  97  98 101 106 107 108 109 110
# [67] 111 114 119 121 123 126 128 129 130 132 134 135 139 142 143 144 147 149 150 151 154 156
# [89] 158 159 160 162 163 164 170 173 174 177 179 181 182 183 184 185 187 188 190 191 192 193
# [111] 197 200 201 203 205 208 209 211 214 215 216 217 220 222 227 228 229 230 232 233 234 237
# [133] 239 240 241 243 244 245 247 248 249 250 251 252 257 258 259 261 262 264 265 266 267 272
# [155] 274 277 279 281 282 285 286 287 290 291 292 300 301 302 303 304 306 310 314 318 319 320
# [177] 321 322 323 326 327 328 329 330 332 333 336 338 341 342 343 345 346 347 349 350 351 352
# [199] 353 354 358 362 363 364 366 371 373 374 378 380 382 383 387 389 390 393 394 396 397 399
# [221] 400 401 403 406 407 411 414 415 416 418 420 422 425 426 427 428 429 430 434 437 440 441
# [243] 442 443 444 446 447 449 452 453 455 456 457 459 460 462 463 464 466 467 469 470 472 474
# [265] 477 479 480 481 485 487 488 490 491 494 495 496 497 498 499 501 502 504 508 509 510 513
# [287] 514 516 519 520 522 524 525 526 528 529 531 532 533 535 536 537 539 541 542 544 545 546
# [309] 547 548 550 551 552 557 558 559 560 565 566 567 569 570 572 574 576 577 578 579 580 585
# [331] 587 588 590 591 593 595 598 601 603 605 608 611 612 614 615 624 625 626 629 634 636 639
# [353] 643 647 648 649 651 652 654 655 656 657 659 664 665 666


#----------------------------------------------------------------------------------------------------------
# Section 3: Exclude Respondents ####
#----------------------------------------------------------------------------------------------------------



### Truncation Rule Two:
AllCriteria <- data.frame("IDs" = unique(Full_Long$ID[ (Full_Long$Q25Understanding >=7) &
                                                         (Full_Long$Q8DominatedTest == 0) &
                                                         (Full_Long$Q12CECertainty >= 1) &
                                                         (Full_Long$Q20Consequentiality >= 1) ])) 


## Here checking if any of the remaining respondents were on the protest list: 
AllCriteria <- AllCriteria[ !(AllCriteria$IDs %in% c(ProtestVotes)),]
Full_Full <- Full_Final[ (Full_Final$ID) %in% c(AllCriteria),] ## Fully truncated:
# Full_Full <- Full_Long[ (Full_Long$ID) %in% c(AllCriteria),] ## Can truncate the Full_Long dataframe too if needing to estimate in MLOGIT
Full_Excluded <- Full_Final[ !(Full_Final$ID) %in% c(AllCriteria),] ## The excluded responses
nrow(Full_Full)/8



#----------------------------------------------------------------------------------------------------------
# Section 4: Export Truncated Data ####
#----------------------------------------------------------------------------------------------------------


## Export the data with 304 respondents (NOT the 670)
### Yeah sorry about this variable name, I promise I'm better now
write.csv(Full_Full,"Full_Full.csv")


# End Of Script ------------------------------------------------------------------------------------------------------

