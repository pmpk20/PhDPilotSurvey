## Peter King PhD Thesis Replication Repo.


----
Hi, this should be all the code necessary to replicate my doctoral thesis: *[Estimation of the Value of Precautionary Restrictions On Microplastics.](https://researchportal.bath.ac.uk/en/studentTheses/estimation-of-the-value-of-precautionary-restrictions-on-micropla)*

----
### How to replicate:
  -  `Thesis_SetupCode.R`: transforms raw data *FinalData.csv* to *Test_Apollo.csv* which is necessary for estimation with Apollo.
  -  `Replication_PhD_C4CEModels.R`: estimates just those choice models models in-text. `Thesis_ApolloMXL.R` performs the specification search for the MXL.
  -  `Replication_PhD_LCM2Class.R`: a trimmed version of `Thesis_ApolloLCM.R` that reports latent class models.
  -  `Replication_PhD_C5CVModels.R`: reports all the contingent valuation models.
  -  `Replication_PhD_CBA.py`: enables replicating the CBA tables. 
  -  `Thesis_Replication.R`: puts these into one file to estimate the key parts of the thesis.  

----
### Other files:
- `Thesis_Graphing.R`:  
  - All the plots in the thesis can be found here - alongside some no longer used.
  - Happy to take suggestions on better visualisation choices!

- `Thesis_CEICLV.R` and `Thesis_CVICLV.R`:  
  - Integrated Choice-Latent Variables Models; split into CE and CV sections. 
  - These take longer to estimate but form the basis for my second paper: [Link](https://researchportal.bath.ac.uk/en/publications/willingness-to-pay-for-precautionary-control-of-microplastics-a-c)

- Finally, `FullAnalysis.R` is the compilation of all these but may be outdated and unreadable.   
  
  
----
### These are sufficient to enable replication as of 19/10/2021. Any issues or comments let me know at: [p.m.king@kent.ac.uk](p.m.king@kent.ac.uk)

