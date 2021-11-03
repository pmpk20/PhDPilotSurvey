## Peter King PhD Thesis Replication Repo.



Hi, this should be all the code necessary to replicate my doctoral thesis ``Estimation of the Value of Precautionary Restrictions On Microplastics``.


Files included are:
- `Thesis_SetupCode.R`:  
  - This imports the raw data (**) from the survey company and transforms it to (*Test_Apollo.csv*) for estimation with Apollo.

- Files beginning with *Replication* are streamlined code to replicate specific sections of the thesis.

- `Thesis_Graphing.R`:  
  - All the plots in the thesis can be found here - alongside some no longer used.
  - Happy to take suggestions on better visualisation choices!

- `Thesis_ApolloMXL.R`:  
  - All the Mixed Logit models I estimated are here.
  - This is the specification search so there's a huge amount of trial and error evident here. 

- `Thesis_CEICLV.R` and `Thesis_CVICLV.R`:  
  - Integrated Choice-Latent Variables Models; split into CE and CV sections. 
  - These take longer to estimate but form the basis for my second paper: https://researchportal.bath.ac.uk/en/publications/willingness-to-pay-for-precautionary-control-of-microplastics-a-c

- `Thesis_ApolloLCM.R`:  
  - Specification search for the Latent Class Models.

- Finally, `FullAnalysis.R` is the compilation of all these but may be outdated and unreadable. 

These may be updated/appended later but for now that should enable replication. Any issues or comments let me know at: p.m.king@kent.ac.uk


03/10 To-Do:
- Bivariate probit ICLV
- Finish Stata and Python replication attempts
- Publish papers.
