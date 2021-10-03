# Peter King PhD Thesis Analysis and Replication Repo.


## Hi, this should be all the code necessary to replicate my doctoral thesis ``Estimation of the Value of Precautionary Restrictions On Microplastics``.


To replicate I have made standalone scripts for:
- The setup (Thesis_SetupCode.R) which imports the raw data from the survey company and transforms it enough to facilitate estimation with Apollo

- The replication file (Thesis_Replication.R) which should be just the estimation code in order it appears in-text (Note: Files beginning ``Replication_`` will be better versions of this).

- The graphs (Thesis_Graphing.R) - I made a huge amount for my thesis and many are no longer used but they're still there if you want to visualise the results further. Happy to take suggestions on better visualisation choices!

- Mixed Logit models (Thesis_ApolloMXL.R). These are the most important models but there's a huge amount of trial and error evident here. 

- Integrated Choice-Latent Variables Models; split into CE (Thesis_CEICLV.R) and CV (Thesis_CVICLV.R) sections. these take longer to estimate but form the basis for my second paper.

- Latent-Class Models (Thesis_ApolloLCM.R); all specification searches detailed here.

- Random-Regret Models (Thesis_RRM.R); for P3

- Finally, FullAnalysis.R is the compilation of all these but may be outdated and unreadable. 

These may be updated/appended later but for now that should enable replication. Any issues or comments let me know at: p.m.king@kent.ac.uk


03/10 To-Do:
- Bivariate probit ICLV
- Finish Stata and Python replication attempts
- Publish papers.
