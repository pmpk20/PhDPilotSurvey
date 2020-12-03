# PhDPilotSurvey
PhD full survey analysis:


Hi, this is my repo for all the code necessary to replicate my thesis ``Economic valuation of benefits from the proposed REACH restriction of intentionally-added microplastics''. 

To replicate I have made standalone scripts for:
- The setup (Thesis_SetupCode.R) which imports the raw data from the survey company and transforms it enough to facilitate estimation with Apollo

- The graphs (Thesis_Graphing.R) - I made a huge amount for my thesis and many are no longer used but they're still there if you want to visualise the results further. Happy to take suggestions on better visualisation choices!

- Mixed Logit models (Thesis_ApolloMXL.R). These are the most important models but there's a huge amount of trial and error evident here. They don't take too long to estimate at least. My first MXL models were using MLOGIT so there may be some functionality from that I need to translate to Apollo.

- Integrated Choice-Latent Variables Models; split into CE (Thesis_CEICLV.R) and CV (Thesis_CVICLV.R) sections. these take longer to estimate but form the basis for my second paper.

- Latent-Class Models (Thesis_ApolloLCM.R); all specification searches detailed here.

- Finally, FullAnalysis.R is the compilation of all these but may be outdated and unreadable. 

These may be updated/appended later but for now that should enable replication. Any issues or comments let me know at: p.m.king@bath.ac.uk


03/12 To-Do:
- Bivariate probit ICLV
- Finish Stata and Python replication attempts