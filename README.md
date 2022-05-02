## Peter King PhD Thesis Replication Repo.


----
Tthis should be all the code necessary to replicate my doctoral thesis: *[Estimation of the Value of Precautionary Restrictions On Microplastics.](https://researchportal.bath.ac.uk/en/studentTheses/estimation-of-the-value-of-precautionary-restrictions-on-micropla)*

#### Disclaimer: This code has not been updated since 2021 and, looking back, is not the prettiest ever written so code with caution.

----
### How to replicate:
  +  *Thesis_SetupCode.R*: 
      +  Transforms raw data *FullSurvey.csv* to *Test_Apollo.csv* which is necessary for estimation with Apollo.
  -  *Thesis_C4CEModels.R*: 
      -  Sequentially estimates just those choice models models in-text.
      -  Each model should have a separate script instead of compiling in one script like this. 
  -  *Thesis_C5CVModels.R*: 
      -  Sequentially reports all the contingent valuation models from the main-text.
  


----
### Other Files:
- *KING_PETER_THESIS_191021.PDF*: The submitted copy of my thesis.
  - Note: Chapter 5 ICLV Estimated WTP uses incorrect formula so do not use the figures.
- *FinalData.csv*: 
    - This is all data I have per respondent per choice.
- *FullSurvey.csv*:
    - Data from the survey company in one row per respondent. 
- *FullSurvey2.csv*:
    - Data from the survey but with fitted-WTP and more added per respondent.  
- *Test_Apollo.csv*:
    - Data for estimating choice models. Has one row per respondents choice (N = 670 * 4 choices = 5360 rows)
    - Previously I used a truncated version of this but I've dropped this following my Viva.  

----
### In the **OldScripts** Folder:
- Disclaimer: These are seriously old, unmaintained, and follow bad practice. They are provided for any insight but not to be relied upon. 
- *Thesis_Graphing.R*: All the plots in the thesis can be found here - alongside some no longer used.
   - Happy to take suggestions on better visualisation choices!

-  *Thesis_ApolloMXL.R*: A lengthy and horribly written specification search for the MXL. 
    - This should have been written in separate files with sensible filenames.

- *Thesis_CEICLV.R* and *Thesis_CVICLV.R*: Specification searches for the Integrated Choice-Latent Variables Models

- *Replication_PhD_LCM2Class.R*: a trimmed version of **Thesis_ApolloLCM.R** that reports latent class models.

- *Replication_PhD_CBA.py*: enables replicating the CBA tables. 

- *Thesis_Replication.R*: puts these into one file to estimate the key parts of the thesis.  

  
  
----
### These are sufficient to enable replication as of 02/05/2022. Any issues or comments let me know at: [p.m.king@kent.ac.uk](p.m.king@kent.ac.uk)

