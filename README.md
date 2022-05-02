## Peter King PhD Thesis Replication Repo.


----

Tthis should be all the code and data necessary to replicate my doctoral thesis: *[Estimation of the Value of Precautionary Restrictions On Microplastics.](https://researchportal.bath.ac.uk/en/studentTheses/estimation-of-the-value-of-precautionary-restrictions-on-micropla)*


#### Disclaimer: This code has not been updated. I have included almost everything I wrote during my PhD so there will be old, unmaintained, and bad practice scripts here but they are provided for any insight. See [here](https://github.com/pmpk20?tab=repositories) for my more recent work.



----
### Replication Code:
  +  *Thesis_SetupCode.R*: 
      +  Transforms raw data *FullSurvey.csv* to *Test_Apollo.csv* which is necessary for estimation with Apollo.
  -  *Thesis_C4CEModels.R*: 
      -  Sequentially estimates just those choice models models in-text.
      -  Each model should have a separate script instead of compiling in one script like this. 
  -  *Thesis_C5CVModels.R*: 
      -  Sequentially reports all the contingent valuation models from the main-text.
  


----
### Replication Data:
- *KING_PETER_THESIS_191021.PDF*: The submitted copy of my thesis.
  - Note: Chapter 5 ICLV Estimated WTP used incorrect formula so the reported values for that subsubsection are outdated.
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
- *Thesis_Graphing.R*: All the plots in the thesis can be found here - alongside some no longer used.
   - Happy to take suggestions on better visualisation choices!

-  *Thesis_ApolloMXL.R*: A lengthy and horribly written specification search for the MXL. 
    - This should have been written in separate files with sensible filenames.

- *Thesis_CEICLV.R* and *Thesis_CVICLV.R*: Specification searches for the Integrated Choice-Latent Variables Models

- *Replication_PhD_LCM2Class.R*: a trimmed version of **Thesis_ApolloLCM.R** that reports latent class models.

- *Replication_PhD_CBA.py*: enables replicating the CBA tables. This is python because I liked the discounting in Numpy.

- *Thesis_Replication.R*: puts these into one file to estimate the key parts of the thesis.  

 
 ----
### In the **FilesForCVPaper** Folder:
- Scripts, model outputs, and estimated WTP for the two CV questions.
- These should be enough to replicate [this](https://researchportal.bath.ac.uk/en/publications/willingness-to-pay-for-precautionary-control-of-microplastics-a-c) working paper which is R&R at Marine Policy.
  
----
### These are sufficient to enable replication as of 02/05/2022. Any issues or comments let me know at: [p.m.king@kent.ac.uk](p.m.king@kent.ac.uk)

