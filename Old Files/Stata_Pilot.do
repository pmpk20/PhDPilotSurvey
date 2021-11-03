// Set github directory:
cd H:/PhDPilotSurvey
ssc install wtp
ssc install singleb
ssc install doubleb
ssc install mixlogitwtp


// randregret instalation 
cap ado uninstall randregret
net install randregret, from("https://raw.githubusercontent.com/alvarogutyerrez/randregret/master/src/")


////////////////////////////////////////////////////////////////////////////////
// Choice Experiment Section
////////////////////////////////////////////////////////////////////////////////


//// Setup ////
import delimited "H:\PhDPilotSurvey\Full_Long.csv", clear 
cmset chid alt
generate CS = 0
replace  CS = 1 if  idxalt =="B"
encode choice , generate (choice2)
label values choice2 .
generate choice3 = 1
replace  choice3 = 0 if  choice2 ==2
gen nprice = -price
gen nperformance = -performance
gen nemission = -emission

//// Truncation ////
keep if q25understanding >=7
keep if q8dominatedtest ==0 
keep if q12cecertainty  >=1
keep if q20consequentiality >=1
egen Protestors = anymatch(id), values(24,33,44,61,121,127,182,200,211,219,239,251,275,306,320,326,341,360,363,371,399,464,467,479,480,506,579,591,649,654,931,932,935,953,989,1002,1011,1024,14,35,39,54,79,106,130,146,149,155,163,203,214,215,217,244,246,249,252,267,268,282,290,327,343,362,364,374,380,393,398,407,414,425,426,433,477,519,524,536,543,545,547,557,567,575,589,590,595,614,617,629,637,638,639,651,665,674,680,915,933,940,950,959,960,975,978,996,1026,1027,1028)
drop if Protestors


//// Modelling ////


// Conditional Logit:
cmclogit choice3 price performance emission

// Multinomial Logit:
cmclogit choice3 price performance emission, casevars(q1gender q2age q3distance q4trips q16bp q18charity q20consequentiality q25understanding q24aincome q23employment q22education q21experts q12cecertainty order task) basealternative(B)

// Mixed Logit Attributes Only:
cmmixlogit choice3, random(price, lnormal) random(performance, lnormal) random(nemission,lnormal) basealternative(A) intmethod(halton) technique(bfgs) 

// Mixed Logit SDs:
cmmixlogit choice3, random(price, lnormal) random(performance, lnormal) random(nemission,lnormal) casevars(q1gender q2age q3distance q4trips q16bp q18charity q20consequentiality q25understanding q24aincome q23employment q22education q21experts q12cecertainty order task) basealternative(A) intmethod(halton) technique(bfgs) 

wtp price performance nemission
// vce(bootstrap, seed(123) dots(1))


//// Alternative MXL approach:
mixlogitwtp choice3 , group(idxchid ) id(v1 ) price(price ) rand(performance nemission ) ln(2) nrep(1000)


//// Integrated Model which don't currently work:
gsem (SD-> Utility, ) ///
(q1gender q2age q3distance q12cecertainty q24aincome q23employment  q21experts q20consequential q16bp q18charity  -> SD, family(m) link(logit)) ///
(Utility -> choice3, family(m) link(logit)) ///
 (EnvAttitude -> Utility, ) ///
 (EnvAttitude -> q13-q15, family(ordinal) link(logit)) ///
 , latent(Utility EnvAttitude SD ) nocapslatent


////////////////////////////////////////////////////////////////////////////////
// Random-Regret Minimisation Section
////////////////////////////////////////////////////////////////////////////////



// Data setup:
import delimited "H:\PhDPilotSurvey\Full.csv", clear 
keep obs id task price_a price_b performance_a performance_b  emission_a emission_b  choice
rename (choice)  (choice_w)
generate choice = 0
replace  choice = 1 if  choice_w=="B"
drop choice_w
reshape long price performance emission , i(obs) j(Alt) string
generate alt = 1
replace  alt = 0 if  Alt=="_a"
drop Alt
rename (choice)  (choice_w)
generate choice = 0
replace  choice = 1 if  choice_w==alt


// Different Regret Models:
// Classic RRM+ cluster(id)
randregret choice  price performance emission, gr(obs) alt(alt) rrmfn(classic)  cluster(id)	nocons
// muRRM + cluster(id) does not converge yet
randregret choice  price performance emission, gr(obs) alt(alt) rrmfn(mu) cluster(id) show  	nocons
// Generalized RRM + cluster(id)
randregret choice  price performance emission, gr(obs) alt(alt) rrmfn(gene) cluster(id) show nocons 
// Pure RRM + cluster(id)
randregret choice  , neg(price performance emission) gr(obs) alt(alt) rrmfn(pure) cluster(id) nocons   


////////////////////////////////////////////////////////////////////////////////
// Contingent Valuation Section
////////////////////////////////////////////////////////////////////////////////


import delimited "H:\PhDPilotSurvey\FinalData.csv", clear 

//// Q6 SB Bid-Only:
singleb q6bid q6researchresponse	 

//// Q6 SB Covariates:
singleb q6bid q6researchresponse	 

//// Q7 SB Bid-Only:
singleb q7bid q7treatmentresponse 

//// Q7 DB Bid-Only:
doubleb q7bid q7bid2  q7treatmentresponse q7response2

//// WTP:
nlcom (WTP:(_b[_cons])), noheader


