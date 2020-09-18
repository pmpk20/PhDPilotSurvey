// Set github directory:
cd H:/PhDPilotSurvey


////////////////////////////////////////////////////////////////////////////////
// Random-Regret Minimisation Section
////////////////////////////////////////////////////////////////////////////////


// randregret instalation 
cap ado uninstall randregret
net install randregret, from("https://raw.githubusercontent.com/alvarogutyerrez/randregret/master/src/")


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


ssc install singleb
ssc install doubleb


import delimited "H:\PhDPilotSurvey\FinalData.csv", clear 

singleb q7bid q7treatmentresponse 
doubleb q7bid q7bid2  q7treatmentresponse q7response2

nlcom (WTP:(_b[_cons])), noheader

////////////////////////////////////////////////////////////////////////////////
// Choice Experiment Section
////////////////////////////////////////////////////////////////////////////////


import delimited "H:\PhDPilotSurvey\Full_Long.csv", clear 
cmset chid alt
split v1 ,parse(".")
generate CS = 0
replace  CS = 1 if  v12 =="B"
encode choice , generate (choice2)
label values choice2 .
generate choice3 = 1
replace  choice3 = 0 if  choice2 ==2
cmclogit choice3 price performance emission

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
