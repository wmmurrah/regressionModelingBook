/*********************************************************************
		
	Description:

	This program creates table 3 of the bonuses project in Stata. 

	Input:		
		base01
	Output: 
		table3.dta

**********************************************************************/


clear
set more off
set matsize 800
set logtype text
global name table3
local today="$S_DATE"
local day=word("`today'",1)
local month=word("`today'",2)
local year=substr(word("`today'",3),3,2)
if `day'<10 local day="0`day'" 
global date="`day'""`month'""`year'"


* EDIT THIS CD STATEMENT IN ORDER TO LOCATE THE LOGS IN YOUR LOCAL DIRECTORY
cd "W:\Student bonuses\Final Code Tables and Data Folder May 2008\logs"

cap log close
log using "${name}.log", replace

* EDIT THIS CD STATEMENT IN ORDER TO LOCATE THE DATA IN YOUR LOCAL DIRECTORY
cd "W:\Student bonuses\Final Code Tables and Data Folder May 2008\data"

************** PROGRAMS *****************
use base01, clear
gen all = 1
gen girl = !boy
local demographics1 = "semarab semrel educav ah4 ole5 ls50 ls75 ls100"
local demographics2 = "semarab semrel educav educem ah4 ole5 ls50 ls75 ls100"

foreach gender in "all" "boy" "girl" {
	
	use base01, clear
	
	cap rename semelmos school_id
	
	if "`gender'" == "all"  keep if 1
	if "`gender'" == "boy"  keep if boy
	if "`gender'" == "girl" keep if ~boy
	keep if treated == 0
	gen ah4 = 1 if m_ahim >=4
	replace ah4 = 0 if m_ahim < 4 & m_ahim~=.
	
	/*Make quartiles from lag score*/
	*** Lagged Score quartiling - an algorithm that conforms with the SAS algorithm

	gen p25 = 0	
	gen p50 = 0	
	gen p75 = 0	
	gen p100= 0	
	
	
	gen ls25 = 0 
	gen ls50 = 0 
	gen ls75 = 0 
	gen ls100= 0 
	
	*** Lagged Score quartiling
	sum lagscore , detail 
	replace ls25  = 1 if lagscore<r(p25) 
	replace ls50  = 1 if ls25==0 & lagscore<r(p50) 
	replace ls75  = 1 if ls50+ls25==0 & lagscore<r(p75) 
	replace ls100 = 1 if ls50+ls25+ls75==0 
	
	
	*** Note that the here the top/bottom are quartiles, not halves
	gen top_ls=(ls75+ ls100==1) 
	gen bot_ls=(ls25+ ls50 ==1) 
	
	su zakaibag 
	local mean_`gender'=r(mean)
	logit zakaibag `demographics1'   , cluster (school_id)
	local nobs_`gender'=e(N)
	local no_clust_`gender'=e(N_clust)
	foreach var in `demographics1' {
		local coeff1_`var'_`gender' = _b[`var']
		di "COEFFICIENT 1 `coeff1_`var'_`gender''"
		di "coeff1_`var'_`gender'"
	}
	brl zakaibag `demographics1'  , logit cluster (school_id)
	di "*********"
	di "brl zakaibag `demographics1'  , logit cluster (school_id)"
	di "*********"
	foreach var in `demographics1' {
		local se1_`var'_`gender'=_se[`var']
		di "STANDARD ERROR 1 `se1_`var'_`gender''"
		di "se1_`var'_`gender'"
	}
	
	su zakaibag 
	local mean_`gender'=r(mean)
	logit zakaibag `demographics2'  , cluster (school_id)
	local nobs_`gender'=e(N)
	local no_clust_`gender'=e(N_clust)
	foreach var in `demographics2'  {
		local coeff2_`var'_`gender' = _b[`var']
		di "COEFFICIENT 2 `coeff2_`var'_`gender''"
		di "coeff2_`var'_`gender'"
	}
	brl zakaibag `demographics2'  , logit cluster (school_id)
	foreach var in `demographics2'  {
		local se2_`var'_`gender'=_se[`var']
		di "STANDARD ERROR 2 `se2_`var'_`gender''"
		di "se2_`var'_`gender'"
	}
}


*********** END OF PROGRAMS *************
/*loop over all, boys, girls, and then over the two specifications, and then over the covariates*/

/*locals for the missing values*/
local coeff1_educem_all = .
local coeff1_educem_boy = .
local coeff1_educem_girl = .

local se1_educem_all = .
local se1_educem_boy = .
local se1_educem_girl = .

postfile table3 	str20 stat col1 	col2 space1 	col3 	col4 	space2 col5 	col6 using table3.dta, replace
post table3 	("Dep mean") 	(`mean_all') 	(.) (.)	(`mean_boy') 	(.) (.)	(`mean_girl') 	(.)
post table3 ("") (.) (.) (.) (.) (.) (.) (.) (.) 
foreach var in `demographics2' {
post table3 ("`var'")  (`coeff1_`var'_all') (`coeff2_`var'_all') (.) (`coeff1_`var'_boy') (`coeff2_`var'_boy') (.) (`coeff1_`var'_girl') (`coeff2_`var'_girl')  
post table3 ("`var'") (`se1_`var'_all')    (`se2_`var'_all')  (.)  (`se1_`var'_boy')    (`se2_`var'_boy')    (.) (`se1_`var'_girl')    (`se2_`var'_girl')  
post table3 ("") (.) (.) (.) (.) (.) (.) (.) (.) 
}
post table3 	("Observations") 		(`nobs_all') 	(.) (.)	(`nobs_boy') 	(.) (.)	(`nobs_girl') 	(.)
post table3 	("Number of Schools") 	(`no_clust_all') 	(.) (.)	(`no_clust_boy') 	(.) (.)	(`no_clust_girl') 	(.)

postclose table3

use  table3, clear
browse
clear
cap erase table3.dta

log close
