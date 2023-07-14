/*********************************************************************
	Description:

	This program does balancing tests for 
	bagrut rate, arab school, religious school, father and mother education
	number of siblings immigrant lagged score
	
	Input:		
		base01
	Output: 
		table3.dta

**********************************************************************/

clear
set more off
set matsize 800
set logtype text
global progname="tableA2"
local today="$S_DATE"
local day=word("`today'",1)
local month=word("`today'",2)
local year=substr(word("`today'",3),3,2)
if `day'<10 local day="0`day'" 
global date="`day'""`month'""`year'"


* EDIT THIS CD STATEMENT IN ORDER TO LOCATE THE LOGS IN YOUR LOCAL DIRECTORY
cd "W:\Student bonuses\Final Code Tables and Data Folder May 2008\logs"
cap log close
log using ${progname}.log, replace

* EDIT THIS CD STATEMENT IN ORDER TO LOCATE THE DATA IN YOUR LOCAL DIRECTORY
cd "W:\Student bonuses\Final Code Tables and Data Folder May 2008\data"

************** PROGRAMS *****************

use base01, clear
cap rename semelmos school_id
gen all = 1
gen girl = !boy
local varlist = "zakaibag semarab semrel educav educem m_ahim ole5 lagscore"

foreach gender in "all" "boy" "girl" {
	foreach file in 01 00  {
		use base`file', clear
		cap rename semelmos school_id
		if "`gender'" == "all"  keep if 1
		if "`gender'" == "boy"  keep if boy
		if "`gender'" == "girl" keep if ~boy
		foreach var in `varlist' {
			di "************************************************************"
			di ""
			di "For gender `gender' Year `file' Variable `var'"
			di ""
			di "************************************************************"
			su `var'
			local mean_`var'_`gender'_`file' = r(mean)
			local se_`var'_`gender'_`file' = r(sd)
			reg `var' treated, cluster(school_id) 
			mat b = e(b)
			mat se = e(V)
			local beta_`var'_`gender'_`file' = b[1,1]
			local std_`var'_`gender'_`file' = sqrt(se[1,1])
			local N_`var'_`gender'_`file' = e(N)
			di "beta_`var'_`gender'_`file' `beta_`var'_`gender'_`file'' std_`var'_`gender'_`file' `std_`var'_`gender'_`file''"
			di "Mean mean_`var'_`gender'_`file' `mean_`var'_`gender'_`file'' Mean se_`var'_`gender'_`file' `se_`var'_`gender'_`file''"
			di "N `N_`var'_`gender'_`file''"
		}
		count
		local N_`gender'_`file' = r(N)
		di "N_`gender'_`file' `N_`gender'_`file''"
	}
}

postfile balancing 	str20 variable year 	all_mean all_diff 	boy_mean 	boy_diff 	girl_mean girl_diff using balancing.dta, replace
post balancing ("") (.) (.) (.) (.) (.) (.) (.)  
foreach file in 01 00  {
	foreach var in `varlist' {
		di "************************************************************"
		di ""
		di "For gender `gender' Year `file' Variable `var'"
		di ""
		di "************************************************************"
		su `var'
		local mean_`var'_`gender'_`file' = r(mean)
		post balancing ("`var'") (`file') (`mean_`var'_all_`file'') (`beta_`var'_all_`file'') (`mean_`var'_boy_`file'') (`beta_`var'_boy_`file'') ///
								 (`mean_`var'_girl_`file'') (`beta_`var'_girl_`file'')   
		post balancing ("`var'") (`file') (`se_`var'_all_`file'') (`std_`var'_all_`file'') (`se_`var'_boy_`file'') (`std_`var'_boy_`file'') ///
								 (`se_`var'_girl_`file'') (`std_`var'_girl_`file'')  
		post balancing ("") (.) (.) (.) (.) (.) (.) (.) 
	}
post balancing ("N") (`file') (`N_all_`file'') (.) (`N_boy_`file'') (.) (`N_girl_`file'') (.) 	
post balancing ("") (.) (.) (.) (.) (.) (.) (.) 
}
postclose balancing

use  balancing, clear
browse

cap erase balancing.dta

log close
