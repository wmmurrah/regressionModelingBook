/*************************************************************************************
	Description:

	Descriptive Stats for table A1

	Input:		
		
		base99
		base00
		base01
		
	Output: 
		tablea1.dta

***************************************************************************************/

clear
set more off
set matsize 800
set logtype text
global progname="tableA1"
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

/*can't do this using a bysort need to use a for loop over all the schools*/
set more off
foreach file in "base99" "base00" "base01" "base02" {
	use `file', clear
	cap rename semelmos school_id
	replace pair = 6 if school_id==670299 
	
	sort pair treated
	forvalues ipair = 1(1)20  {
		di `ipair'
		forvalues itreat = 0(1)1 {
			di `itreat'
			local pair_treat = `ipair'`itreat'
			di `pair_treat'
			foreach var in "pair" "treated" "semarab" "semrel" "zakaibag" {
				su `var' if pair==`ipair' & treated==`itreat'
				local m`file'`var'`ipair'`itreat' = r(mean)
				local n`file'`var'`ipair'`itreat' = r(N)
			}
		}
	}
}

capture postclose tablea1
postfile tablea1 pair treated  noncomplier arabschool religschool enroll99 enroll00 enroll01 enroll02 break bag99 bag00 bag01 bag02 using tablea1.dta, replace


set more off
forvalues ipair = 1(1)20  {
	di `ipair'
	forvalues itreat = 0(1)1 {
		di `itreat'
		local pair_treat = `ipair'`itreat'
		di `pair_treat'

		#delimit ;
		if !(`ipair'==6 &`itreat'==0) post tablea1 	(`ipair') 	(`itreat') 	(.) 	(`mbase99semarab`ipair'`itreat'') 	(`mbase99semrel`ipair'`itreat'') 	
					(`nbase99zakaibag`ipair'`itreat'') 	(`nbase00zakaibag`ipair'`itreat'') 	(`nbase01zakaibag`ipair'`itreat'') (`nbase02zakaibag`ipair'`itreat'') (.)
					(`mbase99zakaibag`ipair'`itreat'') 	(`mbase00zakaibag`ipair'`itreat'')  (`mbase01zakaibag`ipair'`itreat'') (`mbase02zakaibag`ipair'`itreat'') ;
		#delimit cr
	}
}


postclose tablea1

use tablea1, clear
browse

clear
erase tablea1.dta


log close
