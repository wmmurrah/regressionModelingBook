

/********************************************************************

Name:   070807 - table 5 - Stacked estimates in covariate subgroups - non delta.do
Data:   base99/00/01/02 

NON-DELTA-METHOD
This program creates table 5 - Stacked estimates in covariate subgroups
    * This is a first Stata replication of the SAS programs.

aim:

To recreate the 4 coefficients from table 5, into a separate table, 
table a4, which reports all the coefficients. 

We want it just for the following:

by lagged Score
for boys and girls
for stacked 2000 and 2001
for the top
for sch covs and group linear control

**********************************************************************/

clear
set more off
set matsize 800
set logtype text
global progname="tableA4"
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

*** Looping over cohorts to create the subgroups

foreach y in 00 01 02  {

	foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {

  	use "base`y'.dta" , clear
  	cap rename semelmos school_id
  	gen year`y'=1

 	*sample 20
    drop if (1-boy)==`gend'
    gen ah4=m_ahim>=4
 
  *** Lagged Score quartiling - an algorithm that conforms with the SAS algorithm

    gen p25=0	
    gen p50=0
    gen p75=0
    gen p100=0


   gen ls25=0	
    gen ls50=0
    gen ls75=0
    gen ls100=0

	*** Lagged Score quartiling
    sum lagscore, detail
    replace ls25 =1 if lagscore<r(p25)
    replace ls50 =1 if ls25==0 & lagscore<r(p50)
    replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
    replace ls100=1 if ls50+ls25+ls75==0


    *** Note that the here the top/bottom are quartiles, not halves
    gen top_ls=(ls75+ls100==1)
    gen bot_ls=(ls25+ls50 ==1)

    *** Predicted Probability quartiling
    logit zakaibag educav educem ah4 ole5 semarab semrel ls50 ls75 ls100
    predict p
    
    sum p, detail
    replace p25 =1 if p<r(p25)
    replace p50 =1 if p25==0 & p<r(p50)
    replace p75 =1 if p50+p25==0 & p<r(p75)
    replace p100=1 if p50+p25+p75==0


    *** Note that the here the top/bottom are halves
    gen top_p=(p75+p100==1)
    gen bot_p=(p25+p50 ==1)

    gen ls=lagscore
    /* this is just to have the linear effect simply inserted 
       within the loop. see - else local control==... */

   	*** Keeping the halfs in temporary files

   preserve
	keep if top_ls==1
	save _y`y'_gend`gend'_ls_top, replace
	restore 


   


	} /* close gender loop*/
} /* close year loop*/

clear

 /*****************   REGRESSIONS  **********************************/

foreach stack in s01 s12  {
  foreach gend of numlist 0 1  {
    
        di "*******************************"
        di "*******************************"
        di "*******************************"
        di "Stack is `stack'"
        di "Gender is `gend'"
        di "Subgroups are by `halfby'"
        di "Subgroup is `half'"
        di "*******************************"
        di "*******************************"
        di "*******************************"

        use _y01_gend`gend'_ls_top, clear

        if "`stack'"=="s01" | "`stack'"=="s012" ///
        append using _y00_gend`gend'_ls_top
        if "`stack'"=="s01" | "`stack'"=="s012" ///
        di _N " observations are added from year 2000"


        if "`stack'"=="s12" | "`stack'"=="s012" ///
        append using _y02_gend`gend'_ls_top
        if "`stack'"=="s12" | "`stack'"=="s012" ///
        di _N " observations are added from year 2002"



       *** interacting treatment and school-type with year
        *** the cap is for the stacks where a certain year is missing
        cap replace year00=(year00==1)
        cap replace year01=(year01==1)
       cap replace year02=(year02==1)
       cap replace treated=treated*year01	
       cap gen semrel01 =semrel*year01
       cap gen semrel02 =semrel*year02
       cap gen semarab01=semarab*year01
    	cap gen semarab02=semarab*year02

        *** Making sure that each schools appears in all 2 or 3 years
        *** If a year is not participating, then its x`year' should not be generated
        sort school_id
        cap egen x0=max(year00), by (school_id)
        cap egen x1=max(year01), by (school_id)
        cap egen x2=max(year02), by (school_id)

       cap noisily keep if x0==1
        cap noisily keep if x1==1
        cap noisily keep if x2==1

        cap drop x0

       cap drop x1
        cap drop x2

        *** Implementing step-5 
        egen zak_max=max(zakaibag), by (school_id)
        egen zak_min=min(zakaibag), by (school_id)
        keep if zak_max==1 & zak_min==0
        drop zak_max zak_min

        ** keeping the number of observations and the means 

       local n_`stack'_ls_top_gend`gend'=_N

       sum zakaibag
        local mean_`stack'_ls_top_gend`gend'=r(mean)

        *** Running the regressions
        
        	local halfbycont="ls"
          	di "**********Regression is:"
			di "logit zakaibag treated semarab0* semrel0* `halfbycont' year0* i.school_id, robust"
			di "***"
			di "The stack used is"
			di "`stack'"
						
			* note we use a year control here hard coded to year 2001, which means for the 2000/2002 stack the results will be incorrect
			* but we dont report those results here so thats fine
			xi: logit zakaibag treated semarab01 semrel01 ls year01 i.school_id, robust
        	
			foreach control in "treated" "semarab01" "semrel01" "ls" "year01" {
				cap local est_`stack'_gend`gend'_`control'=_b[`control']
	        	cap local se_`stack'_gend`gend'_`control'=_se[`control']
	        	di "the estimate of `control' is " 
	        	di "est_`stack'_gend`gend'_`control'" 
	        	di "`est_`stack'_gend`gend'_`control''"
		       	di "the se of `control' is" 
		       	di "se_`stack'_gend`gend'_`control'" 
		       	di "`se_`stack'_gend`gend'_`control''"
		    }
         	        	
	} /* close gender loop */

} /* close stack loop */


macro list

/**********************************************************************
   CREATING THE TABLE
**********************************************************************/
postfile stack_est str14(ctrl stat) ///
  b_top_ls01 space1 g_top_ls01 ///
  space2 ///
  b_top_ls12 space3 g_top_ls12 space4 ///
  using ${date}_ta4_logit_coeffs, replace

post stack_est ("Dep Mean") ("") ///
    (`mean_s01_ls_top_gend1') (.) ///
    (`mean_s01_ls_top_gend0') (.) ///  
	(`mean_s12_ls_top_gend1') (.) ///
    (`mean_s12_ls_top_gend0') (.)   

post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)         

post stack_est ("treated") ("coeff") ///
(`est_s01_gend1_treated') (.) ///
(`est_s01_gend0_treated') (.) ///  
(`est_s12_gend1_treated') (.) ///
(`est_s12_gend0_treated') (.)                 
           
post stack_est ("treated") ("se") ///
(`se_s01_gend1_treated') (.) ///
(`se_s01_gend0_treated') (.) ///  
(`se_s12_gend1_treated') (.) ///
(`se_s12_gend0_treated') (.)                 


post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)   

post stack_est ("semarab") ("coeff") ///
(`est_s01_gend1_semarab01') (.) ///
(`est_s01_gend0_semarab01') (.) ///  
(`est_s12_gend1_semarab01') (.) ///
(`est_s12_gend0_semarab01') (.)                 
           
post stack_est ("semarab") ("se") ///
(`se_s01_gend1_semarab01') (.) ///
(`se_s01_gend0_semarab01') (.) ///  
(`se_s12_gend1_semarab01') (.) ///
(`se_s12_gend0_semarab01') (.)              

post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)   

post stack_est ("semrel01") ("coeff") ///
(`est_s01_gend1_semrel01') (.) ///
(`est_s01_gend0_semrel01') (.) ///  
(`est_s12_gend1_semrel01') (.) ///
(`est_s12_gend0_semrel01') (.)                 
           
post stack_est ("semrel01") ("se") ///
(`se_s01_gend1_semrel01') (.) ///
(`se_s01_gend0_semrel01') (.) ///  
(`se_s12_gend1_semrel01') (.) ///
(`se_s12_gend0_semrel01') (.)     

post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)   

post stack_est ("ls") ("coeff") ///
(`est_s01_gend1_ls') (.) ///
(`est_s01_gend0_ls') (.) ///  
(`est_s12_gend1_ls') (.) ///
(`est_s12_gend0_ls') (.)                 
               
post stack_est ("ls") ("se") ///
(`se_s01_gend1_ls') (.) ///
(`se_s01_gend0_ls') (.) ///  
(`se_s12_gend1_ls') (.) ///
(`se_s12_gend0_ls') (.)                 

post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)   

post stack_est ("year01") ("coeff") ///
(`est_s01_gend1_year01') (.) ///
(`est_s01_gend0_year01') (.) ///  
(`est_s12_gend1_year01') (.) ///
(`est_s12_gend0_year01') (.)                 
               
post stack_est ("year01") ("se") ///
(`se_s01_gend1_year01') (.) ///
(`se_s01_gend0_year01') (.) ///  
(`se_s12_gend1_year01') (.) ///
(`se_s12_gend0_year01') (.)                 

post stack_est ("") ("") (.) (.) (.) (.)  (.) (.) (.) (.)        
    

post stack_est  ("Observations") ("") ///
    (`n_s01_ls_top_gend1') (.) ///
    (`n_s01_ls_top_gend0') (.) ///
    (`n_s12_ls_top_gend1') (.) ///
    (`n_s12_ls_top_gend0') (.)
    
postclose stack_est

 
*** Erasing the subasamples
clear
dir
foreach y of numlist 0 1 2 {
  foreach gend of numlist 0 1  {
    foreach halfby in ls {
      foreach half in top {

        erase _y0`y'_gend`gend'_ls_top.dta

      }
    }
  } 
}

use ${date}_ta4_logit_coeffs, clear
browse

cap log close

