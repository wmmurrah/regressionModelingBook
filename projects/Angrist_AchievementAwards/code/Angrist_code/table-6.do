/****************************************************************

Aim: to create the levels and stacked estimates for table 6, in each year. 


****************************************************************/

clear
set more off
set matsize 800
set logtype text
global progname="table6"
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

foreach y of numlist 0 1  {

  	foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {

  		use "base0`y'.dta" , clear
  		cap rename semelmos school_id
  		drop if (1-boy)==`gend'	
		gen ah4=m_ahim>=4
 		
		di "*******************************"
		di "*******************************"
		di "*******************************"

    	di "Gender is `gend'"
		di "Subgroups are by `halfby'"
		di "Year is `y'"
      	di "*******************************"
		di "*******************************"
		di "*******************************"

    	*** Lagged Score quartiling - an algorithm that conforms with the SAS algorithm
		gen p25=0	
		gen p50=0
		gen p75=0
		gen p100=0

    	gen ls25=0	
		gen ls50=0
		gen ls75=0
		gen ls100=0

		sum lagscore, detail
		replace ls25 =1 if lagscore<r(p25)
		replace ls50 =1 if ls25==0 & lagscore<r(p50)
		replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
		replace ls100=1 if ls50+ls25+ls75==0

    	*** Note that the here the top/bottom are quartiles, not halves
		gen top_ls=(ls100==1)
		gen bot_ls=(ls75 ==1)

		*** Predicted Probability quartiling
		logit zakaibag educav educem ah4 ole5 semarab semrel ls50 ls75 ls100
   		predict p
    
		sum p, detail
		replace p25 =1 if p<r(p25)
		replace p50 =1 if p25==0 & p<r(p50)
		replace p75 =1 if p50+p25==0 & p<r(p75)
		replace p100=1 if p50+p25+p75==0


    	*** Note that here the top/bottom are QUARTILES
		gen top_p=(p100==1)
		gen bot_p=(p75 ==1)

		gen ls=lagscore
       	/* this is just to have the linear effect simply inserted 
          	within the loop. see - else local control==... */


		foreach halfby in ls p {
      		foreach half in top bot {
        			*** Keeping the halfs in temporary files
        			preserve
        			keep if `half'_`halfby'==1
        			save _y0`y'_gend`gend'_`halfby'_`half', replace
        			restore
			
			} /* close halfs loop (top/bottom) */
    	} /* close halfby loop (ls/p) */
	}/*close gender loop*/
}/*close year loop*/


          				

foreach y of numlist 0 1 {
	foreach gend of numlist 0 1  {
		foreach halfby in ls p {
      		foreach half in top bot {

        			
					
        			use _y0`y'_gend`gend'_`halfby'_`half', clear
 					
        			local n_`y'_`halfby'_`half'_gend`gend'=_N
        			sum zakaibag
        			local mean_`y'_`halfby'_`half'_gend`gend'=r(mean)
					
        			*** Running the regressions
        			foreach ctrl in main lin {
          				local halfbycont="`halfby'"
          				*if "`ctrl'"=="main" local halfbycont="`halfby'25-`halfby'100"
          				* this is not right - we shouldnt be putting in any quartiles at all - it causes multicollinearity
          				if "`ctrl'"=="main" local halfbycont=""
          				
          				di "*******************************"
        				di "*******************************"
        				di "*******************************"
	        			di "Gender is `gend'"
	        			di "Subgroups are by `halfby'"
	        			di "Subgroup is `half'"
						di "Year is `y'"
						di "Control is `ctrl'"
	        			di "*******************************"
	        			di "*******************************"
	        			di "*******************************"
          			
          				brl zakaibag treated semarab semrel `halfbycont', logit cluster(school_id)
						
          				/*** Calculating the marginal effect,
               			based on the treated group, assuming treatment=0
             			Non-Delta Method:
              			ME=Est*h0, S.E.(ME)=S.E.*h0
               			h0=pr(1-pr)
               			pr=(exp(pxb)/(1+exp(pxb))
               			pxb is the linear predicted value, assuming no treatment

         				***/
				         				
         				// THESE ARE THE USUAL COMMANDS TO GET THE MARGINAL EFFECTS 
        				local est=_b[treated]
        				local se=_se[treated]
        				predict pxb,xb
        				replace pxb=pxb-`est' if treated==1
        				gen pr=(exp(pxb))/(1+exp(pxb))
        				gen h0=pr*(1-pr)
        				sum h0 if treated
        				local h0=r(mean)
        				drop h0 pr pxb 
        				//This drops the variable, not the local
        				local est_`y'_`halfby'_`half'_gend`gend'_`ctrl'=.
        				local  se_`y'_`halfby'_`half'_gend`gend'_`ctrl'=.
        				cap local est_`y'_`halfby'_`half'_gend`gend'_`ctrl'=`h0'*`est'
        				cap local  se_`y'_`halfby'_`half'_gend`gend'_`ctrl'=`h0'*`se'
        				
						di "***********************************************************"
						di " "

						di "DEBUGGING - MARGINAL EFFECTS"
						di " "
						di "*******************************"
        				di "Gender is `gend'"
        				di "Subgroups are by `halfby'"
        				di "Subgroup is `half'"
						di "Year is `y'"
        				di "*******************************"
						di "est_`y'_`halfby'_`half'_gend`gend'_`ctrl' is "
						di `est_`y'_`halfby'_`half'_gend`gend'_`ctrl''
						di "se_`y'_`halfby'_`half'_gend`gend'_`ctrl' is "
						di `se_`y'_`halfby'_`half'_gend`gend'_`ctrl''
						di " "
						di "***********************************************************"

        				macro drop _h0 _est _se

        			} /* close control loop */
			}/*end half loop*/
		}/*end halfby loop*/
	}/*end gender loop*/
}/*end of year loop*/

clear
set mem 10m
set more off
*set trace on
set logtype text

*** Looping over cohorts to create the subgroups

foreach y in 00 01 {

  	foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {

	  	use "base`y'.dta", clear
	  	
	  	cap rename semelmos school_id
	  	
	  	gen year`y'=1
		*  sample 20
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
		
	    sum lagscore, detail
	    replace ls25 =1 if lagscore<r(p25)
	    replace ls50 =1 if ls25==0 & lagscore<r(p50)
	    replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
	    replace ls100=1 if ls50+ls25+ls75==0

	    *** Note that the here the top/bottom are quartiles, not halves
	    gen top_ls=(ls100==1)
	    gen bot_ls=(ls75 ==1)
	
	    *** Predicted Probability quartiling
	    logit zakaibag educav educem ah4 ole5 semarab semrel ls50 ls75 ls100
	    predict p
	    
	    sum p, detail
	    replace p25 =1 if p<r(p25)
	    replace p50 =1 if p25==0 & p<r(p50)
	    replace p75 =1 if p50+p25==0 & p<r(p75)
	    replace p100=1 if p50+p25+p75==0
		
	    *** Note that here the top/bottom are QUARTILES
	    gen top_p=(p100==1)
	    gen bot_p=(p75 ==1)

	    gen ls=lagscore
	       /* this is just to have the linear effect simply inserted 
	          within the loop. see - else local control==... */
	
	    foreach halfby in ls p {
	      	foreach half in top bot {
	        	*** Keeping the halfs in temporary files
	        	preserve
		        keep if `half'_`halfby'==1
		        save _y`y'_gend`gend'_`halfby'_`half', replace
		        restore 
      		} /* close halfs loop (top/bottom) */
    	} /* close halfby loop (ls/p) */
  	} /* close gender loop*/
} /* close year loop*/

clear

 /*****************   REGRESSIONS  **********************************/

foreach stack in s01 {
	foreach gend of numlist 0 1  {
    	foreach halfby in ls p {
      		foreach half in top bot {

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
		
		        use _y01_gend`gend'_`halfby'_`half', clear
		
		        if "`stack'"=="s01" | "`stack'"=="s012" ///
		          append using _y00_gend`gend'_`halfby'_`half'
		        if "`stack'"=="s01" | "`stack'"=="s012" ///
		          di _N " observations are added from year 2000"
		
		
		        if "`stack'"=="s12" | "`stack'"=="s012" ///
		          append using _y02_gend`gend'_`halfby'_`half'
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

		        *** keeping the number of observations and the means 
		
		       	local n_`stack'_`halfby'_`half'_gend`gend'=_N
		
		       	sum zakaibag
				local mean_`stack'_`halfby'_`half'_gend`gend'=r(mean)
		
		        *** Running the regressions
		        foreach ctrl in main lin {
		        	local halfbycont="`halfby'"
		          	if "`ctrl'"=="main" local halfbycont="`halfby'25-`halfby'100"
		
		
		          	xi: logit zakaibag treated semarab0* semrel0* ///
		            `halfbycont' year0* i.school_id, robust


				      /*** Calculating the marginal effect,
			               based on the treated group, assuming treatment=0
			             Non-Delta Method:
			               ME=Est*h0, S.E.(ME)=S.E.*h0
			               h0=pr(1-pr)
			               pr=(exp(pxb)/(1+exp(pxb))
			               pxb is the linear predicted value, assuming no treatment
			
			         ***/

			        local est=_b[treated]
			        local se=_se[treated]
			        predict pxb,xb
			        replace pxb=pxb-`est' if treated==1
			        gen pr=(exp(pxb))/(1+exp(pxb))
			        gen h0=pr*(1-pr)
			        sum h0 if treated
			        local h0=r(mean)
			        drop h0 pr pxb
			        local est_`stack'_`halfby'_`half'_gend`gend'_`ctrl'=.
			        local  se_`stack'_`halfby'_`half'_gend`gend'_`ctrl'=.
			        cap local est_`stack'_`halfby'_`half'_gend`gend'_`ctrl'=`h0'*`est'
			        cap local  se_`stack'_`halfby'_`half'_gend`gend'_`ctrl'=`h0'*`se'

					di "*************************************************8"
					di " "
					di "DEBUGGING - MARGINAL EFFECTS"
					di " "
					di "est_`stack'_`halfby'_`half'_gend`gend'_`ctrl' is " `est_`stack'_`halfby'_`half'_gend`gend'_`ctrl''
					di " "
					di "se_`stack'_`halfby'_`half'_gend`gend'_`ctrl' is " `se_`stack'_`halfby'_`half'_gend`gend'_`ctrl''
					di " "
					di "*************************************************8"



			        macro drop _h0 _est _se

        		} /* close control loop */
      		} /* close half loop (top/bottom) */
    	} /* close halfby loop (ls/p) */
  	} /* close gender loop */
} /* close stack loop */


macro list
/**********************************************************************

   CREATING THE TABLE
**********************************************************************/
postfile levels_t6 str14(year ctrl stat) ///
  b_top_ls b_bot_ls space1 g_top_ls g_bot_ls ///
  space2 ///
  b_top_p  b_bot_p  space3 g_top_p  g_bot_p ///
  using t6_stacked_levels_estimates_quarts_NDM, replace

foreach y of numlist 1 0 {

  	post levels_t6 ("`y'") ("Dep Mean") ("") ///
    (`mean_`y'_ls_top_gend1') (`mean_`y'_ls_bot_gend1') (.) ///
    (`mean_`y'_ls_top_gend0') (`mean_`y'_ls_bot_gend0') (.) ///
    (`mean_`y'_p_top_gend1')  (`mean_`y'_p_bot_gend1')  (.) ///
    (`mean_`y'_p_top_gend0')  (`mean_`y'_p_bot_gend0')
	
  	post levels_t6 ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

	foreach ctrl in main lin {
    	foreach stat in est se {

	      	post levels_t6 ("`y'") ("`ctrl'") ("`stat'") ///
	        (``stat'_`y'_ls_top_gend1_`ctrl'') (``stat'_`y'_ls_bot_gend1_`ctrl'') (.) ///
	        (``stat'_`y'_ls_top_gend0_`ctrl'') (``stat'_`y'_ls_bot_gend0_`ctrl'') (.) ///
	        (``stat'_`y'_p_top_gend1_`ctrl'')  (``stat'_`y'_p_bot_gend1_`ctrl'')  (.) ///
	        (``stat'_`y'_p_top_gend0_`ctrl'')  (``stat'_`y'_p_bot_gend0_`ctrl'')
	    
    	} /* close stat loop */

    	post levels_t6 ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
 	} /* close control loop */

  	post levels_t6 ("`y'") ("Observations") ("") ///
    (`n_`y'_ls_top_gend1') (`n_`y'_ls_bot_gend1') (.) ///
    (`n_`y'_ls_top_gend0') (`n_`y'_ls_bot_gend0') (.) ///
    (`n_`y'_p_top_gend1')  (`n_`y'_p_bot_gend1')  (.) ///
    (`n_`y'_p_top_gend0')  (`n_`y'_p_bot_gend0')

  	post levels_t6 ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
	post levels_t6 ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

} /* close y loop */

foreach stack in s01 {

  	post levels_t6 ("`stack'") ("Dep Mean") ("") ///
    (`mean_`stack'_ls_top_gend1') (`mean_`stack'_ls_bot_gend1') (.) ///
    (`mean_`stack'_ls_top_gend0') (`mean_`stack'_ls_bot_gend0') (.) ///
    (`mean_`stack'_p_top_gend1')  (`mean_`stack'_p_bot_gend1')  (.) ///
    (`mean_`stack'_p_top_gend0')  (`mean_`stack'_p_bot_gend0')
	
  	post levels_t6 ("`stack'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

  	foreach ctrl in main lin {
    	foreach stat in est se {

      		post levels_t6 ("`stack'") ("`ctrl'") ("`stat'") ///
	        (``stat'_`stack'_ls_top_gend1_`ctrl'') (``stat'_`stack'_ls_bot_gend1_`ctrl'') (.) ///
	        (``stat'_`stack'_ls_top_gend0_`ctrl'') (``stat'_`stack'_ls_bot_gend0_`ctrl'') (.) ///
	        (``stat'_`stack'_p_top_gend1_`ctrl'')  (``stat'_`stack'_p_bot_gend1_`ctrl'')  (.) ///
	        (``stat'_`stack'_p_top_gend0_`ctrl'')  (``stat'_`stack'_p_bot_gend0_`ctrl'')

    	} /* close stat loop */

    	post levels_t6 ("`stack'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

  	} /* close control loop */

	post levels_t6 ("`stack'") ("Observations") ("") ///
	   (`n_`stack'_ls_top_gend1') (`n_`stack'_ls_bot_gend1') (.) ///
	    (`n_`stack'_ls_top_gend0') (`n_`stack'_ls_bot_gend0') (.) ///
	    (`n_`stack'_p_top_gend1')  (`n_`stack'_p_bot_gend1')  (.) ///
	    (`n_`stack'_p_top_gend0')  (`n_`stack'_p_bot_gend0')
	
	post levels_t6 ("`stack'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)
} /* close stack loop */

postclose levels_t6

*** Erasing the subasamples
clear
dir
foreach y of numlist 0 1 {
  	foreach gend of numlist 0 1  {
    	foreach halfby in ls p {
      		foreach half in top bot {
	       		erase _y0`y'_gend`gend'_`halfby'_`half'.dta
      		}
    	}
  	} 
}

use t6_stacked_levels_estimates_quarts_NDM, clear
browse
clear
cap erase t6_stacked_levels_estimates_quarts_NDM.dta

log close
