clear
set more off
set matsize 800
set logtype text
global progname="tableA5"
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


/********************************************************************

Name:   070807 - table 5 - Stacked estimates in covariate subgroups - non delta.do
Data:   base99/00/01/02 

NON-DELTA-METHOD
This program creates table 5 - Stacked estimates in covariate subgroups
    * This is a first Stata replication of the SAS programs.

**********************************************************************/

*** Looping over cohorts to create the subgroups

foreach y in 00 01 02  {

  foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {

  use "base`y'.dta" , clear
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

foreach stack in s01   {
  foreach gend of numlist 0 1  {
    foreach halfby in ls  {
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
postfile stack_est str14(year ctrl stat) ///
  b_top_ls b_bot_ls space1 g_top_ls g_bot_ls ///
   using table5half, replace

foreach stack in s01   {

  post stack_est ("`stack'") ("Dep Mean") ("") ///
    (`mean_`stack'_ls_top_gend1') (`mean_`stack'_ls_bot_gend1') (.) ///
    (`mean_`stack'_ls_top_gend0') (`mean_`stack'_ls_bot_gend0') 
	
  post stack_est ("`stack'") ("") ("") (.) (.) (.) (.) (.) 

  foreach ctrl in main lin {
    foreach stat in est se {

      post stack_est ("`stack'") ("`ctrl'") ("`stat'") ///
        (``stat'_`stack'_ls_top_gend1_`ctrl'') (``stat'_`stack'_ls_bot_gend1_`ctrl'') (.) ///
        (``stat'_`stack'_ls_top_gend0_`ctrl'') (``stat'_`stack'_ls_bot_gend0_`ctrl'') 

    } /* close stat loop */

  post stack_est ("`stack'") ("") ("") (.) (.) (.) (.) (.) 
  } /* close control loop */

  post stack_est ("`stack'") ("Observations") ("") ///
    (`n_`stack'_ls_top_gend1') (`n_`stack'_ls_bot_gend1') (.) ///
    (`n_`stack'_ls_top_gend0') (`n_`stack'_ls_bot_gend0')

  

} /* close stack loop */



postclose stack_est


*** Erasing the subasamples
clear
dir
foreach y of numlist 0 1 2 {
  foreach gend of numlist 0 1  {
    foreach halfby in ls p {
      foreach half in top bot {

        erase _y0`y'_gend`gend'_`halfby'_`half'.dta

      }
    }
  } 
}

use "base00.dta" , clear

*** Assigning Quartiles
gen ah4=m_ahim>=4
gen p25=0	
gen p50=0
gen p75=0
gen p100=0

gen ls25=0	
gen ls50=0
gen ls75=0
gen ls100=0

*** Assigning Quartiles
*** Lagged Score quartiling - an algorithm that conforms with the SAS algorithm
sum lagscore, detail
replace ls25 =1 if lagscore<r(p25)
replace ls50 =1 if ls25==0 & lagscore<r(p50)
replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
replace ls100=1 if ls50+ls25+ls75==0

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

gen top_p=(p75+p100==1)
gen bot_p=(p25+p50 ==1)

gen ls=lagscore

save "base00bagrutrates.dta", replace

collapse (mean) zakaibag, by(school_id)
rename zakaibag bag_00
sort school_id
save "base00bag_00.dta", replace
use "base00bagrutrates.dta", clear
collapse (mean) zakaibag, by(school_id boy)
rename zakaibag bagA_00
sort school_id boy
save "base00bagA_00.dta", replace
use "base00bagrutrates.dta", clear
collapse (mean) zakaibag, by(school_id boy top_ls)
rename zakaibag bagB_00
sort school_id boy top_ls
save "base00bagB_00.dta", replace
unique school_id
unique school_id boy
unique school_id boy top_ls
merge school_id boy using "base00bagA_00.dta", nokeep _merge(bagA)
sort school_id
unique school_id
unique school_id boy
unique school_id boy top_ls
merge school_id using "base00bag_00.dta", nokeep _merge(bag00)
unique school_id
unique school_id boy
unique school_id boy top_ls
tab bagA
tab bag00

preserve
keep if boy ==1 
sort school_id top_ls
keep bag_00 bagA_00 bagB_00 school_id top_ls 
save "base00bag1.dta", replace
restore

preserve
keep if boy ==0 
sort school_id top_ls
keep bag_00 bagA_00 bagB_00 school_id top_ls 
save "base00bag0.dta", replace
restore


*** Looping over cohorts

foreach y in /*00*/ 01 /*02*/ /*99*/ {
  foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {
  	
	use "base`y'.dta" , clear
	cap rename semelmos school_id
 *  sample 20
    drop if (1-boy)==`gend'
    gen ah4=m_ahim>=4
 
    *** Assigning Quartiles

    gen p25=0	
    gen p50=0
    gen p75=0
    gen p100=0

    gen ls25=0	
    gen ls50=0
    gen ls75=0
    gen ls100=0

    *** Assigning Quartiles
    *** Lagged Score quartiling - an algorithm that conforms with the SAS algorithm
    sum lagscore, detail
    replace ls25 =1 if lagscore<r(p25)
    replace ls50 =1 if ls25==0 & lagscore<r(p50)
    replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
    replace ls100=1 if ls50+ls25+ls75==0

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

    gen top_p=(p75+p100==1)
    gen bot_p=(p25+p50 ==1)

    gen ls=lagscore
    
    sort school_id top_ls
    merge school_id top_ls using "base00bag`gend'.dta", _merge(bag) nokeep
       
    
       /* this is just to have the linear effect simply inserted 
          within the loop. see - else local control==... */


 /*****************   REGRESSIONS  **********************************/

  foreach ctrl in sc_main sc_lin {
    foreach halfby in ls /*p*/ {
      foreach half in top bot {

      preserve

        keep if `half'_`halfby'==1

      local schools="semarab semrel"

      if "`half'"=="top" local halfbyvar="`halfby'100"
        else local halfbyvar="`halfby'50"

      /* For bottom boys, lagged score and p, 
         All religious schools students have bagrut=0. 
         In addition, all first p-quartile have zero bagrut.
         in normal logit
         it drops automatically, and gives the following note:
         note: semrel != 0 predicts failure perfectly
         in BRL we have to do that by hand, otherwise we get an error 

         In other samples, it's better be done automatically*/


      if "`half'"=="bot" & `y'==0 & `gend'==1 local schools="semarab"
      if "`half'"=="bot" & `y'==0 & `gend'==1 & "`halfby'"=="p" local halfbyvar=""
      if "`ctrl'"=="sc_main" local control="`schools' `halfbyvar'"
        else local control="`schools' `halfby'"
      
    
      di "**************************************************************************************"
      di "Year is `y'"
      di "Gender is `gend'"
      di "Half-by-var is `halfby'"
      di "Half is `half'"
      di "control is `control'"
      di "**************************************************************************************"

      *** Taking mean of dep.var, N.obs and N.schools

      sum zakaibag
      local y`y'_gend`gend'_`halfby'_`half'_mean=r(mean)
      local y`y'_gend`gend'_`halfby'_`half'_n   =r(N)
      sum bag_00
      local y`y'_gend`gend'_`halfby'_`half'_bag00=r(mean)
      sum bagA_00
      local y`y'_gend`gend'_`halfby'_`half'_bagA=r(mean)
	  sum bagB_00
      local y`y'_gend`gend'_`halfby'_`half'_bagB=r(mean)
      egen school_id2 = group(school_id)
      sum school_id2 
      local y`y'_gend`gend'_`halfby'_`half'_ns= r(max)
      drop school_id2

      *** LOGIT REGRESSION
      ereturn clear
      cap mat drop b se
            
      brl zakaibag bagB_00 treated `control' ,logit cluster(school_id)
          
      mfx
      mat b =e(Xmfx_dydx)
	  mat se=e(Xmfx_se_dydx)
		
		cap local y`y'_gend`gend'_`ctrl'_`halfby'_`half'_best=b[1,1]
    	cap local  y`y'_gend`gend'_`ctrl'_`halfby'_`half'_bse=se[1,1]
      di "y`y'_gend`gend'_`ctrl'_`halfby'_`half'_best `y`y'_gend`gend'_`ctrl'_`halfby'_`half'_best'"
      di "y`y'_gend`gend'_`ctrl'_`halfby'_`half'_bse `y`y'_gend`gend'_`ctrl'_`halfby'_`half'_bse'"
      
      
        local est=_b[treated]
        local se=_se[treated]
        predict pxb,xb
            /*Currently, there's an error with the predict post-est:,
            the xb option is always on - instead of giving the pred. value
            it always gives the xb predicted value */
        replace pxb=pxb-`est' if treated==1
        gen pr=(exp(pxb))/(1+exp(pxb))
        gen h0=pr*(1-pr)
        sum h0 if treated
        local h0=r(mean)
        drop h0 pr pxb
        local y`y'_gend`gend'_`ctrl'_`halfby'_`half'_est=.
        local y`y'_gend`gend'_`ctrl'_`halfby'_`half'_se =.
        cap local y`y'_gend`gend'_`ctrl'_`halfby'_`half'_est=`h0'*`est'
        cap local y`y'_gend`gend'_`ctrl'_`halfby'_`half'_se =`h0'*`se'
        macro drop _h0 _est _se
        restore

        } /* close halfs loop (top/bottom) */

      } /* close halfby loop (ls/p) */

    } /* close control loop (main effect/linear) */

  } /* close gender loop*/

macro list

} /* close years loop*/


/**********************************************************************

   CREATING THE TREATMENT EFFECT TABLE
**********************************************************************/

	postfile levels_est str14(year ctrl stat) ///
      space3  b4_top_ls b4_bot_ls space5 g4_top_ls g4_bot_ls ///        
        using table4half, replace

	foreach y in 01  {

        post levels_est ("`y'") ("Dep Mean") ("") ///
                        (.) (`y`y'_gend1_ls_top_mean') (`y`y'_gend1_ls_bot_mean') (.) ///
                        (`y`y'_gend0_ls_top_mean') (`y`y'_gend0_ls_bot_mean') 
	                    
	
      post levels_est ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) 

      foreach ctrl in sc_main sc_lin {
         foreach stat in est se {
			
            post levels_est ///
                ("`y'") ("`ctrl'") ("`stat'") ///
                (.) (`y`y'_gend1_`ctrl'_ls_top_`stat'')     ///
                (`y`y'_gend1_`ctrl'_ls_bot_`stat'') (.) ///
                (`y`y'_gend0_`ctrl'_ls_top_`stat'')     ///
                (`y`y'_gend0_`ctrl'_ls_bot_`stat'') 

            } /* close stat loop */
		

            post levels_est ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) 


          } /* close control loop */


	  post levels_est	("`y'") ("Observations") ("") ///
				(.) (`y`y'_gend1_ls_top_n') (`y`y'_gend1_ls_bot_n') (.) ///
				(`y`y'_gend0_ls_top_n') (`y`y'_gend0_ls_bot_n') 


        

        }


postclose levels_est



use table4half, clear
cap drop rowOrder
gen rowOrder = _n
sort rowOrder
save table4half, replace

use table5half.dta, clear
cap drop rowOrder
gen rowOrder = _n
sort rowOrder
joinby rowOrder using table4half
drop rowOrder
browse
clear

cap erase table4half
cap erase table5half

cap log close


