clear
set more off
set matsize 800
set logtype text
global progname="table4"
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

Name:   070802 - table 4 - levels estimates in covariate subgroups.do
Data:   base99/00/01/02 

NON-DELTA METHOD
This program creates table 4 - levels estimates in covariate subgroups
    * This is a first Stata replication of the SAS programs.

**********************************************************************/

*** Looping over cohorts

foreach y in 00 01 02 /*99*/ {
  foreach gend of numlist 0 1 /* 2 - 2 is for Boys+Girl */ {
  use "base`y'.dta" , clear

 *  sample 20
 
 	cap rename semelmos school_id
 	
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
       /* this is just to have the linear effect simply inserted 
          within the loop. see - else local control==... */


 /*****************   REGRESSIONS  **********************************/

  foreach ctrl in sc_main sc_lin {
    foreach halfby in ls p {
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
      di "**************************************************************************************"
      di "**************************************************************************************"
      di "Year is `y'"
      di "Gender is `gend'"
      di "Half-by-var is `halfby'"
      di "Half is `half'"
      di "control is `control'"
      di "**************************************************************************************"
      di "**************************************************************************************"
      di "**************************************************************************************"
  


      *** Taking mean of dep.var, N.obs and N.schools

      sum zakaibag
      local y`y'_gend`gend'_`halfby'_`half'_mean=r(mean)
      local y`y'_gend`gend'_`halfby'_`half'_n   =r(N)

      egen school_id2 = group(school_id)
      sum school_id2 
      local y`y'_gend`gend'_`halfby'_`half'_ns= r(max)
      drop school_id2

      *** LOGIT REGRESSION
      ereturn clear
      cap mat drop b se
      brl zakaibag treated `control',logit cluster(school_id)
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
        b_top_ls b_bot_ls space1 g_top_ls g_bot_ls ///
        space2 ///
        b_top_p  b_bot_p  space3 g_top_p  g_bot_p ///
        using t4_levels_estimates_halves_NDM, replace

	foreach y in 01 00 02 /*99*/ {

        post levels_est ("`y'") ("Dep Mean") ("") ///
                        (`y`y'_gend1_ls_top_mean') (`y`y'_gend1_ls_bot_mean') (.) ///
                        (`y`y'_gend0_ls_top_mean') (`y`y'_gend0_ls_bot_mean') (.) ///
                        (`y`y'_gend1_p_top_mean')  (`y`y'_gend1_p_bot_mean') (.) ///
                        (`y`y'_gend0_p_top_mean')  (`y`y'_gend0_p_bot_mean')
	
      post levels_est ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

      foreach ctrl in sc_main sc_lin {
         foreach stat in est se {

            post levels_est ///
                ("`y'") ("`ctrl'") ("`stat'") ///
                (`y`y'_gend1_`ctrl'_ls_top_`stat'')     ///
                (`y`y'_gend1_`ctrl'_ls_bot_`stat'') (.) ///
                (`y`y'_gend0_`ctrl'_ls_top_`stat'')     ///
                (`y`y'_gend0_`ctrl'_ls_bot_`stat'') (.) ///
                (`y`y'_gend1_`ctrl'_p_top_`stat'')      ///
                (`y`y'_gend1_`ctrl'_p_bot_`stat'')  (.) ///
                (`y`y'_gend0_`ctrl'_p_top_`stat'')      ///
                (`y`y'_gend0_`ctrl'_p_bot_`stat'')

            } /* close stat loop */

            post levels_est ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

          } /* close control loop */

	  post levels_est	("`y'") ("Observations") ("") ///
				(`y`y'_gend1_ls_top_n') (`y`y'_gend1_ls_bot_n') (.) ///
				(`y`y'_gend0_ls_top_n') (`y`y'_gend0_ls_bot_n') (.) ///
				(`y`y'_gend1_p_top_n')  (`y`y'_gend1_p_bot_n')  (.) ///
				(`y`y'_gend0_p_top_n')  (`y`y'_gend0_p_bot_n')


        post levels_est ("`y'") ("") ("") (.) (.) (.) (.) (.) (.) (.) (.) (.) (.) (.)

        }

postclose levels_est

use t4_levels_estimates_halves_NDM, clear
browse
clear
cap erase t4_levels_estimates_halves_NDM.dta
cap log close


