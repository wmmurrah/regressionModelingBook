/********************************************************************

Name:   table7.do
Data:   base00 base01 


Running table 7 with the same sample for both stacked and levels. 

Non-Delta Method
This version is similar to the version of 30th July, except that:
  (1) The lower panel of "bagrut cond. on Awarded" is omitted
  (2) Advanced subjects is omitted
  (3) S.E.s in the stacked are robust
  (4) Levels estimates converted to BRL (before they were temporarily just clustered)

Output:
	${date}_mediating_outcs
	${date}_means_comp

**********************************************************************/

clear
set more off
set matsize 800
set logtype text
global progname="table7"
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

** Looping over cohorts

foreach y of numlist 0 1 {

  	use "base0`y'.dta" , clear
  	
  	cap rename semelmos school_id
  	
  	*** Defining new variables
  	foreach n_units of numlist 18(2)24  {


    	* Bagrut condtional on Attmpted and Awarded 18/20/22/24
		* Note that there are missing values generated!		
		gen bag_cond_awr`n_units'= zakaibag if awr`n_units'==1
	
   		gen bag_cond_att`n_units'= zakaibag if att`n_units'==1
		gen awr_cond_att`n_units'= awr`n_units' if att`n_units'==1
	
		*move comp`n_units'_b_1 comp`n_units'_b_m
	}/*end numlist loop*/

	global att		= "att18 att20 att22 att24"
	global awr		= "awr18 awr20 awr22 awr24"
	global achv		= "achv_math achv_eng achv_hib"
	global cond_att	= "bag_cond_att18 bag_cond_att20 bag_cond_att22 bag_cond_att24"
	global outcomes	= "${att} ${awr} ${achv} ${cond_att}"
		
	*** !!! Bagrut conditional on attempted and advanced subjects are omitted in this version
	di "${outcomes}"

	/**********************************************************************

	Treatment Control Differences
	**********************************************************************/
	
foreach boy of numlist 0 1 {
	
		preserve
		keep if boy==`boy'
	  	gen ls25=0
	  	gen ls50=0
	  	gen ls75=0
	  	gen ls100=0
	
	
   		*** quartiling algorithm that conforms with the SAS algorithm
	  	sum lagscore, detail
	  	replace ls25 =1 if lagscore<r(p25)
	  	replace ls50 =1 if ls25==0 & lagscore<r(p50)
	  	replace ls75 =1 if ls50+ls25==0 & lagscore<r(p75)
	  	replace ls100=1 if ls50+ls25+ls75==0
	
	  	keep if ls75+ls100==1
		save _NDM_top_`boy'_0`y', replace
	
		restore
	}/*end boy loop*/
}/*end year loop*/


/**********************************************************************

Creating a stacked 2000 & 2001 data and running treatment effects
**********************************************************************/
*set trace on

foreach boy of numlist 0 1 {

  	use _NDM_top_`boy'_01, clear
  	gen year01=1
	
  	append using _NDM_top_`boy'_00
    di _N

  	replace year01=(year01==1)
  	gen year00 = !year01
  		
	* Making sure that each schools appears in both years
  	sort school_id
  	egen x1=max(year01), by (school_id)
    egen x0=min(year01), by (school_id)
  	keep if x1==1 & x0==0
  	drop x1 x0
  	
  	/* preparing the ground for implementation of new-step-5: 
      	tagging for each outcomes schools where there's no variation in outcome */
  	foreach outcome of varlist ${outcomes} {
    	egen zak_`outcome'=max(`outcome'), by (school_id)
  	}
  	 
  	save _NDM_stack_`boy', replace
  	 	
  	* Running the Treatment Effects
    foreach outcome of varlist ${outcomes} {
    	use _NDM_stack_`boy', clear
    	
    	keep if zak_`outcome'
    	   	
    	local n_`outcome'_`boy'_s = _N
    	
    	gen treated01= treated*year01	
  		gen semrel01 = semrel*year01
  		gen semarab01= semarab*year01
	  	
	  	di "*******************************************************************************"
	    di ""
	    di "STACKED"
	    di ""
	    di "Gender is `boy'"
	    di ""
	    di "xi: logit `outcome' treated01 semarab01 semrel01 ls100 year01 i.school_id, robust "
	    di ""
	    di "*******************************************************************************"
    
	    xi: logit `outcome' treated01 semarab01 semrel01 ls100 year01 i.school_id, robust 
	        		
	    local est=_b[treated01]
	    local se=_se[treated01]
	    local  nreg_`outcome'_`boy'_s= e(N)
	    predict pxb,xb
	    replace pxb=pxb-`est' if treated01==1
	    gen pr=(exp(pxb))/(1+exp(pxb)) 
	  	gen h0=pr*(1-pr)
	  	sum h0 if treated 
	  	local h0=r(mean)
	  	drop h0 pr pxb
	  	local est_`outcome'_`boy'_s=.
	 	local  se_`outcome'_`boy'_s=.
	  	cap local est_`outcome'_`boy'_s=`h0'*`est'
	  	cap local  se_`outcome'_`boy'_s=`h0'*`se'
	  	cap macro drop _h0 _est _se
	  	
	  	di "*******************************************************************************"
	    di ""
	    di "STACKED"
	    di ""
	    di "Gender is `boy'"
	    di ""
	    di "marginal effects NDM coefficient est_`outcome'_`boy'_s is  `est_`outcome'_`boy'_s'"
	    di ""
	    di "marginal effects NDM std error se_`outcome'_`boy'_s is `se_`outcome'_`boy'_s'"
	    di ""    
	    di "*******************************************************************************"
	    
	}/*end outcome loop*/
} /* close boy loop */

foreach boy of numlist 0 1 {
	use _NDM_stack_`boy', clear
	local controls 			= "treated semarab semrel ls100" 
	foreach y of numlist 0 1 {	
		preserve
		keep if year0`y'
		save _NDM_stack_`boy'_`y', replace
		
		foreach outcome of varlist ${outcomes} {
			use _NDM_stack_`boy'_`y', clear			
			keep if zak_`outcome'
			
			local  n_`outcome'_`boy'_0`y'=_N
						
			sum `outcome' 
			local p1_mean_`outcome'_`boy'_0`y'=r(mean)
	
			brl `outcome' `controls',logit cluster(school_id)   
			
			di "*******************************************************************************"
    		di ""
		    di "LEVELS"
		    di ""
		    di "Gender is `boy', Year is `y'"
		    di ""
		    di "brl `outcome' `controls',logit cluster(school_id)   "
		    di ""
		    di "*******************************************************************************"
			su `outcome' `controls' 
			
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
			cap local  nreg_`outcome'_`boy'_0`y'=e(N)
			predict pxb,xb
			replace pxb=pxb-`est' if treated==1
			gen pr=(exp(pxb))/(1+exp(pxb))
			gen h0=pr*(1-pr)
			sum h0 if treated
			local h0=r(mean)
			drop h0 pr pxb
			local est_`outcome'_`boy'_0`y'=.
			local  se_`outcome'_`boy'_0`y'=.
			cap local est_`outcome'_`boy'_0`y'=`h0'*`est'
			cap local  se_`outcome'_`boy'_0`y'=`h0'*`se'
			cap macro drop _h0 _est _se
	
			di "*******************************************************************************"
    		di ""
		    di "LEVELS"
		    di ""
		    di "Gender is `boy', Year is `y'"
		    di ""
		    di "brl `outcome' `controls' ,logit cluster(semelmos)   "
		    di ""
		    di "Marginal Effects est_`outcome'_`boy'_0`y' is `est_`outcome'_`boy'_0`y''"
		    di ""
		    di "Marginal Effects se_`outcome'_`boy'_0`y' is `se_`outcome'_`boy'_0`y''""
		    di ""
		    di "*******************************************************************************"
			
		}/*end year loop*/
	 	restore	 
	}/*end boy  loop*/
}/*end outcome  loop*/

erase _NDM_top_0_00.dta	
erase _NDM_top_0_01.dta
erase _NDM_top_1_00.dta
erase _NDM_top_1_01.dta
*erase _NDM_stack_0.dta
*erase _NDM_stack_1.dta

*note there are a lot of superfluous data sets which need cleaning up here

/**********************************************************************

   CREATING THE TREATMENT EFFECT TABLE
**********************************************************************/

postfile mediating_outcsmeans  str14(outcome) ///
  	 b_mean01 b_te00 b_te01 b_te_s space1 ///
  	 g_mean01 g_te00 g_te01 g_te_s ///
	using table_7_mediating_outcs_NDMMEANS, replace

local outcomes="${outcomes}"
di "`outcomes'"
local att = "${att}"
local awr = "${awr}"
local achv = "${achv}"
local cond_att = "${cond_att}"
local groups "att awr achv cond_att"

/* there was a strange unexpected error here, in spite of the $ sign
   Stata thinks that the brace following it was meant for the loop
   and complains that "code follows on the same line as open brace".
   so there's no choise but to use a local instead of the global */

foreach group in `groups' {
	foreach outcome of varlist ``group'' {
	
		post mediating_outcsmeans ("`outcome'") ///
		   (`p1_mean_`outcome'_1_01') (`est_`outcome'_1_00') ///
	        (`est_`outcome'_1_01')     (`est_`outcome'_1_s') ///
	        (.) ///
		  (`p1_mean_`outcome'_0_01') (`est_`outcome'_0_00') ///
	        (`est_`outcome'_0_01')     (`est_`outcome'_0_s')
	
	    post mediating_outcsmeans ("`outcome'") ///
		     (.)                       (`se_`outcome'_1_00') ///
	        (`se_`outcome'_1_01')      (`se_`outcome'_1_s') ///
	        (.) ///
		      (.)                      (`se_`outcome'_0_00') ///
	        (`se_`outcome'_0_01')      (`se_`outcome'_0_s')
	}
	
	post mediating_outcsmeans ("")   (.)   (.)  (.)  (.) (.) (.) (.) (.) (.) 
}



  postclose mediating_outcsmeans



use table_7_mediating_outcs_NDMMEANS, clear
browse
clear
cap erase table_7_mediating_outcs_NDMMEANS.dta

cap log close
