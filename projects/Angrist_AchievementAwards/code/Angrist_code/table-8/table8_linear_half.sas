/********************************************************************
Date:   August 26, 2008
Author: Yannay
Name:			table8_linear_half.sas
Input:			base_ns.sas (data file with national security variables)
Output:			t8_lin_h_&outcome.sas7bdat

Description:	This program produces the OLS outcomes by lagged-score
				halves for table 8.
				For each outcome it produces a separate table.

Instructions:	(*)	Set the "home" libname below to the path of the folder
					in which the national security file is held
				(*)	Set the "target" libname below to the path of the folder
					where the results tables should be written
				(*) Run the program

NOTE:	The results are not set in the same order as in the
		paper that appears in the table, and there are additional
		results that don't show in the paper (namely the panel with 
		"1_main" as rowname).
		The results that show in the paper are only those that appear
		under columns whose titles start with
		"c_ls_b" (lagged-score halves, boys) and
		"c_ls_g" (lagged-score halves, girls).
		Each estimator shows with 3 different standard errors:
		Stderr: 	clustered s.e.s (not reported in the paper)
		RobSE:		Robust s.e.s (reported in the paper)
		NoClusSE:	Conventional s.e. (not reported in the paper) 

********************************************************************/

options nocenter nodate spool ls=75;

libname home  "W:\Student bonuses\Yannay Aug 08\data";
libname target  "W:\Student bonuses\Yannay Aug 08\Code\table 8\output";



data bonus_00_01;
set home.base_ns;
run;


%macro outcome (outcome);
********************************************************************;
	*** Creating the top and bottom files for each gender;
%macro base_gender (gend);

*** 2001 ***;
DATA BASE01;
set bonus_00_01;
a=1;

b=(boy=1);
g=(boy=0);
bg=1;

if year=2001;
if &gend=1;

proc univariate noprint;
var lagscore;
output out=temp pctlpts=25 to 100 by 25 pctlpre=count;
run;

DATA TEMP;
set temp;
a=1;
run;

proc sort data=base01; by a;
proc sort data=temp; by a;

DATA BASE01;
merge base01 temp;
by a;
ls25=0; ls50=0; ls75=0; ls100=0;
if      lagscore<count25 then ls25=1;
else if lagscore<count50 then ls50=1;
else if lagscore<count75 then ls75=1;
else                          ls100=1;

ah4=(m_ahim=>4);

proc genmod descending;
class school_id;
model zakaibag=educav educem ah4 ole5 semarab semrel
               ls50 ls75 ls100 /dist=bin link=logit;
repeated subject=school_id /type=ind ;
output out=temp2 p=p_hat;
proc univariate noprint;
var p_hat;
output out=temp3 pctlpts=25 to 100 by 25 pctlpre=count_p;
run;

DATA TEMP3;
set temp3;
a=1;
run;

proc sort data=temp2; by a;
proc sort data=temp3; by a;

DATA BASE01;
merge temp2 temp3;
by a;
p25=0; p50=0; p75=0; p100=0;
if      p_hat<count_p25 then p25=1;
else if p_hat<count_p50 then p50=1;
else if p_hat<count_p75 then p75=1;
else                         p100=1;
run;

proc datasets; delete temp temp2 temp3;

*** 2000 ***;
DATA BASE00;
set bonus_00_01;
a=1;

b=(boy=1);
g=(boy=0);
bg=1;

if year=2000;
if &gend=1;

proc univariate noprint;
var lagscore;
output out=temp pctlpts=25 to 100 by 25 pctlpre=count;
run;

DATA TEMP;
set temp;
a=1;
run;

proc sort data=base00; by a;
proc sort data=temp; by a;

DATA BASE00;
merge base00 temp;
by a;
ls25=0; ls50=0; ls75=0; ls100=0;
if      lagscore<count25 then ls25=1;
else if lagscore<count50 then ls50=1;
else if lagscore<count75 then ls75=1;
else                          ls100=1;

ah4=(m_ahim=>4);

proc genmod descending;
class school_id;
model zakaibag=educav educem ah4 ole5 semarab semrel
               ls50 ls75 ls100 /dist=bin link=logit;
repeated subject=school_id /type=ind ;
output out=temp2 p=p_hat;
proc univariate noprint;
var p_hat;
output out=temp3 pctlpts=25 to 100 by 25 pctlpre=count_p;
run;

DATA TEMP3;
set temp3;
a=1;
run;

proc sort data=temp2; by a;
proc sort data=temp3; by a;

DATA BASE00;
merge temp2 temp3;
by a;
p25=0; p50=0; p75=0; p100=0;
if      p_hat<count_p25 then p25=1;
else if p_hat<count_p50 then p50=1;
else if p_hat<count_p75 then p75=1;
else                         p100=1;
run;

proc datasets; delete temp temp2 temp3;

DATA BASE_&gend;
set base01 base00;
school_id_a=school_id;
if year=2000 then do;
   treated=0;
   semrel=0;
   semarab=0;
   school_id_a=school_id*100;
   end;
*** Missing are omitted here ****;
if missing_=0;
year01=(year=2001);
run;


%macro base_halves (halfby,top_bot);

********************************************************************;
	*** Creating the top and bottom files;

	DATA &halfby._&gend._&top_bot;
	set base_&gend;
	top=(&halfby.75=1 or  &halfby.100=1);
	bot=(&halfby.75=0 and &halfby.100=0);
	if &top_bot=1;

	proc sort; by school_id;
	proc summary; by school_id;
	var year &outcome;
	output out=temp min=minY j1 max=maxY j2 mean=j3 zak;
	run;

	DATA &halfby._&gend._&top_bot;
	merge &halfby._&gend._&top_bot temp;
	by school_id;
	*** We make sure that each school appears in 2000 and 2001;
	if minY=2000 and maxY=2001;
	*** We also drop schools with no students eligible to Bagrut,
	    since these schools fail the school FE estimation;
	run;

********************************************************************;
%mend base_halves;

%base_halves (ls,top);
%base_halves (ls,bot);
%base_halves (p,top);
%base_halves (p,bot);
********************************************************************;
%mend base_gender;

%base_gender (bg);
%base_gender (b);
%base_gender (g);
********************************************************************;
********************************************************************;
********************************************************************;
	*** Regression Macros;
%macro reg_gender (gend);
%macro reg_halves (halfby,top_bot);


*** Dependent Means;
title "Means/ &outcome._&halfby._&gend._&top_bot";
	proc genmod data=&halfby._&gend._&top_bot;
		class school_id;
		model &outcome=/link=id;
		repeated subject=school_id /type=ind;

	ods output GEEEmpPEst=means;
	run;

	data means;
		set means;

		length rowname $ 12;
		length outcome $ 10;
		outcome="&outcome";
		rowname="0_dep_means";
		if Parm="Intercept";
		keep rowname outcome estimate;
		rename estimate=c_&halfby._&gend._&top_bot;
	run;

	data trick;
		length outcome $ 10;
		length rowname $ 12;
		length _NAME_ $ 12;
	run;

	data means;
		set trick means;
	run;

********************************************************************;
	*** OLS - main effect + linear control;

%macro reg(lg,name);
title "OLS/ &outcome._&halfby._&gend._&top_bot._&name";
	ods output GEEEmpPEst=&halfby._&gend._&top_bot._&name;
	ods output ParameterEstimates=&halfby._&gend._&top_bot._&name._noclus;
	ods output NObs=&halfby._&gend._&top_bot._N;
	proc genmod data=&halfby._&gend._&top_bot descending;  
	class school_id_a school_id;
	model &outcome=treated semarab semrel &lg 
	               year01 school_id /link=id; 
	repeated subject=school_id_a /type=ind ;
	run;

	Data &halfby._&gend._&top_bot._&name;
	set &halfby._&gend._&top_bot._&name
		(keep=parm Estimate StdErr);
	length rowname $ 12;
	length outcome $ 10;
	outcome="&outcome";
	rowname="&name";
	if parm="treated";
	keep rowname outcome estimate StdErr;
	run;

	data &halfby._&gend._&top_bot._&name._noclus;
		set &halfby._&gend._&top_bot._&name._noclus
			(keep=StdErr parameter);
		if parameter="treated";
		keep StdErr;
		rename StdErr=NoClusSE;
	run;


title "OLS-ROBUST/ &outcome._&halfby._&gend._&top_bot._&name";
	ods output GEEEmpPEst=&halfby._&gend._&top_bot._&name._robust;
	proc genmod data=&halfby._&gend._&top_bot descending;  
	class student_id school_id;
	model &outcome=treated semarab semrel &lg 
	               year01 school_id /link=id; 
	repeated subject=student_id /type=ind ;
	run;

	Data &halfby._&gend._&top_bot._&name._robust;
	set &halfby._&gend._&top_bot._&name._robust
		(keep=parm StdErr);
	if parm="treated";
	keep StdErr;
	rename StdErr=RobSE;
	run;

	Data &halfby._&gend._&top_bot._&name;
		merge /* NO BYVAR REQUIRED! */
				&halfby._&gend._&top_bot._&name
				&halfby._&gend._&top_bot._&name._robust
				&halfby._&gend._&top_bot._&name._noclus;
	run;

	proc transpose data=&halfby._&gend._&top_bot._&name out=&halfby._&gend._&top_bot._&name
			(rename=(col1=c_&halfby._&gend._&top_bot));
	   by outcome rowname;
	run;
%mend reg;
%reg (ls25--ls100,1_main);
%reg (lagscore,2_linear);

********************************************************************;
	*** Number of Observations;

data &halfby._&gend._&top_bot._N;
set &halfby._&gend._&top_bot._N; 
if label="Number of Observations Used";
		length rowname $ 12;
		length outcome $ 10;
		outcome="&outcome";
		rowname="3_Number_Obs";
keep rowname N outcome;
rename
	N=c_&halfby._&gend._&top_bot;
run;

data space1 space2 space3;
	c_&halfby._&gend._&top_bot=.;
run;

********************************************************************;
	*** Creating a column;

data c_&halfby._&gend._&top_bot;
	set
		means
		space1
		&halfby._&gend._&top_bot._1_main
		space2
		&halfby._&gend._&top_bot._2_linear
		space3
		&halfby._&gend._&top_bot._N;
	drop _label_;
run;



********************************************************************;
%mend reg_halves;

%reg_halves (ls,top);
%reg_halves (ls,bot);
%reg_halves (p,top);
%reg_halves (p,bot);
********************************************************************;
%mend reg_gender;

%reg_gender (bg);
%reg_gender (b);
%reg_gender (g);
********************************************************************;
	*** Creating space columns;

%macro spacecol;
	%do i=1 %to 5;	
		data space_col_&i;
			space_&i=.;
		run;
	%end;
%mend spacecol;
%spacecol;
********************************************************************;
	*** Creating the table for &outcome;

data target.t8_lin_h_&outcome;
	merge /* NO NEED FOR A BYVAR! */
		c_ls_bg_top
		c_ls_bg_bot
		space_col_1

		c_ls_b_top
		c_ls_b_bot
		space_col_2

		c_ls_g_top
		c_ls_g_bot
		space_col_3

		c_p_bg_top
		c_p_bg_bot
		space_col_4

		c_p_b_top
		c_p_b_bot
		space_col_5

		c_p_g_top
		c_p_g_bot
	;
run;
%mend outcome;
********************************************************************;
	*** Creating tables for all outcomes;

%outcome(university);
%outcome(college0);
%outcome(college3);
%outcome(zakaibag);
		
