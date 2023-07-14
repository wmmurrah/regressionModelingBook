/******************************************************************
Date:   25/08/2008
Author: Yannay
Name:   2step01.sas
Data:   All pairs, 2001

MODIFIED VERSION TO RUN ON PC

This program produces the Two-Step Procedure for Table A3 as follows:
     A.  Creating variables for first step regression
     B.  First step regression
     C.1 Second step regressions - unweighted
     C.2 Second step regressions - weighted

Note: In order to calculate heteroscedasticity-consistent standard
      errors, you should take the quare root of 'treated x treated' 
      element in the covariance matrix printed after OLS estimates.
*******************************************************************/

options nocenter spool ls=75;
*** When running without the batch file, remove the asterisks to operate "libname";
*libname home  "V:\yannay\bonuses\data";

/****************************************************
A. Creating variables for first step regression
*****************************************************/
data base;
set home.base01;
if pair=6 then pair=7;
gender="&gender";
if gender="BOYS"   and boy=0 then delete;
if gender="GIRLS"  and boy=1 then delete;
a=1;
proc means; var zakaibag;
run;
proc univariate noprint;
var lagscore;
output out=temp pctlpts=25 to 100 by 25 pctlpre=count;

data temp;
set temp;
a=1;

proc sort data=base; by a;
proc sort data=temp; by a;

data base;
merge base temp;
by a;
ls25=0; ls50=0; ls75=0; ls100=0;
if      lagscore<count25 then ls25=1;
else if lagscore<count50 then ls50=1;
else if lagscore<count75 then ls75=1;
else                          ls100=1;

array d (39) d1-d39;
do j=1 to 39;
  d(j)=(school_id=j);
  end;
run;

/*************************
B. First step regression
*************************/

proc reg data=base outest=temp noprint;
model zakaibag= d1-d39
                ls25 ls50 ls75 ls100 /noint;
run;

data temp;
set temp;
a=1;
drop _TYPE_;
proc sort; by a;
run;

data base;
set base;
proc sort; by school_id;
proc summary; by school_id;
var a;
id treated pair semrel semarab; 
output out=temp2 mean=a n=numstud;
proc sort; by a;
run;

data base;
merge temp2 temp;
by a;
array d (39) d1-d39;
do j=1 to 39;
  if school_id=j then dd=d(j);
end;


array pairs (19) pair1-pair19;
do j=1 to 19;
  pairs(j)=(pair=j);
  end;

/***************************************
C.1 Second step regressions - unweighted
***************************************/

title "Unweighted - covs";
run;
proc reg;
model dd = treated semrel semarab /ACOV;
run;

title "Unweighted - covs pairs";
run;
proc reg;
model dd = treated semrel semarab pair1-pair19 /ACOV;
run;

/**************************************
C.2 Second step regressions - weighted
**************************************/

title "Weighted - covs";
run;
proc reg;
weight numstud;
model dd = treated semrel semarab /ACOV;
run;

title "Weighted - covs pairs";
run;
proc reg;
weight numstud;
model dd = treated semrel semarab pair1-pair19 /ACOV;
run;

title "";
run;

