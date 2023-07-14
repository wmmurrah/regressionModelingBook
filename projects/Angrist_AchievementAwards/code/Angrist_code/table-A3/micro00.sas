/******************************************************************
Date:   25/08/2008
Author: Yannay
Name:   micro00.sas
Data:   All pairs, 2000

This program produces Micro Data estimates for Table A3,
  Panel B (2000), columns (7), (8) and (9)

Note: This program does not compute the BRL standard errors.
*******************************************************************/

options nocenter spool ls=75;
*** When running without the batch file, remove the asterisks to operate "libname";
*libname home  "V:\yannay\bonuses\data";

***************************************************************************;
*** A. The means;
***************************************************************************;
data base;
set home.base00;
if pair=6 then pair=7;
gender="&gender";
if gender="BOYS"   and boy=0 then delete;
if gender="GIRLS"  and boy=1 then delete;
a=1;
proc freq; tables gender;
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

***************************************************************************;
*** B Regressions;
***************************************************************************;

proc means; var zakaibag;

*>> B.1. Sch Covs; 
proc genmod data=base;
class school_id pair;
model zakaibag= treated ls25 ls50 ls75 ls100 semrel semarab/link=id;
repeated subject=school_id /type=ind maxit=1;

*>> B.2. Pair Efects+ Sch controls;
proc genmod data=base;
class school_id pair;
model zakaibag= treated ls25 ls50 ls75 ls100 pair semarab semrel /link=id;
repeated subject=school_id /type=ind maxit=1;

run;
