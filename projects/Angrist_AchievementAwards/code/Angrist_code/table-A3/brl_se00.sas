/********************************************************************
Date:   24/08/2008
Author: Yannay
Name: brl_se00.sas
Data: All pairs, 2000 data

This program produces the BRL standard errors
  for Table A3, columns (7), (8) and (9)

Note: In the output you should look for 'SE_BRL' value.
********************************************************************/

options nocenter spool ls=75;
*** When running without the batch file, remove the asterisks to operate "libname" and "include";
*libname home  "V:\yannay\bonuses\data";
* Calling for two macros: brl.sas & dummy.sas;
*%include "V:\yannay\bonuses\Revision July 07\Table 2 - Estimates using Micro Data - 2Step\BRL.sas";
*%include "V:\yannay\bonuses\Revision July 07\Table 2 - Estimates using Micro Data - 2Step\dummy.sas";

data base;
set home.base00;
if pair=6 then pair=7;
gender="&gender";
if gender="BOYS"   and boy=0 then delete;
if gender="GIRLS"  and boy=1 then delete;
a=1;
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


*>> School Covariates;
%hreg(zakaibag, treated semarab semrel ls25 ls50 ls75,base,cluster=school_id); 

*>> School Covariates + Pair;
%dummy(data=base,var=pair);
%hreg(zakaibag, treated semarab semrel ls25 ls50 ls75 D_1 D_2 D_3 D_4 D_5 
      D_7 D_8 D_9 D_10 D_11 D_12 D_13 D_14 D_15 D_16 D_17 D_18 D_19,base,
      cluster=school_id);

run;
