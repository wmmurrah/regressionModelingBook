/***********************************************
Date:   24/08/2008
Author: Yannay
Name: batch table A3.sas
Data: BASE00 BASE01

THIS IS A BATCH PROGRAM THAT RUNS ALL THE PROGRAMS THAT
CREATE TABLE A3

It sends the log and output files to 3 sub-directories
	For each of 3 Genders (all/boys/girls)
	For each of 2 years (2000/2001)
	It runs 3 programs
		(micro - creates the estimates and cinventional S.E.s, cols 7,8;
		 2step - creates the all the estimates and S.E.s for cols 1-6;
		 brl_se - creates the BRL S.E.s for cols 7,8)

INSTRUCTIONS:
(*)	Set the path of the "home" library to the folder where
	the data files are located (line starting with "libname")
(*)	Set the paths of the BRL and DUMMY macros to their correct
	location (following 2 lines starting with "%include")
(*)	Set the folder paths where the log and list files should be written
	to their correct location (lines starting with PRINT= and LOG=).
	In the folder there should be 3 sub-folders named ALL, GIRLS and BOYS,
	which are represented by the "&gender" variable. Make sure not to
	overwrite the final parts of the paths (\&gender\&filename._&gender..lst
	and \&gender\&filename._&gender..log)!
(*) Set the path of the folder where the programs are kept
	(line starting with %include under "running the program")
	Make sure not to overwrite the final part of the path
	(\&filename..sas)
(*) The outcomes will be written into the list files produced
	by the programs: 
	(**)The outcomes of columns (1)-(6) are produced by the
		"2step" programs
	(**)The coefficients and s.e.s of columns (7)-(9)
		are produced by the "micro" programs"
	(**)The BRL s.e.s are produced by the BRL_se programs
		 
***********************************************/

options nocenter spool ls=75;
libname home  "W:\Student bonuses\Yannay Aug 08\data";
%include "W:\Student bonuses\Yannay Aug 08\Code\table A3\BRL.sas";
%include "W:\Student bonuses\Yannay Aug 08\Code\table A3\dummy.sas";

/***********************************************
Defining the Macro
***********************************************/;

%macro runprog (gender,filename);

*** opening output and log file;
PROC PRINTTO
PRINT=	"W:\Student bonuses\Yannay Aug 08\Code\table A3\&gender\&filename._&gender..lst"
		NEW;
PROC PRINTTO
LOG=	"W:\Student bonuses\Yannay Aug 08\Code\table A3\&gender\&filename._&gender..log"
		NEW;
run;

*** running the program;
%include "W:\Student bonuses\Yannay Aug 08\Code\table A3\&filename..sas";
run;

PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;
%mend runprog;

/***********************************************
Running the set of programs
***********************************************/;
%macro rungend (gend);
	%macro runtype (type);
			%runprog (&gend,&type.00);
			%runprog (&gend,&type.01);
	%mend runtype;

	%runtype (micro);
	%runtype (brl_se);
	%runtype (2step);

%mend rungend;
%rungend (All);
%rungend (BOYS);
%rungend (GIRLS);
run;

