/***************************************
Program:	Table 1 descript stats
Modified:	13.8.2008
By:			Yannay

This program produces columns (1),(2) and (3)
of table 1. The output is a SAS data file

Instructions:
(*)	Change path at the two proc printto procedures
	at the top of the program to the folder in which
	the list and log files should be written to
(*) Change path in the libname declaration to the
	folder where the data file is held, and the
	output tables should be writtern to
****************************************/

PROC PRINTTO
PRINT=	"W:\Student bonuses\Yannay Aug 08\logs\table1.lst"
		NEW;
PROC PRINTTO
LOG=	"W:\Student bonuses\Yannay Aug 08\logs\table1.log"
		NEW;
run;

options nocenter nodate spool ls=80 mergeNoBy=warn;

libname home  "W:\Student bonuses\Yannay Aug 08\data";



*** Adding indications for missing demographic
	variables in the bases;
%macro addmiss(year);
	data base&year;
		set home.base&year;
		miss_av=org_educav in (.,0,88,99);
		miss_em=org_educem in (.,0,88,99);
		miss_ah=org_m_ahim in (.,0,88,99);
	run;
%mend addmiss;
%addmiss (00);
%addmiss (01);



/*Descriptive Stats for Table 1*/
%macro filedesc(year);
title"base &year ALL";
run;
data base&year;
	set base&year;
	std_educ_av = .;
	std_educ_em = .;
	std_ahim = .;
	std_lagscore = .;

run;
proc summary data=base&year;
var zakaibag semarab semrel educav std_educ_av educem std_educ_em m_ahim std_ahim
ole5 lagscore std_lagscore miss_av miss_em miss_ah ; 
where boy in (0,1);
output out=base&year.allsum; 
run;
title"base &year Boys";
run;
proc summary data=base&year;
where boy=1;
var zakaibag semarab semrel educav std_educ_av educem std_educ_em m_ahim std_ahim
ole5 lagscore std_lagscore miss_av  miss_em  miss_ah ; 
output out=base&year.boyssum;
run;
title"base &year Girls";
proc summary data=base&year;
where boy=0;
var zakaibag semarab semrel educav std_educ_av educem std_educ_em m_ahim std_ahim
ole5 lagscore std_lagscore miss_av  miss_em  miss_ah ; 
output out=base&year.girlssum;
run;
title" ";
run;

data base&year.allsum;
	set base&year.allsum;;
	if _STAT_ in ("MEAN", "STD");
	col = 1;
run;
data base&year.boyssum;
	set base&year.boyssum;
	if _STAT_ in ("MEAN", "STD");
	col = 2;
run;
data base&year.girlssum;
	set base&year.girlssum;
	if _STAT_ in ("MEAN", "STD");
	col = 3;
run;


%mend filedesc;

%filedesc (00);
%filedesc (01);

data firsthalf;
	set Base00allsum Base00boyssum Base00girlssum;
run;

data secondhalf;
	set Base01allsum Base01boyssum Base01girlssum;
run;

proc transpose data=firsthalf 	out	= firsthalfT;  	run;
proc transpose data=secondhalf 	out	= secondthalfT; run;

data firsthalfT;
	set firsthalfT;
	lagcol2  = lag(col2);
	lagcol4  = lag(col4);
	lagcol6  = lag(col6);
	
	if col1  = . then col1  = lagcol2;
	if col3  = . then col3  = lagcol4;
	if col5  = . then col5  = lagcol6;
	drop col2 col4 col6 lagcol:;
run;

data secondthalfT;
	set secondthalfT;
	lagcol2  = lag(col2);
	lagcol4  = lag(col4);
	lagcol6  = lag(col6);
	
	if col1  = . then col1  = lagcol2;
	if col3  = . then col3  = lagcol4;
	if col5  = . then col5  = lagcol6;
	drop col2 col4 col6 lagcol:;
run;

data home.table1;
	set  secondthalfT firsthalfT;
run;

PROC PRINTTO PRINT=PRINT LOG=LOG ;
RUN;
