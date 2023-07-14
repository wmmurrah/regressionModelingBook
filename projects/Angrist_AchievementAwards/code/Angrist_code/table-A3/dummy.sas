
*macro to create dummy variables;

%macro dummy( 
   data=_last_ ,    /* name of input dataset                  */
   out=&data,       /* name of output dataset                 */
   var= ,           /* variable(s) to be dummied              */
   base=_last_,     /* base category                          */
   prefix = D_,     /* prefix for dummy variable names        */
   format =,        /* format used to categorize variable     */
   name  = VAL,     /* VAL: variable names are D_value        */
   fullrank=1       /* Eliminate dummy for baseline category? */
   );

	%let abort = 0;
   %if (%length(&var) = 0) %then %do;
       %put ERROR: DUMMY: VAR= must be specified;
		 %let abort=1;
       %goto done;
       %end;

%let base = %upcase(&base);
%let name = %upcase(&name);

%if %upcase(&data) = _LAST_ %then %let data = &syslast;
%if %upcase(&data) = _NULL_ %then %do;
	%put ERROR: There is no default input data set (_LAST_ is _NULL_);
	%let abort=1;
	%goto DONE;
	%end;
	
options nonotes;

%*-- Initialize output data set;
%if &out ^= &data %then %do;
	data &out;
		set &data;
	%end;
	
%let prefix = %upcase(&prefix);

%*-- j indexes variables, vari is the current variable name;
%local j vari;
%let j=1;
%*-- Find the current variable name;
%let vari= %scan(&var,    &j, %str( ));

%******************************************************************;
%*-- Loop over variables; 
%******************************************************************;
%do %while(&vari ^= );

	%*-- Find the current prefix for dummies;
	%let pre = %scan(&prefix, &j, %str( ));
	%if &pre = VARNAME | &pre = %then %let pre=&vari._;
	%*-- Keyword BLANK for prefix indicates no prefix;
	%if &pre=BLANK %then %let pre=;

	%*-- Find the current base for dummies;
	%let baseval = %scan(&base, &j, %str( ));
	%if &baseval = %then %let baseval=_LAST_;

	%*-- Find the current format for dummies;
	%let fmt = %scan(&format, &j, %str( ));

*-- determine values of variable to be dummied;
proc summary data = &out nway ;
     class &vari ;
     %if %length(&fmt) gt 0 %then %do;
	  		%*-- Make sure format name includes a '.';
        %if "%substr(&fmt, %length(&fmt))" ne "." 
		  		%then %let fmt = &fmt..;
        format &vari &fmt;
     %end;
     output out = _cats_ ( keep = &vari ) ;
	%if &syserr > 4 %then %let abort=1; 
	%if &abort %then %goto DONE;

	%if &fullrank %then %do;
	*-- Eliminate the base category;
	data _cats_;
		set _cats_ end=_eof_;
		%if &baseval = _FIRST_ | &baseval = LOW 
			%then %str( if _n_ = 1 then delete;);
		%else %if &baseval = _LAST_ | &baseval = HIGH
			%then %str( if _eof_ then delete;);
		%else %str(if &vari = &baseval then delete;);
	run;
	%end;

data _null_ ;
 set _cats_ nobs = numvals ;

 if _n_ = 1 then do;
	%*-- If there are no non-baseline values - abort macro; 
	call symput('abort',trim( left( put( (numvals=0), best. ) ) ) ) ;
	%*-- Place number of dummies into macro variable num;
	call symput( 'num', trim( left( put( numvals, best. ) ) ) ) ;
	end;

	%*-- Number the values, place in macro variables c##; 
	%if %length(&fmt) gt 0 %then %do;
		call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim(left(put(&vari,&fmt)) ) );
	%end;
	%else %do;
	call symput ( 'c' || trim ( left ( put ( _n_,     best. ) ) ),
								trim ( left ( &vari ) ) ) ;
	%end;
run ;
%if &syserr > 4 %then %let abort=1; %if &abort %then %goto DONE;

%******************************************************************;
%* Create list of dummy variables for the j-th input variable;
%******************************************************************;

%if "&name" = "VAL" %then %do ;
	%*-- Names by variable value;
	%let vl&j =; 
	%do k=1 %to &num;
		%if &sysver < 7 & %length(&pre&&c&k) > 8 %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto DONE;
			%end;
		%let vl&j = &&vl&j  &pre&&c&k;
		%end; ;
%*put vl&j = &&&vl&j;

data &out;
	set &out ;
	
	array __d ( &num ) %do k=1 %to &num ;	&pre&&c&k
							%end ; ;
	%put DUMMY: Creating dummy variables &pre&&c1 .. &pre&&c&num for &vari;
	%end ;

%else %do ;
	%*-- Numeric suffix names;
	%let vl&j =; 
	%do k=1 %to &num; 
		%if &sysver < 7 & %length(&pre.&k) > 8 %then %do;
			%put ERROR: Cannot generate names longer than 8 characters;
			%let abort=1;
			%goto endloop;
			%end;
		%let vl&j = &&vl&j  &pre.&k;
		%end; ;
%*put vl&j = &&&vl&j;
run;
	
%******************************************************************;
%* Assign values to the dummy variables for the j-th input variable;
%******************************************************************;
data &out  ( rename = ( %do k=1 %to &num ;
						d&k = &pre.&k
						%end ; ) ) ;
	set &out ;
	%put DUMMY: Creating dummy variables &pre.1 .. &pre.&num;
	array __d ( &num ) d1-d&num ;
	%end ;

	%*---------------------------------------------------------;
   %*   Handle missing values (for V7+ only);
	%*-- (to do this for V6.12 requires separate processing for
	      character and numeric variables);
	%*---------------------------------------------------------;
	%if &sysver >= 7 %then %do;
     if missing(&vari) then do;
	  	 do j=1 to &num;
        __d(j)=.;
		  end;
		return;
     end;
	%end;

	%*---------------------------------------------------------;
   %*   Assign values to dummy variables;
	%*---------------------------------------------------------;
	drop j;
	do j = 1 to &num ; /* initialize to 0 */
		__d(j) = 0 ;
	end ;


     %if %length(&fmt) eq 0 %then %do;
     %*-- Case 1:  No format;
        if &vari = "&c1" then __d ( 1 ) = 1 ;  /* create dummies */
        %do i = 2 %to &num ;       
           else if &vari="&&c&i" then __d ( &i ) = 1 ;
        %end;
     %end;

     %else %do;
     %*-- Case 2:  with format;
        if put(&vari,&fmt) = "&c1" then __d ( 1 ) = 1 ;
        %do i = 2 %to &num ;       
           else if put(&vari,&fmt)="&&c&i" then __d ( &i ) = 1;
        %end;
     %end;
run ;

%*-- Find the next variable;

%let j=%eval(&j+1);
%let vari = %scan(&var, &j, %str( ));

%*put End of loop(&i): vari = &vari  pre=&pre;
%endloop:
%end;  /* %do %while */

%done:
%if &abort %then %put ERROR: The DUMMY macro ended abnormally.;
options notes;
%mend dummy ;

