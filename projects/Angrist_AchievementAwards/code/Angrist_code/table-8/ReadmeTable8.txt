TABLE 8 - Instructions

Table 8 is produced by 4 differenct SAS programs. Each one of them produces 4 sas7bdat tables (SAS data file format) containing the results, one file for each outcome.

table8_linear_half.sas		- Runs the linear model on lagged-score halves
table8_linear_quart.sas		- Runs the linear model on lagged-score quartiles
table8_logit_half.sas		- Runs the logit model on lagged-score halves
table8_logit_quart.sas		- Runs the logitmodel on lagged-score quartiles

Preparation:
(1)	In each of the program, modify the path names according to the instructions at the top.
(2)	Run the programs
(3)	Obtain results from the SAS data files created by the programs.

Names of outcome variables:
Panel A (university)			- university
Panel B (All academics)			- college0
Panel C (Academic, Teachers and PE)	- college3
Panel C (Bagrut)			- zakaibag

Notes on the result tables:

This sequence produces more output than was reported in the paper in a slightly different format, but it includes the reported results.  (The paper reports results for girls only, and for a particular split into top and bottom quartiles, and using a single standard error calculation).  

The results reported in the paper can be found under the columns starting with c_ls_b and c_ls_g, on rows whose rowname is 2_linear. Note that the linear model tables show 3 different kinds of standard errors, where the ones of interest are the robust s.e.s (RobSE), and that the logit model tables show 2 different s.e.s, where the robust s.e.s is the figure reported when available, and the conventional s.e.s are reported whenever the maximum likelihood process has failed to converge.  For example, in order to find the standard error of the linear model when the dependent variable is University on the 3rd quartile of the girls (panel A, column 8, third row), open the result file T8_lin_q_university ("lin" stands for linear, "q" stands for quartiles), look under the column c_ls_g_bot ("g" stands for girls, "bot" stands for bottom to distinct the 3rd quartile from the 4th quartile), and find the figure in the row where the rowname is 2_linear and the "name of former varilable" is RobSE (row 11).