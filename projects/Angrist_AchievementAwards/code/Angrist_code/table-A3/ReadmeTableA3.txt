TABLE A3 – Instructions

Table A3 is produced by 8 different programs that are called by the batch file BatchTableA3.sas. After completing the preliminary listed below, run the batch file in SAS and obtain the results from the SAS listing files.

Preparation:
(1)	Prepare a parent folder that includes the following SAS programs:
	BatchTableA3
	brl
	dummy
	2step00
	2step01
	micro00
	micro01
	brl_se00
	brl_se01
(2)	Create 3 subfolders named ALL, GIRLS and BOYS
(3)	Open the batch file on SAS and make changes to the paths according to the instructions at the top of the program

At this point you should be able to run the batch file, obtaining results from the corresponding gender subfolder. 

Notes:

Programs starting with "2step" produce both unweighted and weighted results using the two-steps procedure (columns 1-6). Programs starting with "micro" produce the coefficients and the clustered standard errors for the micro data regressions (columns (7)-(9)). Programs starting with brl_se produce the brl standard errors for the micro data regressions.

Note that in order to obtain the heteroscedasticity consistent standard errors in columns (1)-(6) you have to compute the square root of the appropriate cells in the reported robust variance-covariance matrices.

