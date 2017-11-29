# StatisticalCalculator
Building Statistical Calculator using R.


# Group : Anjali and Vipin

#********************************************* Assignment-3 *********************************************
#												READ ME 	

_____________
I. File List:
-------------
MyStatsCalci.r


_____________
II. Approach:
-------------
Dividing the functionality of the program into separate modules. 


____________________________________________
III. Reference for R Tutorials and Formulas:
--------------------------------------------
a. R pdfs.
b. R from Tutorials Point.
c. R Documentation for specific functions descriptions.
d. John E. Freund's Mathematical Statistics with Applications by Irwin Miller.


__________________
IV. Software Used:
------------------
a. R-Studio (3.4.1)
b. R Console (3.4.1)
c. Sublime Text


_______________
Function used : 
---------------

___________________
Inbuilt Functions :
-------------------

a. sqrt(...)                : Computes the square root of the specified value.
b. sort(...)                : Sort a vector or factor into ascending or descending order.
c. cat(...)                 : Outputs the objects, concatenating the representations.
d. readline(...)            : Reads a line from the terminal (in interactive use).
e. install.packages(...)	: To install packages.
f. library(...)			    : To load packages.
g. as.numeric(...)			: Creates or coerces objects of type "numeric".
h. length(...)              : Get or set length of vectors (including lists) and factors.
i. fread(...)    			: To read the file.
j. as.integer(...)          : Converts to an integer value.
k. exp(...)                 : Computes the exponential of the given value.
l. formatC(...)             : Formatting numbers individually and flexibly.
m. qchisq(...)              : Density, distribution function, quantile function and random generation for chi-squared distribution.
n. qt(...)                  : Density, distribution function, quantile function and random generation for the t distribution.
o. qnorm(...)               : Density, distribution function, quantile function and random generation for the normal distribution.
p. pnorm(...)               : Density, distribution function, quantile function and random generation for the normal distribution.
q. hist(...) 				: Computes a histogram of the given data values.
r. plot(...)				: Generic function for plotting of R objects. 
s. barplot(...)				: Creates a bar plot with vertical or horizontal bars.
t. pie(...) 				: Draw a pie chart.
u. boxplot(...) 			: Produce box-and-whisker plot(s) of the given (grouped) values.
v. qqplot(...) 				: QQ plot of observed P-values vs expected P-values.
w. qqnorm(...) 				: Produces a normal QQ plot of the values in y. 
x. qqline(...) 				: Adds a line to a “theoretical” quantile-quantile plot which passes through the probs quantiles.
y. stem(...) 				: Produces a stem-and-leaf plot of the values in x.
z. pareto.chart(...) 		: Plot a Pareto chart.

	
	*(...) depicts number of arguments.
	

________________________
User Defined Functions :
------------------------

1. MAIN : Executing function.
2. USERINPUT : For takeing command line user input.
3. READFROMCSV : For taking input from file.
4. INPUT : For selecting data entry method (either by USERINPUT or READFROMCSV).

________________________________
Module 1 (Descriptive Analysis):
--------------------------------

1. MEAN 		: For computation of arithmetic mean.
2. VARIANCE2 	: For computation of population variance.
3. VARIANCE2 : For computation of sample variance.
4. SD : For computation of standard deviation.
5. MEDIAN : For computation of median.
6. MODE : For finding number with maximum frequency.
7. MEANABSDEV : For computation of mean absolute deviation.
8. MINIMUM : For finding number with minimum value. 
9. MAXIMUM : For finding number with maximum value.
10. RANGE : For finding range (difference of maximum and minmum value).
11. QUARTILES : For finding the quartiles (q1, q2, q3).
12. IQR : For finding the Inter quartile range.
13. SKEWNESS : For finding skewness(g1).
14. KURTOSIS : For finding kurtosis(g2).
15. MOMENTS : For finding central moments.
16. DESCRIPTIVEANALYSIS : Executing function for Descriptive Analysis.

_______________________________
Module 2 (Predictive Analysis):
-------------------------------

1. CORRELATION : For finding Karl Pearson Coefficient of correlation.
2. MULTREG : For finding line of multiple linear regression for three variables.
3. PREDANALYSIS : Executing function for Predictive Analysis.

________________________________
Module 3 (Probability Analysis):
--------------------------------

1. FACTORIAL : For finding factorial of a number.
2. PERMUTATION : For finding nPr.
3. COMBINATION : For finding nCr.
4. BASICPROBABILITY : For finding Basic probability.
5. BAYESTHEOREM : For finding P(Ai|B) using P(B|Ai) and P(Ai).
6. PROBABILITYANALYSIS : Executing function for Probability Analysis.

___________________________________________
Module 4 (Discrete Distribution Functions):
-------------------------------------------

1. UNIFORMDISTRIBUTION : For finding probability of a random variable following uniform distribution.
2. BERNOULLI : For finding probability of a random variable following bernoulli distribution.
3. BINOMIALDISTRIBUTION : For finding probability of a random variable following binomial distribution.
4. GEOMETRIC : For finding probability of a random variable following geometric distribution.
5. HYPERGEOMETRIC : For finding probability of a random variable following hypergeometric distribution.
6. NEGATIVEBIN : For finding probability of a random variable following negative binomial distribution.
7. POISSON : For finding probability of a random variable following poisson distribution.
8. MULTINOMIAL : For finding probability of a random variable following multinomial distribution.
9. MULTIHYPGEO : For finding probability of a random variable following multi hypergeometric distribution.
10. DISCRETEDISTRIBUTION : Executing function for Discrete Distribution Functions.

_____________________________________________
Module 5 (Continuous Distribution Functions):
---------------------------------------------

1. UNICONTINOUS : For finding probability of a random variable following uniform continuous distribution. 
2. NORMALDIST : For finding probability of a random variable following normal distribution.
3. GAMMADIST : For finding probability of a random variable following gamma distribution.
4. CONTINOUSDISTRIBUTION : Executing function for Continuous Distribution Functions.

______________________________________________
Module 6 (Sample Distribution Test Statistic):
----------------------------------------------

1. CHISQDIST : Density, distribution function, quantile function and random generation for the chi-squared (chi^2) distribution.
2. TDIST : Density, distribution function, quantile function and random generation for the t distribution.
3. FDIST : Density, distribution function, quantile function and random generation for the f distribution.
4. ZDIST : Density, distribution function, quantile function and random generation for the z distribution.
5. SAMPLEDISTTEST : Executing function for Sample Distribution Test Statistic.

_______________________________
Module 7 (Interval Estimation):
-------------------------------

1. INTERVALESTIMATION : Executing function for Interval Estimation.

___________________________________
Module 8 (Non-Parametric Analysis):
-----------------------------------

1. SIGNTEST : For testing hypothesis on basis of sign given by null hypothesis.
2. NONPARAMANALYSIS : Executing function for Non-Parametric Analysis.

__________________________
Module 9 (Visualizations):
--------------------------

1. HISTOGRAM : Computes a histogram of the given data values.
2. LINEGRAPH : Computes a line graph of the given data values.
3. BARGRAPH : Computes a bar graph of the given data values.
4. PIECHART : Computes a pie chart of the given data values.
5. SCATTERPLOT : Computes a scatter plot of the given data values.
6. BOXPLOT : Computes a box plot of the given data values.
7. QQPLOT : Computes a q-q plot of the given data values.
8. STEMLEAFPLOT : Computes a stem leaf plot of the given data values.
9. PARETOCHART : Computes a pareto chart of the given data values.
10. VISUALIZATIONS : Executing function for Visualizations.

