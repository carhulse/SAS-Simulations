/*STAT 466 Assignment 2*/
/*Carly Hulse 20034006*/
/*Question 1*/
TITLE 'By Carly Hulse';

/*Question 2*/
TITLE2 'Question 2';
/*(a)*/
DATA samples;
	CALL streaminit(123);
	DO sample=1 to 10;
		DO n=1 to 10;
			/*the inner loop takes on values 1 to n*/
			/*Creates 10, 000 samples of size 10*/
			normaldistn=rand('NORMAL', 2, 2);
			uniformdistn=rand('UNIFORM', 1, 3);
			exponential=rand('EXPONENTIAL', 2);
			bernoulli=rand('BERNOULLI', (2/3))*3;
			poisson=rand('POISSON', 2);
			chisquare=rand('CHISQUARE', 2);
			cauchy=rand('CAUCHY')+2;
			multinom=rand('TABLE', .4, .3, .2, .1);
			t1=rand('T', 1)+2;
			t2=rand('T', 2)+2;
			t3=rand('T', 3)+2;
			OUTPUT;
		END;
	END;
RUN;

/*(b)*/
PROC UNIVARIATE DATA=samples NORMAL PLOTS;
	VAR normaldistn uniformdistn exponential bernoulli poisson chisquare cauchy 
		multinom t1 t2 t3;
	HISTOGRAM /normal kernel;
RUN;

/*(c)*/
PROC MEANS NOPRINT DATA=samples LCLM UCLM MEAN;
	BY sample;
	VAR normaldistn uniformdistn exponential bernoulli poisson chisquare cauchy 
		multinom t1 t2 t3;
	OUTPUT out=averages(drop=_type_ _freq_) MEAN=lclm=uclm= /AUTONAME;
RUN;

PROC PRINT DATA=averages;
RUN;

/*(d)*/
PROC UNIVARIATE DATA=averages NORMAL PLOTS;
	VAR normaldistn_mean uniformdistn_mean exponential_mean bernoulli_mean 
		poisson_mean chisquare_mean cauchy_mean multinom_mean t1_mean t2_mean 
		t3_mean /*normaldistn_lclm uniformdistn_lclm exponential_lclm
		bernoulli_lclm poisson_lclm chisquare_lclm cauchy_lclm multinom_lclm t1_lclm
		t2_lclm t3_lclm normaldistn_uclm uniformdistn_uclm exponential_uclm
		bernoulli_uclm poisson_uclm chisquare_uclm cauchy_uclm multinom_uclm t1_uclm
		t2_uclm t3_uclm*/;
	HISTOGRAM /normal kernel;
RUN;

/*Normal, uniform, exponential, chisquare, cauchy, multinomial, t2, and t3 seem to have means that appear approximately normal.
T3: By observation, the distribution of the t3 mean follows the normal reference curve, suggesting that the mean is approximately normal. Mu≅2, which is expected considering we centered the distribution around 2.
T2: By observation, we see that the distribution of the t2 mean shows a fair amount of resemblance to that of the normal reference line. As well, sigma≅1, as expected for a normal distribution.
Multinomial: Based on observation, the multinomial mean approximation looks very close to normal. Mu is very close to 2 and sigma very close to 0.5. Based on observation, we can see that a lower Kolmogorov-Smirnov D statistic means a better fit. The Kolmogorov-Smirnov D statistic is quite small, 0.164, suggesting suitability of our fit.
Cauchy: By observation, the mean approximation does an alright job of following the normal reference curve. The estimated and observed quantiles are also fairly similar.
Chisquare: Chisquare is very similar to the multinomial approximation. Mu is very close to 2, sigma very close to 0.5 and the Kolmogorov-Smirnov D statistic is quite small at 0.120, indicating that our fit is a suitable model..
Exponential: Observation of the fit suggests a somewhat decent approximation. Mu≅2 and sigma≅1, however the D statistic for KS is fairly large.
Uniform: The fit observed doesn’t look too far off from normal. Mu≅2 which is good, but sigma is much lower than desired.
Normal: The normal mean approximation is centered quite well, but the magnitude is off. The Kolmogorov-Smirnov D statistic is relatively low, 0.182, but not the best we’ve seen.
Based on observation, we can see that a lower Kolmogorov-Smirnov D statistic means a better fit. The Cauchy fit gives D=0.125 which is relatively low, indicating that our fit is a suitable model.
*/

/*(e)*/
DATA averages;
	SET averages;
	normaltruemean=(normaldistn_lclm<2 & normaldistn_uclm>2);
	uniformtruemean=(uniformdistn_lclm<2 & uniformdistn_uclm>2);
	exponentialtruemean=(exponential_lclm<2 & exponential_uclm>2);
	bernoullitruemean=(bernoulli_lclm<2 & bernoulli_uclm>2);
	poissontruemean=(poisson_lclm<2 & poisson_uclm>2);
	chisquaretruemean=(chisquare_lclm<2 & chisquare_uclm>2);
	cauchytruemean=(cauchy_lclm<2 & cauchy_uclm>2);
	multinomtruemean=(multinom_lclm<2 & multinom_uclm>2);
	t1truemean=(t1_lclm<2 & t1_uclm>2);
	t2truemean=(t2_lclm<2 & t2_uclm>2);
	t3truemean=(t3_lclm<2 & t3_uclm>2);
RUN;

PROC PRINT;
	VAR normaltruemean--t3truemean;
RUN;

/*(f)*/
PROC FREQ DATA=averages;
	table normaltruemean / nocum;
	table uniformtruemean / nocum;
	table exponentialtruemean / nocum;
	table bernoullitruemean / nocum;
	table poissontruemean / nocum;
	table chisquaretruemean / nocum;
	table cauchytruemean / nocum;
	table multinomtruemean / nocum;
	table t1truemean / nocum;
	table t2truemean / nocum;
	table t3truemean / nocum;
RUN;

PROC PRINT;
RUN;
/*If the coverage rate rounds to between 0.94 and 0.96, we consider the coverage rate nominal. None of the distibutions have nominal coverage rates with sample size 10.*/

/*(g)*/
/*A 95% confidence interval means that if you collect a large number of samples and construct the corresponding confidence intervals, then about 95% of the intervals will contain (or "cover") the parameter.
By the Law of Large Numbers, the average of the results obtained from a large number of trials should be close to the expected value and should come closer as more trials are performed. 
Thus, for a sample size of 100, the distributions should all have nominal or conservative coverage rates.*/

/*Question 3*/
PROC IMPORT OUT=ExcerciseRTC 
		DATAFILE='/folders/myfolders/STAT466/Assignment 2/ExerciseRCT.xlsx' DBMS=xlsx 
		REPLACE;
	GETNAMES=YES;
RUN;

TITLE2 "Question 3";

/*(a)*/
DATA Changes;
	SET ExcerciseRTC;
	Baseline=VO2maxV00;
	Changew4=VO2maxV04-VO2maxV00;
	Changew8=VO2maxV08-VO2maxV00;
	Changew16=VO2maxV16-VO2maxV00;
	Changew24=VO2maxV24-VO2maxV00;
	LABEL Changew4="Change at 4 weeks" Changew8="Change at 8 weeks" 
		Changew16="Change at 16 weeks" Changew24="Change at 24 weeks";
RUN;

/*(b)*/
PROC SORT Data=Changes;
	BY Groupnum DESCENDING Sex;
RUN;

PROC TABULATE DATA=Changes;
	CLASS sex/DESCENDING;
	CLASS group/ORDER=data;
	VAR Baseline Changew:;
	TABLE ALL='Overall'*(Baseline Changew:) sex='By Sex'*(Baseline Changew:), 
		ALL='Total'*(N) (Group='Treatment Group')*(N Mean StDerr='SE')/ 
		MISSTEXT=[Label="NA"] BOX=[Label='VO2max (L/min)'];
	FOOTNOTE 'SE-Standard Error, LALI-low amount at low intensity,
   HALI-low amount at low intensity, HAHI-high amount at high intensity, NA-not assessed';
RUN;

/*(c)*/
PROC SGPANEL DATA=Changes;
	PANELBY sex;
	ROWAXIS GRID;
	REFLINE 0 /LINEATTRS=(pattern=ShortDashDot color=red);
	VBOX Changew24 / GROUP=group;
	LABEL Changew24='24-week change in VO2max (L/min)';
RUN;

/*Question 4*/
TITLE2 'Question 4';
DATA Fibonacci (drop=i);
	ARRAY F[20];

	DO i=1 TO 20;

		IF i=1 THEN
			F[i]=0;
		ElSE If 1<i<=3 Then
			F[i]=1;
		ELSE
			F[i]=F[i-1]+F[i-2];
	END;
	ARRAY G[20];

	DO i=1 To 20;

		IF i<=2 Then
			G[i]=.;
		ELSE
			G[i]=F[i]/F[i-1];
	END;
RUN;

PROC PRINT;
RUN;