---
title: "Nonparametic Homework 4"
author: "Johann Piedras"
date: "2019-05-17"
output: html_document

tags: [Statistics, Non Parametic]
excerpt: "Figuring out how to post my R/HTML/RMD file onto git"
---

```{r , echo=FALSE, include=FALSE}
library(readr)
library(BSDA)
library(gtools)
library(coin)
library(FSA)
library(dplyr)
library(stringr)
```
<br/>
<h2> Question 1 </h2>

 The opinions of doctors and nurses were taken to see if they preferred an old procedure, a new procedure, or had no preference between the two. The results are contained online in the file procedure.csv.
<br/>
***(1a)***
<br/>
<strong> Find and report the 2 x 3 contingency table. </strong>
<br/>
```{r , echo=FALSE}
procedure = read.csv("procedure.csv", header = TRUE)
```

```{r , echo=FALSE}
zee.table = table(procedure)
zee.table
```

```{r , echo=FALSE}
ni. = rowSums(zee.table) # 23 and 28
n.j = colSums(zee.table) # new = 23 ,none = 5 , old = 23
```
<br/>

***(1b)***
<br/>
<strong> State the null and alternative hypothesis in words. </strong>
<br/>
$H_o: Variables \ Prof \ and \ Opinion \ Are \ Independent$
<br/>
$H_a: Variables \ Prof \ and \ Opinion \ Are \ Dependent$
<br/>

***(1c)***
<br/>
<strong> Find the value of $\chi^2_S,OBS$</strong>
<br/>
```{r , echo=FALSE}
the.test = chisq.test(zee.table,correct = FALSE)
eij = the.test$expected
chi.sq.obs = as.numeric(the.test$statistic)

### The Chi Square Statistics is  
chi.sq.obs
```
<br/>

***(1d)***
<br/>
<strong> Use R to find the permutation based p-value, based on R = 3000 random shufflings </strong>
<br/>
```{r include=FALSE}
# Prof = Hair
# Opinion = Eyes
R = 3000
r.perms = sapply(1:R,function(i){
  perm.data = procedure
  perm.data$Opinion = sample(perm.data$Opinion,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})
perm.pval = mean(r.perms >= chi.sq.obs)
perm.pval
```

Permutation P-Value is 011, or rounded up:
```{r, echo=FALSE}
perm.pval = mean(r.perms >= chi.sq.obs)
perm.pval
```

<br/>
***(1e)***
<br/>
<strong> State your conclusion in terms of the problem if $\alpha$ = 0.05. </strong>
<br/>
Because our P-Value is .011, at alpha of .05<br/>
.011 < .05 is True, therefore reject the null hypothesis. The two variables are dependent, according to the alternative hypothesis.  
<br/>

<h2> Question 2</h2>
<br/>
***(2a)***
<br/>
<strong> Find the Z values for comparing the row values for each column (i.e, the opinion given they were a doctor vs the opinion given they were a nurse).
 </strong>
<br/>
```{r , echo=FALSE}
n = sum(zee.table)
ni. = rowSums(zee.table)
n.j = colSums(zee.table)
all.pjG1 = zee.table[1,]/ni.[1] #all conditional probabilites for row 1
# Uses 5(new), 3(none), 15(old)
# then divides that by the total number of values in row 1
# The results are
# 0.2173913, 0.1304348 , 0.6521739
# these are stored in all.pjG1

all.pjG2= zee.table[2,]/ni.[2] #all conditional probabilites for row 2

all.pbar = n.j/n #all probabilities regardless of group
all.Zij = c(all.pjG1 - all.pjG2)/sqrt(all.pbar*(1-all.pbar)*(1/ni.[1] + 1/ni.[2])) #The z-test-statistics
all.Zij
```
<br/>
***(2b)***
<br/>
<strong> Which two groups show the most difference based on (a)? </strong>
<br/>
The new and old both are really different.

<br/>
***(2c)***
<br/>
<strong> Use R to find the Tukey inspired cutoff value and report it (using α = 0.05). </strong>
<br/>
```{r , echo=FALSE}
r.perms.cutoff = sapply(1:R,function(i){
  perm.data = procedure
  perm.data$Prof = sample(perm.data$Prof,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(table(perm.data))
  col.sum = colSums(table(perm.data))
  all.pji = table(perm.data)[1,]/row.sum[1]
  all.pji.= table(perm.data)[2,]/row.sum[2]
  all.pbar = col.sum/sum(row.sum)
  all.Zij = c(all.pji - all.pji.)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[2]))
  Q.r = max(abs(all.Zij))
  return(Q.r)
})
alpha = 0.05
cutoff.q = as.numeric(quantile(r.perms.cutoff,(1-alpha)))
```
```{r , echo=FALSE}
cutoff.q
```

<br/>
***(2d)***
<br/>
<strong> Using (c), what pairings were significantly different? </strong>
<br/>
Based on the cutoff, group 3, the old group was bigger than our cutoff, thereofre the most significantly different.


<br/>
***(2e)***
<br/>
<strong> For the pairings that were different, what is the actual difference (which profession preferred what procedure)? </strong>
<br/>
Old treatment was preferred by doctos, and nurses preferred the new procedure

<br/>
<h2> Question 3</h2>
A sociologist wished to see if degree type (Advanced, College, No College) differed between urban and rural households. The data is online under education.csv.
<br/>
***(3a)***
<br/>
<strong> Find and report the 2 x 3 contingency table. </strong>
<br/>
```{r , echo=FALSE}
education = read.csv("education.csv")
zee.table.2 = table(education)
zee.table.2
```


<br/>
***(3b)***
<br/>
<strong> State the null and alternative hypothesis in terms of conditional probabilities. </strong>
<br/>
$H_o: P_{j|i} = P_{.j}$
<br/>
$H_o: Variables \ Degree \ Type \ and \ Location \ Are \ Independent$
<br/>
$H_a: P_{j|i} \ne P_{.j}$
<br/>
$H_a: Variables \ Degree \ Type \ and \ Location \ Are \ Dependent$

<br/>
***(3c)***
<br/>
<strong> Find the value of $\chi^2_{OBS}$ </strong>
<br/>
```{r , echo=FALSE}
the.test.2 = chisq.test(zee.table.2,correct = FALSE)
eij.2 = the.test.2$expected
chi.sq.obs.2 = as.numeric(the.test.2$statistic)
```

```{r, echo=FALSE}
chi.sq.obs.2
```

<br/>
***(3d)***
<br/>
<strong> Use R to find the permutation based p-value, based on R = 3000 random shufflings. </strong>
<br/>
```{r , echo=FALSE}
R = 3000
r.perms = sapply(1:R,function(i){
  perm.data = education
  perm.data$Degree = sample(perm.data$Degree,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})
perm.pval.2 = mean(r.perms >= chi.sq.obs.2)
```

```{r , echo=FALSE}
perm.pval.2
```

<br/>
***(3e)***
<br/>
<strong> State your conclusion in terms of the problem if $\alpha = \ $ 0.05. </strong>
<br/>

 Because our permuation P-Value is less than alpha, we will reject the null hypothesis. Therefore, we can conclude the alternative. Location and Degree do show dependence.

<br/>
<h2> Question 4</h2>
<br/>
***(4a)***
<br/>
<strong> Find the Z values for comparing the row values for each column (i.e, the education of the subject given the area (urban, rural)).  </strong>
<br/>
```{r , echo=FALSE}
n.2 = sum(zee.table.2)
ni..2 = rowSums(zee.table.2)
n.j.2 = colSums(zee.table.2)
all.pjG1.2 = zee.table.2[1,]/ni..2[1] #all conditional probabilites for row 1
all.pjG2.2= zee.table.2[2,]/ni..2[2] #all conditional probabilites for row 2
all.pbar.2 = n.j.2/n.2 #all probabilities regardless of group
all.Zij.2 = c(all.pjG1.2 - all.pjG2.2)/sqrt(all.pbar.2*(1-all.pbar.2)*(1/ni..2[1] + 1/ni..2[2])) #The z-test-statistics
```

```{r , echo=FALSE}
all.Zij.2 = matrix(all.Zij.2,nrow=  1)
colnames(all.Zij.2) = c("Advanced","College","No College")
rownames(all.Zij.2) = c("Rural vs. Urban")
all.Zij.2
```

<br/>
***(4b)***
<br/>
<strong> Which two groups show the most difference based on (a)?  </strong>
<br/>
The College group shows the most difference. It's the only postive number.

<br/>
***(4c)***
<br/>
<strong> Use R to find the Tukey inspired cutoff value and report it (using α = 0.05). </strong>
<br/>
```{r , echo=FALSE}
R=3000
r.perms.cutoff.2 = sapply(1:R,function(i){
  perm.data = education
  perm.data$Degree = sample(perm.data$Degree,nrow(perm.data),replace = FALSE)
  row.sum = rowSums(table(perm.data))
  col.sum = colSums(table(perm.data))
  all.pji = table(perm.data)[1,]/row.sum[1]
  all.pji.= table(perm.data)[2,]/row.sum[2]
  all.pbar = col.sum/sum(row.sum)
  all.Zij = c(all.pji - all.pji.)/sqrt(all.pbar*(1-all.pbar)*(1/row.sum[1] + 1/row.sum[2]))
  Q.r = max(abs(all.Zij))
  return(Q.r)
})
alpha = 0.05
cutoff.q.2 = as.numeric(quantile(r.perms.cutoff.2,(1-alpha)))
cutoff.q.2
```

<br/>
***(4d)***
<br/>
<strong> Using (c), what pairings were significantly different? </strong>
<br/>
Based on the Z values calculated using the Z-Statistic, the Z-Value for College is greater than the cutoff. The z-score for college is 2.8719 and the cutoff is 2.412, therefore the proportions used in College are significantly different.  


<br/>
***(4e)***
<br/>
<strong> For all pairings, what is the actual difference (which areas had tended to what difference of what education if any)? </strong>
<br/>
There is a difference between the number of degrees that residents get based on wether or not they live in a rural or urban area. In other words, people who live in an urban area get degrees at a different rate than people living in a rural area.

<br/>
<h2> Question 5</h2>
A college wanted to see if the amount of alcohol consump- tion (high, low, moderate) was different between genders. The data is online, under the file alcohol.csv.
<br/>
***(5a)***
<br/>
<strong>  </strong>
<br/>
```{r , echo=FALSE}
alcohol = read.csv("alcohol.csv", header = TRUE)
zee.table.3 = table(alcohol)
zee.table.3
```


<br/>
***(5b)***
<br/>
<strong> State the null and alternative in words. </strong>
<br/>
$H_o: Variables \ Gender \ and \ Alcochol \ Consumption \ Are \ Independent$
<br/>
$H_a: Variables \ Gender \ and \ Alcochol \ Consumption \ Are \ Dependent$
<br/>
<br/>
***(5c)***
<br/>
<strong> How many possible shufflings of the data are there (you do not need to put the exact number, the formula for it is sufficient)? </strong>
<br/>
The total number of ways to shuffle the columns and fix the rows is: <br/>
$\frac{n!}{n_1.! \ n_2.! \ ... \ n_r.!}$
<br/>
The total number of ways to shuffle the rows and fix the columns is: <br/>
$\frac{n!}{n_.1! \ n_.2! \ ... \ n_.c!}$

<br/>
***(5d)***
<br/>
<strong> Find the value of $\chi^2_{OBS}$ and the corresponding parametric p-value. </strong>
<br/>
```{r , echo=FALSE}
ni..3 = rowSums(zee.table.3)
n.j.3 = colSums(zee.table.3)

```

```{r , echo=FALSE}
the.test.3 = chisq.test(zee.table.3,correct = FALSE)
eij.3 = the.test.3$expected
chi.sq.obs.3 = as.numeric(the.test.3$statistic)
chi.sq.obs.3
```
The Corresponding P-Value is
```{r , echo=FALSE}
the.test.3$p.value
```
<br/>
***(5e)***
<br/>
<strong> Find the value of the permutation based p-value for R = 2000. </strong>
<br/>
```{r , echo=FALSE, include=FALSE}
R = 2000
r.perms.3 = sapply(1:R,function(i){
  perm.data = alcohol
  perm.data$Consump = sample(perm.data$Consump,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})
```

```{r , echo=FALSE}
perm.pval.3 = mean(r.perms.3 >= chi.sq.obs.3)
perm.pval.3
```

<br/>
***(5f)***
<br/>
<strong> State your conclusion based on (e) in terms of the problem.</strong>
<br/>
For all common alpha's, the P-value is greater than alpha. So therefore we fail to reject the null hypothesis and conclude that the amount of alochol consumption and gender is independent.
<br/>
***(5g)***
<br/>
<strong> It does not make sense to continue to make a cutoff for differences between specific pairings. Why not? </strong>
<br/>
It'll be redundent considering that the null hypothesis has been rejected. We already know that the pairings are independent, therefore finding which specific pairs are is redundant.
<br/>

<h2> Question 6</h2>
 Extracorporeal membrane oxygenation (ECMO) is a potentially life-saving procedure that is used to treat new- born babies who suffer from severe respiratory failure, while the conventional medical therapy (CMT) is used more often. The data infant.csv contains information on if infants survived or died, and which treatment they were given.
<br/>
***(6a)***
<br/>
<strong>  </strong>
<br/>
```{r , echo=FALSE}
infant = read.csv("infant.csv", header= TRUE)
zee.table.4 = table(infant)
zee.table.4
```

<br/>
***(6b)***
<br/>
<strong> State the null and alternative hypothesis for this test. </strong>
<br/>
$H_o: Variables \ Treatmeant \ and \ Outcome(Survived/Died) \ Are \ Independent$
<br/>
$H_a: VVariables \ Treatmeant \ and \ Outcome(Survived/Died) \ Are \ Dependent$
<br/>

<br/>
***(6c)***
<br/>
<strong> Find the value of $\chi^2_{OBS}$  </strong>
<br/>
```{r , echo=FALSE}
ni..4 = rowSums(zee.table.4)
n.j.4 = colSums(zee.table.4)
```

```{r , echo=FALSE}
the.test.4 = chisq.test(zee.table.4,correct = FALSE)
eij.4 = the.test.4$expected
chi.sq.obs.4 = as.numeric(the.test.4$statistic)
```

```{r , echo=FALSE}
chi.sq.obs.4
```

<br/>
***(6d)***
<br/>
<strong> Use R to find the permutation based p-value, based on R = 3000 random shufflings. </strong>
<br/>
```{r , echo=FALSE, include=FALSE}
R = 3000
r.perms.4 = sapply(1:R,function(i){
  perm.data = infant
  perm.data$Treatment = sample(perm.data$Treatment,nrow(perm.data),replace = FALSE)
  chi.sq.i = chisq.test(table(perm.data),correct = FALSE)$stat
  return(chi.sq.i)
})
```

```{r , echo=FALSE}
perm.pval.4 = mean(r.perms.4 >= chi.sq.obs.4)
perm.pval.4
```

<br/>
***(6e)***
<br/>
<strong> State your conclusion in terms of the problem if $\alpha$ = 0.01. </strong>
<br/>
When alpha is .01, and the p-value from permuation is .01266, we fail to reject the null, and conclude the treatment used and the outcome are independent.
<br/>

<h2> Question 7</h2>
<br/>
***(7a)***
<br/>
<strong> What aspect of our data suggested non-parametric statistics should be used? Explain. </strong>
<br/>

Firstly the data was greater imbalanced. There were 34 samples of survived, but 5 of died.
eij wasn't greater than 5 for all i.j, which means or implies that non-parametric should be used.

<br/>
***(7b)***
<br/>
<strong> What assumptions did we need in order to conduct the test in problem 6d? Explain </strong>
We assumed that a random sample was taken.

<br/>
***(7c)***
<br/>
<strong> Should we create cutoffs for confidence intervals in this case? Explain. </strong>
I don't think we should create cutoffs because we determined already at an alpha level of .01 that the treatment used and outcome are independent. But also moer data should be gathered.

<br/>
***(7d)***
<br/>
<strong> What procedure was more likely to result in the infant surviving, based on this example? Explain. </strong>

Given the small sample size of died for ECMO, and large number of survivors, the ECMO procedures seems better. $\frac{28}{29}$ infants survived. Compared to $\frac{6}{10}$, it seems like ECMO procedure is more effective than CMT.

<br/>
<h2> Question 8</h2>
Answer the following questions with TRUE or FALSE. It is good practice to explain your answers.

<br/>
***(8a)***
<br/>
<strong> A non-parametric tests for a contingency table does not require any assumptions. </strong>
<br/>

False. The assumption we use for the contigency table is that a random sample was taken.

<br/>
***(8b)***
<br/>
<strong> If we reject the null hypothesis in a chi-squared non-parametric test for independence, we immediately know what difference in conditional probabili- ties there is. </strong>
<br/>
False! Rejecting the null means that we understand that the two variables are dependent. That's it. We dont know which specific groups are dependent.

<br/>
***(8c)***
<br/>
<strong> If we reject the null hypothesis in a chi-squared non- parametric test for independence, we know that all categories of one variable are dependent on all other categories of the other variable. </strong>
<br/>
AGAIN, FALSE. When the null is rejected, we just know that the two variables are dependent, but we dont have information about which categories specifically. In order to figure this out, we can use a CutOff!
<br/>
***(8d)***
<br/>
<strong> P(A|B) = P(AC|B) implies events A and B are in- dependent (assuming AC denotes the complement of A). </strong>
<br/>
False! Why so many false? $P(A|B) =P(A|B^c)$ implies that events A and B are independent.

### R Appendix
```{r, ref.label=knitr::all_labels(),echo=TRUE,eval=FALSE}
```
