---
title: "Blog Post"
output: html_document
---
# Survival Analysis with PBC Data Set



#1. What is Survival Analysis?

Survival analysis encompasses a variety of techniques to predict at which probability and especially when a subject under observation will experience a certain event. The aim is to find out the hazard of an event going to happen or the probability that a subject will survive, meaning the event did not occur within the given time frame. In contrast to predictive regression models such as logit or least squares which are suitable to find out whether a certain event is going to happen, survival analysis will provide information about the time to event and the chance of that event, at any point in time. With the ever-increasing amount of data being generated in almost every business sector, the sensible employment of survival analysis techniques has become a key asset in ensuring future success for many companies.

Typical fields of application are:
*Medical studies
*Credit risk modeling
*Marketing
*Failure prediction and monitoring of technical devices

Predictors are domain specific:
*Health data: blood sugar levels ,heart rate, eating habits
*Customer data: income, job, family situation
*Monitoring data: machine status, uptimes, error counts

Apart from application by practitioners, survival analysis has also been subject to extensive research, focusing on benchmarking and improving existing techniques in several different settings such as gradient boosting, deep learning and the application of neural networks.
Three Main Groups:

Non-Parametric:
Kaplan Meier
Decision trees

Semi-Parametric:
Cox Proportional hazard
Gradient Boosting with cox

Parametric:
Accelerated Failure Time




*Non-parametric models rely only on the observed outcomes in the data to calculate the hazard and estimate future development without considering any covariates. Therefore, no assumption is imposed on survival time to follow any distribution.

*Semi-parametric models take into consideration both - the underlying hazard as well as the relative influence of the covariates, the parametric part. These models assume a distribution only for the parametric part.

*Fully parametric models assume a distribution for both the survival time and the covariates.


#2. Data Set Overview
We are using a public available data set called pbc from 1991 which is included in the package "survival". It includes data on patients that suffer from Primary Biliary Cirrhosis, rare but fatal chronic liver disease of unknown cause. The data includes observations on 418 individuals that are receiving either the drug D-penicillamine (DPCA) or a placebo. There were 424 patients who met the eligibility criteria seen at the Clinic while the trial was open for patient registration. Both the treating physician and the patient agreed to participate in the randomized trial in 312 of the 424 cases. At the end of the observation time, 125 of the 312 patients had died, with only 11 not attributable to PBC. Eight patients had been lost to follow up, and 19 had undergone liver transplantation.

```{r include = FALSE}
#pbc dataset
data(pbc,package = "survival")


ds <-  matrix( nrow=1, ncol=7)
ds <- as.data.frame(ds)
colnames(ds) <- c("Dataset:","Nr. Observations","Nr. Features","Time Varying Covariates","Censored","Type", "Number of Events")
ds[1,1] <- c("pbc")
ds[1,2] <- NROW(pbc)
ds[1,3] <- NROW(colnames(pbc))
ds[1,4] <- c("no")
ds[1,5] <- c("right")
ds[1,6] <- c("Medical")
ds[1,7] <- c("161")

```

```{r xtable2, results = 'asis'}
print(ds,type ="html")
```

####Structure:
```{r}
print(head(pbc,10))
```


#3. Survival Analysis Techniques
## 3.1. Preparation - Train and test data sets for PDC set
It is recommendable to clean the data before producing the models and plots and to construct a training and test set. Moreover, the data on status should be binary in order to analyze it properly.
```{r}
## Change status to be binary (Originally: 1 alive, but liver transplantation, 0 is alive, 2 is dead)
pbc$status[which(pbc$status == 1)] <- 0
pbc$status[which(pbc$status == 2)] <- 1

## Clean Data Set
pbc_clean <- pbc[!is.na(pbc$age)
                 & !is.na(pbc$edema)
                 & !is.na(pbc$bili)
                 & !is.na(pbc$albumin)
                 & !is.na(pbc$protime)
                 & !is.na(pbc$status)
                 ,]

#Selecting 75%  from initial population
samplepbc <- sample.int(n = NROW(pbc_clean), size = floor(.33*NROW(pbc_clean)), replace = F)
trainpbc_1_3 <- pbc_clean[samplepbc,]
testpbc_2_3  <- pbc_clean[-samplepbc,]
samplepbc <- sample.int(n = NROW(pbc_clean), size = floor(.66*NROW(pbc_clean)), replace = F)
trainpbc_2_3 <- pbc_clean[samplepbc,]
testpbc_1_3  <- pbc_clean[-samplepbc,]
```


## 3.2. Kaplan Meyer
The Kaplan Meier Estimate is based on the survival function S(t) and therefore purely non-parametric. The curve displays the probability that an event did not occur in i, hence one can observe the chances of surviving in a given length of time while considering many small time steps. The survival function at i is defined as:

\begin{center}
$\hat{S}(t)=\prod_{i:t_i\leq t}(1- \frac{d_i}{n_i})$
\end{center}
\begin{itemize}
\item $d_i$: Number of events
\item $n_i$: Number of subjects at risk at time i
\item $t$: Total time
\end{itemize}

The Kaplan Meier Estimate at point t is the sum of all survival probabilities of the previous time steps:

\begin{center}
$Kaplan Meier Estimate=\prod_{i:t_i\leq t}({S_i})$
\end{center}
Kaplan Meier is based on three strong assumptions:

\begin{itemize}
\item  Censored subjects have the same survival prospects.
\item  Survival probabilities are the same for all subjects regardless of the timing when the subjects enter the observations.
\item  Events happen at the time specified.
\end{itemize}

Especially the first and second assumption are hard to find since the survival does not always only depend on time only and seasonality is completely ignored. Nevertheless, Kaplan Meier is a good choice for a first visualization of the survival object and its survival probabilities. The survfit function in R-package "survial" allows to model the survival objects by characteristics, such as treatment method for a patient or customer type. These first insights can be very useful to understand the observed data.

### Produce and plot Kaplan-Meier estimator by survfit function
```{r}
model_fit_simple <- survfit(Surv(pbc_clean$time, pbc_clean$status == 1) ~ 1)
autoplot(model_fit_simple) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))
```

### Show Kaplan-Meier estimates for 2nd observation
```{r}
print((summary((model_fit_simple),43)))
```
![KM simple model](.\static\img\seminar\survival_analysis/km_survtime.jpg)

*The output is to be understood as follows:
*n.risk represents the amount of patients that are still alive after the observation.
*n.event represents the amount of patients that have died between the previous and current observation.
*survival represents the chances on survival for a patient at this point in time (after 43 days).

### Plot differences on the different treatments
```{r}
model_fit_trt <- survfit(Surv(pbc_clean$time, pbc_clean$status == 1) ~ pbc_clean$trt)
autoplot(model_fit_trt) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))
```
![KM with two groups](.\static\img\seminar\survival_analysis/km_survtime2.jpg)
The Kaplan Meier curve shows the survival probabilities of the patients with the different treatments where 1 (red line) represents the treatment with the drug and 2 (turquoise line) over time.


### Performing the Mantel-Haenzel test
In order to test on conditional indepencence of a feature like treatment, it is recommendable to perform the Mantel-Haenzel test by function survdiff. This is efficient in comparing groups that differ by categorical variables, but not continuous ones. The test statistic value is less than the critical value (using chi-square table) for degree of freedom equal to one. Hence, we can say that there is no significant difference between the two groups regarding the survival. We can follow that the actual treatment of these patients gave little impact on the survival chances.
```{r}
survdiff(Surv(pbc_clean$time, pbc_clean$status==1)~pbc_clean$trt)
```


### Plot differences on males and females and test log-rank test
The next two plots are representing the different survival functions of males and females and patients that had hepatitis before PBC and those who did not. Performing the Mantel-Haenzel test it is obvious that males and patients who have hepatitis are more likely to die quickly from PBC than females and patients without hepatitis.
```{r pressure, echo=FALSE}
model_fit_sex <- survfit(Surv(pbc_clean$time, pbc_clean$status == 1) ~ pbc_clean$sex)
autoplot(model_fit_sex) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Male and Female Biliary Cirrhosis Patients \n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

survdiff(Surv(pbc_clean$time, pbc_clean$status==1)~pbc_clean$sex)
```
![KM male / female group](.\static\img\seminar\survival_analysis/km_survtime3.jpg)

### Plot differences on patience with hepatitis and without hepatitis and test log-rank test
```{r pressure2, echo=FALSE}
model_fit_hepato <- survfit(Surv(pbc_clean$time, pbc_clean$status == 1) ~ pbc_clean$hepato)
autoplot(model_fit_hepato) +
    labs(x = "\n Survival Time (days) ", y = "Survival Probabilities \n",
    title = "Survival Time of \n Biliary Cirrhosis Patients with and without hepatitis\n") +
    theme(plot.title = element_text(hjust=0.5),
    axis.title.x = element_text(face = "bold", color = "#FF7A33", size = 12),
    axis.title.y = element_text(face = "bold", color = "#FF7A33", size = 12),
    legend.title = element_text(face = "bold", size = 10))

survdiff(Surv(pbc_clean$time, pbc_clean$status==1)~pbc_clean$hepato)
```
![KM hepatitis](.\static\img\seminar\survival_analysis/km_survtime4.jpg)

## 3.3. Accelerated Time Failure
In contrast to the Kaplan Meier Estimate, the AFT model is characterized by being a fully parametric survival model. The explanatory variables act as accelerating factors to speed up or slow down the survival process as compared to the baseline survival function $\hat{S}(t)$\.

\begin{center}
$S(t|X) = S_0(t * exp (- \beta  \times X))$
\end{center}

It follows that the event rate is slowed down whenever $0 < exp(- \beta \times X) < 1$ and is speeded up when $exp(- \beta  \times X) > 1$. The hazard function of AFT models is therefore obviously different from the hazard function of Cox models. The individual hazard at time t is:

\begin{center}
$h(t|X) = h_0(t * exp (- \beta  \times X)) * exp (- \beta  \times X))$
\end{center}

The advantage of the AFT model is that the interpretation of the coefficients is straight forward. While positive coefficients imply that the hazard rate is increasing, negative coefficients force it to decrease, hence the survival time is lengthened. This is due to the dynamics of the regressors to act as accelerators. If a coefficient is for example 0.2, it means the variable is reducing the survival time, hence, the event of death is observed five times faster in this case.

### Building the ATF Models
Within the ATF model function survreg(), one can define the distribution of the error term. It is possible to define, among other, that the error term is follows an exponential, log or Weilbull distribution.

##Example ATF Weibull 1_3
```{r}
atf_model_weibull_1_3 <- survreg(Surv(time, status == 0) ~ age + edema +log(bili) +log(albumin)+log(protime), dist='weibull', data = trainpbc_1_3)
```
Hint: When performing survreg() for exponential and loglogistic distributions, the death event is mixed up in R. Be aware that one needs to change it as otherwise the model will try to generate the survival object based on the wrong event.


## 3.4. Cox Proportional Hazard
The Cox PH model, introduced in 1972, is one of the most popular models used in survival analysis. Its goal is to estimate the influence of independent co-variates on the hazard of an item under observation to experience an event or on the time-to-event, respectively. It is also suitable for repeated measurements for each subject under observation, i.e. time - varying covariates. The hazard function is composed of the product of a baseline hazard  $\lambda_0(t)$ that has the same value for all objects under observation at a certain time and the proportional hazard $\exp \big\{x^T \theta \big\}$ which represents the effect of the covariates on the life time of a subject.

The hazard rate $hr$ for each covariate $\exp(x_i)$ can be interpreted as follows:
\begin{itemize}
\item $hr = 1$: covariate has no effect
\item $hr > 1$: covariate increases hazard
\item $hr < 1$ : covariate decreases hazard
\end{itemize}

The hazard function is expressed by:
\begin{center}
$\lambda \left ( t|x,\theta \right ) = \lambda_0\left ( t \right )exp\left \{ x^T\theta \right \}$
\end{center}

Cox proposed, that in order to find out the relative effect of the covariates,  $\hat{??_0}(t)$ does not need to be specified. Instead, partial likelihood is used:
\begin{center}
$L_p\left ( \theta ;\left \{ x_i,t_i,\delta _i \right \}^n_i=1\right ) = \prod_{i\in E } \frac{exp \big\{ \theta^T x_i \big\} }{ \sum_j:t_j\geq t_i } exp \big\{ \theta^T x_j \big\}$
\end{center}


### Building the trained proportional hazard cox models
```{r include = FALSE}
pbc.cox_1_3 <- coxph(Surv(time,status ==1 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_1_3)
pbc.cox_2_3 <- coxph(Surv(time,status ==1 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_2_3)
pbc.cox_3_3 <- coxph(Surv(time,status ==1 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = pbc_clean)
```



### Predicting hazard ratios on testset
In order to access the hazard of each patient, it is important to first access the predictions of the model and use the basehaz() function in order to display the values of over time.
```{r}
coxpredicted_trainpbc <- predict(pbc.cox_1_3,type="lp")
coxpredicted_testpbc <- predict(pbc.cox_1_3, newdata=testpbc_1_3,type="lp")
print(head(coxpredicted_testpbc,5))
```
```{r}
## Hazard for each subject, for day 110, (basehazard*exp(lp))
cox_bazehaz <- basehaz(pbc.cox_1_3)
print(head(cox_bazehaz,10))
```

### Calculate Survival probabilities of subjects in testset
```{r}
Pred_Prob <- predictSurvProb(pbc.cox_1_3,newdata=testpbc_1_3,times=c(110))
print(head(Pred_Prob, n=10))
```


## 3.5. Decision Trees
Decision trees are a type of supervised learning algorithm often used for classification problems. The general idea is that the root, the entire set or sample population, is split by the decision tree model into two or more homogenous sets by the variable that achieves the most homogenous split. The same process is then applied to the new decision nodes which are splitting the data again into subsets, creating new branches of the tree. Thus the generation of the new sub-nodes increases the homogeneity of resultant sub-nodes further with respect to the target variable. Once the results in terms of outcome are homogeneous and no further improvement can be achieved, the tree ends in the terminal nodes.

The advantages of decision tree models are numerous. Decision trees are easy to understand, useful in data exploration, can handle numeric and categorical parameters and is considered to be non-parametric as decision trees have no assumptions about the space distribution and the classifier structure. However, the risk of overfitting exists, and decision trees cannot handle continuous variables as information is lost when categorizing variables in different categories.

We are using the function $rpart$ from the Rpackage "rpart" to estimate the tree models. Our plots of the decision tree are generated by the Rpackage "partykit" which is very helpful as it produces not only the tree but also visualizations of the corresponding Kaplan-Meier curves at the terminal nodes.

### Constructing the tree models
```{r}
treefit_1_3 <- rpart(Surv(time,status == 0 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_1_3)
treefit_2_3 <- rpart(Surv(time,status == 0 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = trainpbc_2_3)
treefit_3_3 <- rpart(Surv(time,status == 0 ) ~age + edema +log(bili) +log(albumin)+log(protime), data = pbc_clean)
treefit_1_3 <- as.party(treefit_1_3)
treefit_2_3 <- as.party(treefit_2_3)
treefit_3_3 <- as.party(treefit_3_3)
```

### Plot models with package partykit - including Kaplan Meier curves
```{r}
plot(treefit_1_3)
```


## 3.6. Survival Gradient Boosting with Cox
Gradient Boosting comes from the family of ensemble learning techniques. A number of weak learners is sequentially fitted and added to the model, so that a cost function is minimized. The implementation that was used in the benchmark was the $gbm$ Rpackage, the cost function corresponds to the Cox PH distribution. We are using the following code snippet to boost our above described cox model:

### Building a gradient boosting model from the above mentioned cox model
```{r}
gbmpbc_1_3 = gbm(pbc.cox_1_3,
data = trainpbc_1_3,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)

gbmpbc_2_3 = gbm(pbc.cox_2_3,
data = trainpbc_2_3,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)

gbmpbc_3_3 = gbm(pbc.cox_3_3,
data = pbc_clean,
distribution = "coxph",
n.trees = 2500,
shrinkage = 0.02,
n.minobsinnode = 4)
```


# 4. Benchmarking Techniques for Survival Analysis
## 4.1. Receive AUCs for all models
It is very popular in survival analysis research to benchmark models based on the Area Under the Curve (AUC) value of the model as the value is easy to understand and can be computed for many different kind of models. The following steps are always the same:

\begin{itemize}
\item     Train the model based on a train set.
\item     Save the predictions of the performed model.
\item     Compare the saved predictions of the model with the actual values of the train set.
\item     Plot the true positive rate versus the false positive rate.
\item     Calculate the integral.
\end{itemize}

The AUC value is the integral of the plot of true positive rate versus false positive rate, hence the area under the curve. The advantage of AUC is that evaluation is possible at any point in time in the survival curve. Moreover, it is possible to impose restrictions on the false positive rate. If for example one false positive prediction is costlier than a true positive prediction, it is possible to restrict the AUC calculation only on a certain max false positive rate. This feature is very often used in clinical studies as a false positive prediction can cause costs and even death of the patient. The plots below show the the receiver operating characteristic curve for the Cox model without gradient boosting on a 33 and 66 percentages train set out of the full data set.

```{r}
par(mfrow = c(2, 1))
pred_atf <- prediction(predict(pbc.cox_1_3), trainpbc_1_3$status)
perf_atf <- performance(pred_atf,"tpr","fpr")
plot(perf_atf)
pred_atf <- prediction(predict(pbc.cox_2_3), trainpbc_2_3$status)
perf_atf <- performance(pred_atf,"tpr","fpr")
plot(perf_atf)
```
![AUC ATF](.\static\img\seminar\survival_analysis/perf_atf.jpg)


### Generate predictions on all models
In order to calculate the AUC values, we require the predictions for all models .
## Example coxph_1_3
```{r include = FALSE}
pred_coxph_1_3 <- prediction(predict(pbc.cox_1_3), trainpbc_1_3$status)
```
## Example coxgbm_1_3
```{r include = FALSE}
pred_gbmpbc_1_3 = prediction(predict(object = gbmpbc_1_3,
newdata = trainpbc_1_3,
n.trees = 1500,
type = "response"), trainpbc_1_3$status)
```

## Example ATF
```{r include = FALSE}
pred_atf_model_weibull_1_3 <- prediction(predict(atf_model_weibull_1_3), trainpbc_1_3$status)
pred_atf_model_exponential_1_3 <- prediction(predict(atf_model_exponential_1_3), trainpbc_1_3$status)
pred_atf_model_log_1_3 <- prediction(predict(atf_model_log_1_3), trainpbc_1_3$status)
```

## Example Tree
```{r include = FALSE}
pred_treefit_1_3 <- prediction(predict(treefit_1_3), trainpbc_1_3$status)
pred_treefit_2_3 <- prediction(predict(treefit_2_3), trainpbc_2_3$status)
pred_treefit_3_3 <- prediction(predict(treefit_3_3), pbc_clean$status)
```




### Now receive the AUC values for each model
```{r}
## Example coxph_1_3
auc.perf_coxph_1_3 = performance(pred_coxph_1_3, measure = "auc")

```
## AUC Overview

###AUC Cox & Trees

|       | Cox PH |       |       | Cox GBM |       |       | Tree  |       |
|-------|--------|-------|-------|---------|-------|-------|-------|-------|
| 0.806 | 0.854  | 0.838 | 0.891 | 0.88    | 0.866 | 0.606 | 0.651 | 0.708 |


###AUC ATF

|       | ATF Weibull |       |      | ATF Exponential |       |       | ATF Log |       |
|-------|-------------|-------|------|-----------------|-------|-------|---------|-------|
| 0.195 | 0.146       | 0.162 | 0.19 | 0.14            | 0.157 | 0.192 | 0.142   | 0.159 |


## 4.2. Concordance Index
The CI is a global index to measure the performance of survival models. When predicting survival times, the output of a model can be interpreted as a ranking based on survival times. The subset of correctly ordered pairs of observations divided by the number of observations that can be ordered results in the CI.

\begin{center}
$CI =  \frac{1}{ | \rho | } \sum_{(i,j)\in\rho}I\left ( F(x_i)<F(x_j)  \right ) = \frac{1}{ | \rho | } \sum_{i \in E}\sum_{j:t_j>t_i} I\left ( F(x_i) < F(x_j) \right )$
\end{center}

\begin{itemize}
\item $\rho$ : The set of orderable pairs where  $t_i< t_j$
\item $\left | \rho \right |$ : number of pairs in $\rho$
\item $F(x)$:function to predict survival time
\item $I$: Indicator whether condition in $\left ( ... \right )$ has been met
\end{itemize}

### Generate the CI for all models
```{r}
## Example gbmpbc_1_3
gbmtrainpbc = predict(object = gbmpbc_1_3,
newdata = trainpbc_1_3,
n.trees = 1500,
type = "response")


gbmtestpbc = predict(object = gbmpbc_1_3,
newdata = testpbc_2_3,
n.trees = 1500,
type = "response")


Survresptrainpbc <- Surv(trainpbc_1_3$time,trainpbc_1_3$status==1)
Survresptestpbc <- Surv(testpbc_2_3$time,testpbc_2_3$status == 1)
CI_gbmpbc_1_3 <- BeggC(Survresptrainpbc, Survresptestpbc, gbmtrainpbc, gbmtestpbc)
if(CI_gbmpbc_1_3<=0.5){
    CI_gbmpbc_1_3 =1-CI_gbmpbc_1_3
}
```
##CI Overview
### CI Cox

|       | Cox PH |       |       | Cox GBM |       |   | Tree |   |
|-------|--------|-------|-------|---------|-------|---|------|---|
| 0.805 | 0.837  | 0.835 | 0.677 | 0.664   | 0.704 | - | -    | - |

### CI ATF


|      | ATF Weibull |      |      | ATF Exponential |       |       | ATF Log |       |
|------|-------------|------|------|-----------------|-------|-------|---------|-------|
| 0.71 | 0.687       | 0.69 | 0.71 | 0.687           | 0.691 | 0.709 | 0.687   | 0.691 |
