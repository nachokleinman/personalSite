---
title: "Homework 5:  Work and Life Balance"
author: "Ping-Hsuan Lin"
date: "12/1/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, tidy = F)
```

# {.tabset}

## Background

For this assignment, we will be working to understand the impact of different working models on the perceived satisfaction of employees with regard to their work/life balance.  Corporate goals often incentivize working longer hours and in more demanding roles.  However, alternative models exist.  Some businesses and countries are experimenting with shorter working weeks or changes to their corporate cultures.

To further examine the issue, a team of researchers 
partnered with a number of companies to examine the impact of their corporate culture and the length of the working week on the overall satisfaction of employees with regard to their work/life balance.  The research team had previously classified the working culture of these companies as either relaxed or demanding based upon validated research tools.

The researchers conducted an experiment with working weeks of different lengths.  Prior to the experiment, all of these companies operated with conventional 5-day working weeks and standard hours.  Each company was randomized to implement either a 3-day working week, a 4-day working week, or to maintain its conventional 5-day working week.  The overall number of expected working hours was held in proportion to the working week (e.g. 8 hours per day for the number of days worked).  However, the compensation of the participating employees remained fixed at their prior levels.  Training was provided to the managers and the employees to set reasonable expectations for what should be accomplished in the shortened working weeks.  The companies were monitored to ensure compliance with the schedule and the expectations.  The study was conducted over a period of 6 weeks.

At the end of this period, consenting employees were given a survey that assessed their satisfaction with their balance of work and life.  The answers were combined into an overall measure of satisfaction ranging from 0 to 100.

In this assignment, we will be working with the information provided to analyze the satisfaction scores and consider other possible implications of changes in the typical working conditions of companies.

## Data

The data are available in the file **work and life balance.csv**.

For each consenting employee, information on their years of experience and whether they are a manager was collected.  Data about each employee's company was recorded, including its identifier, industry, and the assessment of its working culture.  The company's randomly assigned workweek was included, and each employee's overall satisfaction score was recorded.



## Instructions

Based upon the information above and the data provided, please answer the following questions.  When numeric answers are requested, a few sentences of explanation should also be provided.  Please show your code for any calculations performed.

## Preliminary Code

This section of the report is reserved for any work you plan to do ahead of answering the questions -- such as loading or exploring the data.

```{r read_data}
setwd("C:/Users/IgnacioKleinmanRuiz/Documents/Fall2021/5300/assignment")
data = read.csv("work and life balance.csv", header = TRUE)
```

```{r functions}
library(dplyr)
library(DT)
library(data.table)
library(ggplot2)
#install.packages("ggpubr")
library(ggpubr)
library(tidyverse)
library(broom)
library(pwr)
summary(data$satisfaction)
summary(data$workweek)
summary(data$culture)
summary(data$years.experience)
summary(data$manager)
summary(data$industry)
```


## Questions {.tabset}

### Q1 

#### Question

What are the primary research questions of the study?  State them clearly in plain language.  Then briefly explain the importance of this investigation.

#### Answer
The first primary research question is: Do different corporate cultures and the length of the working week affect overall satisfaction of employees with regard to their work/life balance? 
Explanation: 
This will examine the combination impact of two independent variables (corporate culture, and length of working week) to employees' overall satisfaction. 

Another research question is: Does the length of the working week affect overall satisfaction of employees with regard to their work/life balance? 

Explanation:
Since corporate culture is already validated by research tools, and the experiment is about testing different length of working week, the second question should examine the relationship between working model and perceived satisfaction. If the two variables are independent, it means that the changes in working models wouldn't affect the perceived satisfaction of employees.


```{r q1}

```


### Q2 

#### Question

For each research question you mentioned above, describe how well the study is designed to evaluate the question.

#### Answer
For the first question, in the data set, the two independent variables are well designed to categorical variables that could allow for a two-way ANOVA test. The factorial experiment could also include interaction terms of two independent variables to the model
For the second question, the study is well designed to a linear regression model. We could build a one-way ANOVA model to examine the relationship of two. 
However, the data set given to different working model is randomly assigned per company rather than per employees. This may possible cause some bias to the results since individual employees have different age, gender, attributes, pursuit in life, marital status, participation in social activities, sleeping habits and so on. According to the research question I mentioned in Q1, I am testing the independence between satisfaction score and working model, while working model is randomized per company. Therefore, we need to be more prudent while interpreting the results generated from this data set.

```{r q2}

```



### Q3 

#### Question

What kind of statistical method could be employed to analyze the data and evaluate the research questions?


#### Answer
For the first question, we could build a two-way ANOVA to examine the relationship of separate and combined effects of independent variables to dependent variables. We could also use multiple regression to further evaluate the model.

For the second question, we could build a one-way ANOVA or a linear regression model to examine if two variables are independent or not. 

```{r q3}

```


### Q4

#### Question

Fit your intended model and show a summary of its results.  While you may include other variables, we will specifically exclude the company from the analysis because these effects would not generalize as well to the broader industries.


#### Answer

```{r q4}
#Create function for ANOVA
anova.fn <- function(formula, data) {
    mod.lm <- lm(formula = formula, data = data)
    mod.anova <- anova(object = mod.lm)
    return(mod.anova)
}


##For question 1, we will conduct two-way ANOVA
#Conduct two-way ANOVA to test the independence
anova.fn(formula = satisfaction ~ culture + workweek, data = data)
#Examine Coefficient Estimates
mod1 <- lm(satisfaction ~ culture + workweek, data=data)
anova(mod1)
summary(mod1)
#This shows that workweek and culture both are significant enough evidence to support that they have an effect on satisfaction. 

#Include interactions in the Two-Way ANOVA
#To evaluate every combination, we would also need to include an interaction term in the model:
anova.fn(formula = satisfaction ~ culture + workweek + culture * workweek, data = data)
#The combination of culture and workweek fail to statistically prove a relationship with satisfaction variable 

#method 2 will create the same results
mod2 <- lm(satisfaction ~ culture + workweek + culture * workweek, data=data)
anova(mod2)
summary(mod2)
#Multiple R-squared = 0.4123. Coefficient estimate > workweek * culture > culture
#Workweek of 4 or 5 days in a relaxed culture will still bring a negative impact on satisfaction score.


#For question 2, we will conduct one-way ANOVA to examine the relationship between working model and employees' satisfaction
anova.fn(formula = satisfaction ~ workweek, data = data)
#p-value lower than 0.05, so it's statistically significant to support that workweek will affect satisfaction score

mod3 <- lm(satisfaction ~ workweek, data=data)
anova(mod3)
summary(mod3)
#Working model of more than 4 and 5 days a week has negative impact on satisfaction, based on the coefficient estimates.

```


### Q5

#### Question

Explain the results of your model.  Describe how the estimates relate to your research questions and any other notable findings.

#### Answer
Based on the results in Q4, we found that from the two-way ANOVA, we see culture and workweek both have p-value less than 0.05. This indicates that we are statistically significant enough to support the statement that both variables have an effect on satisfaction. The multiple Regression model (mod1) shows that a workweek of 4 and 5 days may have a negative impact on satisfaction. The interaction of culture and workweek isn't statistically significant enough to prove a relationship with satisfaction. But based on mod2 from multiple regression, 41.23% of the variation in the satisfaction variable can be explained by the predictor variables. Also, a workweek of 4 or 5 days in a relaxed culture will still bring a negative impact on satisfaction score. 

We conduct tests of independence between satisfaction and workweek by one-way ANOVA. We see that p-value is lower than 0.05, so it's statistically significant to support that workweek will affect satisfaction score. Based on a simple linear regression model, working more than 4 and 5 days a week has a negative impact on satisfaction. 

```{r q5}

```



### Q6

#### Question

Would variable interactions also play a role?  If your research question includes multiple independent variables, then include pairwise interactions with them.  If you think there is only one independent variable in the study, then create an interaction between that variable and other measured factors that you might consider relevant.  Show the numeric results and comment on the interactions.


#### Answer
In the research question, we assume that there's variable interaction between culture and workweek. But based on the results below, the p-value of interaction between culture and workweek is 0.169, which is greater than 0.05 significance level. Therefore, we concluded that it's not statistically significant to support the statement that the interaction of the two independent variables is relevant to satisfaction score.

```{r q6}

inter <- aov(satisfaction~culture*workweek,data)
summary(inter)
#The p-value is greater than 0.05
```



### Q7

#### Question

Are there other variables that would be helpful to measure?  Is this even necessary?  Explain your answer and reasoning.

#### Answer
Other variables like manager, industry, years.experience could also be included to test for independence to satisfaction. This is based on the assumption that managers or not and different years of experience will have different perspectives to satisfaction in work/life balance. Different industries may correlate with corporate culture and may have an effect on satisfaction score. Based on the results from anova function below, all 5 variables have p-value lower than 0.05 significance level. Combined with the results from the multiple regression, we see that different industry, manager and years experience may have positive or negative impact on satisfaction.

```{r q7}
anova.fn(formula = satisfaction ~ culture + workweek + manager + industry + years.experience, data = data)
#All 5 variables' p-value are lower than significance level 
mod4 <- lm(satisfaction ~ culture + workweek + manager + industry + years.experience, data=data)
summary(mod4)
```



### Q8

#### Question

What if we wanted to compare all of the average satisfaction scores in the three groups of working weeks?  For this analysis, you may ignore the other variables.  Show the results of a statistical test to simultaneously evaluate the difference in satisfaction for all of the pairs of possible working weeks.  Comment on the results.

#### Answer
The average satisfaction score ranks from the highest working 3 days, 4 days to 5 days. The difference between working 3 and 4 days is about a 9.19 score, while the difference between 3 and 5 days is about 11.88, and the difference between 4 and 5 days is 2.69. 

```{r q8}
#make a table to compare
fm1 <- aov(satisfaction ~ workweek,data)
table <- TukeyHSD(fm1, ordered = TRUE)
table
```




### Q9

#### Question

Now conduct separate tests of whether a shorter working schedule increases satisfaction for each pair of schedules.  Which of these results would remain significant with a Bonferroni correction for multiple comparisons?  Show the p-values for the t-tests, the corrected threshold for a 0.05 significance level, and whether the differences remain significant after the adjustment.

#### Answer
In this case, the adjusted p-value for the mean differences in satisfaction scores between 3 days and 4 days or 5 days are both less than 2e-16, while the adjusted p-value for the difference between 4 and 5 days is 0.00022. We can see the  difference remains significant between 3 and 4 days, 3 and 5 days, as well as 4 and 5 days.

```{r q9}
pairwise.t.test(data$satisfaction, data$workweek, p.adjust.method="bonferroni")
```

### Q10

Do you think the 6 week time frame is an appropriate length to investigate the effect of changes in the working schedule on the satisfaction of work/life balance?  Explain why or why not.

#### Answer
I think a 6 week time frame could help companies to have a general and brisk insight on the effect working schedule could bring. However, I don't think a 6 week time frame is an appropriate length to investigate the effect of changes on the satisfaction of work/life balance. This is because the 6 week time frame will be about 1.5 month, while in industries like education, healthcare and engineering, they usually have a project timeline over 1.5 months. Also, with a longer time frame, we could possibly enlarge the experiment to more employees. By doing so, we could include more data to the data set, and we would also have greater power to reject the null hypothesis when it is false. 

```{r q10}

```





### Q11

#### Question

Now the researchers would like to build upon the work of the first experiment they conducted.  In the comments on the surveys, a sizable number of the employees in the first study noted that they did not get enough sleep with a 5-day working week.  Anecdotally, those working the shorter weeks during the experiment frequently mentioned the benefit of getting enough rest.  

With this in mind, the researchers would like your help in planning their next experiment.  They would once again like to randomize companies to shorter working weeks.  Based on the feedback of the previous experiment, a 3-day working week would not be very practical for the companies, while 4 days seemed more actionable.  Comparing the amount of sleep of employees with 4-day schedules to the amount of sleep of those with 5-day schedules, how would you conduct the experiment to answer this question?  State a research question, comment on the operational designs, and describe the type of data you would gather.

#### Answer
Under the statement that different working schedules will affect employees getting enough sleep or not, the research question in the case is: Does the amount of sleep for employees in 4-day working schedule more than employees in 5-day working schedule?

Operational designs: 
Again, 4 or 5 working days will be randomly assigned to different companies and I would separate the data set to treatment and control group. In this way, I could rule out other possible contributing or confusing factors, and only examine the impact of different workweek schedules (independent variable) to the amount of sleep for employees. I would put a 5-day working schedule in the control group and a 4-day working schedule as a treatment group.
I would conduct the experiment for about a 3 month time frame, since the experiment will include setting up, following up and analysis phases. Also, a 3 month time frame is one financial quarter that could possibly give research a more wholesome insight on the effect workweek would bring. Following the original design, each working day is still 8 hours a day, and training will be provided to groups that have less working days. 

Type of data to gather: 
First, I would gather a numeric variable- the amount of sleep, which will be treated as a dependent variable in the experiment. Then I would still gather employee's satisfaction score, and possibly other background information like age, marital status, quality of sleep (feeling rested after sleeping or not), sleeping habit (how long does it take to fall asleep), suffering from sleeping disorders (such as snoring or gasping for air) or not. The amount of sleep and satisfaction score will be collected per week during the experiment.

```{r q11}

```




### Q12

#### Question

What kind of statistical test would be appropriate for your research question?  Provide sufficient details on all of the choices you would make.

#### Answer
Following Q11, I would use a two-sample t test for the question. 

Null hypothesis is that employees in a 4-day working schedule will have less or equal amount of sleep as a group of 5-day schedules. 

H0 = E(Y)_t <= E(Y)_c 

Alternative hypothesis is:  employees in a 4-day working schedule will have greater amounts of sleep as a group of 5-day schedules. 

Ha = E(Y)_t > E(Y)_c 

E(Y)_t: The amount of sleep employees in the treatment group (4-day work schedule)

E(Y)_c: The amount of sleep employees in the control group (5-day work schedule)

```{r q12}

```



### Q13 

#### Question

What is the smallest amount of additional average sleeping time that would constitute a meaningful improvement for the typical employee?  Explain your reasoning.

#### Answer
According to Centers for Disease Control and Prevention (CDC) reports, a normal adult aged 18-60 is recommended to have 7 or more hours per night, while adults above 60 years old is recommended to have 7-9 hours of sleep per night. However, the average amount of sleep one adult has is 6.5 hour per night. Also, there's scientifically proven report that the deprivation of sleep would possibly cause poorer performance such as struggling to stay focus, or coming with new ideas, harm to long-term physical health and social, emotional, and psychological well-being.Based on these report, we could conclude that as long as we could ensure employees getting a minimum 7 hours of sleep per night, which is an additional 0.5 hour of sleeping time, we could possibly expect a meaningful improvement for the typical employee.

```{r q13}

```




### Q14

#### Question

The researchers are hoping to sample approximately 200 employees for the new study, evenly divided into two groups of 100.  What would be the power of your proposed statistical test in this scenario?  Use your suggested effect size from the previous question in units of hours and a significance level of 0.05.  For now, assume that the standard deviation of sleeping times is 1 hour.  Produce a numeric answer and then comment on the results.

#### Answer

```{r q14}
(7-6.5)/1
#Under the assumption that the treatment group (4-day working schedule) will get an average of 7 hours of sleep per night, while the control group (5-day working schedule) will get an average of 6.5 hour of sleep per night, the standardized mean difference between groups is 0.5. The effect size is 0.5 meaning the average sleep in the treatment group is 0.5 above the average sleep in the control group.

#Calculate the power for two-sample t test of equal sample size
pwr.t.test(n = 100, d = 0.5, sig.level = 0.05, power = NULL, type = "two.sample", alternative = "greater")
#We see a power of around 96.98% to reject the null hypothesis when it is false, which is very powerful.

```



### Q15

#### Question

It may be difficult to convince companies to consider a 4-day working week and to convince employees to provide you with their records of sleep.  How would these results change if you could only get 30 employees in the 4-day working week?  Assume that the other inputs from the previous question will be used.  Calculate the power and comment on the results, along with the differences from the previous question.



#### Answer

```{r q15}
#Calculate the power for two-sample t test in different sizes
pwr.t2n.test(n1 = 30, n2= 170, d = 0.5, sig.level = 0.05, power = NULL, alternative = "greater")

#The power went down to around 80.82%, which is smaller than the previous question. We are even less powerful to identify false negatives. That is, we have a higher chance of failing to reject the null hypothesis (the 4-day working schedule will have less or equal amount of sleep to the 5-day counterpart), when the statement is wrong. However, given that 80.82% of power is not awful in detecting false negatives, we could still conduct the experiment based on the sample size of 30 employees in the treatment group.

```


### Q16

#### Question

Assuming that we hold the other inputs fixed from the previous 2 questions, what sample size would be needed in the 4-day working week group to achieve a power of 0.9?  Make sure to round your answer up to a whole number.


#### Answer

```{r q16}
#If we hold the sample size of two groups (treatment and control) the same:
pwr.t.test(d = 0.5, power = 0.90, sig.level = 0.05, type = "two.sample", alternative = "greater")
#From the results, we need 70 sample sizes per group to achieve a power of 0.9.

```



### Q17

#### Question

Describe the trade-offs between power and sample size in this setting.  Including considerations of the statistical issues along with the practical aspects of running the experiment.

#### Answer
The trade-offs between power and sample size (n) is that as n grows larger, the power increases. That is, the likelihood of rejecting H0 when it is false increases as n increases. This is because as n grows, the standard error shrinks, and the true effect will go away from the null value. We will have greater statistical power to reject the null hypothesis when it is wrong. When p-value tells us if there is a statistically significant difference between treatment and control groups, an effect size can quantify the difference between two groups. Therefore, when running an experiment, we need to consider the effect size since it could tell us the exact difference.

```{r q17}

```



### Q18

#### Question

In our earlier analyses, we had assumed that the standard deviation of sleeping times was 1 hour.  What if this assumption is incorrect?  For now, you may consider an experiment with 100 sampled employees in each treatment group and a significance level of 0.05.  Describe how the power changes if our assumption is wrong in each direction.

#### Answer

```{r q18}
#If the standard deviation of 1 hour sleeping times was wrong, the effect size could be smaller or greater than 0.5. We use conventional effect size to get a general idea on how the power could change.

#If the effect size is smaller to 0.2
cohen.ES(test = "t",size = "small")
pwr.t.test(n = 100, d = 0.2, sig.level = 0.05, power = NULL, type = "two.sample", alternative = "greater")
#The power will go down to 40.69%, which is less powerful than before.

#If the effect size is greater to 0.8
cohen.ES(test = "t",size = "large")
pwr.t.test(n = 100, d = 0.8, sig.level = 0.05, power = NULL, type = "two.sample", alternative = "greater")
#The power will go up to 99.99%, which is more powerful than before, and it almost could detect all false negative situation.

```



### Q19

#### Question

Suppose we had been able to add a third group to the planned study so that we could test the 3-day, 4-day, and 5-day working weeks.  We would like to study the differences in mean nightly sleeping time across these groups using a one-way ANOVA model.  The experiment would have 100 employees in each group while planning for a power of 0.8 using a significance level of 0.05.  Under these circumstances, what effect size could be detected?  Convert the calculated effect size into minutes under an assumption that the standard deviation is 1 hour.

#### Answer

```{r q19}
pwr.anova.test(k = 3, n = 100, f = NULL, sig.level = 0.05, power = 0.8)
0.1801187 *60

#The effect size would be 0.1801187 hour; that is around 11 minute of effect size could be detected.
```


### Q20

#### Question

Taking into account your analyses and statistical planning, what kind of recommendations would you make to the companies in order to help them to improve the satisfaction of their employees with regard to work/life balance?

#### Answer
From Q4 to Q8, we see that workweek, industry, years.experience, manager or not, and culture are all statistically significant to pull an impact of satisfaction score with regard to work/life balance. Even though the sample in the original data may not be that much randomly assigned per employees, we see different pattern in amount of sleep from Q13 to Q19. The effect size of sleeping amount could be brought forth from different working models. Based all results above, we could see that in general, less working days will bring a positive effect to satisfaction score and possibly more amount of sleep. If the companies want to improve the satisfaction score of their employees and to help them increase the amount of sleep, it is recommended to implement less working days into the corporation policies. To successfully enforce the policy, companies could start with a 4-day working schedule with crucial training and compensation provided. It is recommended to start a 3-month time frame for one experiment, and adjust the policy according to work performance and satisfaction score. The satisfaction score and feedback from employees will be collected weekly to better monitor effect of different working model.
```{r q20}

```

