## Effects of Job Training on Wages

setwd("~/Documents/MIDS/Modeling and Repr of Data/R Codes and Data/TeamProject1")

library(ggplot2)
library(gridExtra)
library(grid)
library(dplyr)
library(lattice)
library(tidyverse)
library(arm)
library(pROC)

lalonde <- read.csv("lalondedata.csv")

# Let's analyze the data
summary(lalonde)




## LET'S FIT THE MODEL!
reg0 = glm(posAfter ~ as.factor(treat) + age_cent + as.factor(educ_cat) + as.factor(race) + as.factor(married)
           + as.factor(nodegree), data = lalonde, family = binomial)

reg1 = glm(posAfter ~ as.factor(treat) + age_cent + as.factor(educ_cat) + as.factor(race) + as.factor(married)
           + as.factor(nodegree) + re74_cent + re75_cent, data = lalonde, family = binomial)

reg2 = glm(posAfter ~ as.factor(treat) + age_cent + as.factor(educ_cat) + as.factor(race) + as.factor(married)
           + as.factor(nodegree) + posBefore, data = lalonde, family = binomial)

anova(reg0, reg1, test= "Chisq")
anova(reg0, reg2, test= "Chisq")
# It seems that the previous earnings as a continous variable are more significant than
# a dummy variable of zero vs non-zero previous earnings. 
# I will use reg1.

# Once we choose what predictors about previous earnings, 
# let's see if nodegree and married are significative:
reg3 = glm(posAfter ~ treat + age_cent + as.factor(educ_cat) + as.factor(race)
            + re74_cent + re75_cent, data = lalonde, family = binomial)

anova(reg1, reg3, test= "Chisq")
# Seems that married and nodegree are not statistically significant. 
# However, we will keep them because they could work as good controls.


# Interactions? I will analyze in the regression 
# the combined effect of race, married and positive wages,
# the combined effect of education categories, married and positive wages,
# and some interactions using the treatment variable.


reg5 = glm(posAfter ~ as.factor(treat) + as.factor(married) + as.factor(educ_cat) + as.factor(race)*age_cent
           + as.factor(nodegree) + re74_cent + re75_cent, data = lalonde, family = binomial)
summary(reg5) 
# Not significant

reg6 = glm(posAfter ~ as.factor(treat)*as.factor(married) + age_cent + as.factor(educ_cat) + as.factor(race) 
           + as.factor(nodegree) + re74_cent + re75_cent, data = lalonde, family = binomial)
summary(reg6) 
# A little sifnificant and makes sense in the science of the question! Let's 

reg7 = glm(posAfter ~ as.factor(treat)*age_cent + as.factor(race) + as.factor(educ_cat) + as.factor(married)
           + as.factor(nodegree) + re74_cent + re75_cent, data = lalonde, family = binomial)
summary(reg7) 


anova(reg1, reg6, test= "Chisq")
anova(reg1, reg7, test= "Chisq")
anova(reg1, reg5, test= "Chisq")


roc(lalonde$posAfter, fitted(reg1), plot=T, legacy.axes=T)
roc(lalonde$posAfter, fitted(reg6), plot=T, legacy.axes=T)
roc(lalonde$posAfter, fitted(reg7), plot=T, legacy.axes=T)
# Model 6 seems to be the best one! Also, the interaction added makes a lot of sense.

# Let's do the binned residuals plots:
rawresid6 = lalonde$posAfter - fitted(reg6)
binnedplot(x=lalonde$age_cent, y = rawresid6, xlab = "Age centered", ylab = "Residuals", main = "Binned residuals versus age")
binnedplot(x=lalonde$re74_cent, y = rawresid6, xlab = "Earnings 1974 centered", ylab = "Residuals", main = "Binned residuals versus age")
binnedplot(x=lalonde$re75_cent, y = rawresid6, xlab = "Earnings 1975 centered", ylab = "Residuals", main = "Binned residuals versus age")
# Cool, seems that the residuals vs. age are randomly distributed!
# There is just a little trouble with the earnings from 1975 for the 
# highest earning, but that could be because we don't have a lot of people with such big earnings.

tapply(rawresid6, lalonde$treat, mean) 
tapply(rawresid6, lalonde$married, mean) 
tapply(rawresid6, lalonde$nodegree, mean) 
tapply(rawresid6, lalonde$educ_cat, mean) 
tapply(rawresid6, lalonde$race, mean) 
# We are expecting to see number near to zero and that's what we are getting! So the model seems to fit the data pretty well.

#let's do the confusion matrix

threshold = 0.50
table(lalonde$posAfter, reg6$fitted > threshold)
Accuracy = (14+466)/(14+5+129+466)
Precision = (466)/(129+466)
Recall = (466)/(466+5)

Accuracy
Precision
Recall

## Model interpretations
summary(reg6)
exp(reg6$coefficients)
confint.default(reg6)   #on log odds scale
exp(confint.default(reg6))   #on odds scale

## 1) Is there any evidence that workers who receive job training tend to be more likely to have positive (non-zero)
## wages than workers who do not receive job training? What is a likely range for the effect of training?


## 2) Is there any evidence that the effects differ by demographic groups (ethnicity/education)?


## 3) Are there other interesting associations with positive wages that are worth mentioning?


# LIMITATIONS:
# Previous work experience of the workers
# IQ















