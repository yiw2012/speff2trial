# speff2trial
The main goal of this study is to compare two therapies for the treatment of HIV-infected patients. The dataset from this study is available in the R package speff2trial
library(speff2trial)
library(tidyverse)
library(glmnet)
library(vcdExtra)
library(tidyverse)
library(manipulate)
library(caret)
library(sandwich)
library(lmtest)
library(boot)
library(pROC)
library(vars)
library(car)
library(Amelia)
library(randomForest)
library(knitr)
library(psych)
speff2trial::ACTG175
?ACTG175
#ACTG175$zprior
datan <- ACTG175
datan$zprior <- NULL
original_names <- colnames(datan)

#Single Munipulation
set.seed(123)
a.out <- amelia(datan, m = 1, parallel = "multicore")

dat <- data.frame(a.out$imputations)
colnames(dat) <- original_names
data <- dat
a <- (dat$cd420-dat$cd40)/dat$cd40
data$Y <- as.factor(ifelse(a >= 0.5, 1, 0))
data1 <-data

# Mannually TABLE 1
nrow(data1)
length(data1$str2[data1$str2==0])
length(data1$str2[data1$str2==1])
mean(data1$age)
sd(data1$age)
mean(data1$wtkg)
sd(data1$wtkg)
length(data1$gender[data1$gender==1])
length(data1$gender[data1$gender==1])*100 /2139

length(data1$race[data1$race==0])
length(data1$race[data1$race==0])*100 /2139

length(data1$race[data1$race==1])
length(data1$race[data1$race==1])*100 /2139

length(data1$homo[data1$homo==1]) #1=yes
length(data1$homo[data1$homo==1])*100 /2139

length(data1$drugs[data1$drugs==1]) #1=yes
length(data1$drugs[data1$drugs==1])*100 /2139

length(data1$hemo[data1$hemo==1]) #1=yes
length(data1$hemo[data1$hemo==1])*100 /2139

length(data1$karnof[data1$karnof==100]) #1=yes
length(data1$karnof[data1$karnof==100])*100 /2139

length(data1$symptom[data1$symptom==1]) #1=yes
length(data1$symptom[data1$symptom==1])*100 /2139

length(data1$z30[data1$z30==1]) #1=yes
length(data1$z30[data1$z30==1])*100 /2139

mean(data1$preanti)
sd(data1$preanti)

length(data1$oprior[data1$oprior==1]) #non-zidovudine antiretroviral therapy
length(data1$oprior[data1$oprior==1])*100 /2139

length(data1$gender[data1$gender==1 & data1$str2==0]) # 1=male
length(data1$gender[data1$gender==1& data1$str2==0])*100 /886
length(data1$gender[data1$gender==1 & data1$str2==1]) 
length(data1$gender[data1$gender==1& data1$str2==1])*100 /1253

length(data1$race[data1$race==0& data1$str2==0]) # 0=white
length(data1$race[data1$race==0& data1$str2==0])*100 /886
length(data1$race[data1$race==0& data1$str2==1]) 
length(data1$race[data1$race==0& data1$str2==1])*100 /1253

length(data1$race[data1$race==1& data1$str2==0])  # 1=non-white
length(data1$race[data1$race==1& data1$str2==0])*100 /886
length(data1$race[data1$race==1& data1$str2==1]) 
length(data1$race[data1$race==1& data1$str2==1])*100 /1253

length(data1$homo[data1$homo==1& data1$str2==0]) #1=yes
length(data1$homo[data1$homo==1 & data1$str2==0])*100 /886
length(data1$homo[data1$homo==1& data1$str2==1]) #1=yes
length(data1$homo[data1$homo==1 & data1$str2==1])*100 /1253

length(data1$drugs[data1$drugs==1& data1$str2==0]) #1=yes
length(data1$drugs[data1$drugs==1& data1$str2==0])*100 /886
length(data1$drugs[data1$drugs==1& data1$str2==1]) #1=yes
length(data1$drugs[data1$drugs==1& data1$str2==1])*100 /1253

length(data1$hemo[data1$hemo==1& data1$str2==0]) #1=yes
length(data1$hemo[data1$hemo==1& data1$str2==0])*100 /886
length(data1$hemo[data1$hemo==1& data1$str2==1]) #1=yes
length(data1$hemo[data1$hemo==1& data1$str2==1])*100 /1253

length(data1$karnof[data1$karnof==100& data1$str2==0])
length(data1$karnof[data1$karnof==100& data1$str2==0])*100 /886
length(data1$karnof[data1$karnof==100& data1$str2==1])
length(data1$karnof[data1$karnof==100& data1$str2==1])*100 /1253

length(data1$symptom[data1$symptom==1& data1$str2==0])
length(data1$symptom[data1$symptom==1& data1$str2==0])*100 /886
length(data1$symptom[data1$symptom==1& data1$str2==1])
length(data1$symptom[data1$symptom==1& data1$str2==1])*100 /1253

length(data1$z30[data1$z30==1 & data1$str2==0])
length(data1$z30[data1$z30==1 & data1$str2==0]) /886
length(data1$z30[data1$z30==1 & data1$str2==1])
length(data1$z30[data1$z30==1 & data1$str2==1]) /1253

length(data1$strat[data1$strat==3 & data1$str2==0]) #0=naive
length(data1$strat[data1$strat==3 & data1$str2==0]) /886
length(data1$strat[data1$strat==1 & data1$str2==1])
length(data1$strat[data1$strat==1 & data1$str2==1]) /1253

length(data1$oprior[data1$oprior==1 & data1$str2==0]) # 1=male
length(data1$oprior[data1$oprior==1& data1$str2==0])*100 /886
length(data1$oprior[data1$oprior==1 & data1$str2==1]) 
length(data1$oprior[data1$oprior==1& data1$str2==1])*100 /1253

# continuous variables

mean(data1$age[data1$gender==1])

baselineByGroup<- describeBy(data1,data1$str2,mat=TRUE)
baselineByGroup <- baselineByGroup[,c(-12,-13,-14,-15,-16,-17,-18,-19,-8,-9)]
str(baselineByGroup)
NO_PRIOR <- baselineByGroup[baselineByGroup$group1==0, ]
NO_PRIOR
PRIOR <- baselineByGroup[baselineByGroup$group1==1, ]
PRIOR


###Data Munipulation by removing unrelated variables and spliting data into training and validation data
data <- dat
a <- (dat$cd420-dat$cd40)/dat$cd40
data$Y <- as.factor(ifelse(a >= 0.5, 1, 0))
data1 <-data

#remove unrelated variables
#min(data1$zprior)
data1$pidnum <- NULL
data1$cd40 <- NULL
data1$cd420 <- NULL
data1$cd496 <- NULL
data1$r <- NULL
data1$offtrt <- NULL
data1$cd80 <- NULL
data1$cd820 <- NULL
data1$zprior <- NULL
data1$cens <- NULL
data1$days <- NULL
data1$treat <- NULL
data1$strat <- NULL

#factorize the indicators
data1$hemo <- factor(data1$hemo)
data1$homo <- factor(data1$homo)
data1$drugs <- factor(data1$drugs)
data1$oprior <- factor(data1$oprior)
data1$z30 <- factor(data1$z30)
data1$race <- factor(data1$race)
data1$gender <- factor(data1$gender)
data1$str2 <- factor(data1$str2)
#data1$strat <- factor(data1$strat)
data1$symptom <- factor(data1$symptom)
data1$arms <- factor(data1$arms)

#setting up the reference level
data1$hemo <- relevel(data1$hemo, ref = "0")
data1$homo <- relevel(data1$homo, ref = "0")
data1$drugs <- relevel(data1$drugs, ref = "0")
data1$oprior <- relevel(data1$oprior, ref = "0")
data1$z30 <- relevel(data1$z30, ref = "0")
data1$race <- relevel(data1$race, ref = "0")
data1$gender <- relevel(data1$gender, ref = "0")
data1$str2 <- relevel(data1$str2, ref = "0")
#data1$strat <- relevel(data1$strat, ref = "1")
data1$symptom <- relevel(data1$symptom, ref = "0")
data1$arms <- relevel(data1$arms, ref = "0")

#length(data1)

#spliting data into validation and training data
n<-nrow(data1)
set.seed(123)
train<- sample(n, n*0.6)
data1train<- data1[train,]
data1test<- data1[-train,]


###model selection by checking out-of-sample AUC

fit1<- glm(Y ~ . , data=data1train, family=binomial)
fit2<- glm(Y ~ .+age*karnof , data=data1train, family=binomial)
fit3<- glm(Y ~ .+wtkg*homo , data=data1train, family=binomial)
fit4<- glm(Y ~ .+hemo*arms , data=data1train, family=binomial)
fit5<- glm(Y ~ .+homo*arms, data=data1train, family=binomial)
fit6<- glm(Y ~ .+race*arms , data=data1train, family=binomial)
fit7<- glm(Y ~ .+race*symptom , data=data1train, family=binomial)
fit8<- glm(Y ~ .+gender*symptom , data=data1train, family=binomial)
fit9<- glm(Y ~ .+arms*symptom , data=data1train, family=binomial)
fit10<- glm(Y ~ .+arms*wtkg , data=data1train, family=binomial)
fit11<- glm(Y ~ .+arms*drugs , data=data1train, family=binomial)##winner!!

#alias(fit1)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)
summary(fit8)
summary(fit9)

#AUC
pred1test <- predict(fit1, newdata = data1test, type = 'response')
pred2test <- predict(fit2, newdata = data1test, type = 'response')
pred3test <- predict(fit3, newdata = data1test, type = 'response')
pred4test <- predict(fit4, newdata = data1test, type = 'response')
pred5test <- predict(fit5, newdata = data1test, type = 'response')
pred6test <- predict(fit6, newdata = data1test, type = 'response')
pred7test <- predict(fit7, newdata = data1test, type = 'response')
pred8test <- predict(fit8, newdata = data1test, type = 'response')
pred9test <- predict(fit9, newdata = data1test, type = 'response')
pred10test <- predict(fit10, newdata = data1test, type = 'response')
pred11test <- predict(fit11, newdata = data1test, type = 'response')
#as.numeric(ctest$Y)* log(pred1test)

roc1t<-roc(data1test$Y~pred1test)
roc2t<-roc(data1test$Y~pred2test)
roc3t<-roc(data1test$Y~pred3test) 
roc4t<-roc(data1test$Y~pred4test)
roc5t<-roc(data1test$Y~pred5test) 
roc6t<-roc(data1test$Y~pred6test)
roc7t<-roc(data1test$Y~pred7test)
roc8t<-roc(data1test$Y~pred8test)
roc9t<-roc(data1test$Y~pred9test) 
roc10t<-roc(data1test$Y~pred10test)
roc11t<-roc(data1test$Y~pred11test)

group_compare <-rbind(roc1t$auc,roc2t$auc,roc3t$auc,roc4t$auc,roc5t$auc,roc6t$auc,roc7t$auc,roc8t$auc,roc9t$auc,roc10t$auc,roc11t$auc)

group_compare <- data.frame(group_compare)

rownames(group_compare) <- c("model1", "model2", "model3", "model4", "model5", "model6", "model7", "model8", "model9", "model10", "model11")

colnames(group_compare) <- "AUC"

knitr::kable(group_compare)


