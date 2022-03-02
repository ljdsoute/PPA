#QUESTIONS ABOUT THIS DOCUMENT: Matthias.Bogaert@ugent.be

rm(list=ls())
setwd('./data/')

if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, MASS, skimr)

set.seed(10) #set.seed will make sure that you will have the same splits everytime

##########################################################################################################
# Exercise 1: Linear Regression
##########################################################################################################

#### 1. Linear regression interpretation ####
#############################################

#In this file, we use the advertising dataset
load('Advertising.Rdata')

#### 1.1. Linear Regression in R
lm1 <- lm(Sales ~ .,data=Advertising)

#Summary to look at all the important metrics
summary(lm1)

#Take out the individual performance metrics (Rsq and Adj. Rsq), use the dollar operator
summary(lm1)$r.sq
summary(lm1)$adj.r.sq

#to take a look at the coefficients
coef(lm1)

#To plot all the relevant plots
par(mfrow=c(2,2))
plot(lm1)

#Get the 95% confidence interval for the predictors.
confint(lm1)

#Interaction terms
#Include an interaction term using ':'
lm_interact_1 <- lm(Sales ~ TV + Radio + Newspaper + TV:Newspaper,data=Advertising)
summary(lm_interact_1)

#Include both the individual terms and the interaction term using '*'
lm_interact_2 <- lm(Sales ~ TV*Newspaper + Radio,data=Advertising)
summary(lm_interact_2)

plot(lm_interact_2)

#What if we want to determine the most important variable?
#Can we compare the effect sizes?
Advertising_scaled <- Advertising %>% mutate(across(1:3,scale))
lm1_sc <- lm(Sales ~ .,data=Advertising_scaled)

#### 2. Prediction ####
#######################

#Split data in 50/50 train and test
ind <- sample.int(n=nrow(Advertising),size=nrow(Advertising)*0.50)
train <- Advertising[ind,]
test <- Advertising[-ind,]

#Make a linear model using the training set
lm_model <- lm(Sales ~ .,data=train)

#Using the model built in B., make a prediction using the test set
preds <- predict(lm_model,test)

#Calculate R^2 as a performance measure.
#Rsq = 1 - RSS/TSS
(RSS = sum((test$Sales - preds)**2))
(TSS = sum((test$Sales - mean(test$Sales))**2))
(Rsq = 1 - RSS/TSS)

#Or another way?
#cor(preds, test$Sales)**2

#### 3. Model Selection ####
############################

#### a. Model selection done manually.

#null model
lm_null <- lm(Sales ~ 0,data=Advertising)

#one predictor models: one with TV, one with Radio, one with sales
lm_onepred_TV <- lm(Sales ~ TV,data=Advertising)
lm_onepred_Radio <- lm(Sales ~ Radio,data=Advertising)
lm_onepred_Newspaper <- lm(Sales ~ Newspaper,data=Advertising)

#Two predictor model: one with TV + Radio, one with TV + Newspaper, Radio + Newspaper
lm_twopred_TV_Radio <- lm(Sales ~ TV + Radio,data=Advertising)
lm_twopred_TV_Newspaper <- lm(Sales ~ TV + Newspaper,data=Advertising)
lm_twopred_Radio_Newspaper <- lm(Sales ~ Radio + Newspaper,data=Advertising)

#One three predictor model
lm_threepred_TV_Radio_Newspaper <- lm(Sales ~ TV + Radio + Newspaper,data=Advertising)

#Select best null, one predictor, two predictor and three predictor model
#use the summary function for all of the above created models
summary(lm_null) #selected
summary(lm_onepred_TV) #selected
summary(lm_onepred_Radio)
summary(lm_onepred_Newspaper)
summary(lm_twopred_TV_Radio) #selected
summary(lm_twopred_TV_Newspaper)
summary(lm_twopred_Radio_Newspaper)
summary(lm_threepred_TV_Radio_Newspaper) #selected

#### b. Approximate test error using selection criteria: AIC, BIC, Adj.Rsq, CVSS
#AIC
AIC(lm_null)
AIC(lm_onepred_TV)
AIC(lm_twopred_TV_Radio)
AIC(lm_threepred_TV_Radio_Newspaper)

#BIC
BIC(lm_null)
BIC(lm_onepred_TV)
BIC(lm_twopred_TV_Radio)
BIC(lm_threepred_TV_Radio_Newspaper)

#Cross-validated sum of squares
CVSS <- function(model) {
  return(sum((model$residuals/(1 - hatvalues(model)))^2))
}

CVSS(lm_null)
CVSS(lm_onepred_TV)
CVSS(lm_twopred_TV_Radio)
CVSS(lm_threepred_TV_Radio_Newspaper)

#Adj.Rsq
summary(lm_null)$adj.r.sq
summary(lm_onepred_TV)$adj.r.sq
summary(lm_twopred_TV_Radio)$adj.r.sq
summary(lm_threepred_TV_Radio_Newspaper)$adj.r.sq

#ALL measures point to lm_twopred_TV_Radio as best model

#### c. Purist approach: calculate the cross-validated test error

#In this case we use K-fold cross validation.
k <- 3 #Number of folds

#Split data into k folds: 
#scramble the observations in the training set, 
#next make 3 folds using this scrambled dataset.
advertising_random <- Advertising[sample.int(n=nrow(Advertising),
                                             size=nrow(Advertising)),]
#split will will divide the data in several 'splits' based on certain grouping variables
#The grouping variable can be created wit the cut function: 
#here you cut your data into 3 non-overlapping groups
folds <- split(x = advertising_random,
               f = cut(1:nrow(advertising_random),breaks=k))

#Create empty MSE slots
MSE_lm_null <- MSE_lm_onepred <- MSE_lm_twopred <- MSE_lm_threepred <- numeric()

#Start the loop
for (j in 1:k) {
  #1. Make the training and test set
  test <- folds[[j]]
  train <- bind_rows(folds[-j])
  
  #2. Build the models using the best model per number of predictors identified in the beginning of step 2.1.
  lm_null <- lm(Sales ~ 0,data=train)
  lm_onepred <- lm(Sales ~ TV,data=train)
  lm_twopred <- lm(Sales ~ TV + Radio,data=train)
  lm_threepred <- lm(Sales ~ TV + Radio + Newspaper,data=train)
  
  #3. Make a prediction for each model using the test set
  pred_lm_null <- predict(lm_null,test)
  pred_lm_onepred <- predict(lm_onepred,test)
  pred_lm_twopred <- predict(lm_twopred,test)
  pred_lm_threepred <- predict(lm_threepred,test)
  
  #4. Calculate MSE
  MSE_lm_null[j] <- mean((test$Sales - pred_lm_null)**2)
  MSE_lm_onepred[j] <- mean((test$Sales - pred_lm_onepred)**2)
  MSE_lm_twopred[j] <- mean((test$Sales - pred_lm_twopred)**2)
  MSE_lm_threepred[j] <- mean((test$Sales - pred_lm_threepred)**2)
  
}

#Select best model
mean(MSE_lm_null)
mean(MSE_lm_onepred)
mean(MSE_lm_twopred) #Best model
mean(MSE_lm_threepred)

#Another way is to use the rsample function to create the folds
p_load(rsample)

#Let's create 3-folds with 1 repeats
folds <- Advertising %>% vfold_cv(v=3) 

#Let's have a look: 3 splits in total
folds$splits
#splits contains 100 splits: analysis is the training set, assess the test set
folds$splits[[1]] %>% analysis() %>% head()
folds$splits[[1]] %>% assessment() %>% head()

#Make a prediction model: model lm on the folds and compute rmse
model_mse <- function(data, vars) {
  rf <- lm(Sales ~ ., data = analysis(data)[,vars, drop = FALSE])
  pred <- predict(rf,assessment(data))
  mse <- mean((pred - assessment(data)$Sales)^2)
  return(mse)
}

#Now calculate the performance for each model
MSE_lm_null <- map_dbl(folds$splits, function (data) {
  model_mse(data, vars = 'Sales')
})
mean(MSE_lm_null)
MSE_lm_onepred <- map_dbl(folds$splits, function (data) {
  model_mse(data, vars = c('Sales', 'TV'))
})
mean(MSE_lm_onepred)
MSE_lm_twopred <- map_dbl(folds$splits, function (data) {
  model_mse(data, vars = c('Sales', 'TV', 'Radio'))
})
mean(MSE_lm_twopred)
MSE_lm_twopred <- map_dbl(folds$splits, function (data) {
  model_mse(data, vars = c('Sales', 'TV', 'Radio', 'Newspaper'))
})
mean(MSE_lm_threepred)

#### d. Select best model automatically using AIC

lm_stepAIC <- stepAIC(lm_threepred_TV_Radio_Newspaper,method="forward")
stepAIC(lm_threepred_TV_Radio_Newspaper,method="backward")
stepAIC(lm_threepred_TV_Radio_Newspaper,method="both")


#### e. Inspect for misformulations
train$predictions <- NULL

ggplot(train, aes(TV, Sales))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  stat_smooth(method = 'lm', col = "red")

ggplot(train, aes(Radio, Sales))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  stat_smooth(method = 'lm', col = "red")

# Linear model is good specification here