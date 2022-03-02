#QUESTIONS ABOUT THIS DOCUMENT: Matthias.Bogaert@ugent.be

rm(list=ls())
setwd('./data/')

if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, MASS, skimr)

set.seed(10) #set.seed will make sure that you will have the same splits everytime

##########################################################################################################
# Exercise 2: Logistic Regression
###########################################################################################################

#### 1. Check basetable & Split the data ####
#############################################

load('basetable.Rdata')
basetable %>% skim()

#create indicators for the train/val/test set
ind <-sample(x =1:nrow(basetable), size = nrow(basetable),replace = FALSE)
trainind <- ind[1:round(length(ind)*.70)]
valind <- ind[(round(length(ind)*.70)+1):round(length(ind)*.85)]
testind <- ind[round(length(ind)*.85+1):length(ind)] 

#Test whether there are no intersects
intersect(trainind, valind)
intersect(valind, testind)
intersect(trainind,testind)

#Create the sets and separate the response
train <- basetable[trainind,]
y_train <- train$Acquisition
train$Acquisition <- NULL

test <- basetable[testind,]
y_test <- test$Acquisition
test$Acquisition <- NULL

val <- basetable[valind,]
y_val <- val$Acquisition
val$Acquisition <- NULL

trainBIG <- rbind(train,val)
y_trainBIG <- as.factor(c(as.character(y_train),
                          as.character(y_val)))

intersect(row.names(trainBIG), row.names(test))

table(y_train); table(y_val); table(y_test); table(y_trainBIG)

# State is a factor with 46 levels. Not all algorithms handle categorical variables that well.
# Therefore, it is always safer to transform the category-variables to dummies.
# Since there are 46 levels we will not include all states; this would lead to an explosion of the nbr of variables (and the athena servers)
# Therefore, we decide to focus on the top 10 states. In the dummy package you can do this with the option 'p'
# Make sure your dummies are all factors. 
# Another way would be to calculate the weight of evidence, feel free to do so as an exercise
# To avoid data leakage make sure that you select the categories on the train set 

p_load(dummy)
cats <- categories(train[,'state', drop = FALSE], p = 10)
dummies_train <- dummy::dummy(x = train[,'state', drop = FALSE],object = cats, 
                              int = FALSE)

#By focusing on the top 10 states, we make sure our data is not too sparse
sapply(dummies_train, table)/nrow(dummies_train)

#Now do this for val and test and trainBIG, let's just use the cats from train
#There is no data leakage in this case, but a purist approach would be to do it 
#for trainBIG as well
dummies_trainBIG <- dummy::dummy(x = trainBIG[,'state', drop = FALSE],object = cats, 
                                 int = FALSE)
dummies_test <- dummy::dummy(x = test[,'state', drop = FALSE],object = cats, 
                             int = FALSE)
dummies_val <- dummy::dummy(x = test[,'state', drop = FALSE],object = cats, 
                            int = FALSE)

train$state <- test$state <- val$state <- trainBIG$state <- NULL

train <- bind_cols(train, dummies_train)
trainBIG <- bind_cols(trainBIG, dummies_trainBIG)
test <- bind_cols(test, dummies_test)
val <- bind_cols(val, dummies_val)
#save(train,val,test,trainBIG,y_train, y_test, y_val, y_trainBIG, file = 'trainvaltest.Rdata')

#### 3. Logistic regression ####
################################

rm(list=ls())

load('trainvaltest.Rdata')

p_load(AUC)

#### a. a normal and a stepwise logistic regression
LR <- glm(formula = y_trainBIG~., data = trainBIG, 
          family = binomial("logit"))
LR

#Take a look at the coefficients
#The log odds ratio
coefficients(LR)
#The odds ratio
exp(coefficients(LR))

#make a prediction
predLR <- as.numeric(predict(LR,test, type='response'))
(auc_lr <- AUC::auc(AUC::roc(predLR, y_test))) 

#Build a stepwise losgitic regression model
LRstep <- step(object = LR, direction = 'both')
predLRstep <- as.numeric(predict(LRstep, test,type = 'response'))
(auc_step <- AUC::auc(roc(predLRstep, y_test)))          

# performance of our original model is better then the stepwise function
# the step has deleted several states and the variance of the registrations
# it could be that the original LR is overfitting, however to be sure we should perform regularization

#### b. LASSO
p_load(glmnet)

#Build a regularized logistic regression model
#Set alpha=0 if you want ridge regression
#The default if LASSO
#The package standardizes the variables for you
(LRl1 <- glmnet(x=data.matrix(train),y=y_train,
                family="binomial"))
#Df is the number of variables that are active (i.e., coefficient > 0)
#%Dev is an evaluation metric we are not really interested in.
#Lambda is the degree to which the sum of the absolute values of the
#coefficients is penalized. Higher lambda means that coefficients will
#be smaller.

#Plot the lambda paramater
par(mfrow = c(1,1))
plot(LRl1, xvar= 'lambda') #Setting label = TRUE will add the variable number

#Look at the coefficients of the LR model for high and low values of lambda
coef(LRl1)[,1:2]
coef(LRl1)[,(ncol(coef(LRl1))-1):ncol(coef(LRl1))]

### i. interpretation
#Bigger values of lambda result in more variables being shrunk to zero
#However, we see that even with a very small lambda some variables are deleted (VAR)
#For large values of lambda there is still a variable active: the intercept and recency

#Lets to a prediction on the validation set to see the effect of high and low values of lambda
#high lambda
predLRl1 <- predict(LRl1,newx=data.matrix(val),
                    type="response",
                    s=LRl1$lambda[1])
AUC::auc(roc(as.numeric(predLRl1),y_val))

predLRl1 <- predict(LRl1,newx=data.matrix(train),
                    type="response",
                    s=LRl1$lambda[1])
AUC::auc(roc(as.numeric(predLRl1),y_train))

predLRl1 <- predict(LRl1,newx=data.matrix(val),
                    type="response",
                    s=LRl1$lambda[2])
AUC::auc(roc(as.numeric(predLRl1),y_val))

#extremely low lambda
predLRl1 <- predict(LRl1,newx=data.matrix(val),
                    type="response",
                    s=LRl1$lambda[ncol(coef(LRl1))])
AUC::auc(roc(as.numeric(predLRl1),y_val))

#With the largest value of lambda, we clearly notice underfitting (high bias).
#There is a small difference between the second value of lambda and the largest value, 
#This means that recency is probably a strong indicator of acquisition.

### ii. cross-validate
#All lambda values are stored in the LRl1 glmnet model
#Just select the lambda with the $-operator from the object and sequence over all its values
aucs <- numeric()
for (i in 1:length(LRl1$lambda)) {
  print(i)
  predLRl1 <- predict(LRl1,newx=data.matrix(val),
                      type="response",
                      s=LRl1$lambda[i])
  aucs[i] <- AUC::auc(roc(as.numeric(predLRl1),y_val))
}

#Let's determine the optimal lambda value
plot(1:length(LRl1$lambda),aucs,type="l")
(LR.lambda <- LRl1$lambda[which.max(aucs)])

#With this final lambda we re-do the analysis on the big training set
LRl1 <- glmnet(x=data.matrix(trainBIG),y=y_trainBIG,
               family="binomial")
#We then use that model with the optimal lambda.
predLRl1 <- as.numeric(predict(LRl1,newx=data.matrix(test),
                               type="response",
                               s=LR.lambda))
#Finally we assess the performance of the model
(auc_l1 <- AUC::auc(roc(predLRl1,y_test)))
# We see that the performance is bigger than in our previous step (with the train and val set) 
# This is because we now train on our trainBIG.
