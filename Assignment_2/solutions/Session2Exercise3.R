#QUESTIONS ABOUT THIS DOCUMENT: Matthias.Bogaert@ugent.be

rm(list=ls())
setwd('./data/')

if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, MASS, skimr)

set.seed(10) #set.seed will make sure that you will have the same splits everytime

##########################################################################################################
# Exercise 3: Credit Scoring
###########################################################################################################

rm(list= ls())

#Read in dataset
hmeq <- read_csv(file = 'hmeq.csv', col_names = TRUE, 
                 locale = locale(decimal_mark = "."))

#Explore the data
hmeq %>% glimpse()
hmeq %>% skim()
#More information on the data set? https://www.kaggle.com/ajay1735/hmeq-data 

#Create splitting indices
ind <-sample(x =1:nrow(hmeq), size = nrow(hmeq),replace = FALSE)
trainind <- ind[1:round(length(ind)*.70)]
valind <- ind[(round(length(ind)*.70)+1):round(length(ind)*.85)]
testind <- ind[round(length(ind)*.85+1):length(ind)] 

### 1. Non-linear model: decision tree
p_load(rpart)

#Create the sets and separate the response
train <- hmeq[trainind,]
y_train <- as.factor(as.character(train$BAD))
train$BAD <- NULL

test <- hmeq[testind,]
y_test <- as.factor(as.character(test$BAD))
test$BAD <- NULL

val <- hmeq[valind,]
y_val <- as.factor(as.character(val$BAD))
val$BAD <- NULL

trainBIG <- rbind(train,val)
y_trainBIG <- as.factor(c(as.character(y_train),
                          as.character(y_val)))

#If we want to run model: must always first handle missing values
#Because it wasn't specifically stated in the assignment, does not mean you don't have to do it.
#since we are working with a train, val and test set: avoid data leakage!
p_load(imputeMissings)

#Let's perform median and mode imputation
#for train & val
values <- imputeMissings::compute(train)
train <- imputeMissings::impute(train,object=values)
val <- imputeMissings::impute(val,object=values)

#for trainbig & test
values2 <- imputeMissings::compute(trainBIG)
trainBIG <- imputeMissings::impute(trainBIG,object=values2)
test <- imputeMissings::impute(test,object=values2)
#Is it really necessary to do this separately for trainBIG?
#Can't you just use the values of train? 

colSums(is.na(train))
colSums(is.na(trainBIG))
colSums(is.na(val))
colSums(is.na(test))

#Tune cp parameter
candidates <- seq(0.00001,0.2,by=0.0010)
aucstore <- numeric(length(candidates))

j <- 0
for (i in candidates) {
  j <- j + 1
  tree <- rpart(y_train ~ .,
                control=rpart.control(cp = i),
                train)
  predTree <- predict(tree,val)[,2]
  aucstore[j] <- AUC::auc(roc(predTree,y_val))
  if (j %% 20==0) cat(j/length(candidates)*100,"% finished\n")
}

tree <- rpart(y_trainBIG ~ ., 
              control=rpart.control(cp = candidates[which.max(aucstore)]),
              trainBIG)
predTREE <- predict(tree,test)[,2]

#Evaluate performance
(auc_dt <- AUC::auc(roc(predTREE,y_test))) 
#Decent performance but bank wants to use logistic regression ==> check if resulting relationships are linear

### 2. Plotting: see if fitted (==> use trainbig) relationships are linear

probabilities <- predict(tree, trainBIG)[,2]

#since we can only apply transformations on the numeric variables, only select these ones (>= 2 to ensure no inclusion of dummy variables)
train_num <- trainBIG %>% 
  dplyr::select_if(is.numeric) %>% 
  dplyr::select_if(~max(., na.rm = TRUE) >= 2)
predictors <- names(train_num)

p_load(tidyr)

#Make a long format table with a key value pair (the predictor and its value) and the logit
train_num <- train_num %>%
  mutate(logit = probabilities) %>%
  pivot_longer(-logit, names_to = "predictors", values_to = "predictor.value",)

#Finally, create a ggplot for all variables and logit using a loess smoother to spot for nonlinearities
ggplot(train_num, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  stat_smooth(method = 'lm', col = "red") +
  facet_wrap(~predictors, scales = "free")

#Clear non-linearities

### 3. Create linear model: logistic regression

# Log reg requires all character variables to be one-hot encoded
cats <- categories(train[,c('REASON', 'JOB')])
dummies_train <- dummy::dummy(x = train[,c('REASON', 'JOB'), drop = FALSE],
                              object = cats,
                              int = FALSE) %>% mutate_all(as.factor)
dummies_train$REASON_ <- dummies_train$JOB_ <- NULL #Remove 'unknown' categories
dummies_trainBIG <- dummy::dummy(x = trainBIG[,c('REASON', 'JOB'), drop = FALSE],
                                 object = cats,
                                 int = FALSE) %>% mutate_all(as.factor)
dummies_trainBIG$REASON_ <- dummies_trainBIG$JOB_ <- NULL #Remove 'unknown' categories
dummies_test <- dummy::dummy(x = test[,c('REASON', 'JOB'), drop = FALSE],
                             object = cats,
                             int = FALSE) %>% mutate_all(as.factor)
dummies_test$REASON_ <- dummies_test$JOB_ <- NULL #Remove 'unknown' categories
dummies_val <- dummy::dummy(x = val[,c('REASON', 'JOB'), drop = FALSE],
                            object = cats,
                            int = FALSE) %>% mutate_all(as.factor)
dummies_val$REASON_ <- dummies_val$JOB_ <- NULL #Remove 'unknown' categories

train_lr <- bind_cols(train %>% dplyr::select(-c(REASON,JOB)), dummies_train)
trainBIG_lr <- bind_cols(trainBIG %>% dplyr::select(-c(REASON,JOB)), dummies_trainBIG)
test_lr <- bind_cols(test %>% dplyr::select(-c(REASON,JOB)), dummies_test)
val_lr <- bind_cols(val %>% dplyr::select(-c(REASON,JOB)), dummies_val)

#Train LR: Use LASSO

(LRl1 <- glmnet(x=data.matrix(train_lr),y=y_train,
                family="binomial"))
aucs <- numeric()
for (i in 1:length(LRl1$lambda)) {
  predLRl1 <- predict(LRl1,newx=data.matrix(val_lr),
                      type="response",
                      s=LRl1$lambda[i])
  aucs[i] <- AUC::auc(roc(as.numeric(predLRl1),factor(y_val)))
}
(LR.lambda <- LRl1$lambda[which.max(aucs)]) #Low regularization

LRl1 <- glmnet(x=data.matrix(trainBIG_lr),y=y_trainBIG,
               family="binomial")
predLRl1 <- as.numeric(predict(LRl1,newx=data.matrix(test_lr),
                               type="response",
                               s=LR.lambda))
#Finally we assess the performance of the model
(auc_l1 <- AUC::auc(roc(predLRl1,factor(y_test))))

(auc_dt-auc_l1)/auc_dt

#The LR clearly performs much worse + clear indications of nonlinearities
#Let us check whether transformations result into much better performance

### 4. Transform data: use Yeo Johnson 
p_load(car)

train_yj <- train_lr %>% mutate_if(is.numeric,~yjPower(., 0.2)) 
trainBIG_yj <- trainBIG_lr %>% mutate_if(is.numeric,~yjPower(., 0.2)) 
test_yj <- test_lr %>% mutate_if(is.numeric,~yjPower(., 0.2)) 
val_yj <- val_lr %>% mutate_if(is.numeric,~yjPower(., 0.2)) 

(LRl1 <- glmnet(x=data.matrix(train_yj),y=y_train,
                family="binomial"))
aucs <- numeric()
for (i in 1:length(LRl1$lambda)) {
  predLRl1 <- predict(LRl1,newx=data.matrix(val_yj),
                      type="response",
                      s=LRl1$lambda[i])
  aucs[i] <- AUC::auc(roc(as.numeric(predLRl1),factor(y_val)))
}
(LR.lambda <- LRl1$lambda[which.max(aucs)]) #Low regularization

LRl1 <- glmnet(x=data.matrix(trainBIG_yj),y=y_trainBIG,
               family="binomial")
predLRl1 <- as.numeric(predict(LRl1,newx=data.matrix(test_yj),
                               type="response",
                               s=LR.lambda))
#Finally we assess the performance of the model
(auc_yj <- AUC::auc(roc(predLRl1,factor(y_test))))

### Able to drive up performance slightly, but remains far off the performance of DT 
### ==> better to use DT and interpret tree (still highly interpretable)

#Go the extra mile: add another loop to determine the optimal YJ parameter (e.g., for DEBTINC and VALUE)
#Make sure that you also use the optimal lambda value for every YJ parameter


###########################################################################################################

