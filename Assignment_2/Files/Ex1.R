rm(list=ls())
setwd('D:/2e_sem/PPA/gith_project/PPA/Assignment_2')


# Load the tidyverse package
if (!require("pacman")) install.packages("pacman")
require("pacman", character.only = TRUE, quietly = TRUE)
p_load(tidyverse)

# Set seed to make results reproducible
set.seed(123)

##Exercise 1
###Q1:
#Read in advertising Data
load("./Data/Advertising.RData")

Advertising %>% glimpse()
summary(Advertising)
#???build linear regression model
Sales.lm <- lm(Advertising$Sales ~ Advertising$TV+Advertising$Radio+Advertising$Newspaper+Advertising$TV*Advertising$Newspaper, data = Advertising)

summary(Sales.lm)

###Q2:
# create idicators randomize order of indicators
allind <- sample(x = 1:nrow(Advertising), size = nrow(Advertising))

# split in two equal parts
trainind <- allind[1:round(length(allind) * 0.5)]
testind <- allind[(round(length(allind) * (0.5)) + 1):length(allind)]
# actual subsetting
train <- Advertising[trainind, ]
test <- Advertising[testind, ]

nrow(train)

#Let's make default model.
model = lm(train$Sales~., data=train)
summary(model)
par(mfrow=c(2,2))
plot(model)

#predictors
pred <- predict(model, newdata = test)
#function to calculate R-squared
r_squared <- function(true, predictions) {
  return(1 - ((sum((true - predictions)^2))/sum((true - mean(true))^2)))
}
#print R-squared value
r_squared(test$Sales, pred)

###Q3:
###a)
m0 <- lm(train$Sales~1, data = train)
m1tv <- lm(train$Sales~train$TV, data = train)
m1radio <- lm(train$Sales~train$Radio, data = train)
m1newspaper <- lm(train$Sales~train$Newspaper, data = train)
m2tvradio <-  lm(train$Sales~train$TV+train$Radio, data = train)
m2tvnewspaper<-  lm(train$Sales~train$TV+train$Newspaper, data = train)
m2newspaperradio<-  lm(train$Sales~train$Newspaper+train$Radio, data = train)
m3<-  lm(train$Sales~train$TV+train$Radio+train$Newspaper, data = train)

#use summary to find the best models
summary(m0)
summary(m1tv)
summary(m1radio)
summary(m1newspaper)
summary(m2newspaperradio)
summary(m2tvnewspaper)
summary(m2tvradio)
summary(m3)
#best models per group: m0, m1tv, m2tvradio, m3

###b)







