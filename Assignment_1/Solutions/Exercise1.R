# QUESTIONS ABOUT THIS DOCUMENT: matthias.bogaert@ugent.be

rm(list=ls())
setwd('D:/2e_sem/PPA/gith_project/PPA/Assignment_1')

if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse)

##########################################################################################################
#### Exercise 1: a newspaper publishing company ####
###########################################################################################################

########## 1. READING IN DATA ##############
############################################

########Subscriptions
###Read in
subscriptions <- read_delim(file = "Data/subscriptions.txt", 
                            delim = ";",
                            col_names = TRUE,
                            col_types = cols("c",
                                             "c",
                                             "c",
                                             "c",
                                             "c",
                                             "c",
                                             "i",
                                             "i",
                                             "c",
                                             "f",
                                             "f",
                                             "c",
                                             "c",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n"),
                            locale = locale(decimal_mark = "."))
#subscriptions %>% glimpse()
###find out if an ID can start with a 0; 
#if not we can encode it as an integer. Otherwise as character. 
min(as.integer(str_sub(subscriptions$SubscriptionID,
                       start=1,end=1)))
min(as.integer(str_sub(subscriptions$CustomerID,start=1,end=1)))
min(as.integer(str_sub(subscriptions$ProductID,start=1,end=1)))
min(as.integer(str_sub(subscriptions$FormulaID,start=1,end=1)))
#no ID starts with a 0.
subscriptions$SubscriptionID <- as.integer(subscriptions$SubscriptionID)
subscriptions$CustomerID <- as.integer(subscriptions$CustomerID)
subscriptions$ProductID <- as.integer(subscriptions$ProductID)
subscriptions$FormulaID <-  as.integer(subscriptions$FormulaID)
#subscriptions %>% glimpse()

###Handle dates
p_load(lubridate)
#we have read dates as characters and transform them to dates 
vars <- c("StartDate","EndDate","PaymentDate","RenewalDate")
subscriptions<- subscriptions %>% 
  mutate(across(vars,dmy))
#subscriptions %>% glimpse()

#Look at a summary
summary(subscriptions)

#Or use the skimr package
p_load(skimr)
subscriptions %>% skim()
subscriptions %>% skim() %>% summary()

########Credit
credit <- read_delim("./Data/credit.txt", delim = ";",
                     col_names = TRUE,
                     col_types = 
                       cols("n", "n", "f", "c", "f", "n", "n"))
#credit %>% glimpse()
#transform date 
credit$ProcessingDate <- dmy(credit$ProcessingDate)
summary(credit)

########Customers
customers <- read_delim(file = "./Data/customers.txt", delim=";",
                        col_names=TRUE, 
                        col_types =cols("i","f","c","f","c","c"))

customers$DOB <- dmy(customers$DOB)

#customers %>% glimpse()

########Complaints
complaints <- read_delim("./Data/complaints.txt", delim =";", 
                        col_names = TRUE,
                        col_types = cols("i","i","f","c", "f", "f", "f"))

complaints$ComplaintDate <- dmy(complaints$ComplaintDate)
#complaints %>% glimpse()

###########################################################################################################

########## 2. ERD ##############
################################

####SUBSCRIPTIONS --> CREDIT
#left outer join
merged <-left_join(subscriptions[,"SubscriptionID"],
               data.frame(CreditID=credit$CreditID,
                          SubscriptionID=credit$SubscriptionID,
                          fromy=1),
               by="SubscriptionID")
glimpse(merged)
#count the number of credit per subscription
agg <- merged %>% group_by(SubscriptionID) %>% 
  summarise(x = sum(fromy))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 0..25 = 0..N

#### CREDIT --> SUBSCRIPTIONS
#right outer join
merged <-right_join(data.frame(SubscriptionID=subscriptions$SubscriptionID,
                               fromx=1),
               data.frame(CreditID=credit$CreditID,
                          SubscriptionID=credit$SubscriptionID),
               by="SubscriptionID")
glimpse(merged)
#count the number of subscriptions per credit
agg <- merged %>% group_by(CreditID) %>% 
  summarise(x = sum(fromx))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 1

#### SUBSCRIPTIONS --> CUSTOMERS
#left outer join
merged <-left_join(data.frame(SubscriptionID=subscriptions$SubscriptionID,
                              CustomerID=subscriptions$CustomerID),
                   data.frame(CustomerID=customers$CustomerID, 
                              fromy=1),
                   by="CustomerID")
glimpse(merged)
#count the number of customers per subscription
agg <- merged %>% group_by(SubscriptionID) %>% 
  summarise(x = sum(fromy))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 1


#### CUSTOMERS --> SUBSCRIPTIONS
#right outer join
merged <-right_join(data.frame(SubscriptionID=subscriptions$SubscriptionID,
                               CustomerID=subscriptions$CustomerID,
                               fromx=1),
               data.frame(CustomerID=customers$CustomerID),
               by="CustomerID")
glimpse(merged)
#count the number of subscriptions per customer
agg <- merged %>% group_by(CustomerID) %>% 
  summarise(x = sum(fromx))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 1..50 = 1..N

#### CUSTOMERS --> COMPLAINTS
#left outer join
merged <-left_join(data.frame(CustomerID=customers$CustomerID),
                   data.frame(CustomerID=complaints$CustomerID,
                              ComplaintID=complaints$ComplaintID,
                              fromy=1),
                   by="CustomerID")
glimpse(merged)
#count the number of complaints per customer
agg <- merged %>% group_by(CustomerID) %>% 
  summarise(x = sum(fromy))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 0..40 = 0..N

#### COMPLAINTS --> CUSTOMERS
#right outer join
merged <-right_join(data.frame(CustomerID=customers$CustomerID,fromx=1),
                    data.frame(CustomerID=complaints$CustomerID,ComplaintID=complaints$ComplaintID),
                    by="CustomerID")
glimpse(merged)
#count the number of customers per complaint
agg <- merged %>% group_by(ComplaintID) %>% 
  summarise(x = sum(fromx))
glimpse(agg)
#set NA to 0
agg$x[is.na(agg$x)] <- 0
min(agg$x) ; max(agg$x)
# 1..2 = 1..N

###########################################################################################################

####### 3. VISUALIZATION ########
#################################

#### a. Subscriptions: Are the number of subscriptions and customers declining? 

#See how the number of subscriptions evolves over the years
#By start date
subs <- subscriptions %>% group_by(date = year(StartDate)) %>% 
  summarise(n = n())
ggplot(subs, aes(x = date , y = n)) + geom_bar(stat = 'identity') + 
  ggtitle('Subscriptions over the years') + 
  labs (x = 'Start date (year)')
#By renewal date
subs <- subscriptions %>% group_by(date = year(RenewalDate)) %>% 
  summarise(n = n())
ggplot(subs, aes(x = date, y = n)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Subscriptions over the years') + labs(x = 'Renewal date (year)')

#What do you notice?
# -We see that the number of people that start a subscription is declining since 2008.
# -We see that the number of people that renew their subscription is slighty declining in 2010.
# -Both graphs suggest that the firms has a potential churn problem, 
#so the company must take a pro-active stance in order to reverse this trend.

#See how the number of customers evolves over the years
#By start date
cust <- subscriptions %>% 
  distinct(CustomerID, .keep_all = TRUE) %>% 
  select(c(CustomerID, StartDate))
cust <- cust %>%  group_by(date = year(StartDate)) %>% 
  summarise(n = n())
ggplot(cust, aes(x = date, y = n)) + geom_bar(stat = 'identity') + 
  ggtitle('Customers over the years') + labs(x = 'Start date (years)')
#By renewal date
cust <- subscriptions %>% 
  distinct(CustomerID, .keep_all = TRUE) %>% 
  select(c(CustomerID, RenewalDate))
cust <- cust %>%  group_by(date = year(RenewalDate)) %>% 
  summarise(n = n())
ggplot(cust, aes(x = date, y = n)) + geom_bar(stat = 'identity') + 
  ggtitle('Customers over the years') + 
  labs(x = 'Renewal date (year)')

#What do you notice?
# - Since we only look at the unique customers, 
# the plot shows us that there are not a lot of new customers joining. 
# - The second plot demonstrates the churn problem. The number of customers that are renewing is declining, hence we are loosing customers. 

#See how the number of incoming vs outgoing customers
#This plot will nicely show you the customer turnover

#Calculate the number of incoming customers (starting customers)
in_customers <- subscriptions %>% 
  group_by(StartDate) %>% 
  summarise(NbrStartingCust = n()) %>%
  rename(Date = StartDate) %>% 
  arrange(Date)

#Calculate the number of outgoing customers (ending customers)
out_customers <- subscriptions %>% 
  group_by(EndDate) %>% 
  summarise(NbrEndingCust = n()) %>%
  rename(Date = EndDate) %>% 
  arrange(Date)

#Join and calculate the difference between the two 
#and take the cumulative sum
in_out <- full_join(in_customers, out_customers) %>% 
  replace_na(list(NbrStartingCust = 0, NbrEndingCust = 0)) %>% 
  arrange(Date) %>% 
  mutate(Diff = NbrStartingCust - NbrEndingCust) %>% 
  mutate(Cumulative = cumsum(Diff))

#Plot it
ggplot(in_out, aes(x = Date, y = Cumulative)) + geom_line()


#### b. Customers
# Look at the age distribution of the customers
age <- data.frame(customerID = customers$CustomerID,
                   age =floor((as.numeric(Sys.Date()) - as.numeric(customers$DOB))/365))
summary(age)
ggplot(data = age, aes(x = age)) + geom_histogram(binwidth = 5)
#Impute negatives and people with age of 121 with NA
#The oldest man ever in Belgium was 108, so it is impossible that people are of older age. 
#Clear issue with 121 as 'default' age ( 1/1/1900 vs. data of calculation)
age$age <- ifelse(age$age < 0 | age$age > 108, NA, age$age)

#Make a histogram
ggplot(data = age, aes(x = age)) + geom_histogram(binwidth = 5)

#Already much better, but what with the huge number of missing values? 

#Check whether there is a difference between men and women
age$Gender <- customers$Gender
summary(age)
#Impute the empty cells with NA
age$Gender <- as.factor(ifelse(as.character(age$Gender) == '', 
                               NA, 
                               as.character(age$Gender)))

#Make a boxplot
ggplot(data = age, aes(y = age, x = Gender)) + geom_boxplot()
#Add a different colour for each gender
ggplot(data = age, aes(y = age, x = Gender, fill = Gender)) + geom_boxplot()

#Compare the distributions
ggplot(data = age, aes(x = age, y = ..density.., colour = Gender)) + 
  geom_freqpoly(binwidth = 5)

#Different plots always tell slightly different nuances, so don't be satisfied after creating one plot and really try to dig deeper each time.
#It is clear that gender has an influence on the typical age, use this in a situation where you would include age in your model

#### c. Credit
# See the evolution of the credit amount of the years
cred <- credit %>% group_by(date = year(ProcessingDate),
                            Action = ActionType) %>% 
  summarise(n = n())
ggplot(cred, aes( y= n, x = date)) + geom_bar(stat = 'identity') + labs(x = 'Year', 'Total credit')

# See how the different action types are used over the years
ggplot(cred, aes( y= n, x = date, 
                  colour = Action, fill = Action)) +
  geom_bar(stat = 'identity')

#How does this translate to the offered credit to the customers?
cred_amount <- credit %>% group_by(date = year(ProcessingDate), Action = ActionType) %>% 
  summarise(x = sum(Amount))

#Show how this financially impacts the firm
ggplot(cred_amount, aes( y= x, x = date)) + geom_bar(stat = 'identity') + labs(x = 'Year', 'Total credit')
ggplot(cred_amount, aes( y= x, x = date, colour = Action, fill = Action)) + geom_bar(stat = 'identity')

#Interestingly, we observe that the switch to offering extra newspapers rather than opening a credit line has caused a 
#significant reduction in costs, which may have been the motivation behind this. However, overall, costs were only 0-2000. If customers value
#EN different than CC, this may influence churn rates (more unsatisfied customers). This kind of visualization can be the input for calculating 
#a specific variable in an eventual churn model, which is also why it is key to visualize such situations, rather than directly diving into the
#predictive modeling.

#Payout is also very costly, especially given the relatively low frequency of it

#Check the cause behind the credit lines
table(credit$CreditSource, credit$ActionType)

#Create credit is (was given the evolution?) always preferred option, but is it the ideal option? ==> check in model

#### d. Complaints

#Check the most common complaints
#Instead of always making a ggplot object and adding layers, you can also use qplot.
#This function is very similar to the plot function (i.e., it is generic, meaning that it adapts output according to the data).
#1:non-delivery
qplot(complaints$ComplaintType)

#For non-delivery what were the most common solutions:
comp <- complaints %>% group_by(Complaint = ComplaintType, 
                               Solution = SolutionType) %>% 
  summarise(n = n())
#To improve readability at labels to solution type
comp$Solution  <- fct_recode(comp$Solution, 
                             'Unknown' = '', 
                             Credit = '1', 
                             "Post delivery" = '2', 
           "No solution" = "3", "Take precautions" = "4")
ggplot(comp[comp$Complaint == 1,], aes( x = Solution, y = n, fill = Solution)) + geom_bar(stat = 'identity')

#For non delivery what were the most common feedback types:
comp <- complaints %>% group_by(Complaint = ComplaintType, 
                                Feedback = FeedbackType) %>% 
  summarise(n = n())
comp$Feedback <- fct_recode(comp$Feedback, 
                            'Unknown' = '',
                            'Force Majeure' = '1', 
                            'Weather' = '2', 
                            'Error Postman' = '3', 
                            'Late' = '4', 
                            'Confirmed Delivery' = '5', 
                            'Strike' = '6')
ggplot(comp[comp$Complaint == 1,], aes( x = Feedback, y = n, fill = Feedback)) + geom_bar(stat = 'identity')

#A lot of man-made issues, perhaps problem is focused at some employees ==> inquiry might be interesting

###########################################################################################################

####### 4. BASETABLE CREATION ########
######################################

#We will start with a 'clean sleeve' but you can of course continue working with the existing data you already created.
rm(list = ls())

##### Subscription
#Read in the data from disk again
subscriptions <- read_delim(file = "subscriptions.txt", delim = ";",
                            col_names = TRUE,
                            col_types = cols("c",
                                             "c",
                                             "c",
                                             "c",
                                             "c",
                                             "c",
                                             "i",
                                             "i",
                                             "c",
                                             "f",
                                             "f",
                                             "c",
                                             "c",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n",
                                             "n"),
                            locale = locale(decimal_mark = "."))
vars <- c("StartDate","EndDate","PaymentDate","RenewalDate")
subscriptions<- subscriptions %>% 
  mutate(across(vars,dmy))

#Look at the structure of the data frame
#subscriptions %>% glimpse()
#First choose five variables and the CustomerID
subscriptions_sel <-subscriptions %>% 
  select(c("CustomerID","Pattern",
                                     "StartDate","PaymentType",
                                     "GrossFormulaPrice","TotalCredit"))

#seperate CustomerID from the other variables for easy processing
CustomerID <- subscriptions_sel %>% pull(CustomerID)
subscriptions_sel <- subscriptions_sel[,-1]

#subscriptions_sel %>% glimpse()

#We want to use pattern ==> know patterns
qplot(subscriptions_sel$Pattern)

#Almost all subscriptions are full (only not on sunday) ==> use this as reference category

#Create dummy variables
p_load(dummy)
dummy_vars <- dummy::dummy(subscriptions_sel %>% 
                             select(where(is.factor) | where(is.character)), 
                           int= TRUE)
#we can remove one reference category per original variable ==> use most common
table(subscriptions_sel$PaymentType)
dummy_vars$Pattern_1111110 <- dummy_vars$PaymentType_BT <- NULL
dummy_vars <- dummy_vars %>% 
  group_by(CustomerID = CustomerID) %>% 
  summarise_all(sum)

#Compute recency
maxStartDate <- subscriptions_sel %>% 
  group_by(CustomerID = CustomerID) %>% 
  summarise(x = max(StartDate))
glimpse(maxStartDate)
end_independent <- as.Date(Sys.Date())
maxStartDate <- maxStartDate %>% 
  mutate(recency = as.integer(end_independent-x))
maxStartDate$x <- NULL

#Check distribution
ggplot(data = maxStartDate, aes(x = recency)) + geom_histogram(binwidth = 100)

#Lowest recency > 3500: because data set used is old
#Also notice the two large peaks: probably annual effects (e.g., no renewal at end of the year)

#Finally compute the sum of GrossFormulaPrice and TotalCredit per customer
monetary <- subscriptions_sel %>% 
  group_by(CustomerID = CustomerID) %>% 
  summarise(GrossFormulaPrice_sum  = sum(GrossFormulaPrice),
            TotalCredit_sum = sum(TotalCredit))
glimpse(monetary)

#Is order of CustomerID still preserved?
identical(monetary$CustomerID,maxStartDate$CustomerID)
# TRUE
identical(dummy_vars$CustomerID,maxStartDate$CustomerID)
# TRUE

#Yes, so we don't need to merge, we can simply bind the three data frames.
maxStartDate$CustomerID <- monetary$CustomerID <- NULL
subs_final <- data.frame(dummy_vars,maxStartDate,
                         monetary)
glimpse(subs_final)
#Set the ID as an integer
subs_final$CustomerID <- as.integer(subs_final$CustomerID)

###### Credit
credit <- read_delim("credit.txt", delim = ";",
                     col_names = TRUE,
                     col_types = cols("n", "c", "f", "c", "f", "n", "n"))
credit$ProcessingDate <- dmy(credit$ProcessingDate)

#Merge credit with subs
subs_cred <- left_join(subscriptions[,c('SubscriptionID', 'CustomerID')],
                  credit[, c('CreditID', 'SubscriptionID')], by = 'SubscriptionID')
subs_cred$credit <- ifelse(is.na(subs_cred$CreditID), 0,1)
#Count the number of credit lines per customer
cred_final <- subs_cred %>% group_by(CustomerID) %>% 
  summarise(nbr_credit = sum(credit)) 

glimpse(cred_final)
#Set ID as integer
cred_final$CustomerID <- as.integer(cred_final$CustomerID)

###### Customers
customers <- read_delim(file = "customers.txt", delim=";",
                        col_names=TRUE, 
                        col_types =cols("i","f","c","f","c","c"))

customers$DOB <- dmy(customers$DOB)
cust_final <- customers %>% select(c('CustomerID', 'Gender'))

#Note that the unknown genders are registered as ''
#So replace empties with NA
cust_final$Gender <- ifelse(cust_final$Gender == 'M', 1,
                            ifelse(cust_final$Gender == 'F',0, NA)) %>% as.factor()

glimpse(cust_final)

###### Complaints
complaints <- read_delim("complaints.txt", delim =";", 
                         col_names = TRUE,
                         col_types = cols("i","i","f","c", "f", "f", "f"))

complaints$ComplaintDate <- dmy(complaints$ComplaintDate)
comp_final <- complaints %>% group_by(CustomerID) %>% 
  summarise(nbr_complaints = n())

glimpse(comp_final)

##### Merge them all together
#Put it all in one list to merge efficiently
mergelist <- list(subs_final, cred_final, 
                  cust_final, comp_final)
basetable <- Reduce(function(x,y) 
  left_join(x,y, by = 'CustomerID'), mergelist)

#Check for NAs
colSums(is.na(basetable))

#Nbr complaints is not randomly missing: no complaints recorded -> impute with zero
basetable$nbr_complaints[is.na(basetable$nbr_complaints)] <- 0 
colSums(is.na(basetable))

#Not true for TotalCredit & GrossFormulaPrice ==> use more elaborate imputation
#If no credit: total credit = 0
basetable$impute_flag <- ifelse(is.na(basetable$TotalCredit),1,0)
basetable$TotalCredit <- ifelse((basetable$nbr_credit==0 & is.na(basetable$TotalCredit)),0,basetable$TotalCredit)

#Use randomForest imputation for other values
p_load(imputeMissings)
basetable <- imputeMissings::impute(basetable, 
                                    method = 'randomForest', 
                                    flag = FALSE) 
#Just use on this data set, so you don't need to compute upfront
#Do not flag as we already have created flag upfront

colSums(is.na(basetable))

