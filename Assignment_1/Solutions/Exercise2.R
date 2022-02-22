# QUESTIONS ABOUT THIS DOCUMENT: matthias.bogaert@ugent.be

rm(list=ls())
setwd('D:/2e sem/pred&prescr/gith project/PPA_Assignment_1')

#######################################################################################################################
#### Exercise 2: Weibo, a social media dataset ####
#######################################################################################################################

#######################################################################################################################
##### Data understanding

if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(bit64, dummy, tidyverse, skimr)

# Read in the data
weibo_user <- read_csv('weibo_user.csv', col_names = TRUE)
user_post <- read_csv('user_post.csv', col_names = TRUE,
                         col_types = cols("n",
                                          "c",
                                          "c",
                                          "n",
                                          "c",
                                          "i",
                                          "i",
                                          "l",
                                          "c"))
Response <- read_csv('labels.csv', col_names = TRUE)

# inspect the data
weibo_user %>% skim()
user_post %>% skim()

table(Response$x)

# The first column of the weibo_user and the Response table is the row number. We will remove it since it is not useful.
weibo_user$X1 <- Response$...1 <- NULL

#######################################################################################################################
##### User post

## Clean-up the table
# repost_id & inner_flag is never completed so delete it
user_post$repost_post_id <- user_post$inner_flag <- NULL

# if you don't have a post_id or poster_id we cannot merge it with the other tables, so remove these rows
vars <- c("post_id", "poster_id")
user_post_imp <- user_post %>% drop_na(vars)

#content due to bad translation ==> no worries, won't use it anyway
user_post_imp %>% skim()

#One line which misses url, reposts, and comments ==> drop as well
user_post_imp <- user_post_imp %>% drop_na('poster_url')
user_post_imp %>% skim()

# set the date variable to a Date object
p_load(lubridate)
#Look at the post_time variable to see the format
user_post_imp %>% select(post_time) %>% slice_head()
#2014/8/17 21:00 -> year, month, day hour:minutes
user_post_imp$post_time <- ymd_hm(user_post_imp$post_time)

## Prepare the table
# Our level of analysis is the the poster ID, so we need to aggregate on poster ID level
user_post_imp$poster_id %>% n_unique() #431 unique users

# Calculate the total number of reposts and comments per poster ID
cols <- c('repost_num', 'comment_num')
total <- user_post_imp %>% group_by(poster_id) %>% 
  summarise(across(all_of(cols), sum))

# Calculate the recency of a post , we will use the Sys.Date()
end_date <- Sys.time()
recency <- user_post_imp %>% group_by(poster_id) %>% 
  summarise(recency = end_date-max(post_time))

#Combine the tables
user_post_prep <- left_join(total, recency)

# add the prefix 'posts_'. In this way, we know from which table the variables come from.
user_post_prep <- user_post_prep %>%
  rename_at(names(user_post_prep)[-1],~paste('posts', ., sep = '_'))

#######################################################################################################################
#### Weibo user

# The variables we want to keep are: gender, class, post_num, follower_num, followee_num
# Make dummy vars of class and gender
weibo_user$class <- as.factor(weibo_user$class)

#inspect
qplot(weibo_user$class)
table(weibo_user$gender)

dummies <- dummy(x = weibo_user[, c('gender', 'class')])

# remove the reference class
dummies$gender_female <- dummies$class_7 <- NULL

# Bind everything together
weibo_user_prep <- bind_cols(weibo_user[, c('user_id', 'post_num', 'follower_num', 'followee_num')], 
                         dummies)
# Calculate the ratio follower/followee
weibo_user_prep$ratio <- weibo_user_prep$follower_num/weibo_user_prep$followee_num

# There can be infinite values (dividing by 0), set these to 0
weibo_user_prep$ratio <- weibo_user_prep$ratio %>% 
  replace(., is.infinite(.), 0)

# Rename the columns
weibo_user_prep <- weibo_user_prep %>%
  rename_at(names(weibo_user_prep)[-1],~paste('users', ., sep = '_'))

#######################################################################################################################
#### Make the basetable

# Merge the basetable. Do a left outer join since we want to keep all the users in our sample
basetable <- left_join(weibo_user_prep, user_post_prep, 
                       by = c("user_id" = "poster_id"))

#Look for NA's and impute if necessary
basetable %>% skim()

# All NAs posts_: Users that couldn't be matched -> impute with 0, they don't have posts
basetable_imp <- basetable %>% 
  mutate_at(c("posts_repost_num", "posts_comment_num","posts_recency"), ~replace(.,is.na(.), 0))
basetable_imp %>% skim()

# Check for infinite value
map_int(basetable_imp, function(x) sum(is.infinite(x)))
#No values to be found
# otherwise replace with the code below
#basetable_imp$user_ratio <- basetable_imp$user_ratio %>% replace(., is.infinite(.), 0)


# Add the response
names(Response) <- 'y'
basetable_final <- data.frame(Response, basetable_imp)

# Reform all factor variables to numeric format
basetable_final <- basetable_final %>% 
  mutate_if(is.factor,as.character) %>% 
  mutate_if(is.character,as.numeric)
#Drop ID
basetable_final$user_id <- NULL

#Let's have a final glimpse
basetable_final %>% glimpse()

# fit model
log_reg <- glm(y ~ ., 
               family = binomial, 
               data = basetable_final)
summary(log_reg) #AIC = 985

#Transform variable adequately
#First, visually inspect
ggplot(data = basetable_final, aes(x = users_ratio)) + geom_histogram(bins = 100) 
#Many low values, but extreme outliers
ggplot(data = filter(basetable_final, users_ratio > 0 & users_ratio < 1000000), aes(x = users_ratio)) + 
  geom_histogram(bins = 100)
ggplot(data = filter(basetable_final, users_ratio > 0 & users_ratio < 1000), aes(x = users_ratio)) +
  geom_histogram(bins = 100)
ggplot(data = filter(basetable_final, users_ratio > 0 & users_ratio < 2), aes(x = users_ratio)) + 
  geom_histogram(bins = 100)

#How to bin such distribution? ==> try some manual binning
p_load(rbin)

#Play a bit around with the cutting points, observe IVs
bins <- rbin_manual(basetable_final, y, 
                    users_ratio, c(0.2, 0.5, 1, 3, 5, 50))
bins
plot(bins) #Especially the 3 -> 5 bin has effect ==> just add this bin as dummy

#Try with one bin
basetable_one_bin <- basetable_final
basetable_one_bin$ratio_bin <- ifelse((basetable_one_bin$users_ratio>=3
                                       &basetable_one_bin$users_ratio<5),1,0)
basetable_one_bin$users_ratio <- NULL

log_reg2 <- glm(y ~ ., family = binomial, data = basetable_one_bin)
summary(log_reg2) #AIC = 984.45

#Try with all bins
basetable_all_bins <- rbin_create(basetable_final, 
                                  users_ratio, bins)
basetable_all_bins$user_ratio <- NULL

log_reg3 <- glm(y ~ ., 
                family = binomial, data = basetable_all_bins)
summary(log_reg3) #AIC = 986.46

#Best method is with one bin, but overall small effect, 
#maybe best method may even have been to exclude ratio from model.

