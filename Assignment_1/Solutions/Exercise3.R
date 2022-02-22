# QUESTIONS ABOUT THIS DOCUMENT: matthias.bogaert@ugent.be

rm(list=ls())
setwd('D:/2e sem/pred&prescr/gith project/PPA_Assignment_1')


##########################################################################################################
#### Exercise 3: Resampling ####
###########################################################################################################


########## 1. Read in and transform the data ##############
###########################################################
if(!require(pacman, quietly = TRUE)) install.packages('pacman') ; require(pacman, quietly = TRUE)
p_load(tidyverse, skimr)

#Read in the general facebook demographics
fb_general <- read_delim('Ex3_demographics.csv', delim = ';')
fb_general <- fb_general %>% 
  rename('AgeGroup' = 'Age group')
fb_general_by_group <- fb_general %>% 
  group_by(AgeGroup) %>% 
  summarise(Total = sum(Total))
fb_general_by_group

#Load the Facebook sample and transform the gender and age variable 
load('Ex3.Rdata')
fb_oursample %>% glimpse()

#Gender
fb_oursample$genderfemale <- as.factor(fb_oursample$genderfemale)
fb_oursample$Gender <- as.factor(if_else(fb_oursample$genderfemale == 1, 'F', 'M'))

#Age
fb_oursample$age <- floor(as.numeric(fb_oursample$age)/365)  

fb_oursample %>% head()

########## 2. Transform our data per age group per gender ##############
##################################################################

#Let us use something else than the rbin package this time
fb_oursample$age_group <- case_when(
  fb_oursample$age >= 13 & fb_oursample$age <= 24 ~ '13-24',
  fb_oursample$age >= 25 & fb_oursample$age <= 34 ~ '25-34',
  fb_oursample$age >= 35 & fb_oursample$age <= 54 ~ '35-54',
  fb_oursample$age >= 55 ~ '55+'
)
fb_oursample %>% glimpse()

fb_oursample_by_group <- fb_oursample %>% 
  group_by(Gender, age_group) %>% 
  summarise(Total = n()/nrow(.))
fb_oursample_by_group

########## 3. Resampling ##########
###################################

#To resample: we undersample groups from our sample that are larger than the FB sample
#We oversample for groups that are smaller than the FB average

#First count how many observations we need to get the general Facebook distribution
#For example, count how many observations must be from a certain age group to get the percentage with 2000 obs
fb_general$counts <- fb_general$Total * nrow(fb_oursample)
fb_general

#Make a list to store all indicators for each gender and each group separately.
l_indicators <- list()

#Make sure that fb_general and fb_oursample_by_group are in same order
fb_general <- fb_general %>% 
  arrange(AgeGroup, Gender)
fb_oursample_by_group <- fb_oursample_by_group %>% 
  arrange(age_group, Gender)

for (i in 1:nrow(fb_general)) {
  
  #Define variables that store the current gender and age group
  i_gender <- fb_oursample_by_group$Gender[i]
  i_group <- fb_oursample_by_group$age_group[i]
  
  cat('Resampling gender', i_gender, ' & age', i_group, '\n')
  
  #Store the desired sample size (= number of observations that should be in the age group per gender) 
  #and the sample size of the age group per gender in our case
  n_desired <- round(fb_general$counts[i])
  n_oursample <- length(which(fb_oursample$Gender == i_gender &
                                fb_oursample$age_group == i_group ))
  #Store which instances (rows) are in the current age and gender group (store row indicators)
  ind_oursample <- which(fb_oursample$Gender == i_gender &
                           fb_oursample$age_group == i_group )
  
  #Check whether the desired counts are larger or smaller. If larger we undersample, if smaller we oversample.
  if(n_oursample > n_desired) {
    ind_resample <- sample(x = ind_oursample, 
                           size = n_desired, 
                           replace = FALSE)
  } else {
    ind_resample <- sample(x = ind_oursample, 
                           size = n_desired, 
                           replace = TRUE)
  }
  l_indicators[[i]] <- ind_resample
}

#Resample our facebook sample
fb_resample <- fb_oursample[unlist(l_indicators),]

#Now group it per age category and look at the distribution
fb_resample_by_group <- fb_resample %>% 
  group_by(age_group, Gender) %>% 
  summarise(Total = length(age)/nrow(fb_oursample))
fb_resample_by_group

#check if this is the same as the fb_general sample
(fb_resample_by_group$Total - fb_general$Total)
#Extremely minor rounding differences

########## 4. Plot the distribution ##########
##############################################

#Add both together the general distribution and our distribution together (this is done to make plotting with ggplot more easy)
fb_general <- fb_general %>% rename('age_group' = 'AgeGroup')
fb_general_oursample <- rbind(fb_general[,c(2,1,3)], 
                              fb_oursample_by_group)
fb_general_oursample$data <- c(rep('FB general', nrow(fb_general)), 
                               rep('Original sample', nrow(fb_general)))
fb_general_oursample$Total <- as.numeric(fb_general_oursample$Total)

#Plot age and gender distribution
g <- ggplot(data = fb_general_oursample, mapping = aes(y = Total, x = age_group, fill= Gender)) + 
  geom_col(position = 'dodge')
g2 <- g + ylim(0,0.50) + scale_fill_grey() + 
  facet_grid(.~data) + theme_bw() + labs(x = 'Age group', y = 'Percentage users')
g2 + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14), 
           strip.text = element_text(size = 14, face = "bold"), 
           legend.text = element_text(size = 12))

#Plot the age distribution
Gender_general <- fb_general %>% 
  group_by(Gender) %>% 
  summarise(sum = sum(Total))
Gender_sample <- fb_oursample_by_group %>% 
  group_by(Gender) %>% summarise(sum = sum(Total))
Gender_plot <- data.frame(rbind(Gender_general, Gender_sample),
                          data = c(rep('FB general', nrow(Gender_general)), 
                                   rep('Original sample', nrow(Gender_general))))

g3 <- ggplot(data = Gender_plot, mapping = aes(y = sum, x = data, fill= Gender)) + geom_col(position = 'dodge')
g4 <- g3 + scale_fill_grey() + theme_bw() + labs(x = 'Data', y = 'Percentage users')
g4 + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14), 
           strip.text = element_text(size = 14, face = "bold"), 
           legend.text = element_text(size = 12))

#Redo for the resampled data to check if we succeeded
fb_general_resample <- rbind(fb_general[,c(2,1,3)], fb_resample_by_group)
fb_general_resample$data <- c(rep('FB general', nrow(fb_general)), rep('Resampled data', nrow(fb_general)))
fb_general_resample$Total <- as.numeric(fb_general_resample$Total)
g5 <- ggplot(data = fb_general_resample, mapping = aes(y = Total, x = age_group, fill= Gender)) + 
  geom_col(position = 'dodge')
g6 <- g5 + ylim(0,0.50) + scale_fill_grey() + facet_grid(.~data) + theme_bw() + 
  labs(x = 'Age group', y = 'Percentage users')
g6 + theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14), 
           strip.text = element_text(size = 14, face = "bold"), 
           legend.text = element_text(size = 12))
