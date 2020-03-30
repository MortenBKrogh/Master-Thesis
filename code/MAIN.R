# setting the working directory and cleaning memory.
#Clearing variables
rm(list=ls())

# loading required packages
cat('Installing and loading required packages.')
source("code/load_packages.R")
cat('Packages loaded.')
#Clearing variables
rm(list=ls())


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - Load Functions  - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# source script where functions to download and prepare data are locate
source('code/functions.R')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - Download Data - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Download  sample files and place them in the data folder
# Data(1999, 2018, sample=FALSE)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - -  Prepare Data - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Then we need to do some preprocessing of the original data aswell as calculating performance features
source('code/load_data.R')


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - Initial Data Analysis - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
sosource('code/initial_data_analysis_v2.R')
# See R script for details. Figures are saved in the folder Figure/pw1 and so on.




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - Estimate Models - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
source('code/Models.R')

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - Estimate Models - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #













## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
FM_data %<>% mutate(default = as.numeric(default))
balanced.test  <- DMwR::SMOTE(default ~ ., data = test, perc.under = 200, perc.over = 100) 
table(balanced.test$default)



summary(data_model$default)

data_model_1 <- data_model %>%
  mutate(default = factor(if_else(default == 1, 'TRUE', 'FALSE')))
head(data_model_1)


# balanced.data <- DMwR::SMOTE(default ~ ., data = data_model_1, perc.over = 1200, perc.under = 130)
# as.data.frame(table(balanced.data$default))

# obtain balanced train and test dataset
set.seed (20200206) # Seed: The day I am handing in my thesis
( train_test_split <- initial_split (data_model_1, prop = 0.7) )
balanced.train <- training (train_test_split) %>%  DMwR::SMOTE(default ~ ., data = ., perc.over = 1200, perc.under = 130) # %>% sample_n(15000)
balanced.test  <- testing (train_test_split) %>%  DMwR::SMOTE(default ~ ., data = ., perc.over = 1200, perc.under = 130) 
