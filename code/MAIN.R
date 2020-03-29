# setting the working directory and cleaning memory.
#Clearing variables
rm(list=ls())

# loading required packages
cat('Installing and loading required packages.')
source("code/load_packages.R")
cat('Packages loaded.')
#Clearing variables
rm(list=ls())

# source script where functions to download and prepare data are locate
source('code/functions.R')

# Download  sample files and place them in the data folder
# Data(1999, 2018, sample=FALSE)

# Then we need to do some preprocessing of the original data aswell as calculating performance features

#var_names <- prepare_FMdata(1999, 2018, sample=TRUE)
# dput prints the vector as vector input.
# dput(var_names)

# Load the variables we need in the analysis.
# all_var_names holds all the names of the variables in the csv file
all_var_names <- c("id_loan", "dt_first_pi", "fico", "flag_fthb", "dt_matr", "cd_msa", 
                    "mi_pct", "cnt_units", "occpy_sts", "cltv", "dti", "orig_upb", 
                    "ltv", "int_rt", "channel", "ppmt_pnlty", "prod_type", "st", 
                    "prop_type", "zipcode", "loan_purpose", "orig_loan_term", "cnt_borr", 
                    "seller_name", "servicer_name", "flag_sc", "repch_flag", "cd_zero_bal", 
                    "dt_zero_bal", "dt_lst_pi", "lst_upb", "loan_age", "mths_remng", 
                    "lst_int_rt", "#current_l12", "#30_dl_l12", "#60_dl_l12", "#90_dl_l12", 
                    "default", "prepaid")

# however we only need the variables below
# used_var_names <- c('default','dt_first_pi', 'fico', 'flag_fthb', 'mi_pct', 'cnt_units',
#                     'occpy_sts', 'cltv', 'dti', 'orig_upb', 'ltv', 
#                     'int_rt', 'channel', 'st', 'prop_type', 'loan_purpose',
#                     'orig_loan_term', 'cnt_borr', 'current_int_rt', 'loan_age', 'mths_remng', 
#                     'cd_zero_bal', '#30_dl', '#60_dl', '#current_l12', '#30_dl_l12')
# 
# # so we will only load these variables to save space
# used_var_index <- match(used_var_names, all_var_names)


FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_Sample_.*\\.txt", full.names=TRUE)
FM_data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = all_var_names, stringsAsFactors = TRUE) #, select = used_var_index) 

rm(all_var_names, used_var_names, used_var_index, Data, prepare_FMdata, new.packages, list.of.packages, FM_data_files)


#check how many missing values in the selected variables are present
Missing <- FM_data %>% 
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))

# remove rows with missing default, fico and zero balance values,
# since the first two are required for institutions, and the last
# because we only look at dead loans. 
FM_data %<>% drop_na(default, fico, orig_upb, cd_zero_bal, zipcode) %>% select(-cd_zero_bal, -prepaid)

# Now we will apply
# some rules for handling missing values, described in the data section.
#library(magrittr)
FM_data %<>% mutate(year_first_pi = as.factor(substr(as.character(dt_first_pi), 1, 4)),
                    mnth_first_pi = as.factor(substr(as.character(dt_first_pi), 6, 7)),
                    year_zero_bal = as.factor(substr(as.character(dt_zero_bal), 1, 4)),
                    mnth_zero_bal = as.factor(substr(as.character(dt_zero_bal), 6, 7)),
                    flag_fthb    = fct_explicit_na(flag_fthb, na_level = 'U'),
                    occpy_sts    = fct_explicit_na(occpy_sts, na_level = 'U'),
                    st           = fct_explicit_na(st,        na_level = 'U'),
                    prop_type    = fct_explicit_na(prop_type, na_level = 'U'),
                    loan_purpose = fct_explicit_na(loan_purpose, na_level = 'U'),
                    cltv         = replace_na(cltv, stats::median(cltv, na.rm = T)),
                    dti          = replace_na(dti,  stats::median(dti, na.rm = T)),
                    ltv          = replace_na(ltv, stats::median(dti, na.rm = T)),
                    cnt_borr     = replace_na(cnt_borr, 1)
                    ) %>%
              mutate(st          = as.character(st)) %>%
              filter(!st %in% c('VI','GU')) %>%
              mutate(st          = as.factor(st))

# make the target variable numeric
# FM_data %<>% mutate(default = as.factor(default))

# FM_data %<>% select(-id_loan, - dt_first_pi, - dt_matr, -prod_type, -ppmt_pnlty, -flag_sc,
#                    -repch_flag, -cd_msa, -dt_lst_pi)

FM_data_v1 <- FM_data %>% select(
                                 #"id_loan", 
                                 #"dt_first_pi", 
                                 "fico", 
                                 "flag_fthb", 
                                 # "dt_matr", 
                                 # "cd_msa", 
                                 "mi_pct", 
                                 "cnt_units", 
                                 "occpy_sts", 
                                 # "cltv", this is the same as ltv in most cases
                                 "dti", 
                                 "orig_upb", 
                                 "ltv", 
                                 "int_rt", 
                                 "channel", 
                                 "ppmt_pnlty", 
                                 # "prod_type", 
                                 "st", 
                                 "prop_type", 
                                 "zipcode", 
                                 "loan_purpose", 
                                 # "orig_loan_term", 
                                 "cnt_borr", 
                                 # "seller_name", 
                                 # "servicer_name", 
                                 # "flag_sc", 
                                 # "repch_flag", 
                                 # "cd_zero_bal", removed earlier
                                 # "dt_zero_bal", 
                                 # "dt_lst_pi", 
                                 "lst_upb", 
                                 "loan_age",
                                 "mths_remng", 
                                 # "lst_int_rt", 
                                 "#current_l12", 
                                 "#30_dl_l12", "#60_dl_l12", "#90_dl_l12",
                                 'year_first_pi', 'mnth_first_pi', 
                                 'year_zero_bal', 'mnth_zero_bal',
                                 "default" 
                                 #"prepaid" 
                                 )


# obtain train and test dataset
set.seed (20200206) # Seed: The day I am handing in my thesis
( train_test_split <- initial_split (FM_data_v1, prop = 0.7) )
train <- training (train_test_split) 
test  <- testing (train_test_split)

rm(Missing, train_test_split)

# check that train and test has approximately the same balance..
table(FM_data$default) 
table(train$default)
table(test$default)

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
