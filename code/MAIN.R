# setting the working directory and cleaning memory.
#Clearing variables
rm(list=ls())

# loading required packages
cat('Installing and loading required packages.')
source("code/load_packages.R")
cat('Packages loaded.')
#Clearing variables
rm(list=ls())

# source script where functions to download and prepare data are located
source('code/functions.R')

# Download  sample files and place them in the data folder
# Data(2012, 2018, sample=FALSE)

# Then we need to do some preprocessing of the original data aswell as calculating performance features
var_names <- prepare_FMdata(1, 79, sample=FALSE

var_names
# Load 'raw' data
var_names <- c("id_loan","dt_first_pi","fico","flag_fthb","dt_matr","cd_msa","mi_pct","cnt_units",
               "occpy_sts","cltv","dti","orig_upb","ltv","int_rt","channel","ppmt_pnlty",
               "prod_type","st","prop_type","zipcode","loan_purpose","orig_loan_term","cnt_borr","seller_name",
               "servicer_name","flag_sc","default", "dt_default", "dt_delq", "delq_age", "default_age", "delq_remng",
               "default_remng","current","delq_sts","current_upb","current_int_rt","loan_age",
               "mths_remng","cd_zero_bal","#current","#30_dl","#60_dl","#90+_dl","#current_l12","#30_dl_l12",
               "#60_dl_l12","#90+_dl_l12")

FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_.*\\.txt", full.names=TRUE)
FM_data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = TRUE) #readr::read_delim("data/Final_1999.txt", delim = " ", col_names = FALSE)

#FM_data$default <- as.numeric(FM_data$default)
#FM_data$delq_sts <- as.character(FM_data$delq_sts)

# select the data for the model (OBS: delq_sts and current causes problems.. hence they are not included.)
data <- subset(FM_data, !is.na(FM_data$default)) %>%
  #select(-excluded_vars)
  select(default, fico, flag_fthb, mi_pct, cnt_units,
         occpy_sts, cltv, dti, orig_upb, ltv, int_rt,
         channel, st, prop_type, loan_purpose, delq_sts,
         orig_loan_term, cnt_borr,
         seller_name, servicer_name, 
         current_int_rt, loan_age,
         mths_remng, cd_zero_bal, `#current`, `#30_dl`,
         `#60_dl`, `#current_l12`, `#30_dl_l12`) %>% drop_na()

# #prop_type, seller_name, servicer_name, current_int_rt, cd_zero_bal, #60_dl 
# data_model <- subset(FM_data, !is.na(FM_data$default)) %>%
#   #select(-excluded_vars)
#   select(default, fico, flag_fthb, mi_pct, cnt_units,
#          occpy_sts, cltv, dti, orig_upb, ltv, int_rt,
#          channel, st, loan_purpose, 
#          orig_loan_term, cnt_borr,
#          loan_age,
#          mths_remng, `#current`, `#30_dl`,
#          `#current_l12`, `#30_dl_l12`) %>% drop_na()      

# obtain train and test dataset
set.seed (20200206) # Seed: The day I am handing in my thesis
( train_test_split <- initial_split (data_model, prop = 0.7) )
train <- training (train_test_split) 
test  <- testing (train_test_split)


## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
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
