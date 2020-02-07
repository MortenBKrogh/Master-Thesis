#Clearing variables
rm(list=ls())

#Setting working directory to where the Rscrip is located
setwd(setwd("~/Google Drive/Uni/Aarhus Universitet/10. semester/Speciale/Code"))
getwd()
library(tidyverse)
library(recipes)
library(keras)
library(caret)

# load origination data
origclass <- c('numeric','numeric','character', 'numeric', 'character', 'numeric', 'numeric',
    'character','numeric','numeric','numeric','numeric','numeric','character','character', 
    'character','character', 'character','character','character','character', 
    'numeric', 'numeric','character','character','character')

# loads all .txt files and collects them in the same dataframe
origination_files = list.files(path=paste0(getwd(), "/data/origination"), pattern="*.txt", full.names=TRUE)
origination = as_tibble(ldply(origination_files, fread, sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=origclass))

origination_files[1]
length(origination_files)


orig_2006 <- read_delim(paste0(origination_files[1]), 
                        "|", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE, col_types = cols(X26 = col_skip()))



as_tibble(ldply(origination_files, fread, sep = "|", escape_double = FALSE, col_names = FALSE, 
trim_ws = TRUE, col_types = cols(X26 = col_skip())))

# str(origination)
# Give the correct names to the variables
names(origination) <- c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa', 'mi_pct','cnt_units','occpy_sts',
                        'cltv','dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type',
                        'zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name','flag_sc')

# appeently some of the variables becomes factors even though they are specifies as real. so lets change that.
# vec <- c(6, 9, 13)
# for (i in 1:length(vec)) {
#   origination[,vec[i]] <- as.numeric(origination[,vec[i]])  
# }


# Some step where we load the performance data and create features from it 
# Combining the origination- and performance data so we have one row for each loan
data <- origination
# creating a fake default flag (target variable)
data$'target' <-  sample(c(TRUE,FALSE), 150000, TRUE) # rbinom(dim(origination)[1], 1, 0.3)
target_num <- 24
# Selecting the variables we want in our model
  data_model <- subset(data, !is.na(data$target)) %>%
  select(c(1,2,4,27)) #,3,5:19,21:27)) 
  # %>% sample_n(500000)
  
index    <- createDataPartition(data_model$target, p = 0.70, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe
model_recipe_steps <-
  recipe(target ~ ., data = training) %>%
  # categorical variables
  step_string2factor(flag_fthb) %>% #, cd_msa,occpy_sts, channel, ppmt_pnlty, prod_type, st, prop_type, zipcode, loan_purpose, seller_name, servicer_name) %>%
  step_dummy(flag_fthb,occpy_sts, channel) %>% #, ppmt_pnlty, prod_type, st, prop_type, zipcode, loan_purpose, seller_name, servicer_name) %>%
  # Numeric variables
  step_range(fico, dti, min = 0, max = 1) # cltv, orig_upb, orig_loan_term, int_rt, ltv,

# model_recipe_steps <- recipe(target ~ ., data = training) %>%
#   # categorical variables
#   step_string2factor(flag_fthb) %>% step_dummy(flag_fthb) %>% step_range(fico)


prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training)
test           <- bake(prepped_recipe, test)

test[is.na(test)] <- 0


x_train  <- training %>% select(-target) %>% as.matrix()
x_test   <- test     %>% select(-target) %>% as.matrix()
y_train  <- training %>% select(target) %>% as.matrix()
y_test   <- test     %>% select(target) %>% as.matrix()

model <- keras_model_sequential() %>%
  layer_dense(units = 128, input_shape = dim(x_train)[2]) %>%
  layer_dropout(0.5) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.4) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.3) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1,   activation = "sigmoid")

model %>% compile(optimizer = 'adam',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy')

model %>% fit(x_train, y_train,
              batch_size = ,
              epochs = 35,
              validation_split = 0.1)













# Load monthly performance data

svcgclass <- c('character','integer','real','character', 'integer','integer','character',
               'character','character','integer','real','real','integer', 'integer', 
               'character','integer','integer','integer','integer','integer','integer', 
               'real','real', 'character','character','real')


# svcgfile_Qnyyyy <- read.table("historical_data1_time_Qnyyyy.txt", sep="|", header=FALSE, colClasses=svcgclass)
#svcgfile_Qnyyyy <- read.table("historical_data1_time_Q12017.txt", sep="|", header=FALSE, colClasses=svcgclass)

performance_files = list.files(path=paste0(getwd(), "/data/performance"), pattern="*.txt", full.names=TRUE)
performance_1 = as_tibble(ldply(performance_files[1], fread, sep="|", header=FALSE, stringsAsFactors = FALSE, colClasses=svcgclass))

names(performance) <- c('id_loan','svcg_cycle','current_upb','delq_sts','loan_age', 
                            'mths_remng', 'repch_flag','flag_mod','cd_zero_bal','dt_zero_bal', 
                            'current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries', 
                            'net_sale_proceeds','non_mi_recoveries','expenses','legal_costs', 
                            'maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss', 
                            'modcost','stepmod_ind','dpm_ind','eltv')



