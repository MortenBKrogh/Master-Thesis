# # Load 'raw' data
# var_names <- c("id_loan","dt_first_pi","fico","flag_fthb","dt_matr","cd_msa","mi_pct","cnt_units",
#                "occpy_sts","cltv","dti","orig_upb","ltv","int_rt","channel","ppmt_pnlty",
#                "prod_type","st","prop_type","zipcode","loan_purpose","orig_loan_term" ,"cnt_borr","seller_name",
#                "servicer_name", "flag_sc","#current","#30_days_late", "#60_days_late","#90_days_late","#payments","min_unpaid",
#                "max_unpaid","surv_binary_30","surv_30","current","default_flag"  )
# 
# # classes <-   c('character','date','numeric', 'factor', 'date', 'numeric', 'numeric',
# #                 'numeric','factor','numeric','numeric','numeric','numeric','character','character', 
# #                 'character','character', 'character','character','character','character', 
# #                 'numeric', 'numeric','character','character','character')
# 
# FM_data_files <- list.files(path=paste0(getwd(), "/data"), pattern="^Final_.*\\.txt", full.names=TRUE)
# FM_data <- ldply(FM_data_files, data.table::fread, sep = "|", header = FALSE, col.names = var_names, stringsAsFactors = TRUE) #readr::read_delim("data/Final_1999.txt", delim = " ", col_names = FALSE)
# #FM_data$cnt_borr <- as.character(FM_data$cnt_borr)
# FM_data$default_flag <- as.numeric(FM_data$default_flag)
# FM_data$surv_binary_30 <- as.numeric(FM_data$surv_binary_30)
# #FM_data$zipcode <- as.numeric(FM_data$zipcode)
# 
# data_model <- subset(FM_data, !is.na(FM_data$default_flag)) %>%
#   select(default_flag, fico, flag_fthb, st, channel, loan_purpose, 
#          dti, prop_type, orig_upb, seller_name, int_rt, cnt_units, 
#          occpy_sts, ltv, cnt_borr, cltv)



# subset(FM_data, !is.na(FM_data$default_flag)) %>%
#   select(default_flag, fico, flag_fthb, st, channel, loan_purpose, 
#          dti, prop_type, orig_upb,  int_rt, cnt_units, 
#          occpy_sts, ltv, cnt_borr, cltv, '#30_days_late')#, 'surv_binary_30', '#60_days_late')#,'#payments') # %>% sample_n(500)


# 
# ## Loading DMwr to balance the unbalanced class
# library(DMwR)
# 
# ## Let's check the count of unique value in the target variable
# as.data.frame(table(data_model$default_flag))
# 
# ## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
# 
# balanced.data <- SMOTE(default_flag ~ ., data = data_model, perc.over = 160, k = 6, perc.under = 200)
# as.data.frame(table(balanced.data$default_flag))


# # obtain train and test dataset
# set.seed (20200206) # Seed: The day I am handing in my thesis
# ( train_test_split <- initial_split (data_model, prop = 0.7) )
# train <- training (train_test_split) 
# test  <- testing (train_test_split)

# prop_type, seller_name, servicer_name, current_int_rt, cd_zero_bal, #60_dl er dårlig
# train$default <- as.factor(train$default)
# train$default <- as.numeric(train$default)

# logit <- caret::train(default ~ fico + flag_fthb + mi_pct + cnt_units + occpy_sts 
#                       + cltv + dti + orig_upb + ltv + int_rt + channel 
#                       + st + loan_purpose + orig_loan_term + cnt_borr 
#                       + loan_age + mths_remng + `#current` + `#30_dl` 
#                       + `#current_l12` + `#30_dl_l12`,
#                       data = train %>% sample_n(5000),
#                       method = 'glm')

# logitmod <- glm(default ~ fico + flag_fthb + mi_pct + cnt_units + occpy_sts 
#                           + cltv + dti + orig_upb + ltv + int_rt + channel 
#                           + st + loan_purpose + orig_loan_term + cnt_borr 
#                           + loan_age + mths_remng + `#current` + `#30_dl` 
#                           + `#current_l12` + `#30_dl_l12`, 
#                 data = train, 
#                 family=binomial(link = "logit"))
# 
# # using preprocessed data from NN_v2
# logitmod <- glm.fit(y = y_train, x = x_train, 
#                 family=binomial(link = "logit"))
# 
# 
# # test$default <- as.factor(test$default)
# test$default <- as.numeric(test$default)
# pred <- predict(logitmod, newdata = test, type = "response")
# 
# pred <- predict(logitmod, x = x_test, type = "response")
# 
# 
# y_pred_num <- ifelse(pred > 0.5, 1, 0)
# y_pred <- factor(y_pred_num, levels=c(0, 1))
# y_act <- test$default
# 
# classlabels = c(0,1)
# 
# performance = perform_class(y_pred, y_act ,classlabels)
# performance$accuracy
# performance


# the keras approach one layer NN
# Build the Artificial Neural Network
Logistic <- keras_model_sequential() %>%
  layer_dense (units              = 1, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "sigmoid",    
               input_shape        = ncol(x_train)) %>% 
  compile (optimizer = 'sgd', #=> Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation


system.time ( 
  history <- fit (
    object           = Logistic,                  # => Our Model
    x                = as.matrix (x_train),     #=> Matrix
    y                = y_train,                 #=> Numeric Vector 
    batch_size       = 500,     #=> #OfSamples/gradient update in each epoch
    shuffle          = TRUE,
    epochs           = 1,     #=> Control Training cycles
    validation_split = 0.10) ) #=> Include 30% data for 'Validation' Model


# Logistic %>% evaluate(x_test, y_test,verbose = 0)

# Predicted Class
yhat_keras_class_vec <- predict_classes (object = Logistic,
                                         x = as.matrix(x_test)) %>%
  as.vector()
# Predicted Probabilities
yhat_keras_prob_vec <- 
  predict_proba (object = Logistic, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_v1 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = yhat_keras_prob_vec )

####
y_pred_num <- ifelse(yhat_keras_prob_vec > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_test)

classlabels = c(0,1)

performance = perform_class(y_pred, y_act ,classlabels)
performance$accuracy
performance
