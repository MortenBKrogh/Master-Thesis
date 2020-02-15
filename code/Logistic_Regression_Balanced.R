# # Load 'raw' data
# prop_type, seller_name, servicer_name, current_int_rt, cd_zero_bal, #60_dl er dårlig
# train$default <- as.factor(train$default)
# train$default <- as.numeric(train$default)
# 
# logit <- caret::train(default ~ fico + flag_fthb + mi_pct + cnt_units + occpy_sts 
#                       + cltv + dti + orig_upb + ltv + int_rt + channel 
#                       + st + loan_purpose + orig_loan_term + cnt_borr 
#                       + loan_age + mths_remng + `#current` + `#30_dl` 
#                       + `#current_l12` + `#30_dl_l12`,
#                       data = train %>% sample_n(5000),
#                       method = 'glm')

logitmod <- glm(default ~ fico + flag_fthb + mi_pct + cnt_units + occpy_sts 
                + cltv + dti + orig_upb + ltv + int_rt + channel 
                + st + loan_purpose + orig_loan_term + cnt_borr 
                + loan_age + mths_remng #+ `#current` + `#30_dl` 
                + `#current_l12` + `#30_dl_l12` 
                ,data = balanced.train, 
                family=binomial(link = "logit"))

# test$default <- as.factor(test$default)
#balanced.test$default <- as.numeric(balanced.test$default)
pred <- predict(logitmod, newdata = balanced.test, type = "response")

y_pred_num <- ifelse(pred > 0.5, TRUE, FALSE)
y_pred <- factor(y_pred_num, levels=c(FALSE, TRUE))
y_act <- balanced.test$default

class(y_pred)
class(y_act)
str(y_pred)
str(y_act)

classlabels = c(0,1)

performance = perform_class(y_pred, y_act ,classlabels)
performance$accuracy
performance


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
    x                = as.matrix (x_balanced.train),     #=> Matrix
    y                = y_balanced.train,                 #=> Numeric Vector 
    batch_size       = 16,     #=> #OfSamples/gradient update in each epoch
    shuffle          = TRUE,
    epochs           = 1,     #=> Control Training cycles
    validation_split = 0.10) ) #=> Include 30% data for 'Validation' Model


# Logistic %>% evaluate(x_test, y_test,verbose = 0)

# Predicted Class
yhat_keras_class_vec <- predict_classes (object = Logistic,
                                         x = as.matrix(x_balanced.test)) %>%
  as.vector()
# Predicted Probabilities
yhat_keras_prob_vec <- 
  predict_proba (object = Logistic, 
                 x = as.matrix(x_balanced.test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_v1 <- tibble(
  truth      = as.factor(y_balanced.test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = yhat_keras_prob_vec )

####
y_pred_num <- ifelse(yhat_keras_prob_vec > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_balanced.test)

classlabels = c(0,1)

performance = perform_class(y_pred, y_act ,classlabels)
performance$accuracy
performance


