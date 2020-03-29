
logitmod <- glm(default ~ fico  + flag_fthb + mi_pct + cnt_units + occpy_sts + 
                  cltv+ dti+ orig_upb+ ltv + int_rt +
                  channel + st + prop_type + loan_purpose + orig_loan_term + 
                  cnt_borr + current_int_rt + loan_age + mths_remng  + 
                  `#30_dl` + `#60_dl` + `#current_l12` + `#30_dl_l12` , 
                data = train,
                family="binomial")

pred <- predict(logitmod, newdata = test, type = "response")





























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
    validation_split = 0.30) ) #=> Include 30% data for 'Validation' Model

save_model_hdf5(Logistic, "Logistic.h5")

keras::get_weights(Logistic)

Logistic %>% evaluate(x_test, y_test)

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