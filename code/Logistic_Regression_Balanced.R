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