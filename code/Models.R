# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - Logistic Regression - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Estimate the logistic regression model using all the features we selected
# in the main document.
system.time ( 
  mod_logreg <- glm(default ~ .,
                    data = train,
                    family="binomial")
)
  
# Make prediction from the logistic regression model
#options(warn=-1)      #turn off warnings
pred_logreg <- predict(mod_logreg, newdata = test, type = "response")
#options(warn=1)      #turn warnings back on


# Confusion matrix and performance metrics
y_pred_num <- ifelse(pred_logreg > 0.5, TRUE, FALSE)
y_pred <- factor(y_pred_num, levels=c(FALSE, TRUE))
y_act <- test$default
classlabels = c(0, 1)
(performance_LR = perform_class(y_pred, y_act, classlabels))



# create roc object
library(pROC)
roc_logreg <- roc(test$default, pred_logreg, data = test,percent=TRUE, plot=TRUE,print.auc = TRUE)
p_roc_logreg <- ggroc(list(logreg = roc_logreg), alpha = 1, linetype = 3, size = 1) #+ thd + th  + scale_color_manual(values = c(basem3, basem4)) 
p_roc_logreg 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -   Neural Network    - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Recipe for preprocessing the data for the Neural Network
preprocessing_steps <-
  recipe(default ~ ., data = train) %>%
  # (1) One-Hot Encoding for the Categorical Variables
  step_string2factor(flag_fthb, occpy_sts, channel, ppmt_pnlty, st, prop_type, loan_purpose, year_first_pi, mnth_first_pi, year_zero_bal, mnth_zero_bal) %>% 
  step_dummy(flag_fthb, occpy_sts, channel, ppmt_pnlty, st, prop_type, loan_purpose, year_first_pi, mnth_first_pi, year_zero_bal, mnth_zero_bal) %>% 
  # (2) Normalizing Variables
  step_center (all_predictors(), -all_outcomes()) %>%
  step_scale (all_predictors(), -all_outcomes()) %>%
  #step_range(fico, dti, orig_upb, int_rt, cnt_units, ltv, '#30_days_late', 'surv_30', min = 0, max = 1)
  # (3) Last step prep
  prep(data = train)


x_train <- bake (preprocessing_steps, new_data = train) %>% 
  select (-default)
x_test <- bake (preprocessing_steps, new_data = test) %>% 
  select (-default)

y_train <- ifelse (pull (train, default) == TRUE, 1, 0)
y_test  <- ifelse (pull (test, default)  == TRUE, 1, 0)

# Build the Artificial Neural Network
ANN_v1 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = 128, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = 0.1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2nd Hidden Layer
  layer_dense (units              = 128,
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = 0.1) %>%  
  # (3) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               #kernel_initializer = "uniform", 
               activation         = "sigmoid") %>% #=> Common for Binary
  # (4) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('accuracy') ) #=> Train/Test Evaluation

# check the model is as it should be
ANN_v1

# Fit the ANN model
system.time ( 
  history <- fit (
    object           = ANN_v1,                  # => Our Model
    x                = as.matrix (x_train),     #=> Matrix
    y                = y_train,                 #=> Numeric Vector 
    batch_size       = 500,     #=> #OfSamples/gradient update in each epoch
    epochs           = 25,     #=> Control Training cycles
    validation_split = 0.30) ) #=> Include 30% data for 'Validation' Model

#save_model_h

# Make predictions
pred_NN_class_vec <- predict_classes (object = ANN_v1,
                                      x = as.matrix(x_test)) %>%
  as.vector()
# Predicted Probabilities
pred_NN_prob_vec <- 
  predict_proba (object = ANN_v1, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_v1 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec )

####
y_pred_num <- ifelse(pred_NN_prob_vec > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_test)
classlabels = c(0,1)
(performance_ANN = perform_class(y_pred, y_act ,classlabels))


# create roc object
library(pROC)
roc_NeuralNet <- roc(test$default, pred_NN_prob_vec, data = test,percent=TRUE, plot=TRUE,print.auc = TRUE)
p_roc_NeuralNet<- ggroc(list(Neural_Network = roc_NeuralNet), alpha = 1, linetype = 3, size = 1) + thd + th  + scale_color_manual(values = c(basem3, basem4)) 
p_rroc_NeuralNet 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - -   ROC Curves    - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
p_roc <- ggroc(list(logreg = roc_logreg, NeuralNet = roc_NeuralNet), alpha = 1, linetype = 2, size = 1) + thd + th  + scale_color_manual(values = c(basem3, basem4)) 
p_roc