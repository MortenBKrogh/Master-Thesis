# Recipe for preprocessing the data for the Neural Network
preprocessing_steps <-
  recipe(default ~ ., data = train) %>%
  # (1) One-Hot Encoding for the Categorical Variables
  step_string2factor(flag_fthb, occpy_sts, channel, st, prop_type, loan_purpose) %>%
  step_dummy(flag_fthb, occpy_sts, channel, st, prop_type, loan_purpose) %>%
  # step_string2factor(flag_fthb, occpy_sts, channel, st, prop_type, loan_purpose, seller_name, servicer_name) %>%
  # step_dummy(flag_fthb, occpy_sts, channel, st, prop_type, loan_purpose, seller_name, servicer_name)%>%
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
x_test_balanced <- bake (preprocessing_steps, new_data = balanced.test) %>% 
  select (-default)

y_train <- ifelse (pull (train, default) == TRUE, 1, 0)
y_test  <- ifelse (pull (test, default)  == TRUE, 1, 0)
y_test_balanced <- ifelse (pull (balanced.test, default)  == TRUE, 1, 0)

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
  compile (optimizer = 'adam', #=> Most Popular for Optimization Algo.
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
    epochs           = 35,     #=> Control Training cycles
    validation_split = 0.20) ) #=> Include 30% data for 'Validation' Model
# Save the model
save_model_hdf5(ANN_v1, "ANN_v1.h5")

print (history)

# Predicted Class
yhat_keras_class_vec <- predict_classes (object = ANN_v1,
                                         x = as.matrix(x_test)) %>%
  as.vector()
# Predicted Probabilities
yhat_keras_prob_vec <- 
  predict_proba (object = ANN_v1, 
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


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - Performance on Balanced test data - - - - - - - - # 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Predicted Class
yhat_keras_class_vec <- predict_classes (object = ANN_v1,
                                         x = as.matrix(x_test_balanced)) %>%
  as.vector()
# Predicted Probabilities
yhat_keras_prob_vec <- 
  predict_proba (object = ANN_v1, 
                 x = as.matrix(x_test_balanced)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_v1 <- tibble(
  truth      = as.factor(y_test_balanced) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(yhat_keras_class_vec) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = yhat_keras_prob_vec )

####
y_pred_num <- ifelse(yhat_keras_prob_vec > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_test_balanced)

classlabels = c(0,1)

performance = perform_class(y_pred, y_act ,classlabels)
performance$accuracy
performance


####



options(scipen = 999)

head(estimates_ANN_v1, 10)

options (yardstick.event_first = FALSE)

# Confusion Matrix
#truth      = as.factor(y_test)
estimates_ANN_v1 %>% conf_mat (truth, estimate)

# Accuracy
estimates_ANN_v1 %>% metrics (truth, estimate)

# Area Under the Curve 
estimates_ANN_v1 %>% roc_auc(truth, class_prob)

# Precision and Recall
tibble (
  precision = estimates_ANN_v1 %>% precision(truth, estimate),
  recall    = estimates_ANN_v1 %>% recall(truth, estimate) )

# F1-score
estimates_ANN_v1 %>% f_meas(truth, estimate, beta = 1)


# Explain the model by LIME (Local Interpretable Model-agnostic Explanations)
# function which tells lime that it is a classification exercise
model_type.keras.models.Sequential <- function(x, ...) {
  "classification"}

# (2) Setup lime::predict_model() for keras
# 
# Now we can create our predict_model(): Wraps keras::predict_proba().
# 
# The trick here is to realize that it’s inputs must be
# 
# x a model,
# newdata a dataframe object (this is important), and,
# type which is not used but can be use to switch the output type.
# The output is also a little tricky because it must be in the format of probabilities by classification (this is important; shown next).

predict_model.keras.engine.sequential.Sequential <- function (x, newdata, type, ...) {
  pred <- predict_proba (object = x, x = as.matrix(newdata))
  data.frame ('Default' = pred, 'NonDefault' = 1 - pred) }


predict_model (x       = ANN_v1, 
               newdata = x_test, 
               type    = 'raw') # %>%
tibble::as_tibble()

explainer <- lime::lime (
  x              = x_train, 
  model          = ANN_v1, 
  bin_continuous = FALSE)

system.time (
  explanation <- lime::explain (
    x_test[1:10, ], # Just to show first 10 cases
    explainer    = explainer, 
    n_labels     = 1, # explaining a `single class`(Polarity)
    n_features   = 4, # returns top four features critical to each case
    kernel_width = 0.5) ) # allows us to increase model_r2 value by shrinking the localized evaluation.

plot_features (explanation) +
  labs (title = "LIME: Feature Importance Visualization",
        subtitle = "Hold Out (Test) Set, First 10 Cases Shown")


plot_explanations (explanation) +
  labs (title = "LIME Feature Importance Heatmap",
        subtitle = "Hold Out (Test) Set, First 10 Cases Shown")

















