# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -   Neural Networks   - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Hidden Layers = 1  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_L1 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               activation         = "sigmoid") %>% #=> Common for Binary
  # (3) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Hidden Layers = 2  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_L2 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               activation         = "sigmoid") %>% #=> Common for Binary
  # (4) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Hidden Layers = 3  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_L3 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L3) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (4) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               activation         = "sigmoid") %>% #=> Common for Binary
  # (5) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Hidden Layers = 4  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_L4 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L3) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (4) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L4) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (5) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               activation         = "sigmoid") %>% #=> Common for Binary
  # (6) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Hidden Layers = 5  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_L5 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L3) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (4) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L4) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (5) 2st Hidden Layer
  layer_dense (units              = units_val, #=> Num Of Nodes
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L5) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (6) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               activation         = "sigmoid") %>% #=> Common for Binary
  # (7) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_accuracy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - Neural Network Sirignano et. al. 2018 - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
ANN_S1 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = 200, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               #kernel_initializer = "uniform", 
               activation         = "sigmoid") %>% #=> Common for Binary
  # (3) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation




ANN_S2 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = 200, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = 140, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               #kernel_initializer = "uniform", 
               activation         = "sigmoid") %>% #=> Common for Binary
  # (4) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation



ANN_S3 <- keras_model_sequential() %>%
  # (1) 1st Hidden Layer
  layer_dense (units              = 200, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu",    
               input_shape        = ncol(x_train)) %>% 
  layer_dropout (rate = dropout_val_L1) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (2) 2st Hidden Layer
  layer_dense (units              = 140, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L2) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (3) 2st Hidden Layer
  layer_dense (units              = 140, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L3) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (4) 2st Hidden Layer
  layer_dense (units              = 140, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L4) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (5) 2st Hidden Layer
  layer_dense (units              = 140, #=> Num Of Nodes
               #kernel_initializer = "uniform", 
               activation         = "relu") %>% 
  layer_dropout (rate = dropout_val_L5) %>%  #=> Dropout Below 10%: Prevent overfitting
  # (6) Output Layer
  layer_dense (units              = 1, #=> Binary/Multi?=>That Number
               #kernel_initializer = "uniform", 
               activation         = "sigmoid") %>% #=> Common for Binary
  # (7) Compile Model
  compile (optimizer = 'sgd', #=> sgd = stochastic gradient descent. adam is the Most Popular for Optimization Algo.
           loss      = 'binary_crossentropy', #=> Binary Classification
           metrics   = c('binary_accuracy') ) #=> Train/Test Evaluation

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - -   Fit the Neural Networks - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# Fit the ANN model
system.time ( 
  history_L1 <- fit (
    object           = ANN_L1,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time (  
  history_L2 <- fit (
    object           = ANN_L2,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time (
  history_L3 <- fit (
    object           = ANN_L3,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time ( 
  history_L4 <- fit (
    object           = ANN_L4,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time ( 
  history_L5 <- fit (
    object           = ANN_L5,
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time ( 
  history_S1 <- fit (
    object           = ANN_S1,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time ( 
  history_S2 <- fit (
    object           = ANN_S2,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

system.time ( 
  history_S3 <- fit (
    object           = ANN_S3,              
    x                = as.matrix (x_train),    
    y                = y_train,             
    batch_size       = batch_val,    
    epochs           = epoch_val,    
    validation_split = validations_split_val))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - -   Plot Learning History   - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

p_learning_L1 <- plot(history_L1) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_L1, filename = "Figures/p_ANN_learning_L1.pdf", width=8, height=6, dpi=600)





p_learning_L2 <- plot(history_L2) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_L2, filename = "Figures/p_ANN_learning_L2.pdf", width=8, height=6, dpi=600)



p_learning_L3 <- plot(history_L3) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_L3, filename = "Figures/p_ANN_learning_L3.pdf", width=8, height=6, dpi=600)




p_learning_L4 <- plot(history_L4) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_L4, filename = "Figures/p_ANN_learning_L4.pdf", width=8, height=6, dpi=600)




p_learning_L5 <- plot(history_L5) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_L5, filename = "Figures/p_ANN_learning_L5.pdf", width=8, height=6, dpi=600)



p_learning_S1 <- plot(history_S1) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_S1, filename = "Figures/p_ANN_learning_S1.pdf", width=8, height=6, dpi=600)

p_learning_S2 <- plot(history_S2) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_S2, filename = "Figures/p_ANN_learning_S2.pdf", width=8, height=6, dpi=600)

p_learning_S3 <- plot(history_S3) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
  theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning_S3, filename = "Figures/p_ANN_learning_S3.pdf", width=8, height=6, dpi=600)




# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - -   Make Predictions  - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Hidden Layers: 1 - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_L1
pred_NN_class_vec_L1 <- predict_classes (object = ANN_L1,
                                      x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_L1 <- 
  predict_proba (object = ANN_L1, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_L1 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_L1) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L1 )

####
#library(pROC)
roc_ANN_L1 <- pROC::roc(test$default, pred_NN_prob_vec_L1, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_L1 <- coords(roc_ANN_L1, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_L1_val <- 
  predict_proba (object = ANN_L1, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_L1_val > alhpa_L1$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_L1 = perform_class(y_pred, y_act ,classlabels))

rm(y_pred_num, y_pred, y_act, classlabels)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Hidden Layers: 2 - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_L1
pred_NN_class_vec_L2 <- predict_classes (object = ANN_L2,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_L2 <- 
  predict_proba (object = ANN_L2, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_L2 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_L2) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L2 )

####
#library(pROC)
roc_ANN_L2 <- pROC::roc(test$default, pred_NN_prob_vec_L2, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_L2 <- coords(roc_ANN_L2, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_L2_val <- 
  predict_proba (object = ANN_L2, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_L2_val > alhpa_L2$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_L2 = perform_class(y_pred, y_act ,classlabels))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Hidden Layers: 3 - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_L1
pred_NN_class_vec_L3 <- predict_classes (object = ANN_L3,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_L3 <- 
  predict_proba (object = ANN_L3, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_L3 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_L3) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L3 )

####
#library(pROC)
roc_ANN_L3 <- pROC::roc(test$default, pred_NN_prob_vec_L3, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_L3 <- coords(roc_ANN_L2, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_L3_val <- 
  predict_proba (object = ANN_L3, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_L3_val > alhpa_L3$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_L3 = perform_class(y_pred, y_act ,classlabels))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Hidden Layers: 4 - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_L4
pred_NN_class_vec_L4 <- predict_classes (object = ANN_L4,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_L4 <- 
  predict_proba (object = ANN_L4, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_L4 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_L4) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L4 )

####
#library(pROC)
roc_ANN_L4 <- pROC::roc(test$default, pred_NN_prob_vec_L4, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_L4 <- coords(roc_ANN_L4, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_L4_val <- 
  predict_proba (object = ANN_L4, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_L4_val > alhpa_L4$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_L4 = perform_class(y_pred, y_act ,classlabels))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Hidden Layers: 5 - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_L5
pred_NN_class_vec_L5 <- predict_classes (object = ANN_L5,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_L5 <- 
  predict_proba (object = ANN_L5, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_L2 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_L5) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L5 )

####
#library(pROC)
roc_ANN_L5 <- pROC::roc(test$default, pred_NN_prob_vec_L5, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_L5 <- coords(roc_ANN_L2, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_L5_val <- 
  predict_proba (object = ANN_L5, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_L5_val > alhpa_L5$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_L5 = perform_class(y_pred, y_act ,classlabels))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - -  ANN Sirignano et. al - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Make predictions ANN_S1
pred_NN_class_vec_S1 <- predict_classes (object = ANN_S1,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_S1 <- 
  predict_proba (object = ANN_S1, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_S1 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_S1) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L2 )

####
#library(pROC)
roc_ANN_S1 <- pROC::roc(test$default, pred_NN_prob_vec_S1, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_S1 <- coords(roc_ANN_S1, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_S1_val <- 
  predict_proba (object = ANN_S1, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_S1_val > alhpa_S1$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_S1 = perform_class(y_pred, y_act ,classlabels))


# Make predictions ANN_S2
pred_NN_class_vec_S2 <- predict_classes (object = ANN_S2,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_S2 <- 
  predict_proba (object = ANN_S2, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_S2 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_S2) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L2 )

####
#library(pROC)
roc_ANN_S2 <- pROC::roc(test$default, pred_NN_prob_vec_S2, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_S2 <- coords(roc_ANN_S2, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_S2_val <- 
  predict_proba (object = ANN_S2, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_S2_val > alhpa_S2$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_S2 = perform_class(y_pred, y_act ,classlabels))


# Make predictions ANN_S3
pred_NN_class_vec_S3 <- predict_classes (object = ANN_S3,
                                         x = as.matrix(x_test)) %>%
  as.vector()

# Predicted Probabilities
pred_NN_prob_vec_S3 <- 
  predict_proba (object = ANN_S3, 
                 x = as.matrix(x_test)) %>%
  as.vector()

# Format data and predictions for yardstick
estimates_ANN_S3 <- tibble(
  truth      = as.factor(y_test) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  estimate   = as.factor(pred_NN_class_vec_S3) %>% 
    fct_recode ('TRUE' = "1", 'FALSE' = "0"),
  class_prob = pred_NN_prob_vec_L2 )

####
#library(pROC)
roc_ANN_S3 <- pROC::roc(test$default, pred_NN_prob_vec_S3, data = test, percent=TRUE, plot=FALSE, print.auc = FALSE)
alhpa_S3 <- coords(roc_ANN_S3, "best", ret = "threshold")


# Predicted Probabilities
pred_NN_prob_vec_S3_val <- 
  predict_proba (object = ANN_S3, 
                 x = as.matrix(x_val)) %>%
  as.vector()

y_pred_num <- ifelse(pred_NN_prob_vec_S3_val > alhpa_S3$threshold, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- as.factor(y_val)
classlabels = c(0,1)
(performance_ANN_S3 = perform_class(y_pred, y_act ,classlabels))




