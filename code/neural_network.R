# Data
FM_data$cnt_borr <- as.character(FM_data$cnt_borr)
data_model <- subset(FM_data, !is.na(FM_data$delic_binary)) %>%
  select(delic_binary, fico, flag_fthb, st, channel, loan_purpose, dti, prop_type, orig_upb, seller_name, int_rt, cnt_units, occpy_sts, ltv, cnt_borr)

# Mice imputation ---------------------------------------------------------
#temp <- mice(data_model, m = 1, maxit=50, meth='pmm', seed = 500)
#imp_data_model <- complete(temp, 1)
#sum(is.na(imp_data_model))
index    <- createDataPartition(data_model$delic_binary, p = 0.7, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe
model_recipe_steps <-
  recipe(delic_binary ~ ., data = training) %>%
  step_string2factor(flag_fthb, st, channel, loan_purpose, prop_type, seller_name, occpy_sts, cnt_borr) %>%
  step_dummy(flag_fthb, st, channel, loan_purpose, prop_type, seller_name, occpy_sts, cnt_borr) %>%
  step_range(fico, dti, orig_upb, int_rt, cnt_units, ltv, min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training)
test           <- bake(prepped_recipe, test)

test[is.na(test)] <- 0

x_train  <- training %>% select(-delic_binary) %>% as.matrix()
x_test   <- test     %>% select(-delic_binary) %>% as.matrix()
y_train  <- training %>% select(delic_binary) %>% as.matrix()
y_test   <- test     %>% select(delic_binary) %>% as.matrix()

model <- keras_model_sequential() %>%
  layer_dense(input_shape = dim(x_train)[2], units = 128) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.2) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dropout(0.1) %>% 
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid", kernel_initializer = 'uniform')

model %>% compile(optimizer = 'adam',
                  loss      = 'binary_crossentropy',
                  metrics   = 'accuracy')

# The patience parameter is the amount of epochs to check for improvement.
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

hist<-model %>% keras::fit(x_train, y_train,
                           batch_size = 256,
                           epochs = 35,
                           validation_split = 0.1,
                           verbose = 0,
                           callbacks = list(early_stop, print_dot_callback))

yhat_keras_class_vec <- predict_classes(object = model, x = x_test) %>%
  as.vector()

yhat_keras_class_vec_prob <- predict_proba(object = model, x = x_test) %>%
  as.vector()
yhat_keras_class_vec_prob

model %>% evaluate(x_test, y_test,verbose = 0)



### ------ Logistic Regression ------ ###
LR_model <- keras_model_sequential() %>%
            layer_dense(input_shape = dim(x_train)[2], 
                        units = 1, 
                        activation = "softmax", kernel_initializer = 'uniform', 
                        kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2=0.1))
          
LR_model %>% compile(optimizer = 'sgd',
                     loss      = 'binary_crossentropy',
                     metrics   = 'accuracy')




hist<-LR_model %>% keras::fit(x_train, y_train,
                           batch_size = 256,
                           epochs = 35,
                           validation_split = 0.1)
LR_model %>% evaluate(x_test, y_test,verbose = 0)








