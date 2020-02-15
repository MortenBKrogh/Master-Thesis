suppressPackageStartupMessages({ library (readxl)
  library (keras);library (lime);library (tidyquant)
  library (rsample);library (recipes);library (yardstick)
  library (corrr);library(knitr);library (DT) })


# Data
FM_data$cnt_borr <- as.character(FM_data$cnt_borr)

data_model <- subset(FM_data, !is.na(FM_data$default_flag)) %>%
  select(default_flag, fico, flag_fthb, st, channel, loan_purpose, 
         dti, prop_type, orig_upb, seller_name, int_rt, cnt_units, 
         occpy_sts, ltv, cnt_borr, cltv, '#30_days_late', 'surv_30')


index    <- createDataPartition(data_model$default_flag, p = 0.7, list = FALSE)
training <- data_model[index,]
test     <- data_model[-index,]

# Recipe
model_recipe_steps <-
  recipe(default_flag ~ ., data = training) %>%
  step_string2factor(flag_fthb, st, channel, loan_purpose, prop_type, seller_name, occpy_sts, cnt_borr) %>%
  step_dummy(flag_fthb, st, channel, loan_purpose, prop_type, seller_name, occpy_sts, cnt_borr) %>%
  step_range(fico, dti, orig_upb, int_rt, cnt_units, ltv, '#30_days_late', 'surv_30', min = 0, max = 1)

prepped_recipe <- prep(model_recipe_steps, training = training)
training       <- bake(prepped_recipe, training)
test           <- bake(prepped_recipe, test)

test[is.na(test)] <- 0

x_train  <- training %>% select(-default_flag) %>% as.matrix()
x_test   <- test     %>% select(-default_flag) %>% as.matrix()
y_train  <- training %>% select(default_flag) %>% as.matrix()
y_test   <- test     %>% select(default_flag) %>% as.matrix()



### ------------------------------------------------------- ###
###                   Multi Layer Percepton                 ###
### ------------------------------------------------------- ###
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
# early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

hist<-model %>% keras::fit(x_train, y_train,
                           batch_size = 256,
                           epochs = 5,
                           validation_split = 0.1)

# yhat_keras_class_vec <- predict_classes(object = model, x = x_test) %>%
#   as.vector()
# 
# yhat_keras_class_vec_prob <- predict_proba(object = model, x = x_test) %>%
#   as.vector()
# yhat_keras_class_vec_prob

model %>% evaluate(x_test, y_test,verbose = 0)



### ------ Logistic Regression ------ ###
data_LR_model <- subset(FM_data, !is.na(FM_data$default_flag)) %>%
  select(default_flag, fico) #, flag_fthb, st, channel, loan_purpose, dti, prop_type, orig_upb, seller_name, int_rt, cnt_units, occpy_sts, ltv, cnt_borr, cltv, '#current', '#30_days_late', '#60_days_late', '#90_days_late', 'surv_30')

training_LR <- data_LR_model[index,] %>% sample_n(100000)
test     <- data_LR_model[-index,] %>% sample_n(30000)

cv <- trainControl(method = "cv", number = 5)

fit_glm <- train(as.numeric(default_flag) ~ .,
                 data      = training_LR,
                 trControl = cv, 
                 method    = "glm",
                 family    = "binomial")


fit_glm %>% evaluate(test$fico, test$default_flag)




# Fit the model and see the result
glm.fit <- glm(default_flag ~ . ,data=training_LR,family=binomial)
summary(glm.fit)
# Generate predicted probabilities Remeber that ! means opposite or where train is not
glm.probs <- predict(glm.fit, ,type="response")
# And classify
glm.pred=rep("Down",sum(!train))
glm.pred[glm.probs>.5] <- "Up"
# Compare with truth
# First collumn is what the model predicted
table(Smarket$Direction[!train],glm.pred)
mean(glm.pred==Smarket$Direction[!train])








