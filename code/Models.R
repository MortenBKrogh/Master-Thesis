# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - Logistic Regression - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# cv <- trainControl(method = "cv", number = 5)
# library(e1071)
# fit_glm <- caret::train(as.factor(default)   ~ .,
#                  data      = train,
#                  trControl = cv, 
#                  method    = "glm", 
#                  #metric    = "Accuracy",
#                  #na.action=na.exclude
#                  )
# 
# table(predict(fit_glm, newdata=test), test$default)

# Estimate the logistic regression model using all the features we selected
# in the main document.
system.time ( 
  mod_logreg <- glm(default ~ .,
                    data = train,
                    family="binomial")
)
  

# summary(mod_logreg)
# stargazer::stargazer(mod_logreg)
# Make prediction from the logistic regression model
#options(warn=-1)      #turn off warnings
pred_logreg <- predict(mod_logreg, newdata = test, type = "response")
#options(warn=1)      #turn warnings back on

sum(FM_data$orig_upb)
# Confusion matrix and performance metrics
y_pred_num <- ifelse(pred_logreg > 0.2, TRUE, FALSE)
y_pred <- factor(y_pred_num, levels=c(FALSE, TRUE))
y_act <- test$default
classlabels = c(0, 1)
(performance_LR = perform_class(y_pred, y_act, classlabels))

#sspec <- function(x,b) {rbind(caret::spec(x), caret::precision(x), accuracy(x), recall(x), npv(x)) %>% mutate(model = b)}

# save confusion matrix table to disk
cat('\\begin{table}[H]
     \\centering
     \\caption{Confusion Matrix}
     \\label{tab:Confusion_matrix}
     \\begin{tabular}{@{}cc cc@{}}
     \\multicolumn{1}{c}{} &\\multicolumn{1}{c}{} &\\multicolumn{2}{c}{\\textbf{Actual}} \\\\
     \\cmidrule(lr){3-4}
     \\multicolumn{1}{c}{} & 
     \\multicolumn{1}{c}{} & 
     \\multicolumn{1}{c}{Default} & 
     \\multicolumn{1}{c}{Not-default} \\\\
     \\vspace{0.1cm}
     \\cline{2-4}
     \\multirow[c]{2}{*}{\\rotatebox[origin=tr]{90}{\\textbf{Predicted}}}',
    paste0('& Default  & ', format(performance_LR$confus[1], nsmall=1, big.mark="."), ' & ', format(performance_LR$confus[3], nsmall = 1, bog.mark = "."), ' \\\\[1.5ex]'),
    paste0('& Not-default &', format(performance_LR$confus[2],nsmall=1, big.mark="."),  ' & ', format(performance_LR$confus[4], nsmall=1, big.mark="."),' \\\\'), 
    '\\cline{2-4}
     \\end{tabular}
     \\end{table}', file = "Tables/Confus_LR.tex", append = FALSE)


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
    epochs           = 20,     #=> Control Training cycles
    validation_split = 0.30) ) #=> Include 30% data for 'Validation' Model


p_learning <- plot(history) + thd + scale_color_manual(values = c(basem3, basem4)) + scale_fill_manual(values = c(basem3, basem4)) + 
  #labs(title = 'Artificial Neural Netw') +
   theme(legend.title = element_blank(), legend.key  = element_blank()) + labs(caption = 'Data: Freddie Mac Single Home Loan Level Dataset')

# save to disk
ggsave(p_learning, filename = "Figures/p_ANN_learning.pdf", width=8, height=6, dpi=600)


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

# save confusion matrix table to disk
cat('\\begin{table}[H]
     \\centering
     \\caption{Confusion Matrix}
     \\label{tab:Confusion_matrix}
     \\begin{tabular}{@{}cc cc@{}}
     \\multicolumn{1}{c}{} &\\multicolumn{1}{c}{} &\\multicolumn{2}{c}{\\textbf{Actual}} \\\\
     \\cmidrule(lr){3-4}
     \\multicolumn{1}{c}{} & 
     \\multicolumn{1}{c}{} & 
     \\multicolumn{1}{c}{Default} & 
     \\multicolumn{1}{c}{Not-default} \\\\
     \\vspace{0.1cm}
     \\cline{2-4}
     \\multirow[c]{2}{*}{\\rotatebox[origin=tr]{90}{\\textbf{Predicted}}}',
     paste0('& Default  & ', format(performance_ANN$confus[1], nsmall=1, big.mark="."), '&', format(performance_ANN$confus[3], nsmall = 1, bog.mark = "."), ' \\\\[1.5ex]'),
     paste0('& Not-default &', format(performance_ANN$confus[2],nsmall=1, big.mark="."),  '&', format(performance_ANN$confus[4], nsmall=1, big.mark="."),' \\\\'), 
     '\\cline{2-4}
     \\end{tabular}
     \\end{table}', file = "Tables/Confus_ANN.tex", append = FALSE)







# create roc object
library(pROC)
roc_NeuralNet <- roc(test$default, pred_NN_prob_vec, data = test,percent=TRUE, plot=TRUE,print.auc = TRUE)
p_roc_NeuralNet<- ggroc(list(Neural_Network = roc_NeuralNet), alpha = 1, linetype = 3, size = 1) + thd + th  + scale_color_manual(values = c(basem3, basem4)) 
p_roc_NeuralNet 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - -   ROC Curves    - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
p_roc <- ggroc(list(logreg = roc_logreg, NeuralNet = roc_NeuralNet), alpha = 1, linetype = 2, size = 1) + thd + th  + scale_color_manual(values = c(basem3, basem4)) + theme(legend.key = element_blank(), legend.title = element_blank())
p_roc 
# save to disk
ggsave(p_roc, filename = "Figures/p_roc.pdf", width=8, height=6, dpi=600) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - Bar plot performance  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# for creating bar plots
df <- data.frame(model   = rep(c("LR", "ANN"), each=5),
                 metric = rep(c("specificity", "sensitivity", "accuracy", "ppv", "npv"),2),
                 value   = c(performance_LR[[2]], performance_LR[[3]], performance_LR[[4]], performance_LR[[5]], performance_LR[[6]], 
                             performance_ANN[[2]], performance_ANN[[3]], performance_ANN[[4]], performance_ANN[[5]], performance_ANN[[6]]))

library(ggplot2)
p_performance <- ggplot(data=df, aes(x=metric, y=value, fill=model)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=scales::percent(value)), vjust=1, color="white", hjust = 1,
            position = position_dodge(1), size=3.5, angle = 90) +
  #scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c(basem3, basem4)) + th +
  labs(caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
       y       = "",
       x       = "") +
  theme(legend.title = element_blank())
  
ggsave(p_performance, filename = "Figures/p_performance.pdf", width=8, height=6, dpi=600) 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - LIME EXPLANIER  - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Confusion Matrix
#truth      = as.factor(y_test)

estimates_ANN_v1 %>% conf_mat (y_act, y_pred_num)

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
#tibble::as_tibble()

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

# plot explaining features
p_lime_features_ANN <- plot_features (explanation) +
  labs (#title = "LIME: Feature Importance Visualized",
        #subtitle = "First 10 cases of test set explained",
        caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
        y = "") +
  scale_fill_manual(values = c(basem3, basem4)) + th 

ggsave(p_lime_features_ANN, filename = "Figures/p_lime_features_ANN.pdf", width=8, height=8, dpi=600) 

# plot explanations
plot_explanations (explanation) +
  labs (title = "LIME Feature Importance Heatmap",
        subtitle = "Hold Out (Test) Set, First 10 Cases Shown",
        caption = 'Data: Freddie Mac Single Family Loan-Level 2019.') +
  scale_fill_manual(values = c(basem3, basem4)) + th 





