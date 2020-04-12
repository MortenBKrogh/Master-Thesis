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
  
summary(mod_logreg)
stargazer::stargazer(mod_logreg)

# predict on the test set
pred_logreg_test <- predict(mod_logreg, newdata = test, type = "response")
# create roc object
library(pROC)
roc_logreg <- roc(test$default, pred_logreg_test, data = test,percent=TRUE, plot=TRUE,print.auc = TRUE)
logreg_threshold <- coords(roc_logreg, "best", ret = "threshold")

# predict on the validation set
pred_logreg_val <- predict(mod_logreg, newdata = validation, type = "response")

# Confusion matrix and performance metrics
y_pred_num <- ifelse(pred_logreg_val > logreg_threshold$threshold, TRUE, FALSE)
y_pred <- factor(y_pred_num, levels=c(FALSE, TRUE))
y_act <- validation$default
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
x_val <- bake (preprocessing_steps, new_data = validation) %>% 
  select (-default)

y_train <- ifelse (pull (train, default) == TRUE, 1, 0)
y_test  <- ifelse (pull (test, default)  == TRUE, 1, 0)
y_val   <- ifelse (pull (validation, default)  == TRUE, 1, 0)

# Setting hyper parameters
dropout_val_L1 = 0.2
dropout_val_L2 = dropout_val_L3 = dropout_val_L4 = dropout_val_L5 = 0.5
units_val   = 142
epoch_val   = 25
batch_val   = 528
validations_split_val = 0.3

# - - - -  Estimating the ANNs - - - - #
source('code/ANNs.R')

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

coords(roc_NeuralNet, "best", ret = "threshold")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - -   ROC Curves    - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
p_roc <- ggroc(list(logreg = roc_logreg, NeuralNet = roc_NeuralNet), alpha = 1, linetype = 2, size = 1) + thd + th  + scale_color_manual(values = c(basem3, basem4)) + theme(legend.key = element_blank(), legend.title = element_blank())
p_roc 


p_roc <- ggroc(list(logreg = roc_logreg, 
                    ANN_L1 = roc_ANN_L1,
                    ANN_L2 = roc_ANN_L2,
                    ANN_L3 = roc_ANN_L3,
                    ANN_L4 = roc_ANN_L4,
                    ANN_L5 = roc_ANN_L5,
                    ANN_S1 = roc_ANN_S1,
                    ANN_S2 = roc_ANN_S2,
                    ANN_S3 = roc_ANN_S3), alpha = 1, linetype = 2, size = .3) + thd + th   + theme(legend.key = element_blank(), legend.title = element_blank())

# save to disk
ggsave(p_roc, filename = "Figures/p_roc.pdf", width=8, height=6, dpi=600) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - Bar plot performance  - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# for creating bar plots
df <- data.frame(model   = rep(c("LR", "ANN_L1", "ANN_L2", "ANN_L3", "ANN_L4", "ANN_L5", "ANN_S1", "ANN_S2", "ANN_S3"), each=6),
                 metric = rep(c("specificity", "sensitivity", "accuracy", "ppv", "npv", "AUC"),9),
                 value   = c(performance_LR[[2]], performance_LR[[3]], performance_LR[[4]], performance_LR[[5]], performance_LR[[6]], roc_logreg$auc[1]/100, 
                             performance_ANN_L1[[2]], performance_ANN_L1[[3]], performance_ANN_L1[[4]], performance_ANN_L1[[5]], performance_ANN_L1[[6]], roc_ANN_L1$auc[1]/100,
                             performance_ANN_L2[[2]], performance_ANN_L2[[3]], performance_ANN_L2[[4]], performance_ANN_L2[[5]], performance_ANN_L2[[6]], roc_ANN_L2$auc[1]/100,
                             performance_ANN_L3[[2]], performance_ANN_L3[[3]], performance_ANN_L3[[4]], performance_ANN_L3[[5]], performance_ANN_L3[[6]], roc_ANN_L3$auc[1]/100,
                             performance_ANN_L4[[2]], performance_ANN_L4[[3]], performance_ANN_L4[[4]], performance_ANN_L4[[5]], performance_ANN_L4[[6]], roc_ANN_L4$auc[1]/100,
                             performance_ANN_L5[[2]], performance_ANN_L5[[3]], performance_ANN_L5[[4]], performance_ANN_L5[[5]], performance_ANN_L5[[6]], roc_ANN_L5$auc[1]/100,
                             performance_ANN_S1[[2]], performance_ANN_S1[[3]], performance_ANN_S1[[4]], performance_ANN_S1[[5]], performance_ANN_S1[[6]], roc_ANN_S1$auc[1]/100,
                             performance_ANN_S2[[2]], performance_ANN_S2[[3]], performance_ANN_S2[[4]], performance_ANN_S2[[5]], performance_ANN_S2[[6]], roc_ANN_S2$auc[1]/100,
                             performance_ANN_S3[[2]], performance_ANN_S3[[3]], performance_ANN_S3[[4]], performance_ANN_S3[[5]], performance_ANN_S3[[6]], roc_ANN_S3$auc[1]/100))

library(ggplot2)
(p_performance <- ggplot(data=df, aes(x=metric, y=value, fill=model)) +
  geom_bar(stat="identity", position=position_dodge(0.95))+
  geom_text(aes(label=scales::percent(value)), vjust=0, color="white", hjust = 2,
            position = position_dodge(0.95), 
            size=3, angle = 90) +
  #scale_fill_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::percent) +
  #scale_fill_manual(values = c(basem3, basem4)) + 
  th +
  labs(caption = 'Data: Freddie Mac Single Family Loan-Level 2019.',
       y       = "",
       x       = "") +
  theme(legend.title = element_blank()))
  
ggsave(p_performance, filename = "Figures/p_performance.pdf", width=8, height=6, dpi=600) 

# Generate table of performance metrics to LaTeX
stargazer::stargazer(reshape2::dcast(data = df, model  ~ metric),
                     title = "Performance Metrics",
                     summary = FALSE,
                     digits = 5)







# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - LIME EXPLANIER  - - - - - - - - - - - - - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# Confusion Matrix
#truth      = as.factor(y_test)

estimates_ANN %>% conf_mat (y_act, y_pred_num)

# Accuracy
estimates_ANN %>% metrics (truth, estimate)

# Area Under the Curve 
estimates_ANN %>% roc_auc(truth, class_prob)

# Precision and Recall
tibble (
  precision = estimates_ANN %>% precision(truth, estimate),
  recall    = estimates_ANN %>% recall(truth, estimate) )

# F1-score
estimates_ANN %>% f_meas(truth, estimate, beta = 1)


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


predict_model (x       = ANN, 
               newdata = x_test, 
               type    = 'raw') # %>%
#tibble::as_tibble()

explainer <- lime::lime (
  x              = x_train, 
  model          = ANN, 
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





