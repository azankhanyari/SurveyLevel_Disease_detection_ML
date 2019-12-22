
library(xgboost)
data_xbg <- model_train_tomek
data_xbg$Status <- as.numeric(data_xbg$Status)
table(data_xbg$Status)
test_x <- test

test_x$Status <- as.numeric(test_x$Status) 
str(data_xbg$Status)
table(data_xbg$Status)
data_xbg$Status <- ifelse(data_xbg$Status == 2,0,data_xbg$Status)


train_xgb <- data_xbg[,-23]
test_xgb <- test_x[,-23]

train_xgb_2 <- as.matrix(train_xgb)
test_xgb_2 <- as.matrix(test_xgb)

dtrain <- xgb.DMatrix(data = train_xgb_2, label = data_xbg$Status)
dtest <- xgb.DMatrix(data = test_xgb_2, label= test_x$Status)

# train_v2$is_open <- as.numeric(train_v2$is_open)
# test_v2$is_open <- as.numeric(test_v2$is_open)


####################### train a model using our training data  ###############
set.seed(786)
model_xgboost <- xgboost(booster = 'gbtree', data = dtrain, # the data   
                         nrounds=1000, # max number of boosting iterations
                         objective = 'multi:softmax', num_class = 2,max_depth=5,eta = 0.08,silent =1,nthread =12,
                         eval_metric ="merror", min_child_weight =1, subsample = 0.5,colsample_bytree = 0.7)  # the objective function

params <- list(booster = "gbtree", objective = "binary:logistic",num_class = 2,eval_metric ="auc", eta=0.12,silent =1, gamma=0, max_depth=6, min_child_weight=1, subsample=0.5, colsample_bytree=0.7)

params <- list(booster = "gbtree", objective = "binary:logistic",eval_metric ="error", eta=0.12,silent =1, gamma=0, max_depth=20, min_child_weight=1, subsample=0.5, colsample_bytree=0.7)

xgb_cv <- xgb.cv( params = params, data = dtrain, nrounds = 500, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
##best iteration = 490

# xgb_final <- xgboost(data = dtrain,objective = "multi:softmax",num_class = 2,eval_metric ="merror", eta=0.12,silent =1, gamma=0, max_depth=6, min_child_weight=1, subsample=0.5, colsample_bytree=0.7,  nrounds = 490, print_every_n = 10, eval_metric = "merror")

max_auc_idx <- which.max(xgb_cv$evaluation_log[,test_error_mean])

#model prediction 1

pred_xgb <- predict(model_xgboost, dtest)

#chnge to factor for confusion matrix
# pred_real_fact_cv <- factor(test_x$Status, levels = c(0,1), labels = c(0,1) )
# pred_factor_cv <- factor(pred_xgb, levels =  c(0,1), labels = c(0,1))

#recode to factor pred object
library(car)
prediction <- as.factor(as.numeric(pred_xgb > 0.5))
prediction <- recode(prediction,"0 = 'Diabetic';1 = 'Healthy'")

str(pred_xgb)
pred_xgb <- as.factor(pred_xgb)
levels(pred_xgb)
levels(pred_xgb ) <- c('Healthy','Diabetic')

 # pred_xgb<- relevel(pred_xgb, 'Diabetic')

caret::confusionMatrix(pred_xgb,test_x$Status)

str(test_x$Status)
test_x$Status <- as.factor(test_x$Status)
table(test_x$Status)

levels(test_x$Status) <- c('Diabetic','Healthy')


 caret::confusionMatrix(pred_xgb,test_x$Status)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Diabetic Healthy
# Diabetic      100     114
# Healthy        57     278
# 
# Accuracy : 0.6885          
# 95% CI : (0.6479, 0.7271)
# No Information Rate : 0.714           
# P-Value [Acc > NIR] : 0.9137          
# 
# Kappa : 0.3122          
# 
# Mcnemar's Test P-Value : 1.849e-05       
#                                           
#             Sensitivity : 0.6369          
#             Specificity : 0.7092          
#          Pos Pred Value : 0.4673          
#          Neg Pred Value : 0.8299          
#              Prevalence : 0.2860          
#          Detection Rate : 0.1821          
#    Detection Prevalence : 0.3898          
#       Balanced Accuracy : 0.6731          
#                                           
#        'Positive' Class : Diabetic  