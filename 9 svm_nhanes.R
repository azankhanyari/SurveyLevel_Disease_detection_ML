
library(rBayesianOptimization)
df_SVM <- df_18July_featureS_nhanes[,funr_targ]
str(df_SVM)

df_SVM$Status <- as.factor(df_SVM$Status)
levels(df_SVM$Status) <- c('Healthy','Diabetic')
df_SVM$Status <- relevel(df_SVM$Status,'Diabetic')

#z-score standardisation 
z_Scored_df <- as.data.frame(scale(df_SVM[,-23], scale = T, center = T))

df_SVM <- cbind(z_Scored_df,df_SVM$Status)
colnames(df_SVM)[23] <- 'Status'

#  training set into training and test sets NO resampleing

set.seed(123)

library(caret)
train.index<-createDataPartition(df_SVM$Status, p=0.8, list = FALSE)
train.set<- df_SVM[train.index, ]
test.set<- df_SVM[-train.index, ]

table(train.set$Status)

# gbm - bayes control

SVMcustomEmp <- trainControl(classProbs = TRUE,
                             summaryFunction = twoClassSummary,
                             verboseIter = T)


bayesOPtim_SVM <- function(logCost, logGamma) {
  Tuneparams = data.frame(.C = exp(logCost), .sigma = exp(logGamma))
  svm.tune <- train(Status ~.,
                    data = train.set,
                    method = "svmRadial",
                    metric="ROC",
                    tuneGrid = Tuneparams,
                    trControl=SVMcustomEmp)
  list(Score = getTrainPerf(svm.tune)[,"TrainROC"], Pred = 0)
}


set.seed(786)

OPT_Res<- BayesianOptimization(bayesOPtim_SVM, bounds= list(logCost = c(-5, 1),
                                                           logGamma = c(-9, -0.75)),
                               init_grid_dt = NULL, init_points = 50, 
                               n_iter = 20, acq = "ucb", kappa =2.576,
                               eps=0, verbose = TRUE)


# Best Parameters Found: 
#   Round = 10	logCost = -3.3745	logGamma = -7.4833	Value = 0.7964 


# Tuned best fit parameters
best.svm.fit <- c(OPT_Res$Best_Par)
best.svm.fit <- as.list(best.svm.fit)

best.cost <- as.numeric(best.svm.fit[1]) 
best.gamma <- as.numeric(best.svm.fit[2]) 

# Train GBM with best fit parameters
BesttuneSVM = data.frame(.C = exp(best.cost), .sigma = exp(best.gamma))

customCrtlSVM <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = T)

set.seed(1)
best.tuned.SVM <- caret::train(Status ~., 
                               data= train.set,
                               method = "svmRadial",
                               metric = "ROC",
                               tuneGrid = BesttuneSVM,
                               verbose = FALSE,                    
                               trControl = customCrtlSVM)

summary(best.tuned.SVM)
print(best.tuned.SVM)

SVM.pred <- predict(best.tuned.SVM,test.set[-23])

confusionMatrix(SVM.pred,test.set$Status, positive = "Diabetic")


Imp <- varImp(best.tuned.SVM, scale = FALSE)


plot(Imp)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Diabetic Healthy
# Diabetic       58      41
# Healthy        47     220
# 
# Accuracy : 0.7596          
# 95% CI : (0.7124, 0.8025)
# No Information Rate : 0.7131          
# P-Value [Acc > NIR] : 0.02675         
# 
# Kappa : 0.4022          
# 
# Mcnemar's Test P-Value : 0.59403         
#                                           
#             Sensitivity : 0.5524          
#             Specificity : 0.8429          
#          Pos Pred Value : 0.5859          
#          Neg Pred Value : 0.8240          
#              Prevalence : 0.2869          
#          Detection Rate : 0.1585          
#    Detection Prevalence : 0.2705          
#       Balanced Accuracy : 0.6976          
#                                           
#        'Positive' Class : Diabetic        
#                                   
library(ROCR)

perf <- predict(best.tuned.SVM, type = "prob", newdata = test.set[,-23])[,2]
perf_2 <- ROCR::prediction(perf, test.set$Status)
AUC_test <- performance(perf_2, measure = "auc")@y.values[[1]]
AUC_test
# > AUC_test
# [1] 0.7931399

################## Handling Class Imbalance with Tomek Links and Smoteing ############
########### Handling Unbalanced data by TOMEK & SMOTE ####################
library(DMwR)
library(unbalanced)
set.seed(1234)
train_SMOTED <- SMOTE(Status ~ .,train.set)
table(train_SMOTED$Status)

str(train_SMOTED$Status)
levels(train_SMOTED$Status) <- c('1','0')
levels(model_train_tomek$Status) <- c('Diabetic','Healthy')

## Creating tomeklinks and removing the irrelevant datapoints
set.seed(1234)
tomek = ubTomek(train_SMOTED[,-23], train_SMOTED[,23])
model_train_tomek = cbind(tomek$X,tomek$Y)
names(model_train_tomek)[23] = "Status"

table(model_train_tomek$Status)

bayesOPtim_SVM_resamp <- function(logCost, logGamma) {
  Tuneparams = data.frame(.C = exp(logCost), .sigma = exp(logGamma))
  svm.tune <- caret::train(Status ~.,
                    data = model_train_tomek,
                    method = "svmRadial",
                    metric="ROC",
                    tuneGrid = Tuneparams,
                    trControl=SVMcustomEmp)
  list(Score = getTrainPerf(svm.tune)[,"TrainROC"], Pred = 0)
}


set.seed(786)

OPT_Res_2<- BayesianOptimization(bayesOPtim_SVM_resamp, bounds= list(logCost = c(-5, 1),
                                                            logGamma = c(-9, -0.75)),
                               init_grid_dt = NULL, init_points = 50, 
                               n_iter = 20, acq = "ucb", kappa =2.576,
                               eps=0, verbose = TRUE)


# elapsed = 52.79	Round = 57	logCost = 1.0000	logGamma = -1.2716	Value = 0.9687

# Tuned best fit parameters
best.svm.fit_2 <- c(OPT_Res_2$Best_Par)
best.svm.fit_2 <- as.list(best.svm.fit_2)

best.cost_2 <- as.numeric(best.svm.fit_2[1]) 
best.gamma_2 <- as.numeric(best.svm.fit_2[2]) 

# Train GBM with best fit parameters
BesttuneSVM_2 = data.frame(.C = exp(best.cost_2), .sigma = exp(best.gamma_2))

customCrtlSVM <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 5,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              verboseIter = T)

set.seed(1)
best.tuned.SVM_2 <- caret::train(Status ~., 
                               data= model_train_tomek,
                               method = "svmRadial",
                               metric = "ROC",
                               tuneGrid = BesttuneSVM_2,
                               verbose = FALSE,                    
                               trControl = customCrtlSVM)


print(best.tuned.SVM_2)

SVM.pred_2 <- predict(best.tuned.SVM_2,test.set[-23])

confusionMatrix(SVM.pred_2,test.set$Status, positive = "Diabetic")

# Confusion Matrix and Statistics
# 
# Reference
# Prediction Diabetic Healthy
# Diabetic       35      37
# Healthy        70     224
# 
# Accuracy : 0.7077          
# 95% CI : (0.6581, 0.7538)
# No Information Rate : 0.7131          
# P-Value [Acc > NIR] : 0.616527        
# 
# Kappa : 0.2114          
# 
# Mcnemar's Test P-Value : 0.001978        
#                                           
#             Sensitivity : 0.33333         
#             Specificity : 0.85824         
#          Pos Pred Value : 0.48611         
#          Neg Pred Value : 0.76190         
#              Prevalence : 0.28689         
#          Detection Rate : 0.09563         
#    Detection Prevalence : 0.19672         
#       Balanced Accuracy : 0.59579         
#                                           
#        'Positive' Class : Diabetic  



#using a linear kernel , very less sensitivity

#Confusion Matrix and Statistics

#          Reference
#Prediction Diabetic Healthy
#  Diabetic       37      24
#  Healthy        68     237
#                                          
#               Accuracy : 0.7486          
#                 95% CI : (0.7009, 0.7923)
#    No Information Rate : 0.7131          
#    P-Value [Acc > NIR] : 0.07293         
#                                          
#                  Kappa : 0.2977          
#                                          
# Mcnemar's Test P-Value : 7.358e-06       
#                                          
#            Sensitivity : 0.3524          
#            Specificity : 0.9080          
#         Pos Pred Value : 0.6066          
#         Neg Pred Value : 0.7770          
#             Prevalence : 0.2869          
#         Detection Rate : 0.1011          
#   Detection Prevalence : 0.1667          
#      Balanced Accuracy : 0.6302          
                                          
#       'Positive' Class : Diabetic   