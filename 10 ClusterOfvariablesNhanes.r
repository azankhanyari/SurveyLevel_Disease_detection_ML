nums_cols <- unlist(lapply(df_18July_featureS_nhanes, is.numeric))  

nhanes_num <- df_18July_featureS_nhanes[,nums_cols]
nhanes_num <- nhanes_num[,-1]

nhanes_factor <- df_18July_featureS_nhanes[,!nums_cols]

library(ClustOfVar)

tree <- hclustvar(nhanes_num, nhanes_factor)

#plot dendogram to pdf as too big
pdf("dendogram_2.pdf", width=40, height=15)
plot(tree) 
dev.off()

#plot stability to pdf
stab<-stability(tree,B=40)
pdf("stability_Dendo.pdf", width=40, height=15)
plot(stab,main="Stability of the partitions")

boxplot(stab$matCR, main = "Dispersion of the adjusted Rand index")

dev.off()

tree$clusmat
 part<- cutreevar(tree,94) #cut of the tree
 print(part)

 part$var
part$scores
 
 clust_df <- df_18July_featureS_nhanes[,c('drxts080_avg','drxts100_avg','drxtchl_avg',"drxtpfat_avg","drxtsfat_avg","drxtm221_avg",
                                          "drxtacar_avg",'drxtb12a_avg','drxtfa_avg','dbq197','drxtcopp_avg',
                                          'drxtmagn_avg','drxtphos_avg','drxtcalc_avg','drxtkcal_avg','drxtcarb_avg',
                                          'WaistCircumference','BMI','mcq080','paq640','paq655','paq670','mcq365c',
                                          'BPSystolic','dmdmartl','IrregularPulse','ridageyr.y','mcq220','mcq010','dlq010',
                                          'pad680','dpq050','drxtalco_avg','smd030','dmdhhsza','inq320','dmdeduc2','cbd121',
                                          'riagendr','bmxleg','HeartRate','ridreth3','dbd905'
                                          
                                          )]
 
 
 str(clust_df, list.len=ncol(clust_df))
 
 clust_df <- cbind(clust_df, df_18July_featureS_nhanes$Status )
 
 colnames(clust_df)[44] <- 'Status'
 clust_df$Status <- as.factor(clust_df$Status)
 
 ## Data Partition for internal evalution (Stratified Sampling)
 set.seed(1234)
 train.index <- createDataPartition(dendo_df$Status, p = .70, list = FALSE)
 train <-dendo_df[ train.index,]
 test  <- dendo_df[-train.index,]

 dendo_df$Status <- as.factor(dendo_df$Status)
 ################## Handling Class Imbalance with Tomek Links and Smoteing ############
 ########### Handling Unbalanced data by TOMEK & SMOTE ####################
 library(DMwR)
 
 set.seed(1234)
 train_SMOTED <- SMOTE(Status ~ .,train)
 table(train_SMOTED$Status)
 
 control_rf <- trainControl(method='repeatedcv', 
                            number=10, 
                            repeats=3,
                            search="random",
                            summaryFunction = twoClassSummary, 
                            classProbs = T)
 # metric <- "ROC"
 
 
 #mtry <- sqrt(ncol(br_train_NHANES))
 
 #tunegrid <- expand.grid(.mtry=mtry)
 library(caret)
 
 levels(train_SMOTED$Status) <- c('Healthy','Diabetic')
 train_SMOTED$Status <- relevel(train_SMOTED$Status,'Diabetic')
 
 
 levels(test$Status) <- c('Healthy','Diabetic')
 test$Status <- relevel(test$Status,'Diabetic')
 
 set.seed(123)
 
 randomForest <- caret::train(Status ~., 
                              data=train_SMOTED, 
                              method='rf', metric = "ROC",allowParallel = T,
                              # metric='Sens', 
                              # tuneGrid=tunegrid, 
                              trControl=control_rf)
 randomForest
 
 predict_tomek <- predict(randomForest, test[,-59])
 
 confusionMatrix(predict_tomek, test$Status)
 
 library(ROCR)
 perf <- predict(randomForest, type = "prob", newdata = test[,-59])[,2]
 perf_2 <- ROCR::prediction(perf, test$Status)
 AUC_test <- ROCR::performance(perf_2, measure = "auc")@y.values[[1]]
 AUC_test
 # ## Creating tomeklinks and removing the irrelevant datapoints
 # set.seed(1234)
 # tomek = ubTomek(train_SMOTED[,-23], train_SMOTED[,23])
 # model_train_tomek = cbind(tomek$X,tomek$Y)
 # names(model_train_tomek)[23] = "Status" 
 