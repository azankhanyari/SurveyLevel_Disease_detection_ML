set.seed(7)

# load the library
library(mlbench)
library(caret)

# define the control using a random forest selection function
control_s <- rfeControl(functions=rfFuncs, method="cv", number=10, verbose = TRUE, allowParallel = T)

# run the recursive feature  algorithm

results <- rfe(rfe_train[,-1], rfe_train[,1], sizes=c(1:ncol(rfe_train)), rfeControl=control_s)

# summarize the results
print(results)

# list the chosen features from all 152 features
rfe_iter3 <- predictors(results)

features_select <- c(rfe_iter3,'Status')

# plot the results
plot(rfe_iter3, type=c("g", "o"))

rfe_best_feature_df <- df_18July_featureS_nhanes[,features_select]

write.csv(rfe_best_feature_df, "C:\\Users\\Rick\\Desktop\\18July_feature_nhanes\\rfe_best_rfModel.csv")

#### Genetic algorithm feature selection
library(caret)
library(doParallel) # parallel processing
library(dplyr) # Used by caret
library(pROC) # plot the ROC curve


#
trainX <-trainData[,-1] # Create training feature data frame
testX <- testData[,-1] # Create test feature data frame 
y=trainData$Class # Target variable for training


registerDoParallel(6) # Registrer a parallel backend for train

getDoParWorkers() # check that there are 4 workers

ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       method = "cv",    # 10 fold cross validation
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE)
## 
set.seed(10)
lev <- c("1","2","3")     # Set the levels

system.time(rf_ga3 <- gafs(x = train_NHANES, y = train_NHANES$Target,
                           iters = 100, # 100 generations of algorithm
                           popSize = 20, # population size for each generation
                           levels = lev,
                           gafsControl = ga_ctrl))
# user   system  elapsed 
# 19.78    19.15 39822.86 
#took 11 hours to complete

rf_ga3

# Features selected by recursive feature elimination

# [1] "ridageyr.y"         "dmdhrage"           "whd140"             "WaistCircumference"
# [5] "whq150"             "whd050"             "BPSystolic"         "BMI"               
# [9] "Weight"             "paq655"             "bmxleg"             "HeartRate"         
# [13] "drxtalco_avg"       "drxtkcal_avg"       "drxtmfat_avg"       "drxtchol_avg"      
# [17] "drxtphos_avg"       "drxtmagn_avg"       "drxtm181_avg"       "drxts160_avg"      
# [21] "drxtchl_avg"        "drxts180_avg"       


# Genetic Algorithm Feature Selection
# 
# 
# Maximum generations: 100 
# Population per generation: 20 
# Crossover probability: 0.8 
# Mutation probability: 0.1 
# Elitism: 0 
# 
# Internal performance values: Accuracy, Kappa
# Subset selection driven to maximize internal Accuracy 
# 
# External performance values: Accuracy, Kappa
# Best iteration chose by maximizing external Accuracy 
# External resampling method: Cross-Validated (10 fold) 
# 
# During resampling:
#   * the top 5 selected variables :
#   Target (100%), cbd131 (80%), dmdfmsiz.2 (60%), dpq030.3 (60%), dmdeduc2.3 (50%)
# * on average, 58.2 variables were selected (min = 26, max = 101)
# 
# In the final search using the entire training set:
#   * 89 features selected at iteration 2 including:
#   drxtkcal_avg, drxtprot_avg, drxtcarb_avg, drxtsfat_avg, drxtpfat_avg 

#best features from GA
# [1] "drxtkcal_avg"       "drxtmfat_avg"       "drxtpfat_avg"       "drxtret_avg"       
# [5] "drxtvara_avg"       "drxtlyco_avg"       "drxtvb1_avg"        "drxtfdfe_avg"      
# [9] "drxtb12a_avg"       "drxtvc_avg"         "drxtcalc_avg"       "drxtsodi_avg"      
# [13] "drxttheo_avg"       "drxts060_avg"       "drxts120_avg"       "drxts180_avg"      
# [17] "drxtm201_avg"       "drxtm221_avg"       "drxtp182_avg"       "drxtp205_avg"      
# [21] "drxtp226_avg"       "huq020"             "BPSystolic"         "Weight"            
# [25] "Height"             "BMI"                "WaistCircumference" "dmdborn4"          
# [29] "dmdhhsiz"           "dmdfmsiz"           "dmdhrgnd"           "dbd895"            
# [33] "dbd905"             "hiq011"             "inq132"             "mcq010"            
# [37] "mcq080"             "mcq203"             "mcq365c"            "mcq220"            
# [41] "paq650"             "paq655"             "pad660"             "paq710"            
# [45] "smq020"             "cbd121"             "cbd131"             "bpq080"

#### genetic alogrithm version 2
custom_fitness <- function(vars, data_x, data_y, p_sampling)
{
  # speeding up things with sampling
  ix=get_sample(data_x, percentage_tr_rows = p_sampling)
  data_2=data_x[ix,]
  data_y_smp=data_y[ix]
  
  # keep only vars from current solution
  names=colnames(data_2)
  names_2=names[vars==1]
  # get the columns of the current solution
  data_sol=data_2[, names_2]
  
  # get the roc value from the created model
  roc_value=get_roc_metric(data_sol, data_y_smp, names_2)
  
  # get the total number of vars for the current selection
  q_vars=sum(vars)
  
  # time for your magic
  fitness_value=roc_value/q_vars
  
  return(fitness_value)
}

get_roc_metric <- function(data_tr_sample, target, best_vars) 
{
  # data_tr_sample=data_sol
  # target = target_var_s
  # best_vars=names_2
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(.mtry=round(mtry))
  
  fit_model_1 = train(x=data_model, 
                      y= target, 
                      method = "rf", 
                      trControl = fitControl,
                      metric = "ROC",
                      tuneGrid=tunegrid
  )
  
  metric=fit_model_1$results["ROC"][1,1]
  
  return(metric)
}




get_accuracy_metric <- function(data_tr_sample, target, best_vars) 
{
  data_model=select(data_tr_sample, one_of(best_vars))
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(mtry=round(mtry))
  
  fit_model_1 = train(x=data_model, 
                      y= target, 
                      method = "rf",
                      tuneGrid = tunegrid)
  
  
  
  metric=fit_model_1$results["Accuracy"][1,1]
  return(metric)
}  


# Install packages if missing
list.of.packages <- c("parallel", "doParallel", "caret", "randomForest", "funModeling", "tidyverse", "GA")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(caret)
library(randomForest)
library(funModeling)
library(tidyverse)
library(GA)
source("lib_ga.R")

nhanes_2class <- read.csv("C:\\Users\\Azaan\\Downloads\\nhanes_2class.csv")
nhanes_2class <- nhanes_2class[,-1]

data=read_delim("data_breast_cancer2.csv", delim = ",")  

# Data preparation
data2=na.omit(data) # <- use with care...

data_y=as.factor(nhanes_2class$Target)
data_x=nhanes_2class[,-147]
str(data_y)


# GA parameters
param_nBits=ncol(nhanes_2class)
col_names=colnames(data_x)

# Executing the GA 
# Executing the GA 
ga_GA_1 = ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                     data_x =  data_x, 
                                                     data_y = data_y, 
                                                     p_sampling = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover=gabin_uCrossover,  # cross-over method
             elitism = 3, # number of best ind. to pass to next iteration
             pmutation = 0.03, # mutation rate prob
             popSize = 50, # the number of indivduals/solutions
             nBits = param_nBits, # total number of variables
             names=col_names, # variable name
             run=5, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor=plot, # plot the result at each iteration
             keepBest = TRUE, 
             parallel = T,
             seed=84211 
)

# Checking the results
summary(ga_GA_1)

# Following line will return the variable names of the final and best solution
best_vars_ga=col_names[ga_GA_1@solution[1,]==1]

# Checking the variables of the best solution...
best_vars_ga

