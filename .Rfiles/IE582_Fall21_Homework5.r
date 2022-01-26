require(data.table)
require(skimr)
require(ROSE)
require(caret)
require(ranger)
require(gbm)
require(DMwR)
#Sys.setlocale(locale='Chinese')

current_folder = getwd()

train = fread('train.csv')
test = fread('test.csv')

head(train)

head(test)

skim(train)

skim(test)

#check table
table(train$default)

#check classes distribution
prop.table(table(train$default))

# there are some columns to be converted to categoric
train[, c("Var_39", "Var_53")] = lapply(train[, c("Var_39", "Var_53")] , factor)
train[,type:=as.factor(ifelse(default==1, "Bad", "Good"))]

min(train$customer_age)

max(train$customer_age)

train[,age_cluster:=cut(customer_age, breaks=c(18,25,30,45,74), labels=c("young", "graduate", "middle-age", "old"))]

#train_balanced = as.data.table(ovun.sample(default ~ ., data = train, method = "over", N = 3085)$data)

train_smote_balanced = SMOTE(type~., train[,-c("loan_application_id","default")])

table(train_smote_balanced$type)

#plot(type~customer_age, train_balanced)

#plot(type~customer_age, train)

# first we need to write our performance measure

performance = function(data, lev, model){
    obs = data$obs # actual default value
    preds = data$pred # predictions
    # this part is added different from the final, because it filters the loans based on instance index which are used in each fold 
    index = data$rowIndex
    loan = data$weights[index]
    two_class = twoClassSummary(data, lev, model)
    for (i in 1:length(obs)){
        if(obs[i]=="Bad"&preds[i]=="Good"){
           x = loan[i]*(-1)
        } else if (obs[i]=="Good"&preds[i]=="Bad"){
           x = loan[i]*0.15*(-1)    
        } else if (obs[i]=="Good"&preds[i]=="Good"){
           x = 0
        } else if (obs[i]=="Bad"&preds[i]=="Bad"){
           x = 0
        }
      cost[[i]] = x  
    }
    customer_cost = do.call(rbind, cost)
    total_cost = colSums(customer_cost, na.rm = T)
    names(total_cost) = "Cost"
    combined = c(two_class, total_cost)
    combined
}

cost = list()
n_repeats=5
n_folds=10

head(train_smote_balanced)

set.seed(1)

fitControl = trainControl(method = "repeatedcv",
                          number = n_folds,
                          repeats = n_repeats,
                          classProbs=TRUE, 
                          summaryFunction=performance)

tree_fit = train(type ~ ., data = train_smote_balanced[,-c("customer_age", "loan_amount")],  
                 method = "rpart", 
                 trControl = fitControl, 
                 metric = "Cost", 
                 weights = loans_train,
                 tuneLength = 5)

tree_fit

set.seed(1)

fitControl = trainControl(method = "repeatedcv",
                          number = n_folds,
                          repeats = n_repeats,
                          classProbs=TRUE, 
                          summaryFunction=performance)

glm_fit = train(type ~ ., 
                data = train_smote_balanced[,-c("customer_age", "loan_amount")],
                method = "glmnet",
                trControl = fitControl,
                metric = "Cost",
                weights = loans_train,
                tuneLength = 5)

glm_fit

set.seed(1)

fitControl = trainControl(method = "repeatedcv",
                        number = n_folds,
                        repeats = n_repeats,
                        classProbs=TRUE, 
                        summaryFunction=performance)

rf_grid = expand.grid(mtry = c(2, 10, 25, 50),
                      splitrule = c("gini", "extratrees"),
                      min.node.size = 5)

rf_fit = train(type ~ ., 
               data = train_smote_balanced[,-c("customer_age", "loan_amount")],
               method = "ranger",
               trControl = fitControl,
               metric = "Cost",
               weights = loans_train,
               tuneGrid = rf_grid)

rf_fit

set.seed(1)

fitControl=trainControl(method = "repeatedcv",
                        number = n_folds,
                        repeats = n_repeats,
                        classProbs=TRUE, 
                        summaryFunction=performance)

gbmGrid = expand.grid(interaction.depth = c(3, 4, 5), 
                      n.trees = c(3:5)*50, 
                      shrinkage = c(0.01, 0.05, 0.1),
                      n.minobsinnode = 5)

gbm_fit = train(type ~ ., 
                data = train_smote_balanced[,-c("customer_age", "loan_amount")],
                method = "gbm", 
                trControl = fitControl, 
                metric = "Cost",
                weights = loans_train,
                tuneGrid = gbmGrid)

gbm_fit

set.seed(1)
fitControl = trainControl(method = "repeatedcv",
                         number = n_folds,
                         repeats = n_repeats,
                         classProbs=TRUE, 
                         summaryFunction=performance,
                         preProc = c("center", "scale", "nzv"))

svm_fit = train(type ~ ., 
                data = train_smote_balanced[,-c("customer_age", "loan_amount")],
                method = "svmRadialWeights", 
                trControl = fitControl, 
                metric = "Cost",
                weights = loans_train,
                tuneLength = 4)

svm_fit

resamps <- resamples(list(Lasso = gbm_fit,
                          DT = tree_fit,
                          RF = rf_fit,
                          GBM = gbm_fit,
                          SVM = svm_fit))
summary(resamps)

bwplot(resamps,  scales = list(relation = "free"), xlim = list(c(-5000, 0), c(0, 1), c(0, 1), c(0, 1)))

test[, c("Var_39", "Var_53")] = lapply(test[, c("Var_39", "Var_53")] , factor)

min(test$customer_age)

max(test$customer_age)

test[,age_cluster:=cut(customer_age, breaks=c(18,25,30,45,74), labels=c("young", "graduate", "middle-age", "old"))]

prediction = predict(rf_fit, newdata = test)

test$pred_type = prediction

test[,prediction:=ifelse(pred_type=="Bad", 1, 0)]

#check table
table(test$prediction)

result = test[, c("loan_application_id", "prediction")]

fwrite(result, "test_predictions.csv", col.names=T)
