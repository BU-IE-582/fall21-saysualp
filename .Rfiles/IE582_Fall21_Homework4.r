require(data.table)
require(ggplot2)
require(repr)
require(rpart)
require(rattle)
require(glmnet)
require(e1071)
require(plyr)

options(scipen=999)
options(repr.plot.width=15, repr.plot.height=8)

# assuming you have the homework data in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

data = fread('Musk1.csv')
setnames(data, "V1", "BagClass")
setnames(data, "V2", "BagId")
setnames(data, 3:ncol(data), paste0("Feature", 1:(ncol(data)-2)))
classes = unique(data[,c("BagClass", "BagId")]) 
head(data)

pca_list = list()

# extract first eigenvetor for each BagId
for (i in 1:length(unique(data[["BagId"]]))){
    pca = prcomp(data[BagId==i, 3:ncol(data)])
    pca_list[[i]] = t(pca$rotation[,1])
    
}
# merge all representations
pca_rep = as.data.table(do.call(rbind, pca_list))
# assign BagId
pca_rep[,BagId:=1:nrow(pca_rep)]
# merge on BagId to obtain class information
pca_rep = merge(pca_rep, 
                classes,
                by="BagId",
                all=TRUE)
# reorder columns and create final feature matrix
pca_rep = pca_rep[,c(168,1:167)]
head(pca_rep)
pca_rep_classification_data = pca_rep[,c(1,3:168)]

cluster_list = list()

# use Hierarchial clustering for each BagId
for (i in 1:length(unique(data[["BagId"]]))){
    clusters = hclust(dist(data[BagId==i, 3:ncol(data)]))
    # since the minumum number instances belongs to each class is 2, it is selected as cluster parameter
    clusterCut = cutree(clusters, 2)
    dt_clusterCut = as.data.table(clusterCut)
    # count the number of instances in each class and save it as vector
    frequencies = t(dt_clusterCut[,.N,c("clusterCut")][,2])
    cluster_list[[i]] = frequencies
    
}
# merge all representations
cluster_rep = as.data.table(do.call(rbind, cluster_list))
# assign BagId
cluster_rep[,BagId:=1:nrow(cluster_rep)]
# merge on BagId to obtain class information
cluster_rep = merge(cluster_rep, 
                    classes,
                    by="BagId",
                    all=TRUE)
# reorder columns and create final feature matrix
cluster_rep = cluster_rep[,c(4,1:3)]
setnames(cluster_rep, "V1", "in_first_cluster")
setnames(cluster_rep, "V2", "in_second_cluster")
head(cluster_rep)
cluster_rep_classification_data = cluster_rep[,c(1,3,4)]

set.seed(2021)

# with pca_rep 
variables =  paste0("Feature", 1:166)
formula <- as.formula(
  paste("BagClass", 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
folds = split(pca_rep_classification_data, cut(sample(1:nrow(pca_rep_classification_data)), 35))
errs = rep(NA, length(folds))

# apply 10-fold cross-validation
for (i in 1:length(folds)) {
 test = ldply(folds[i], data.frame)
 train = ldply(folds[-i], data.frame)
 tmp.train_mat = as.matrix(subset(train, select=-c(.id, BagClass)))
 tmp.test_mat = as.matrix(subset(test, select=-c(.id, BagClass)))
 # penalized version of Logistic Regression is used since we have p<N features, 10-fold cross-validation is done to obtain lambda.min
 tmp.model = cv.glmnet(tmp.train_mat, train$BagClass, family='binomial', type.measure='class', nfolds=10)
 tmp.predicted_prob = predict(tmp.model, tmp.test_mat, s='lambda.min', type='response')
 tmp.predict = as.integer(tmp.predicted_prob > 0.5)
 conf.mat = table(test$BagClass, tmp.predict)
 errs[i] = 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 10-fold cross-validation: %.3f percent", 100*mean(errs)))

# with cluster_rep
formula = "BagClass ~ in_first_cluster + in_second_cluster"
folds = split(cluster_rep_classification_data, cut(sample(1:nrow(cluster_rep_classification_data)), 35))
errs = rep(NA, length(folds))

# apply 10-fold cross-validation
for (i in 1:length(folds)) {
 test = ldply(folds[i], data.frame)
 train = ldply(folds[-i], data.frame)
 tmp.model = glm(formula , train, family='binomial')
 tmp.predicted_prob = predict(tmp.model, newdata = test, type='response')
 tmp.predict = as.integer(tmp.predicted_prob > 0.5)
 conf.mat = table(test$BagClass, tmp.predict)
 errs[i] = 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 10-fold cross-validation: %.3f percent", 100*mean(errs)))

# with pca_rep
variables =  paste0("Feature", 1:166)
formula <- as.formula(
  paste("BagClass", 
        paste(variables, collapse = " + "), 
        sep = " ~ "))
folds = split(pca_rep_classification_data, cut(sample(1:nrow(pca_rep_classification_data)), 35))
errs = rep(NA, length(folds))

# apply 10-fold cross-validation
for (i in 1:length(folds)) {
 test <- ldply(folds[i], data.frame)
 train <- ldply(folds[-i], data.frame)
 # make crossvalidation, and take the best parameters which gives the least error. The default is k = 10 folds.
 rpart.ranges = list(minsplit=7:10, cp=0.01, minbucket=3:8, maxdepth=8:12)
 tree_tune = tune(rpart, formula, data=train, ranges=rpart.ranges)
 tmp.model <- rpart(formula, train, method = "class",  control=rpart.control(minsplit=tree_tune$best.parameters[,1], 
                                                                            cp=0.01, 
                                                                            minbucket=tree_tune$best.parameters[,3], 
                                                                            maxdepth=tree_tune$best.parameters[,4]))
 tmp.predict <- predict(tmp.model, newdata = test, type = "class")
 conf.mat <- table(test$BagClass, tmp.predict)
 errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 10-fold cross-validation: %.3f percent", 100*mean(errs)))

# with cluster_rep
formula = "BagClass ~ in_first_cluster + in_second_cluster"
folds = split(cluster_rep_classification_data, cut(sample(1:nrow(cluster_rep_classification_data)), 35))
errs = rep(NA, length(folds))

# apply 10-fold cross-validation
for (i in 1:length(folds)) {
 test <- ldply(folds[i], data.frame)
 train <- ldply(folds[-i], data.frame)
 # make crossvalidation, and take the best parameters which gives the least error. The default is k = 10 folds.
 rpart.ranges = list(minsplit=7:10, cp=0.01, minbucket=3:8, maxdepth=8:12)
 tree_tune = tune(rpart, formula, data=train, ranges=rpart.ranges)
 tmp.model <- rpart(formula, train, method = "class",  control=rpart.control(minsplit=tree_tune$best.parameters[,1], 
                                                                            cp=0.01, 
                                                                            minbucket=tree_tune$best.parameters[,3], 
                                                                            maxdepth=tree_tune$best.parameters[,4]))
 tmp.predict <- predict(tmp.model, newdata = test, type = "class")
 conf.mat <- table(test$BagClass, tmp.predict)
 errs[i] <- 1-sum(diag(conf.mat))/sum(conf.mat)
}
print(sprintf("average error using 10-fold cross-validation: %.3f percent", 100*mean(errs)))
