require(repr)
options(scipen=999)
options(repr.plot.width=15, repr.plot.height=8)

require(data.table)

# assuming you have the XYZ cordinates in your working directory in the following format:
# 'working_directory/'
current_folder = getwd()

file_names = list.files(pattern = "uWaveGestureLibrary")
list_files = lapply(file_names, read.table)

list_files = lapply(seq_along(list_files), function(x) {
  list_files[[x]]$V0 = 1:nrow(list_files[[x]]) # column VO is created to specify the sample instance
  list_files[[x]]
})

# 2, 4, 6 are _TRAIN files
x = as.data.table(list_files[2])
x = x[,-317]
colnames(x) = paste0("X", as.numeric(gsub("V", "", colnames(x)))-1)
setnames(x, "X0", "class")

y = as.data.table(list_files[4])
y = y[,-317]
colnames(y) = paste0("Y", as.numeric(gsub("V", "", colnames(y)))-1)
y = y[,-1]

z = as.data.table(list_files[6])
z = z[,-317]
colnames(z) = paste0("Z", as.numeric(gsub("V", "", colnames(z)))-1)
z = z[,-1]

xyz_train = cbind(x, y, z)
xyz_train[, class:=as.factor(class)]
levels(xyz_train$class) <- c("1", "2", "3", "4", "5", "6", "7", "8")

x = as.data.table(list_files[1])
x = x[,-317]
colnames(x) = paste0("X", as.numeric(gsub("V", "", colnames(x)))-1)
setnames(x, "X0", "class")

y = as.data.table(list_files[3])
y = y[,-317]
colnames(y) = paste0("Y", as.numeric(gsub("V", "", colnames(y)))-1)
y = y[,-1]

z = as.data.table(list_files[5])
z = z[,-317]
colnames(z) = paste0("Z", as.numeric(gsub("V", "", colnames(z)))-1)
z = z[,-1]

xyz_test = cbind(x, y, z)
xyz_test[, class:=as.factor(class)]
levels(xyz_test$class) <- c("1", "2", "3", "4", "5", "6", "7", "8")

head(xyz_train)

head(xyz_test)

train_classes = xyz_train$class
test_classes = xyz_test$class

xyz_train_nn = xyz_train[,2:ncol(xyz_train)]
xyz_test_nn = xyz_test[,2:ncol(xyz_test)]

## Proposed distance measures
# Euclidean distance
# Manhattan distance

# For train set;
# calculate Euclidean distance matrix and store
dist_euc = as.matrix(dist(xyz_train_nn, method="euclidean"))
diag(dist_euc) = 10000
fwrite(dist_euc, "euc_train_dist.csv", col.names=F)

# calculate Manhattan distance matrix and store
dist_mht = as.matrix(dist(xyz_train_nn, method="manhattan"))
diag(dist_mht) = 10000
fwrite(dist_mht, "mht_train_dist.csv", col.names=F)

require(TunePareto)

set.seed(2021)
nof_rep = 10
n_fold = 10
# create test indices to be used in cross-validation
cv_indices = generateCVRuns(train_classes, ntimes = nof_rep, nfold = n_fold, leaveOneOut = FALSE, stratified = TRUE)

# function to find nearest neighbours
nn_classify = function(dist_matrix, train_classes, test_indices, k=1){
    
    test_distances_to_train = dist_matrix[test_indices,]
    test_distances_to_train = test_distances_to_train[,-test_indices]
    train_classes = train_classes[-test_indices]
    # order the distances of each instance to another
    ordered_neighbours = apply(test_distances_to_train, 1, order)
    if(k==1){
        nearest_class = as.numeric(ordered_neighbours[as.numeric(ordered_neighbours[1,])])
        nearest_class = data.table(id=test_indices, nearest_class)
    } else {
        # returns each classes' k nearest neighbours 
        nearest_class = apply(ordered_neighbours[1:k,], 2, function(x) {train_classes[x]})
        nearest_class = data.table(id=test_indices, t(nearest_class))
    }
    
    long_nn_class = melt(nearest_class,'id')
    
    # count the classes of nearest neighbours for each instance
    class_counts = long_nn_class[,.N,list(id,value)]
    # calculate the frequency of each neigbour
    class_counts[,predicted_prob:=N/k]
    wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
    wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
    # returns class which has higher probability (i.e most frequent class) as predicted value
    class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
    
    return(list(prediction = class_predictions, prob_estimates = wide_class_prob_predictions))
    
}

dist_files = list.files(pattern = "train_dist")

dist_files

k_levels = c(1:10)
result = vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))

iter=1
# loop to calculate best k value for nearest neighbour with 10-fold-cross-validation, returns accuracy of each run in each fold
for(m in 1:length(dist_files)){ 
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    for(i in 1:nof_rep){
        this_fold = cv_indices[[i]]
        
        for(j in 1:n_fold){
            test_indices = this_fold[[j]]
            
            for(k in 1:length(k_levels)){
                current_k = k_levels[k]
                current_fold = nn_classify(dist_mat, train_classes, test_indices, k = current_k)
                accuracy = sum(train_classes[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                tmp = data.table(distance_measure = dist_files[m], repid = i, foldid = j, k = current_k, acc = accuracy)
                result[[iter]] = tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}

overall_results = rbindlist(result)
overall_results[,list(avg_accuracy = mean(acc), sdev_accuracy=sd(acc)), by=list(distance_measure, k)]

# For test set;
# calculate Euclidean distance matrix and store
dist_euc = as.matrix(dist(xyz_test_nn, method="euclidean"))
diag(dist_euc) = 10000
fwrite(dist_euc, "euc_test_dist.csv", col.names=F)

# calculate Manhattan distance matrix and store
dist_mht = as.matrix(dist(xyz_test_nn, method="manhattan"))
diag(dist_mht) = 10000
fwrite(dist_mht, "mht_test_dist.csv", col.names=F)

best_k_levels = c(4,4)
dist_files = list.files(pattern = "test_dist")

for(m in 1:length(dist_files)){ 
    print(dist_files[m])
    dist_mat = as.matrix(fread(dist_files[m], header=FALSE))
    
    best_k_level_for_current_approach = best_k_levels[m]
    ordered_neighbours = apply(dist_mat, 1, order)
    nearest_class = apply(ordered_neighbours[1:best_k_level_for_current_approach,], 2, function(x) {test_classes[x]})
    nearest_class = data.table(id=1:nrow(dist_mat), t(nearest_class))

    long_nn_class = melt(nearest_class,'id')
    class_counts = long_nn_class[,.N,list(id,value)]
    class_counts[,predicted_prob:=N/best_k_level_for_current_approach]
    wide_class_prob_predictions = dcast(class_counts, id~value, value.var='predicted_prob')
    wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
    class_predictions = class_counts[,list(predicted = value[which.max(N)]), by=list(id)]
        
    # create confusion matrix and report accuracy
    true_status = test_classes
    confusion_matrix = table(true_status, prediction=class_predictions$predicted)
    print(confusion_matrix)
        
    accuracy = sum(true_status==class_predictions$predicted)/length(class_predictions$predicted)
    print(accuracy)
            
}

 # for example; 4-nearest neighbours of first instances are instances 786, 1056, 1170 and 2825
ordered_neighbours[1:4]

# they belong to the 5,5,4,5 clasess respectively
nearest_class[id==1]

# Among 4 neighbours, 3 of them belong to class 5, 1 of them belongs to class 4. Since the max occurance is 5 among these 4, one can predict the class 1 as 5
class_counts[id==1]

xyz_classification_train = copy(xyz_train)
xyz_classification_train[,from_class3:=as.numeric(class==3)]
xyz_classification_train[,class:=NULL]

xyz_classification_test = copy(xyz_test)
xyz_classification_test[,from_class3:=as.numeric(class==3)]
xyz_classification_test[,class:=NULL]

log_reg = glm(from_class3~., xyz_classification_train, family='binomial')

# fit log_reg
class3_ratio = sum(xyz_classification_train$from_class3==1)/nrow(xyz_classification_train)
predicted_prob = predict(log_reg, xyz_classification_test, type='response')
prediction = as.integer(predicted_prob > class3_ratio)

# create confusion matrix and report accuracy
true_status = xyz_classification_test$from_class3
confusion_matrix = table(true_status, prediction)
confusion_matrix

accuracy = sum(true_status==prediction)/length(prediction)
print(accuracy)

require(glmnet)
require(ggplot2)

classification_matrix = as.matrix(xyz_classification_train[,-c('from_class3'), with=F])
cv_logregfit = cv.glmnet(classification_matrix, xyz_classification_train$from_class3, family='binomial', type.measure='deviance')
plot(cv_logregfit)

# shows the coeffs where lambda selected to be give the minumum binomial deviance
coef(cv_logregfit, s="lambda.min")

# select sample from each class and plot all features as time series in order to find good indicator of class3 
# and compare them with the ones obtained from the cv_logregfit above 
samples = xyz_train[ , .SD[sample(x = .N, size = 1)], by = "class"]

# melt samples for long format
colnames(samples) = c("class", 1:945)
samples = melt(samples, id.vars=c('class'))
samples[,time:=as.numeric(variable)]
samples =samples[,list(class, time, value)]
samples = samples[order(class, time)]
ggplot(samples, aes(x=time, y=value)) + geom_line(aes(colour = class == "3", size = class == "3", group = class)) + 
    scale_color_manual(name = "class", labels = c("Other", "3"), values = c("grey50", "red")) +
    scale_size_manual(name = "class", labels = c("Other", "3"), values = c(0.5, 1))

classification_matrix_test = as.matrix(xyz_classification_test[,-c('from_class3'), with=F])
predicted_prob = predict(cv_logregfit, classification_matrix_test, s='lambda.min', type='response')
prediction = as.integer(predicted_prob > class3_ratio)

# create confusion matrix and report accuracy
true_status = xyz_classification_test$from_class3
confusion_matrix = table(true_status, prediction)
confusion_matrix

accuracy = sum(true_status==prediction)/length(prediction)
print(accuracy)

# calculate distances
dist_euc_train = as.matrix(dist(xyz_classification_train))

head(dist_euc_train)

# ensure that it is a N by N matrix
nrow(dist_euc_train)

ncol(dist_euc_train)

distance_classification_matrix = as.matrix(dist_euc_train)
cv_distance_logregfit = cv.glmnet(distance_classification_matrix, xyz_classification_train$from_class3, family='binomial', type.measure='deviance')
plot(cv_distance_logregfit)

coef(cv_distance_logregfit, s="lambda.min")

require(pracma)

distance_classification_matrix_test = distmat(as.matrix(xyz_classification_test[,-c('from_class3'), with=F]), as.matrix(xyz_classification_train[,-c('from_class3'), with=F]))
colnames(distance_classification_matrix_test) = 1:896
predicted_prob = predict(cv_distance_logregfit, distance_classification_matrix_test, s='lambda.min', type='response')
prediction = as.integer(predicted_prob > class3_ratio)

head(distance_classification_matrix_test)

# ensure that the distance matrix is a Ntest by N
nrow(distance_classification_matrix_test)

ncol(distance_classification_matrix_test)

# create confusion matrix and report accuracy
true_status = xyz_classification_test$from_class3
confusion_matrix = table(true_status, prediction)
confusion_matrix

accuracy = sum(true_status==prediction)/length(prediction)
print(accuracy)


