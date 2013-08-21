### Ideas behind boosting

## 1, Boosting is a general term for methods that try to “reinforce” a weak learner into a better one.

## 2, Rather than picking different training sets, reweight the training set.

## 3, Pick the weights based on which examples were misclassiﬁed previously

## Up until now, Data = {<x_i, y_i>, i = 1, ...,n}

## Now Data = {D(i), <x_i, y_i>, i = 1, ...,n} so that D(i) >= 0 & sum(D(i))=1, i=1,...,n

## 2-split trees: A decision stump makes a prediction based on the value of just a single input feature.

## 4-split trees: split the data into a maximum of 4 groups
#################################################################
################ AdaBoost

## Decision trees training in general

## rpart.control(): Various parameters that control aspects of the ‘rpart’ fit.
## cp complexity parameter controlling pruning. In rpart one should set cp=-1, which forces the tree to split until the depth of the tree achieves the maxdepth setting.

## In small data sets, it is useful to appropriately specify the minsplit argument, in order to ensure that at least one split will be obtained. 

## xval number of cross-validations.

## maxdepth sets the maximum depth of any node of the final tree, with the root node counted as depth 0.

default <- rpart.control()
stump <- rpart.control(cp = -1 , maxdepth = 1 , minsplit = 0, xval=0)
four <- rpart.control(cp = -1 , maxdepth = 2 , minsplit = 0, xval=0)

## Decision tree training w/o boosting

m.basic <- rpart(tp~., data=data.train.all, method="class", control=default)
prediction1 <- predict(m.basic, data.test, type="class")

message("Decision trees with default settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction1)))/length(classes.test)*100, digit=2), "%.")

m.basic.stump <- rpart(tp~., data=data.train.all, method="class", control=stump)
prediction2 <- predict(m.basic.stump, data.test, type="class")

message("Decision trees with stump settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction2)))/length(classes.test)*100, digit=2), "%.")

m.basic.four <- rpart(tp~., data=data.train.all, method="class", control=four)
prediction3 <- predict(m.basic.four, data.test, type="class")

message("Decision trees with four settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction3)))/length(classes.test)*100, digit=2), "%.")


## Decision tree training w/ boosting

## Training

m.boost  <- ada(tp~., data = data.train.all, test.x = data.test, test.y = classes.test, type = "discrete", control = default, iter = 10, loss="e")

prediction4 <- predict(m.boost, data.test, type="class")

message("Decision trees w/ boosting with degault settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction4)))/length(classes.test)*100, digit=2), "%.")

plot(m.boost, test=TRUE, kappa=TRUE)

summary(m.boost, n.iter=5)

#############################################################################
#### ROC curve ####
### Demo http://www.anaesthetist.com/mnm/stats/roc/Findex.htm

### Data

data.all <- read.table("pdt-dep-full-pos-freq-all-tmp.csv", header=T)
set.seed(123)               # just to have the same sample everytime
s <- sample(12208)           # instance permutation; 12,232                                        
indices.test <- s[1:1000]    # test indices  
indices.train <- s[1001:4000]	# train indices

data.train <- data.all[indices.train, -97]
data.train.all <- data.all[indices.train, ]
data.test <- data.all[indices.test, -97]

classes.train <- data.all[indices.train, 97]
classes.test <- data.all[indices.test, 97]


## Decision trees  Training & Prediction
require(rpart)          
default <- rpart.control()
x.rp <- rpart(tp ~ ., data=data.train.all, method="class", control=default)
## plot(x.rp)
## text(x.rp)
x.rp.pred <- predict(x.rp, type="class", newdata=data.test)
x.rp.prob <- predict(x.rp, type="prob", newdata=data.test)

## Bagging Training & Prediction
require(ipred)
x.ip <- bagging(tp ~ ., data=data.train.all)
x.ip.prob <- predict(x.ip, type="prob", newdata=data.test)

## SVM Training & Prediction
require(e1071)
# svm requires tuning
x.svm.tune <- tune(svm, tp~., data = data.train.all,  ranges = list(gamma = 2^(-8:1), cost = 2^(0:4)), tunecontrol = tune.control(sampling = "fix"))

bestGamma <- x.svm.tune$best.parameters[[1]]
bestC <- x.svm.tune$best.parameters[[2]]

x.svm <- svm(tp~., data = data.train.all, cost=bestC, gamma=bestGamma, probability = TRUE)
x.svm.prob <- predict(x.svm, type="prob", newdata=data.test, probability = TRUE)

## draw ROC curves
require(ROCR)

# create an ROCR prediction object from rpart() probabilities
x.rp.prob.rocr <- prediction(x.rp.prob[,2], classes.test)
x.rp.perf <- performance(x.rp.prob.rocr, "tpr","fpr")
plot(x.rp.perf, col=2, main="ROC curves: classification performance of three ML alg.")
auc.rp.list <- performance(x.rp.prob.rocr, "auc")
auc.rp <- unlist(slot(auc.rp.list, "y.values"))

# Draw a legend.
legend(0.6, 0.6, c('rpart', 'bagging','svm'), 2:4)

# bagging
x.ip.prob.rocr <- prediction(x.ip.prob[,2], classes.test)
x.ip.perf <- performance(x.ip.prob.rocr, "tpr","fpr")
plot(x.ip.perf, col=5, add=TRUE)
auc.ip.list <- performance(x.ip.prob.rocr, "auc")
auc.ip <- unlist(slot(auc.ip.list, "y.values"))

# svm
x.svm.prob.rocr <- prediction(attr(x.svm.prob, "probabilities")[,2], classes.test,'Class'])
x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")
plot(x.svm.perf, col=6, add=TRUE)
auc.svm.list <- performance(x.svm.prob.rocr, "auc")
auc.svm <- unlist(slot(auc.svm.list, "y.values"))



#################################################################
################ AdaBoost

## Decision trees training in general

## rpart.control(): Various parameters that control aspects of the ‘rpart’ fit.
## cp complexity parameter controlling pruning. In rpart one should set cp=-1, which forces the tree to split until the depth of the tree achieves the maxdepth setting.

## In small data sets, it is useful to appropriately specify the minsplit argument, in order to ensure that at least one split will be obtained. 

## xval number of cross-validations.

## maxdepth sets the maximum depth of any node of the final tree, with the root node counted as depth 0.

default <- rpart.control()
stump <- rpart.control(cp = -1 , maxdepth = 1 , minsplit = 0, xval=0)
four <- rpart.control(cp = -1 , maxdepth = 2 , minsplit = 0, xval=0)

## Decision tree training w/o boosting

m.basic <- rpart(tp~., data=data.train.all, method="class", control=default)
prediction1 <- predict(m.basic, data.test, type="class")

message("Decision trees with default settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction1)))/length(classes.test)*100, digit=2), "%.")

m.basic.stump <- rpart(tp~., data=data.train.all, method="class", control=stump)
prediction2 <- predict(m.basic.stump, data.test, type="class")

message("Decision trees with stump settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction2)))/length(classes.test)*100, digit=2), "%.")

m.basic.four <- rpart(tp~., data=data.train.all, method="class", control=four)
prediction3 <- predict(m.basic.four, data.test, type="class")

message("Decision trees with four settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction3)))/length(classes.test)*100, digit=2), "%.")


## Decision tree training w/ boosting

## Training

m.boost  <- ada(tp~., data = data.train.all, test.x = data.test, test.y = classes.test, type = "discrete", control = default, iter = 10, loss="e")

prediction4 <- predict(m.boost, data.test, type="class")

message("Decision trees w/ boosting with degault settings: Accuracy on the test set is ", round(sum(diag(table(classes.test, prediction4)))/length(classes.test)*100, digit=2), "%.")

plot(m.boost, test=TRUE, kappa=TRUE)

summary(m.boost, n.iter=5)
