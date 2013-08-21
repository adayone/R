library(rpart)
library(ROCR)
library(randomForest)
setwd("E:\\AliDrive\\AliDrive\\我的团队协作区\\男女楼层\\DataSet\\dataset_with_minus_features")
dataset<- read.csv("training.csv")
testset <- read.csv("test_expand.csv")

#testlabel <- testset[1]
#testtrain <- testset[2:38]

ctrl <- rpart.control(cp = -1 , maxdepth = 5 , minsplit = 200, xval=0)
ctrl1 <- rpart.control(cp = -1 , maxdepth = 1 , minsplit = 200, xval=0)
ctrl2 <- rpart.control(cp = -1 , maxdepth = 2 , minsplit = 200, xval=0)
ctrl3 <- rpart.control(cp = -1 , maxdepth = 3 ,minsplit = 2000, xval=0)


func.train <- function(ctrl) {
  dt <- rpart(label ~., method="class", data=dataset, control=ctrl)
  #printcp(dt)
  #plotcp(dt)
  #summary(dt)  
  
  
  return (dt)
}

func.valid <-function(model) {
  res <- c()
  res.pred <- predict(model, type="class", newdata=testset)
  res.prob <- predict(model, type="prob", newdata=testset)
  
res.prob.rocr <- prediction(res.prob[,2], testset$label)
res.perf <- performance(res.prob.rocr, "tpr","fpr")
  plot(res.perf, col=2, main="ROC curves: classification performance")
  res.auc.rp.list <- performance(res.prob.rocr, "auc")
  res.auc.rp <- unlist(slot(res.auc.rp.list, "y.values"))
  print(res.auc.rp)
  res.perf1 <- performance(res.prob.rocr, "prec", "rec")
  plot(res.perf1)
  print(res.perf1)
  return (c(res.perf1, res.perf))
}

func.plot <- function(dt, path) {
  png(filename = path, bg = "transparent")
  plot(dt, uniform=TRUE, main="Classification Tree for Gender Floor")
  text(dt, use.n=TRUE, all=TRUE, cex=.8)

}

dt1 <- func.train(ctrl1)
dt1res <- func.valid(dt1)
dt2 <- func.train(ctrl2)
 dt2res <- func.valid(dt2)
dt3 <- func.train(ctrl)
dt3res <- func.valid(dt3)
dt4 <- func.train(ctrl3)
dt4res <- func.valid(dt4)
dt5 <- rpart(label ~., method="class", data=dataset, control=ctrl, parms = list(prior = c(0.2, 0.8)))
dt5res <- func.valid(dt5)

dt6 <- rpart(label ~., method="class", data=dataset, control=ctrl, parms = list(prior = c(0.22, 0.78)))
dt6res <- func.valid(dt6)

dt7 <- rpart(label ~., method="class", data=dataset, control=ctrl, parms = list(prior = c(0.4, 0.6)))
dt7res <- func.valid(dt7)

plot(dt1res[[2]])
plot(dt2res[[2]])
plot(dt3res[[2]])
plot(dt4res[[2]])





