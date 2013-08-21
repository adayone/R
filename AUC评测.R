library(sqldf)
library(ROCR)

setwd("E:\\AliDrive\\AliDrive\\我的团队协作区\\男女楼层\\DataSet\\dataset_with_id")
true_res <- read.csv("test_expand.csv")



pre_res <- read.csv("gbdt_v1.res", sep="\t")
names(pre_res) <- c("id", "predict")

newdf <- sqldf("select t1.id, predict, label from pre_res t1 left outer join true_res t2 on t1.id = t2.id")

pred <- prediction(newdf$predict, newdf$label)

auc <- performance(pred, "auc")

roc <- performance(pred, "tpr","fpr")

plot(roc)

pr <- performance(pred, "prec", "rec")

plot(pr)