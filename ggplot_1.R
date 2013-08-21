library(ggplot2)
dsmall <- diamonds[sample(nrow(diamonds), 100),]
qplot(carat, price, data=diamonds, alpha= I(1/10))
qplot(carat, price, data=diamonds, alpha= I(1/100))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"))
qplot(carat, price, data = diamonds, geom = c("point", "smooth"))
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span=0.2)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), span=1)

library(mgcv)
qplot(carat, price, data=dsmall, geom=c("point", "smooth"), method="gam", formula = y ~s(x))
qplot(carat, price, data=dsmall, geom=c("point", "smooth"), method="gam", formula = y ~s(x, bs="cs"))
library(splines)
qplot(carat, price, data=dsmall, geom=c("point", "smooth"), method="lm")
library(MASS)
qplot(carat, price, data=dsmall, geom=c("point", "smooth"), method="rlm")
#qqplot(color, price/carat,  data=diamonds, geom="jitter", alpha = I(1/5))
       
#histogram
qplot(carat, data=diamonds, geom="histogram")       
qplot(carat, data=diamonds, geom="density") 
qplot(carat, data=diamonds, geom="histogram", binwidth=1)
qplot(carat, data=diamonds, geom="histogram", binwidth=1, xlim=c(0, 3))
qplot(carat, data=diamonds, geom="histogram", binwidth=0.1, xlim=c(0, 3))
qplot(carat, data=diamonds, geom="density", colour=color)
qplot(carat, data=diamonds, geom="histogram", fill=color)
qplot(color, data=diamonds, geom="bar")
#weight的意思应该是count*weight做为纵轴的意思吧
qplot(color, data=diamonds, geom="bar", weight=carat)
qplot(color, data=diamonds, geom="bar", weight=carat) + scale_y_continuous("carat")


#美国经济数据
#年度失业率
qplot(date, unemploy/pop, data=economics, geom="line")
#平均失业天数
qplot(date, uempmed, data=economics, geom="line")
#融合两个指标
year <- function(x) as.POSIXlt(x) $year + 1900
qplot(unemploy/pop, uempmed, data=economics, geom=c("point", "path"))
qplot(unemploy/pop, uempmed, data=economics, geom="path", colour=year(date)) + scale_area()

#多个facets（切面）展示， 用以对比
qplot(carat, data=diamonds, facets=color~., geom="histogram", binwidth=0.1, xlim=c(0, 3))
qplot(carat, ..density.., data=diamonds, facets=color~., geom="histogram", binwidth=0.1, xlim=c(0, 3))


#plot中的各项设置在ggplot中依然有效
qplot(carat, price, data=dsmall, xlab="Price ($)", ylab="Weight (carats)", main="Price-weight relationship")
qplot(carat, price/carat, data=dsmall, ylab= expression(frac(price, carat)),
      xlab="Weight (carats)",
      main = "Small diamonds",
      xlim=c(0.2, 1))












#test self data   
sceneRes <- read.csv("D:/AliDrive/data/s.res", sep="\t")
names(sceneRes) <- c("step", "is_right", "is_focus")
qplot(is_right, data=sceneRes, geom="histogram", colour=step)
qplot(is_right, data=sceneRes, geom="histogram", facets=step~., binwidth=0.1)
qplot(is_right, ..density.., data=sceneRes, geom="histogram", facets=step~., binwidth=0.1)
qplot(step, data=sceneRes, geom="histogram",  binwidth=0.1)

