library(ggplot2)
library(scales)
p<-ggplot(diamonds, aes(carat, price, colour=cut))
p<-p + layer(geom = "point")


p<-ggplot(diamonds, aes(x=carat))
p <- p + layer(
  geom="bar", 
               geom_params=list(fill="steelblue"), 
               stat="bin", 
               stat_params=list(binwidth=2) 
)

geom_histogram(binwidth=2, fill="steelblue")

#这里展示了不同的layer的融合
ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point()
#这实际上相当于
qplot(sleep_rem/sleep_total, awake, data=msleep)

#给qplot增加layer
qplot(sleep_rem/sleep_total, awake, data=msleep) + geom_smooth()

#相当于
qplot(sleep_rem/sleep_total, awake, data=msleep, geom=c("point", "smooth"))

ggplot(msleep, aes(sleep_rem/sleep_total, awake)) + geom_point() + geom_smooth()

bestfit <- geom_smooth(method="lm", se=F, colour=alpha("steelblue", 0.5), size=2)
qplot(sleep_rem, sleep_total, data=msleep) + bestfit
qplot(awake, brainwt, data=msleep, log="y") + bestfit
qplot(bodywt, brainwt, data=msleep, log="xy") + bestfit

#相同的构图， 不同的数据的话， 采用%+%符号
p<-ggplot(mtcars, aes(mpg, wt, colour=cyl)) + geom_point()
mtcars <- transform(mtcars, mpg=mpg^2)

p %+% mtcars

#数据映射
aes(x = weight, y = height, colour = age)
aes(weight, height, colour=sqrt(age))
