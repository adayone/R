library(ggplot2)
#汽车的缸数， 排量和每加仑行驶的里程数
qplot(displ, hwy, data=mpg, colour=factor(cyl))
#年份作为切面
qplot(displ, hwy, data=mpg, facets = .~year) + geom_smooth()

p <- qplot(displ,hwy, data=mpg, colour = factor(cyl))
summary(p)
#将绘图对象存进磁盘
save(p, file="plot.rdata")
#从磁盘导入
load("plot.rdata")
#将图像存进磁盘
ggsave("d:/plot.png", width=5, height=5)